CLASS zcl_rest_creator_base DEFINITION
  INHERITING FROM zcl_rest_processor_base
  PUBLIC
  CREATE PUBLIC
  ABSTRACT.

  PUBLIC SECTION.

    INTERFACES zif_rest_creator .
    INTERFACES if_serializable_object .
    INTERFACES zif_prc_runnable .

    INTERFACES zif_rest_finder.

    ALIASES create
      FOR zif_rest_creator~create .

    "! Constructor
    "! @parameter io_serv_dao  | Fuiente de info para match campos
    "! @parameter iv_oper      | Operación a realizar
    "! @parameter iv_id_fuente | Identificador de la fuente de la petición
    METHODS constructor
      IMPORTING iv_oper      TYPE zdrestoperacion
                iv_id_fuente TYPE zdserv_source_id.

    "! La clase hija debe retornar el adaptador que será usado en cada caso particular
    "! @parameter ro_validator | validador
    METHODS init_adapter ABSTRACT
      IMPORTING iv_oper      TYPE zdrestoperacion
                iv_id_fuente TYPE zdserv_source_id
                iv_req_id    TYPE zdrestreqid
      CHANGING  co_adapter   TYPE REF TO zcl_rest_adapter_base.

    "! La clase hija debe retornar el caller para la bapi respectiva
    "! @parameter ro_caller | bapi caller
    METHODS init_bapi_caller ABSTRACT
      IMPORTING iv_oper      TYPE zdrestoperacion
                iv_id_fuente TYPE zdserv_source_id
                iv_req_id    TYPE zdrestreqid
      CHANGING  co_caller    TYPE REF TO zcl_rest_bapi_caller_base.

    "! Retorna el objeto que realiza el registro del resultado.
    "! @parameter ro_reg | Register
    METHODS init_register
      IMPORTING iv_id_fuente TYPE zdserv_source_id
                iv_req_id    TYPE zdrestreqid
      CHANGING  co_reg       TYPE REF TO zif_rest_register.

    METHODS register_internal_err
      IMPORTING iv_msg     TYPE bapi_msg OPTIONAL
                iv_msg_num TYPE i        OPTIONAL
                iv_p1      TYPE string   OPTIONAL
                iv_p2      TYPE string   OPTIONAL.

    METHODS init_find
      IMPORTING iv_oper      TYPE zdrestoperacion
                iv_id_fuente TYPE zdserv_source_id
      CHANGING  co_finder    TYPE REF TO zcl_sd_order_rest_finder.

  PROTECTED SECTION.

    DATA o_adapter     TYPE REF TO zcl_rest_adapter_base.
    DATA o_bapi_caller TYPE REF TO zcl_rest_bapi_caller_base.
    DATA o_register    TYPE REF TO zif_rest_register .
    DATA o_rest_dao    TYPE REF TO zif_rest_sys_field_dao.
    DATA v_req_id      TYPE zdrestreqid.
    DATA v_id_fuente   TYPE zdserv_source_id.
    DATA v_payload     TYPE string.
    DATA v_oper        TYPE zdrestoperacion.
    DATA util          TYPE REF TO zcl_rest_util.
    DATA cte           TYPE REF TO zcl_rest_cte.

    DATA o_doc_storer TYPE REF TO zcl_rest_doc_storer.

    METHODS init_instances.

ENDCLASS.



CLASS zcl_rest_creator_base IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    init_instances( ).

    me->v_oper = iv_oper.
    me->v_id_fuente = iv_id_fuente.

  ENDMETHOD.


  METHOD init_find.

  ENDMETHOD.


  METHOD init_instances.
    me->o_rest_dao = NEW zcl_rest_sys_field_dao( ).
    me->cte = NEW zcl_rest_cte( ).
    me->util = NEW #( ).

    me->o_doc_storer = NEW zcl_rest_doc_storer( ).
  ENDMETHOD.


  METHOD init_register.
    co_reg = NEW zcl_rest_register_def( iv_id_fuente = iv_id_fuente iv_req_id = iv_req_id ).
  ENDMETHOD.


  METHOD register_internal_err.

    IF iv_msg IS SUPPLIED.
      DATA(v_msg) = iv_msg.
    ELSE.
      MESSAGE ID 'ZREST' TYPE 'E' NUMBER iv_msg_num WITH iv_p1 iv_p2 INTO v_msg.
    ENDIF.

    append_reg( iv_req_id         = me->v_req_id
                iv_id_fuente      = me->v_id_fuente
                iv_num_doc_ext    = CONV #( '0' )
                iv_status         = cte->c_status_doc-err_interno
                iv_operacion      = me->v_oper
                iv_message        = v_msg ).
  ENDMETHOD.


  METHOD zif_prc_runnable~run.
    TRY.
        me->create( iv_payload = me->v_payload  iv_req_id = me->v_req_id ).
      CATCH zcx_rest_ex INTO DATA(o_ex).
        register_bapi_return_msges(
            iv_id_fuente   = me->v_id_fuente
            iv_req_id      = me->v_req_id
            iv_num_doc_ext = '0'
            it_msges       = VALUE #( ( type = 'E'  message = o_ex->get_text( ) ) )
        ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_rest_creator~create.

    DATA v_num_doc_ext TYPE zdrestnumdocext.
    FIELD-SYMBOLS <t_docs> TYPE ANY TABLE.
    FIELD-SYMBOLS <s_ref> TYPE any.
    FIELD-SYMBOLS <s_ref_info> TYPE any.
    DATA t_data_reproceso TYPE cte->tt_reproceso.

    TRY.
        "La variable de instancia payload indica si el creator está siendo llamado desde afuera.
        "Si está vacía se inicia el proceso interno en un hilo de ejecución aparte
        IF me->v_payload IS INITIAL.
          "Invocando proceso en paralelo
          me->v_payload = iv_payload.
          me->v_req_id  = iv_req_id.
          NEW zcl_prc_isolated_task( me )->start( ).
          RETURN.
        ENDIF.

        init_instances( ).

        DATA(r_payload) = /ui2/cl_json=>generate( json = me->v_payload ).
        IF r_payload IS INITIAL.
          register_internal_err( iv_msg_num = '001' iv_p1 = 'ZCL_REST_CREATOR_BASE->CREATE' ).
          RAISE EXCEPTION TYPE zcx_rest_ex
            EXPORTING
              textid = zcx_rest_ex=>bad_json
              p1     = 'ZCL_REST_CREATOR_BASE->CREATE'.
          RETURN.
        ENDIF.

        " Obtener el identificador de la petición
        ASSIGN r_payload->(cte->c_special_field-informacion_msg) TO <s_ref>.

        me->o_doc_storer->r_payload = r_payload.

        init_adapter( EXPORTING iv_oper      = me->v_oper
                                iv_id_fuente = me->v_id_fuente
                                iv_req_id    = me->v_req_id
                       CHANGING co_adapter   = me->o_adapter ).

        init_bapi_caller( EXPORTING iv_oper      = me->v_oper
                                    iv_id_fuente = me->v_id_fuente
                                    iv_req_id    = me->v_req_id
                           CHANGING co_caller = me->o_bapi_caller ).

        init_register( EXPORTING iv_id_fuente = me->v_id_fuente
                                 iv_req_id    = me->v_req_id
                        CHANGING co_reg       = me->o_register ).

        me->o_register->add_registrable( me->o_adapter ).
        me->o_register->add_registrable( me->o_bapi_caller ).
        me->o_register->add_registrable( me ).

        ASSIGN r_payload->(cte->c_special_field-documentos) TO <s_ref>.
        ASSIGN <s_ref>->* TO <t_docs>.  "Tabla de documentos raiz

        " Recorrido de la tabla que contiene las estructuras de cada DOC
        LOOP AT <t_docs> ASSIGNING <s_ref>.

          DATA(t_bapi_stucts) = me->o_adapter->adapt( ir_doc = <s_ref> ).

          util->get_val_from_str_ref( EXPORTING ir_ref = <s_ref> iv_fldname = CONV #( cte->c_special_field-doc_ref_externo )
                                      IMPORTING ev_val = v_num_doc_ext ).

          " si v_process_ok es vacio se almacena el registro para ser reprocesado posteriormente.
          DATA(v_process_ok) = me->o_bapi_caller->call( iv_num_doc_ext = v_num_doc_ext it_structs = t_bapi_stucts ).
          IF v_process_ok = abap_false.
            me->o_doc_storer->add_doc( EXPORTING ir_doc = <s_ref> iv_req_id = me->v_req_id CHANGING ct_reproceso = t_data_reproceso ).
          ENDIF.

          me->o_register->collect( iv_commit = abap_true ).

          CLEAR v_process_ok.

        ENDLOOP.

        " Guardar registros en la tabla de reproceso
        me->o_doc_storer->save_docs( CHANGING ct_reproceso = t_data_reproceso ).

      CATCH zcx_rest_ex INTO DATA(o_restex).
        register_internal_err( iv_msg = CONV #( o_restex->get_text( ) ) ).
        RAISE EXCEPTION o_restex.
      CATCH cx_root INTO DATA(o_rootex).
        register_internal_err( iv_msg = CONV #( o_rootex->get_text( ) ) ).
        RAISE EXCEPTION TYPE zcx_rest_ex
          EXPORTING
            textid   = zcx_rest_ex=>int_err
            previous = o_rootex
            p1       = 'ZCL_REST_CREATOR_BASE->CREATE'.
        RETURN.
    ENDTRY.

    me->o_register->collect( iv_commit = abap_true ).

  ENDMETHOD.


  METHOD zif_rest_finder~find.

    TRY.


      CATCH zcx_rest_ex INTO DATA(o_restex).
        register_internal_err( iv_msg = CONV #( o_restex->get_text( ) ) ).
        RAISE EXCEPTION o_restex.
      CATCH cx_root INTO DATA(o_rootex).
        register_internal_err( iv_msg = CONV #( o_rootex->get_text( ) ) ).
        RAISE EXCEPTION TYPE zcx_rest_ex
          EXPORTING
            textid   = zcx_rest_ex=>int_err
            previous = o_rootex
            p1       = 'ZCL_REST_CREATOR_BASE->CREATE'.
        RETURN.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
