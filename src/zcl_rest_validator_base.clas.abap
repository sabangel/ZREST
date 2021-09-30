CLASS zcl_rest_validator_base DEFINITION
  PUBLIC
  CREATE PUBLIC
  ABSTRACT.

  PUBLIC SECTION.

    INTERFACES zif_rest_validator.
    ALIASES validate FOR zif_rest_validator~validate.

    "! Constructor
    "! @parameter iv_oper | Operación de negocio a realizar
    METHODS constructor
      IMPORTING iv_oper TYPE zdrestoperacion.

  PROTECTED SECTION.

    DATA o_req       TYPE REF TO if_rest_request.
    DATA v_oper      TYPE zdrestoperacion.
    DATA v_id_fuente TYPE zdserv_source_id.
    DATA cte         TYPE REF TO zcl_rest_cte.
    DATA sys_field_dao TYPE REF TO zif_rest_sys_field_dao.

    "! Validación genérica del método POST.
    "! Incluye la validación de la estructura genérica del payload
    METHODS validate_post
      EXPORTING et_msg       TYPE bapiret2_t
      RETURNING VALUE(rv_ok) TYPE abap_bool.

    "! Validación genérica del método GET.
    "! Se valida el formato de la URL
    METHODS validate_get
      EXPORTING et_msg       TYPE bapiret2_t
      RETURNING VALUE(rv_ok) TYPE abap_bool.

    "! Validación particular según el método y el sistema fuente
    METHODS validate_post_customized ABSTRACT
      IMPORTING it_docs      TYPE ANY TABLE
      EXPORTING et_msg       TYPE bapiret2_t
      RETURNING VALUE(rv_ok) TYPE abap_bool.

    METHODS append_msg
      IMPORTING iv_id   TYPE symsgid    DEFAULT zcl_rest_cte=>c_clase_msgs_def
                iv_ty   TYPE bapi_mtype DEFAULT 'E'
                iv_num  TYPE symsgno
                iv_v1   TYPE symsgv OPTIONAL
                iv_v2   TYPE symsgv OPTIONAL
                iv_v3   TYPE symsgv OPTIONAL
                iv_v4   TYPE symsgv OPTIONAL
      CHANGING  ct_msgs TYPE bapiret2_t .

  PRIVATE SECTION.

    DATA tr_num_doc_ext TYPE RANGE OF zdrestnumdocext.

ENDCLASS.


CLASS zcl_rest_validator_base IMPLEMENTATION.

  METHOD constructor.

    me->v_oper = iv_oper.

    me->cte = NEW zcl_rest_cte( ).

    me->sys_field_dao = NEW zcl_rest_sys_field_dao(  ).

  ENDMETHOD.


  METHOD zif_rest_validator~validate.

    rv_ok = abap_true.

    me->o_req = io_req.

    CLEAR et_msg.

    "----------------------------------------------------
    "Validaciones comunes a todos los métodos
    "----------------------------------------------------
    "id operación debe existir en la url
    DATA(t_uri_segs) = me->o_req->get_uri_segments( ).
    READ TABLE t_uri_segs INDEX 1 ASSIGNING FIELD-SYMBOL(<v_ext_oper_id>).
    IF <v_ext_oper_id> IS NOT ASSIGNED.
      APPEND VALUE #( id = cte->c_clase_msgs_def type = 'E' number = '026' ) TO et_msg.
    ELSE.
      "La operación debe existir en la parametrización interna
      READ TABLE cte->t_operations WITH KEY extern_name = <v_ext_oper_id> TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        APPEND VALUE #( id = cte->c_clase_msgs_def type = 'E' number = '022' message_v1 = <v_ext_oper_id> ) TO et_msg.
      ENDIF.
    ENDIF.

    "Debe venir el sistema fuente y estar definido internamente
    DATA(v_id_fuente) = me->o_req->get_header_field( cte->c_special_field-fuente ).
    READ TABLE cte->t_sources WITH KEY id = v_id_fuente TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      APPEND VALUE #( id = cte->c_clase_msgs_def type = 'E' number = '023' message_v1 = v_id_fuente ) TO et_msg.
    ENDIF.

    me->v_id_fuente = v_id_fuente.

    "----------------------------------------------------
    "Validaciones según el métodos REST
    "----------------------------------------------------
    DATA t_msg TYPE bapiret2_t.
    CASE me->o_req->get_method( ).

      WHEN io_req->gc_method_post.

        rv_ok = validate_post( IMPORTING et_msg = t_msg ).

      WHEN io_req->gc_method_get.

        rv_ok = validate_get( IMPORTING et_msg = t_msg ).

    ENDCASE.

    APPEND LINES OF t_msg TO et_msg.

  ENDMETHOD.

  METHOD validate_post.

    DATA v_req_id   TYPE zdrestreqid.
    DATA v_version  TYPE string.
    DATA v_doc_ext  TYPE string.
    DATA t_doc_exts TYPE STANDARD TABLE OF string.
    DATA t_msg      TYPE bapiret2_t.

    FIELD-SYMBOLS <t_docs>     TYPE ANY TABLE.
    FIELD-SYMBOLS <t_docs_aux> TYPE ANY TABLE.

    TRY.
        "Identificador de request es obligatorio
        v_req_id = me->o_req->get_header_field( cte->c_special_field-id_peticion ).
        IF v_req_id IS INITIAL.
          append_msg( EXPORTING iv_num = '045' CHANGING ct_msgs = et_msg ).
          RETURN.
        ENDIF.

        "Payload es obligatorio
        DATA(v_payload) = me->o_req->get_entity( )->get_string_data( ).
        TRY.
            DATA(r_payload) = /ui2/cl_json=>generate( json = v_payload ).
          CATCH cx_root INTO DATA(o_syex1).
            "JSON está mal formado no seguir
            append_msg( EXPORTING iv_num = '001' CHANGING ct_msgs = et_msg ).
            RETURN.
        ENDTRY.
        IF r_payload IS INITIAL.
          append_msg( EXPORTING iv_num = '027' CHANGING ct_msgs = et_msg ).
          rv_ok = abap_false.
          RETURN.
        ENDIF.

        "Estructura info mensaje es obligatoria
        ASSIGN r_payload->(cte->c_special_field-informacion_msg) TO FIELD-SYMBOL(<r_msg_info>).
        IF <r_msg_info> IS NOT ASSIGNED.
          append_msg( EXPORTING iv_num = '029' iv_v1 = CONV #( cte->c_special_field-informacion_msg ) CHANGING ct_msgs = et_msg ).
        ELSE.
          "Identificador de versión es obligatorio y debe ser 1.0
          TRY.
              zcl_rest_util=>get_val_from_str_ref( EXPORTING ir_ref = <r_msg_info> iv_fldname = CONV #( cte->c_special_field-version_serv )
                                                   IMPORTING ev_val = v_version ).
              IF v_version NE cte->c_versions-stable_1.
                append_msg( EXPORTING iv_num = '028' iv_v1 = CONV #( v_version ) CHANGING ct_msgs = et_msg ).
              ENDIF.
            CATCH zcx_rest_ex.
              append_msg( EXPORTING iv_num = '020' iv_v1 = CONV #( cte->c_special_field-version_serv ) CHANGING ct_msgs = et_msg ).
          ENDTRY.
        ENDIF.

        "Lista de documentos es obligatoria
        ASSIGN r_payload->(cte->c_special_field-documentos) TO FIELD-SYMBOL(<r_docs>).
        IF <r_docs> IS NOT ASSIGNED.
          append_msg( EXPORTING iv_num = '029' iv_v1 = CONV #( cte->c_special_field-documentos ) CHANGING ct_msgs = et_msg ).
        ELSE.
          TRY.
              "Estructura docs[] debe ser una lista
              ASSIGN <r_docs>->* TO FIELD-SYMBOL(<dref>).
              IF <dref> IS NOT ASSIGNED.
                append_msg( EXPORTING iv_num = '031' iv_v1 = CONV #( cte->c_special_field-documentos ) CHANGING ct_msgs = et_msg ).
                rv_ok = abap_false.
                RETURN.
              ENDIF.
              TRY.
                  DATA(o_table_desc) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <dref> ) ).
                CATCH cx_root.
                  append_msg( EXPORTING iv_num = '031' iv_v1 = CONV #( cte->c_special_field-documentos ) CHANGING ct_msgs = et_msg ).
                  RETURN.
              ENDTRY.

              ASSIGN <r_docs>->* TO <t_docs>.
              "Por cada posición de docs[] debe existir un número de doc externo y ser diferente
              LOOP AT <t_docs> ASSIGNING FIELD-SYMBOL(<r_doc>).
                TRY.
                    zcl_rest_util=>get_val_from_str_ref(
                        EXPORTING ir_ref = <r_doc>  iv_fldname = CONV #( cte->c_special_field-doc_ref_externo )
                        IMPORTING ev_val = v_doc_ext ).
                    IF v_doc_ext IS INITIAL.
                      append_msg( EXPORTING iv_num = '020' iv_v1 = CONV #( cte->c_special_field-doc_ref_externo ) CHANGING ct_msgs = et_msg ).
                      RETURN.
                    ENDIF.
                    APPEND v_doc_ext TO t_doc_exts.

                    APPEND INITIAL LINE TO tr_num_doc_ext ASSIGNING FIELD-SYMBOL(<s_num_doc_ext>).
                    <s_num_doc_ext>-sign   = 'I'.
                    <s_num_doc_ext>-option = 'EQ'.
                    <s_num_doc_ext>-low    = v_doc_ext.

                  CATCH zcx_rest_ex.
                    append_msg( EXPORTING iv_num = '020' iv_v1 = CONV #( cte->c_special_field-doc_ref_externo ) CHANGING ct_msgs = et_msg ).
                    RETURN.
                ENDTRY.
              ENDLOOP.

              " Obtenemos los documentos externos ya procesados en SAP con estatus OK para evitar duplicidad
              DATA(t_duplicate) = me->sys_field_dao->find_duplicate_documents(
                        iv_id_fuente = me->cte->t_sources[ 1 ]-id
                        iv_operacion = me->v_oper
                        it_doc_ext   = CONV #( tr_num_doc_ext )
                    ).

              " Si la tabla esta llena, iteramos los registros que se encontraron y llenamos la tabla et_msg para construir
              " el JSON que se mostrara al sistema externo, para que revisen y vuelva a enviar si es necesario.
              IF t_duplicate IS NOT INITIAL.

                LOOP AT t_duplicate ASSIGNING FIELD-SYMBOL(<s_duplicate>).
                  append_msg( EXPORTING iv_num = '046' iv_v1 = CONV #( <s_duplicate> ) CHANGING ct_msgs = et_msg ).
                ENDLOOP.

                RETURN.

              ENDIF.

              IF t_doc_exts IS INITIAL.
                append_msg( EXPORTING iv_num = '032' iv_v1 = CONV #( cte->c_special_field-doc_ref_externo ) CHANGING ct_msgs = et_msg ).
                RETURN.
              ENDIF.

              "Detectar duplicados
*              SORT t_doc_exts.
*              DELETE ADJACENT DUPLICATES FROM t_doc_exts.
*              IF sy-subrc EQ 0.
*                append_msg( EXPORTING iv_num = '033' iv_v1 = CONV #( cte->c_special_field-doc_ref_externo ) CHANGING ct_msgs = et_msg ).
*              ENDIF.

            CATCH cx_root.
              append_msg( EXPORTING iv_num = '029' iv_v1 = CONV #( cte->c_special_field-documentos ) CHANGING ct_msgs = et_msg ).
          ENDTRY.
        ENDIF.

        rv_ok = COND #( WHEN et_msg IS NOT INITIAL THEN abap_false ELSE abap_true ).

        "Si no pasa la validación genérica no hacer la específica
        IF rv_ok EQ abap_false.
          RETURN.
        ENDIF.

        rv_ok = validate_post_customized( EXPORTING it_docs = <t_docs> IMPORTING et_msg = t_msg ).

        APPEND LINES OF t_msg TO et_msg.

      CATCH zcx_rest_ex INTO DATA(o_rootex).
        "Error interno desconocido
        append_msg( EXPORTING iv_num = '030' iv_v1 = CONV #( o_rootex->get_text( ) ) CHANGING ct_msgs = et_msg ).
        rv_ok = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD validate_get.

    DATA t_msg      TYPE bapiret2_t.
    DATA v_fecha_ini_get TYPE d.
    DATA v_fecha_fin_get TYPE d.

    TRY.

        " Obtener de las tablas de configuración los posibles valores que llegan desde los parametros del metodo GET
        DATA(t_fields) = me->sys_field_dao->find_source_fields_get(
                     iv_operacion = me->v_oper
                     iv_id_fuente = me->v_id_fuente
                     iv_seccion   = me->cte->c_metodos-get
                 ).

        v_fecha_ini_get = me->o_req->get_uri_query_parameter( iv_name = me->cte->c_special_field-fecha ).
        v_fecha_fin_get = me->o_req->get_uri_query_parameter( iv_name = me->cte->c_special_field-fecha_fin ).

        " Validamos que el rango a consultar sea menor a 31 días
        DATA(v_dias) = v_fecha_fin_get - v_fecha_ini_get.
        IF v_dias > 31.
          append_msg( EXPORTING iv_num = '048' CHANGING ct_msgs = et_msg ).
          RETURN.
        ENDIF.

        " De los campos opcionales debe ir uno como minimo
        " Opcional
        LOOP AT t_fields ASSIGNING FIELD-SYMBOL(<s_fields>).
          DATA(v_parametro_get) = me->o_req->get_uri_query_parameter( iv_name = CONV #( <s_fields>-campo_hol ) ).
          IF v_parametro_get IS NOT INITIAL.
            rv_ok = abap_true.
            RETURN.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDLOOP.

        append_msg( EXPORTING iv_num = '047' iv_v1 = CONV  #( me->cte->c_special_field-id_peticion )
                                             iv_v2 = CONV  #( me->cte->c_special_field-doc_ref_externo )
                                             iv_v3 = | { me->cte->c_special_field-fecha }-{ me->cte->c_special_field-fecha_fin }| CHANGING ct_msgs = et_msg ).
        APPEND LINES OF t_msg TO et_msg.

      CATCH zcx_rest_ex INTO DATA(o_rootex).
        "Error interno desconocido
        append_msg( EXPORTING iv_num = '030' iv_v1 = CONV #( o_rootex->get_text( ) ) CHANGING ct_msgs = et_msg ).
        rv_ok = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD append_msg.

    APPEND VALUE #( id         = iv_id
                    type       = iv_ty
                    number     = iv_num
                    message_v1 = COND #( WHEN iv_v1 IS SUPPLIED THEN iv_v1 ELSE '' )
                    message_v2 = COND #( WHEN iv_v1 IS SUPPLIED THEN iv_v2 ELSE '' )
                    message_v3 = COND #( WHEN iv_v1 IS SUPPLIED THEN iv_v3 ELSE '' )
                    message_v4 = COND #( WHEN iv_v1 IS SUPPLIED THEN iv_v4 ELSE '' ) ) TO ct_msgs.

  ENDMETHOD.

ENDCLASS.
