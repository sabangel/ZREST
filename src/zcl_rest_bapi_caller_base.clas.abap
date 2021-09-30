CLASS zcl_rest_bapi_caller_base DEFINITION
  INHERITING FROM zcl_rest_processor_base
  PUBLIC
  CREATE PUBLIC
  ABSTRACT.

  PUBLIC SECTION.
    INTERFACES zif_rest_bapi_caller.
    ALIASES call FOR zif_rest_bapi_caller~call.

    METHODS constructor
      IMPORTING iv_oper      TYPE zdrestoperacion
                iv_id_fuente TYPE zdserv_source_id
                iv_req_id    TYPE zdrestreqid.

    METHODS get_bapi_name ABSTRACT
      RETURNING VALUE(rv_name) TYPE string.


  PROTECTED SECTION.

    DATA v_id_fuente TYPE zdserv_source_id.
    DATA v_req_id TYPE zdrestreqid.
    DATA v_operacion TYPE zdrestoperacion.

    METHODS process_bapi_result ABSTRACT
      IMPORTING iv_oper              TYPE zdrestoperacion
                iv_id_fuente         TYPE zdserv_source_id
                iv_req_id            TYPE zdrestreqid
                iv_num_doc_ext       TYPE zdrestnumdocext
                it_result            TYPE abap_func_parmbind_tab
      RETURNING VALUE(rv_process_ok) TYPE abap_bool.

    METHODS find_param_name
      IMPORTING iv_struct_name       TYPE zdreststrucabap
                it_table_params      TYPE rsfb_tbl OPTIONAL
                it_import_params     TYPE rsfb_imp OPTIONAL
      RETURNING VALUE(rv_param_data) TYPE rstbl
      RAISING   zcx_rest_ex.

ENDCLASS.

CLASS zcl_rest_bapi_caller_base IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    me->v_operacion = iv_oper.
    me->v_id_fuente = iv_id_fuente.
    me->v_req_id    = iv_req_id.

  ENDMETHOD.

  METHOD call.

    DATA: lt_except    TYPE rsfb_exc,
          lt_export    TYPE rsfb_exp,
          lt_import    TYPE rsfb_imp,
          lt_tables    TYPE rsfb_tbl,
          o_ref_result TYPE REF TO data.

    DATA: t_table_params_target TYPE abap_func_parmbind_tab,
          s_wa_params_target    TYPE abap_func_parmbind,
          etab                  TYPE abap_func_excpbind_tab,
          etab_line             TYPE abap_func_excpbind,
          t_return              TYPE bapiret2_t.

    FIELD-SYMBOLS: <t_params_target> TYPE ANY TABLE,
                   <s_params_target> TYPE any.

    DATA(v_function_name) = CONV rs38l_fnam( get_bapi_name( ) ).

    CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
      EXPORTING
        funcname           = v_function_name
      TABLES
        exception_list     = lt_except " Table of exceptions
        export_parameter   = lt_export " Table of Export Parameters
        import_parameter   = lt_import " Table of Import Parameters
        tables_parameter   = lt_tables " Table With Tables
      EXCEPTIONS
        error_message      = 1
        function_not_found = 2
        invalid_name       = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    TRY.

        LOOP AT it_structs ASSIGNING FIELD-SYMBOL(<s_structs>).

          " Validar que la estructura este en la BAPI para procesarla
          READ TABLE lt_import ASSIGNING FIELD-SYMBOL(<s_import>)  WITH KEY dbfield = <s_structs>-struct_name.
          IF sy-subrc NE 0.
            READ TABLE lt_tables ASSIGNING FIELD-SYMBOL(<s_tables>) WITH KEY dbstruct = <s_structs>-struct_name.
            IF sy-subrc NE 0.
              CONTINUE.
            ENDIF.
          ENDIF.

          AT NEW struct_name.

            IF <s_tables> IS ASSIGNED.
              DATA(s_param_data) = find_param_name( iv_struct_name = <s_structs>-struct_name  it_table_params = lt_tables ).

              "TODO: case segun tipo
              DATA(o_struct_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( <s_structs>-struct_name ) ).
              DATA(o_table_type) = cl_abap_tabledescr=>create( o_struct_descr ).

              CREATE DATA s_wa_params_target-value TYPE HANDLE o_table_type.
              s_wa_params_target-kind = abap_func_tables.
              s_wa_params_target-name = s_param_data-parameter.

            ELSEIF <s_import> IS ASSIGNED.
              s_param_data = find_param_name( iv_struct_name = <s_structs>-struct_name  it_import_params = lt_import ).

              CREATE DATA s_wa_params_target-value TYPE (<s_structs>-struct_name).
              s_wa_params_target-kind = abap_func_exporting.
              s_wa_params_target-name = s_param_data-parameter.
            ENDIF.

          ENDAT.

          IF <s_tables> IS ASSIGNED.

            ASSIGN s_wa_params_target-value->* TO <t_params_target>.
            ASSIGN <s_structs>-struct_ref->* TO FIELD-SYMBOL(<s_struct_ref>).
            INSERT <s_struct_ref> INTO TABLE <t_params_target>.

          ELSEIF <s_import> IS ASSIGNED.

            ASSIGN s_wa_params_target-value->* TO <s_params_target>.
            ASSIGN <s_structs>-struct_ref->* TO FIELD-SYMBOL(<s_fields>).
            <s_params_target> = <s_fields>.

          ENDIF.


          AT END OF struct_name.

            INSERT s_wa_params_target INTO TABLE t_table_params_target.

            FREE s_wa_params_target-value.
            CLEAR s_wa_params_target.

          ENDAT.

          UNASSIGN:<s_import>,
                   <s_tables>.

        ENDLOOP.

        LOOP AT lt_except INTO DATA(s_exc).
          etab_line-name = s_exc-exception.
          ADD 1 TO etab_line-value.
          INSERT etab_line INTO TABLE etab.
        ENDLOOP.

        IF sy-subrc NE 0.
          etab_line-name = 'OTHERS'.
          etab_line-value = 10.
          INSERT etab_line INTO TABLE etab.
        ENDIF.

        " Exporting
        LOOP AT lt_export ASSIGNING FIELD-SYMBOL(<s_export>).

          IF sy-subrc EQ 0.
            s_wa_params_target-name = <s_export>-parameter.
            s_wa_params_target-kind = abap_func_importing.

            CREATE DATA s_wa_params_target-value TYPE (<s_export>-dbfield).
            ASSIGN s_wa_params_target-value->* TO <s_params_target>.
            ASSIGN s_wa_params_target-value->* TO <s_fields>.
            <s_params_target> = <s_fields>.

            INSERT s_wa_params_target INTO TABLE t_table_params_target.

          ENDIF.

        ENDLOOP.

*        " Tabla de Return
        READ TABLE lt_tables ASSIGNING <s_tables> WITH KEY dbstruct = zcl_rest_cte=>c_name_struct-return. "BAPIRET2
        IF sy-subrc EQ 0.
          CREATE DATA o_ref_result TYPE (zcl_rest_cte=>c_name_struct-return). "BAPIRET2
          ASSIGN o_ref_result->* TO FIELD-SYMBOL(<s_field_result>).

          o_struct_descr = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( zcl_rest_cte=>c_name_struct-return ) ). "BAPIRET2
          o_table_type = cl_abap_tabledescr=>create( o_struct_descr ).

          CREATE DATA s_wa_params_target-value TYPE HANDLE o_table_type.
          s_wa_params_target-kind = abap_func_tables.
          s_wa_params_target-name = <s_tables>-parameter.

          ASSIGN s_wa_params_target-value->* TO <t_params_target>.
          INSERT <s_field_result> INTO TABLE <t_params_target>.


          INSERT s_wa_params_target INTO TABLE t_table_params_target.

        ENDIF.

        CALL FUNCTION v_function_name
          PARAMETER-TABLE t_table_params_target
          EXCEPTION-TABLE etab.

        DATA(v_process_ok) = process_bapi_result( iv_oper        = v_operacion
                                                  iv_id_fuente   = v_id_fuente
                                                  iv_req_id      = v_req_id
                                                  iv_num_doc_ext = iv_num_doc_ext
                                                  it_result      = t_table_params_target ).
        IF v_process_ok EQ abap_true.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          rv_process_ok = abap_true.
        ENDIF.

        " Registrar LOG estandar con todos los mensajes retornados en la tabla interna RETURN
        READ TABLE t_table_params_target ASSIGNING FIELD-SYMBOL(<s_table_params_target>) WITH KEY kind = abap_func_tables "30
                                                                                                  name = 'RETURN'.
        IF sy-subrc EQ 0.
          ASSIGN <s_table_params_target>-value->* TO FIELD-SYMBOL(<s_table_params_return>).
          t_return = <s_table_params_return>.

          register_bapi_return_msges( iv_id_fuente   = v_id_fuente
                                      iv_req_id      = v_req_id
                                      iv_num_doc_ext = iv_num_doc_ext
                                      it_msges       = t_return ).
        ENDIF.

      CATCH zcx_rest_ex INTO DATA(o_restex).
        RAISE EXCEPTION o_restex.
      CATCH cx_root INTO DATA(o_rootex).
        RAISE EXCEPTION TYPE zcx_rest_ex
          EXPORTING
            previous = o_rootex.
    ENDTRY.

  ENDMETHOD.

  METHOD find_param_name.

    IF it_table_params[] IS NOT INITIAL.

      READ TABLE it_table_params WITH KEY dbstruct = iv_struct_name ASSIGNING FIELD-SYMBOL(<s_tables>).
      IF sy-subrc NE 0.

        RAISE EXCEPTION TYPE zcx_rest_ex
          EXPORTING
            textid = zcx_rest_ex=>int_err
            p1     = 'ZCL_REST_BAPI_CALLER_BASE->find_param_name'.

      ENDIF.

      rv_param_data = <s_tables>.

    ELSEIF it_import_params[] IS NOT INITIAL.

      READ TABLE it_import_params WITH KEY dbfield = iv_struct_name ASSIGNING FIELD-SYMBOL(<s_imports>).
      IF sy-subrc NE 0.

        RAISE EXCEPTION TYPE zcx_rest_ex
          EXPORTING
            textid = zcx_rest_ex=>int_err
            p1     = 'ZCL_REST_BAPI_CALLER_BASE->find_param_name'.

      ENDIF.

      MOVE-CORRESPONDING <s_imports> TO rv_param_data.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
