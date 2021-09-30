CLASS zcl_rest_finder_base DEFINITION ABSTRACT
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_rest_finder.
    ALIASES find FOR zif_rest_finder~find.

    METHODS constructor
      IMPORTING io_dao TYPE REF TO zif_rest_finder_dao OPTIONAL.

  PROTECTED SECTION.

    METHODS find_complements ABSTRACT
      EXPORTING er_result TYPE REF TO data.

    METHODS populate_initial
      IMPORTING iv_oper                TYPE zdrestoperacion
                iv_id_fuente           TYPE zdserv_source_id
                it_url_attrib          TYPE tihttpnvp
      RETURNING VALUE(rs_initial_data) TYPE zsrestgetresponsedata.

    METHODS get_standard_log
      IMPORTING iv_id_fuente       TYPE zdserv_source_id
                iv_req_id          TYPE zdrestreqid
                iv_num_doc_ext     TYPE zdrestnumdocext
                iv_fecha           TYPE string
      RETURNING VALUE(rt_mensajes) TYPE ztrestmensajeget.


  PRIVATE SECTION.
    DATA r_complemented_data TYPE REF TO data.
    DATA o_finder_dao        TYPE REF TO zif_rest_finder_dao.

ENDCLASS.

CLASS zcl_rest_finder_base IMPLEMENTATION.

  METHOD constructor.
    IF io_dao IS NOT SUPPLIED.
      me->o_finder_dao = NEW zcl_rest_finder_dao( ).
    ELSE.
      me->o_finder_dao = io_dao.
    ENDIF.
  ENDMETHOD.

  METHOD zif_rest_finder~find.

    DATA          s_complement        TYPE REF TO data.
    FIELD-SYMBOLS <s_generic_initial> TYPE data.

    TRY.

        find_complements( IMPORTING er_result = s_complement ).

        DATA(s_initial_data) = populate_initial( iv_oper = iv_oper
                                                 iv_id_fuente  = iv_id_fuente
                                                 it_url_attrib = it_url_attrib ).

        IF s_complement IS INITIAL.

          CREATE DATA me->r_complemented_data LIKE s_initial_data.
          ASSIGN me->r_complemented_data->* TO <s_generic_initial>.

          <s_generic_initial> = s_initial_data.
          r_result = me->r_complemented_data.
          RETURN.
        ENDIF.

        "Si viene información complementaria crear el tipo extendido y poblarlo
        DATA(t_components) = CAST cl_abap_structdescr(
            cl_abap_typedescr=>describe_by_data( s_initial_data ) )->get_components( ).

        DATA(o_complement_descr) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( s_complement ) ).

        APPEND VALUE #( name = zcl_rest_cte=>c_special_field-complement_data  type = o_complement_descr )
            TO t_components.

        "Nuevo tipo con los componentes iniciales más los complementarios
        DATA(o_dyn_type) = cl_abap_structdescr=>create( p_components = t_components ).

        CREATE DATA me->r_complemented_data TYPE HANDLE o_dyn_type.

        ASSIGN me->r_complemented_data->(zcl_rest_cte=>c_special_field-complement_data)
            TO FIELD-SYMBOL(<s_complement_data>).

        <s_complement_data> = s_complement.

        "Poblar la salida con los valores iniciales
        LOOP AT t_components ASSIGNING FIELD-SYMBOL(<s_component>).

          IF <s_component>-name EQ zcl_rest_cte=>c_special_field-complement_data.
            CONTINUE.
          ENDIF.

          ASSIGN COMPONENT <s_component>-name OF STRUCTURE s_initial_data TO FIELD-SYMBOL(<v_value_initial>).
          ASSIGN me->r_complemented_data->(<s_component>-name) TO FIELD-SYMBOL(<v_value_target>).
          <v_value_target> = <v_value_initial>.

        ENDLOOP.

        r_result = me->r_complemented_data.


      CATCH cx_sy_no_handler.
        "TODO: TRATAR EXCEPCIÓN
    ENDTRY.

  ENDMETHOD.

  METHOD populate_initial.

    DATA t_result TYPE zcl_rest_cte=>tt_datos_log.
    DATA tr_req_id      TYPE etrange_string_tab.
    DATA tr_num_doc_ext TYPE etrange_string_tab.
    DATA tr_fecha       TYPE etrange_string_tab.
    DATA tr_status      TYPE etrange_string_tab.

    DATA s_doc TYPE zsrestdocumentosget.
    DATA s_men TYPE zsrestmensajeget.

    LOOP AT it_url_attrib ASSIGNING FIELD-SYMBOL(<s_url_attrib>).

      CASE <s_url_attrib>-name.
        WHEN zcl_rest_cte=>c_special_field-id_peticion.

          APPEND INITIAL LINE TO tr_req_id ASSIGNING FIELD-SYMBOL(<s_req_id>).
          <s_req_id>-sign   = 'I'.
          <s_req_id>-option = 'EQ'.
          <s_req_id>-low    = <s_url_attrib>-value.

        WHEN zcl_rest_cte=>c_special_field-doc_ref_externo.

          APPEND INITIAL LINE TO tr_num_doc_ext ASSIGNING FIELD-SYMBOL(<s_num_doc_ext>).
          <s_num_doc_ext>-sign   = 'I'.
          <s_num_doc_ext>-option = 'EQ'.
          <s_num_doc_ext>-low    = <s_url_attrib>-value.

        WHEN zcl_rest_cte=>c_special_field-fecha.

          APPEND INITIAL LINE TO tr_fecha ASSIGNING FIELD-SYMBOL(<s_fecha>).

          <s_fecha>-sign   = 'I'.
          <s_fecha>-option = 'BT'.
          <s_fecha>-low  = |{ <s_url_attrib>-value }000000|.
          <s_fecha>-high = |{ <s_url_attrib>-value }235959|.

        WHEN zcl_rest_cte=>c_special_field-fecha_fin.

          <s_fecha>-high = |{ <s_url_attrib>-value }235959|.

        WHEN zcl_rest_cte=>c_special_field-status.
          APPEND INITIAL LINE TO tr_status ASSIGNING FIELD-SYMBOL(<s_status>).
          <s_status>-sign   = 'I'.
          <s_status>-option = 'EQ'.
          <s_status>-low    = <s_url_attrib>-value.

      ENDCASE.

    ENDLOOP.

    TRY.

        me->o_finder_dao->find_data_log(
          EXPORTING
              iv_oper        = iv_oper
              iv_id_fuente   = iv_id_fuente
              ir_req_id      = tr_req_id
              ir_num_doc_ext = tr_num_doc_ext
              ir_fecha       = tr_fecha
              ir_status      = tr_status
          CHANGING
              ct_result      = t_result ).

        LOOP AT t_result ASSIGNING FIELD-SYMBOL(<s_result>).

          " Si existe error revisamos si hay datos en el log estandar
          IF <s_result>-status NE zcl_rest_cte=>c_status_doc-ok.

            DATA(t_mensajes) = get_standard_log(
                      iv_id_fuente   = iv_id_fuente
                      iv_req_id      = <s_result>-req_id
                      iv_num_doc_ext = <s_result>-num_doc_ext
                      iv_fecha       = CONV #( <s_result>-timestamp+0(8) ) ).

            s_doc-mensajes       = t_mensajes.

          ENDIF.

          s_doc-req_id         = <s_result>-req_id.
          s_doc-num_doc_ext    = <s_result>-num_doc_ext.
          s_doc-num_doc        = <s_result>-num_doc.
          s_doc-status         = <s_result>-status.
          s_doc-fecha_creacion = <s_result>-timestamp.

          APPEND s_doc TO rs_initial_data-documentos.

          REFRESH t_mensajes.
          CLEAR s_doc.

        ENDLOOP.

      CATCH zcx_rest_ex INTO DATA(o_rootex).

    ENDTRY.

  ENDMETHOD.

  METHOD get_standard_log.

    DATA t_msg     TYPE bal_t_msgh.
    DATA s_bal_msg TYPE bal_s_msg.
    DATA ls_filter TYPE bal_s_lfil.
    DATA tr_subobject TYPE bal_r_sub.
    DATA tr_object TYPE bal_r_obj.
    DATA tr_aldate TYPE bal_r_date.
    DATA tr_extnumber TYPE bal_r_extn.
    DATA t_log_header TYPE balhdr_t.
    DATA s_mensaje TYPE zsrestmensajeget.

    APPEND INITIAL LINE TO tr_subobject ASSIGNING FIELD-SYMBOL(<s_subobj>).
    <s_subobj>-sign   = 'I'.
    <s_subobj>-option = 'EQ'.
    <s_subobj>-low    = iv_id_fuente.

    APPEND INITIAL LINE TO tr_object ASSIGNING FIELD-SYMBOL(<s_obj>).
    <s_obj>-sign   = 'I'.
    <s_obj>-option = 'EQ'.
    <s_obj>-low    = zcl_rest_cte=>c_object_log-operacionrestservice.  " OPERACIONRESTSERVICE

    APPEND INITIAL LINE TO tr_aldate ASSIGNING FIELD-SYMBOL(<s_date>).
    <s_date>-sign   = 'I'.
    <s_date>-option = 'BT'.
    <s_date>-low    = iv_fecha.
    <s_date>-high   = iv_fecha.

    APPEND INITIAL LINE TO tr_extnumber ASSIGNING FIELD-SYMBOL(<s_extnumber>).
    <s_extnumber>-sign   = 'I'.
    <s_extnumber>-option = 'EQ'.
    CONCATENATE iv_req_id '-' iv_num_doc_ext INTO <s_extnumber>-low.

    " Filtros para buscar en el LOG estandar
    ls_filter-subobject = tr_subobject.
    ls_filter-object    = tr_object.
    ls_filter-aldate    = tr_aldate.
    ls_filter-extnumber = tr_extnumber.

    " Buscar Log datos del LOG
    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_client           = sy-mandt
        i_s_log_filter     = ls_filter
      IMPORTING
        e_t_log_header     = t_log_header
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header = t_log_header
      IMPORTING
        e_t_msg_handle = t_msg.

    LOOP AT t_msg ASSIGNING FIELD-SYMBOL(<s_msg>).

      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <s_msg>
        IMPORTING
          e_s_msg        = s_bal_msg.

      s_mensaje-tipo = s_bal_msg-msgty.
      CONCATENATE s_bal_msg-msgv1 s_bal_msg-msgv2 INTO s_mensaje-texto.

      APPEND s_mensaje TO rt_mensajes.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
