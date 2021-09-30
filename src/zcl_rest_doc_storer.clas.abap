CLASS zcl_rest_doc_storer DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    DATA r_payload TYPE REF TO data.

    METHODS constructor.
    METHODS add_doc     IMPORTING ir_doc       TYPE any
                                  iv_req_id    TYPE zdrestreqid
                        CHANGING  ct_reproceso TYPE zcl_rest_cte=>tt_reproceso.
    METHODS save_docs   CHANGING ct_reproceso  TYPE zcl_rest_cte=>tt_reproceso.

  PRIVATE SECTION.

    DATA cte       TYPE REF TO zcl_rest_cte.

ENDCLASS.

CLASS zcl_rest_doc_storer IMPLEMENTATION.

  METHOD constructor.
    me->cte = NEW zcl_rest_cte( ).
  ENDMETHOD.

  METHOD add_doc.

    FIELD-SYMBOLS <t_documentos_exp> TYPE ANY TABLE.

*    " Recupero info_mensaje de la petición
    ASSIGN me->r_payload->(cte->c_special_field-informacion_msg) TO FIELD-SYMBOL(<s_ref_info>).
    ASSIGN me->r_payload->(cte->c_special_field-documentos)      TO FIELD-SYMBOL(<s_ref_doc>).
    ASSIGN <s_ref_doc>->* TO FIELD-SYMBOL(<t_docs>).

    DATA(o_info_mensaje)    = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( <s_ref_info> ) ).
    DATA(o_documentos)      = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( <t_docs> ) ).

    DATA(o_type_exp) = cl_abap_structdescr=>create(
                  p_components = VALUE #(
                    ( name = 'info_mensaje'    type = o_info_mensaje )
                    ( name = 'documentos'      type = o_documentos ) ) ).

    DATA r_result_exp TYPE REF TO data.
    CREATE DATA r_result_exp TYPE HANDLE o_type_exp.

    ASSIGN r_result_exp->(cte->c_special_field-informacion_msg) TO FIELD-SYMBOL(<s_info_mensaje>).
    <s_info_mensaje> = <s_ref_info>.

    ASSIGN r_result_exp->(cte->c_special_field-documentos) TO <t_documentos_exp>.
    INSERT ir_doc INTO TABLE <t_documentos_exp>.

    DATA(lv_json) = /ui2/cl_json=>serialize( data = r_result_exp compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    DATA(v_payload_binary) = /ui2/cl_json=>string_to_raw( iv_string = lv_json ).

    GET TIME STAMP FIELD DATA(v_ts).

    ASSIGN ir_doc->* TO FIELD-SYMBOL(<r_ref>).
    ASSIGN COMPONENT cte->c_special_field-doc_ref_externo OF STRUCTURE <r_ref> TO FIELD-SYMBOL(<r_value>).
    ASSIGN <r_value>->* TO FIELD-SYMBOL(<v_value>).

    DATA(tr_num_doc_externo) = VALUE /bofu/t_fbi_value_range( ( sign = 'I' option = 'EQ' low = <v_value>  ) ).

    DATA(s_data_reproceso) = VALUE ztrest_reproceso( num_doc_ext = CONV #( <v_value> )
                                                     req_id = iv_req_id
                                                     timestamp = v_ts
                                                     payload = v_payload_binary ).

    APPEND s_data_reproceso TO ct_reproceso.

  ENDMETHOD.

  METHOD save_docs.

    TRY.
        " Insertamos en la tabla ztrest_reproceso de reproceso, si ya existe el num_doc_externo lo sobreescribe para solo tener la ultima version enviada
        MODIFY ztrest_reproceso FROM TABLE ct_reproceso.
      CATCH cx_root.
        RETURN.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
