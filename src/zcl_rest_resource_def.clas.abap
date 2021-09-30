CLASS zcl_rest_resource_def DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    "! Creación de docs FI
    METHODS if_rest_resource~post REDEFINITION .

    "! Info docs creados
    METHODS if_rest_resource~get REDEFINITION .

    "! Respuesta al método options. Informa las operaciones disponibles
    METHODS if_rest_resource~options REDEFINITION .

  PRIVATE SECTION.

    DATA cte TYPE REF TO zcl_rest_cte.

ENDCLASS.



CLASS zcl_rest_resource_def IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    me->cte = NEW #( ).

  ENDMETHOD.


  METHOD if_rest_resource~get.

    DATA t_msg TYPE bapiret2_t.

    TRY.
        DATA o_factory TYPE REF TO zif_rest_factory.
        DATA(v_err_status) = zcl_rest_factory_solver=>solve( EXPORTING io_req     = me->mo_request
                                                             CHANGING  co_factory = o_factory ).
        IF o_factory IS NOT BOUND.
          mo_response->set_status( v_err_status ).
          RETURN.
        ENDIF.

        DATA(v_id_fuente) = me->mo_request->get_header_field( cte->c_special_field-fuente ).

        "Traer el id interno para la operación
        DATA(t_uri_segs) = me->mo_request->get_uri_segments( ).
        DATA(v_ext_oper) = CONV cte->ty_res_id( t_uri_segs[ 1 ] ).
        DATA(v_int_oper) = cte->t_operations[ extern_name = v_ext_oper ]-intern_name.
        DATA(o_validator) = o_factory->init_validator( iv_oper      = v_int_oper
                                                       iv_id_fuente = CONV #( v_id_fuente ) ).

        DATA(v_validate_ok) = o_validator->validate( EXPORTING io_req = me->mo_request
                                                     IMPORTING et_msg = t_msg ).

        DATA(o_entity) = mo_response->create_entity( ).
        o_entity->set_string_data( zcl_rest_util=>json_from_bapimsg( EXPORTING it_msg = t_msg ) ).

        IF v_validate_ok EQ abap_false.
          mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
          RETURN.
        ENDIF.

        DATA(o_finder) = o_factory->init_finder( iv_oper      = v_int_oper
                                                 iv_id_fuente = CONV #( v_id_fuente ) ).

        DATA(r_result) = o_finder->find( iv_oper = v_int_oper iv_id_fuente = CONV #( v_id_fuente ) it_url_attrib = me->mo_request->get_uri_query_parameters( ) ).

        o_entity->set_string_data( zcl_rest_util=>json_from_reference( r_result ) ).

        mo_response->set_status( cl_rest_status_code=>gc_success_ok ).

      CATCH zcx_rest_ex INTO DATA(o_restex).
        mo_response->set_status( cl_rest_status_code=>gc_server_error_internal ).
        DATA(s_int_err) = o_restex->if_t100_message~t100key.
        t_msg = VALUE #( ( id = s_int_err-msgid
                           number = s_int_err-msgno
                           message_v1 = s_int_err-attr1
                           message_v2 = s_int_err-attr2
                           message_v3 = s_int_err-attr3
                           message_v4 = s_int_err-attr4 ) ).
        mo_response->get_entity( )->set_string_data( zcl_rest_util=>json_from_bapimsg( EXPORTING it_msg = t_msg ) ).
      CATCH cx_root INTO DATA(o_rootex).
        mo_response->set_status( cl_rest_status_code=>gc_server_error_internal ).
        t_msg = VALUE #( ( message = o_rootex->get_longtext( ) ) ).
        mo_response->get_entity( )->set_string_data( zcl_rest_util=>json_from_bapimsg( EXPORTING it_msg = t_msg ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD if_rest_resource~options.

    TRY.
      CATCH zcx_rest_ex.
        mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
    ENDTRY.

  ENDMETHOD.


  METHOD if_rest_resource~post.

    DATA t_msg TYPE bapiret2_t.

    TRY.
        DATA o_factory TYPE REF TO zif_rest_factory.
        DATA(v_err_status) = zcl_rest_factory_solver=>solve( EXPORTING io_req     = me->mo_request
                                                             CHANGING  co_factory = o_factory ).
        IF o_factory IS NOT BOUND.
          mo_response->set_status( v_err_status ).
          RETURN.
        ENDIF.

        DATA(v_id_fuente) = me->mo_request->get_header_field( cte->c_special_field-fuente ).

        "Traer el id interno para la operación
        DATA(t_uri_segs) = me->mo_request->get_uri_segments( ).
        DATA(v_ext_oper) = CONV cte->ty_res_id( t_uri_segs[ 1 ] ).
        DATA(v_int_oper) = cte->t_operations[ extern_name = v_ext_oper ]-intern_name.
        DATA(o_validator) = o_factory->init_validator( iv_oper      = v_int_oper
                                                       iv_id_fuente = CONV #( v_id_fuente ) ).

        DATA(v_validate_ok) = o_validator->validate( EXPORTING io_req = me->mo_request
                                                     IMPORTING et_msg = t_msg ).

        DATA(o_entity) = mo_response->create_entity( ).

        DATA(v_req_id) = me->mo_request->get_header_field( cte->c_special_field-id_peticion ).

        o_entity->set_string_data( zcl_rest_util=>json_from_bapimsg( EXPORTING it_msg = t_msg  iv_req_id = CONV #( v_req_id ) ) ).

        IF v_validate_ok EQ abap_false.
          mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
          RETURN.
        ENDIF.

        DATA(o_creator) = o_factory->init_creator( iv_oper      = v_int_oper
                                                   iv_id_fuente = CONV #( v_id_fuente ) ).

        o_creator->create( iv_payload = me->mo_request->get_entity( )->get_string_data( ) iv_req_id = CONV #( v_req_id ) ).

        mo_response->set_status( cl_rest_status_code=>gc_success_ok ).

      CATCH zcx_rest_ex INTO DATA(o_restex).
        mo_response->set_status( cl_rest_status_code=>gc_server_error_internal ).
        DATA(s_int_err) = o_restex->if_t100_message~t100key.
        t_msg = VALUE #( ( id = s_int_err-msgid
                           number = s_int_err-msgno
                           message_v1 = s_int_err-attr1
                           message_v2 = s_int_err-attr2
                           message_v3 = s_int_err-attr3
                           message_v4 = s_int_err-attr4 ) ).
        mo_response->get_entity( )->set_string_data( zcl_rest_util=>json_from_bapimsg( EXPORTING it_msg = t_msg  iv_req_id = CONV #( v_req_id ) ) ).
      CATCH cx_root INTO DATA(o_rootex).
        mo_response->set_status( cl_rest_status_code=>gc_server_error_internal ).
        t_msg = VALUE #( ( message = o_rootex->get_longtext( ) ) ).
        mo_response->get_entity( )->set_string_data( zcl_rest_util=>json_from_bapimsg( EXPORTING it_msg = t_msg  iv_req_id = CONV #( v_req_id ) ) ).
    ENDTRY.

  ENDMETHOD.



ENDCLASS.
