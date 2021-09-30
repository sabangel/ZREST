CLASS lcl_entity DEFINITION CREATE PUBLIC.
  		
  PUBLIC SECTION.
    INTERFACES if_rest_entity .
    ALIASES get_string_data FOR if_rest_entity~get_string_data.

    METHODS constructor
      IMPORTING iv_case TYPE string.

  PRIVATE SECTION.
    DATA v_case TYPE string.

ENDCLASS.

CLASS lcl_entity IMPLEMENTATION.

  METHOD constructor.
    me->v_case = iv_case.
  ENDMETHOD.

  METHOD if_rest_entity~get_string_data.

    IF me->v_case EQ |no_all|.
      rv_data =  `{  }`.
      RETURN.
    ENDIF.

    IF me->v_case EQ |bad_op_bad_fuente_bad_formed_payload|.
      rv_data =
      `{`                        &&
      `  "info_mensaje":`        &&
      `  {`                      &&
      `    "version": "1.0"`     &&
      `  {,`                     &&  "BAD
      `}`.

      RETURN.
    ENDIF.

    IF me->v_case EQ |bad_op_bad_fuente_bad_info|.
      rv_data =
      `{`                            &&
      `  "info_mensaje":`            &&
      `  {`                          &&
      `    "version": "1.0"`         &&
      `  },`                         &&
      `  "docs": [`                  &&
      `    {`                        &&
      `      "num_doc_externo": "1"` &&
      `    }`                        &&
      `  ]`                          &&
      `}`.

      RETURN.
    ENDIF.

    IF me->v_case EQ |no_req_info|.
      rv_data =
      `{`                            &&
      `  "docs": [`                  &&   "bad
      `    {`                        &&
      `      "num_doc_externo": "1"` &&
      `    }`                        &&
      `  ]`                          &&
      `}`.

      RETURN.
    ENDIF.

    IF me->v_case EQ |no_docs_1|.
      rv_data =
    `{`                            &&
    `  "info_mensaje":`            &&
    `  {`                          &&
    `    "req_id": "0000001",`     &&
    `    "version": "1.0"`         &&
    `  }`                          &&  "No docs
    `}`.
      RETURN.
    ENDIF.

    IF me->v_case EQ |no_docs_2|.
      rv_data =
    `{`                            &&
    `  "info_mensaje":`            &&
    `  {`                          &&
    `    "req_id": "0000001",`     &&
    `    "version": "1.0"`         &&
    `  },`                         &&
    `  "documentos":`              && "Docs no es una lista
    `    {`                        &&
    `      "num_doc_externo": "1"` &&
    `    }`                        &&
    `}`.
      RETURN.
    ENDIF.

    IF me->v_case EQ |no_docs_3|.
      rv_data =
    `{`                            &&
    `  "info_mensaje":`            &&
    `  {`                          &&
    `    "req_id": "0000001",`     &&
    `    "version": "1.0"`         &&
    `  },`                         &&
    `  "documentos":`                    &&
    `    {`                        && "No doc externo
    `    }`                        &&
    `}`.
      RETURN.
    ENDIF.

    IF me->v_case EQ |no_doc_ext_1|.
      rv_data =
    `{`                            &&
    `  "info_mensaje":`            &&
    `  {`                          &&
    `    "req_id": "0000001",`     &&
    `    "version": "1.0"`         &&
    `  },`                         &&
    `  "documentos": [`                  &&
    `    {`                        &&
    `    "field_x": "xxx"`         && "No está doc externo
    `    }`                        &&
    `  ]`                          &&
    `}`.
      RETURN.
    ENDIF.

    IF me->v_case EQ |no_doc_ext_2|.
      rv_data =
    `{`                            &&
    `  "info_mensaje":`            &&
    `  {`                          &&
    `    "req_id": "0000001",`     &&
    `    "version": "1.0"`         &&
    `  },`                         &&
    `  "documentos": [`                  &&
    `    {`                        &&
    `    "num_doc_externo": "1"`   &&
    `    },`                       &&
    `    {`                        &&
    `    "num_doc_externo": ""`    && "Doc externo vacío
    `    }`                        &&
    `  ]`                          &&
    `}`.
      RETURN.
    ENDIF.

    IF me->v_case EQ |bad_doc_ext_1|.
      rv_data =
    `{`                            &&
    `  "info_mensaje":`            &&
    `  {`                          &&
    `    "req_id": "0000001",`     &&
    `    "version": "1.0"`         &&
    `  },`                         &&
    `  "documentos": [`                  &&
    `    {`                        &&
    `    "num_doc_externo": "1"`   &&
    `    },`                       &&
    `    {`                        &&
    `    "num_doc_externo": "2"`   &&
    `    },`                       &&
    `    {`                        &&
    `    "num_doc_externo": "1"`   && "Doc externo repetido
    `    }`                        &&
    `  ]`                          &&
    `}`.
      RETURN.
    ENDIF.

    rv_data =
    `{`                            &&
    `  "info_mensaje":`            &&
    `  {`                          &&
    `    "req_id": "0000001",`     &&
    `    "version": "1.0"`         &&
    `  },`                         &&
    `  "documentos": [`                  &&
    `    {`                        &&
    `      "num_doc_externo": "1"` &&
    `    },`                       &&
    `    {`                        &&
    `      "num_doc_externo": "2"` &&
    `    }`                        &&
    `  ]`                          &&
    `}`.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_request DEFINITION CREATE PUBLIC.
  		
  PUBLIC SECTION.
    INTERFACES if_rest_request.
    ALIASES get_entity FOR if_rest_request~get_entity.
    ALIASES get_uri_segments FOR if_rest_request~get_uri_segments.
    ALIASES get_header_field FOR if_rest_request~get_header_field.

    METHODS constructor
      IMPORTING iv_case TYPE string.

  PRIVATE SECTION.
    DATA v_case TYPE string.
    DATA cte TYPE REF TO zcl_rest_cte.

ENDCLASS.

CLASS lcl_request IMPLEMENTATION.

  METHOD constructor.
    me->v_case = iv_case.

    me->cte = NEW #(  ).
  ENDMETHOD.

  METHOD if_rest_request~get_entity.
    ro_entity = NEW lcl_entity( me->v_case ).
  ENDMETHOD.

  METHOD if_rest_request~get_uri_segments.

    "Sin operación
    IF me->v_case EQ 'no_all'.
      RETURN.
    ENDIF.

    "Operación no existe
    IF me->v_case EQ 'bad_op_bad_fuente_bad_formed_payload'.
      rt_segments = VALUE #( ( |XXX| ) ).
      RETURN.
    ENDIF.

    "Los demás responda uno válido
    rt_segments = VALUE #( ( |{ cte->t_operations[ intern_name = 'PV' ]-extern_name }| ) ).

  ENDMETHOD.

  METHOD if_rest_request~get_header_field.

    IF me->v_case EQ 'no_all'.
      rv_value = SWITCH #( iv_name
          WHEN cte->c_special_field-fuente      THEN ''
          WHEN cte->c_special_field-id_peticion THEN ''
      ).
      RETURN.
    ENDIF.

    IF me->v_case EQ 'bad_op_bad_fuente_bad_formed_payload'.
      rv_value = SWITCH #( iv_name
          WHEN cte->c_special_field-fuente      THEN 'sourceX'
          WHEN cte->c_special_field-id_peticion THEN '12345'
      ).
      RETURN.
    ENDIF.

    IF me->v_case EQ 'no_req_id_1'.
      rv_value = SWITCH #( iv_name
          WHEN cte->c_special_field-fuente      THEN 'Q10'
          WHEN cte->c_special_field-id_peticion THEN ''
      ).
      RETURN.
    ENDIF.

    "Si llega hasta aqui responder sin error
    rv_value = SWITCH #( iv_name WHEN cte->c_special_field-fuente      THEN 'Q10'
                                 WHEN cte->c_special_field-id_peticion THEN '12345'
    ).

  ENDMETHOD.

  METHOD if_rest_request~get_method.
    rv_method = 'POST'.
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VALIDADOR. AUXILIAR PARA PRUEBA
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_validator DEFINITION
INHERITING FROM zcl_rest_validator_base
CREATE PUBLIC.
  		
  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS validate_post_customized REDEFINITION.

ENDCLASS.

CLASS lcl_validator IMPLEMENTATION.

  METHOD validate_post_customized.
    rv_ok = abap_true.
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_rest_validator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cte TYPE REF TO zcl_rest_cte.
    DATA o_validator TYPE REF TO lcl_validator.

    METHODS setup.
    METHODS validate_bad_1 FOR TESTING RAISING cx_static_check.
    METHODS validate_ok_1 FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_rest_validator IMPLEMENTATION.

  METHOD setup.
    me->cte = NEW zcl_rest_cte( ).
    me->o_validator = NEW #( iv_oper = '' ).
  ENDMETHOD.

  METHOD validate_bad_1.

    DATA t_msg_act TYPE bapiret2_t.
    DATA t_msg_exp TYPE bapiret2_t.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Caso: sin operación, sin fuente_id, payload vacío
    " Se espera: mensajes sin operación, sin fuente, sin request id
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(v_ok_act) = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = |no_all| )
        IMPORTING et_msg = t_msg_act ).

    t_msg_exp = VALUE #(
      ( id = cte->c_clase_msgs_def type = 'E' number = '026' )
      ( id = cte->c_clase_msgs_def type = 'E' number = '023' )
      ( id = cte->c_clase_msgs_def type = 'E' number = '045' )
    ).
    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_false ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Caso: sin operación, fuente_id mal, payload vacío
    " Se espera: mensajes operación mal, fuente mal, payload mal
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    v_ok_act = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = |bad_op_bad_fuente_bad_formed_payload| )
        IMPORTING et_msg = t_msg_act ).
    t_msg_exp = VALUE #(
      ( id = cte->c_clase_msgs_def type = 'E' number = '022' message_v1 = 'XXX' )
      ( id = cte->c_clase_msgs_def type = 'E' number = '023' message_v1 = 'sourceX' )
      ( id = cte->c_clase_msgs_def type = 'E' number = '001' )
    ).
    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_false ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Caso: sin identificador petición request id
    " Se espera: mensaje de error de falta id requerimiento
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    v_ok_act = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = |no_req_id_1| )
        IMPORTING et_msg = t_msg_act ).
    t_msg_exp = VALUE #(
      ( id = cte->c_clase_msgs_def type = 'E' number = '045' )
    ).
    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_false ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Caso: sin sección de información petición, estructura docs mal
    " Se espera: mensajes error falta request_id, falta de estructura
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    v_ok_act = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = |no_req_info| )
        IMPORTING et_msg = t_msg_act ).
    t_msg_exp = VALUE #(
      ( id = cte->c_clase_msgs_def type = 'E' number = '029' message_v1 = cte->c_special_field-informacion_msg )
      ( id = cte->c_clase_msgs_def type = 'E' number = '029' message_v1 = cte->c_special_field-documentos )
    ).
    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_false ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Caso: sin sección de docs
    " Se espera: mensaje de error de falta estructura docs
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Docs debe existir
    v_ok_act = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = |no_docs_1| )
        IMPORTING et_msg = t_msg_act ).
    t_msg_exp = VALUE #(
      ( id = cte->c_clase_msgs_def type = 'E' number = '029' message_v1 = cte->c_special_field-documentos )
    ).
    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_false ).

    "Docs debe ser una lista
    v_ok_act = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = |no_docs_2| )
        IMPORTING et_msg = t_msg_act ).
    t_msg_exp = VALUE #(
      ( id = cte->c_clase_msgs_def type = 'E' number = '031' message_v1 = cte->c_special_field-documentos )
    ).
    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_false ).

    "Docs no tiene elementos
    v_ok_act = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = |no_docs_3| )
        IMPORTING et_msg = t_msg_act ).
    t_msg_exp = VALUE #(
      ( id = cte->c_clase_msgs_def type = 'E' number = '031' message_v1 = cte->c_special_field-documentos )
    ).
    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_false ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Caso: num doc externo mal
    " Se espera: mensaje de error doc externo
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "num doc externo debe existir
    v_ok_act = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = |no_doc_ext_1| )
        IMPORTING et_msg = t_msg_act ).
    t_msg_exp = VALUE #(
      ( id = cte->c_clase_msgs_def type = 'E' number = '020' message_v1 = cte->c_special_field-doc_ref_externo )
    ).
    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_false ).

    "num doc externo debe existir (tiene la etiqueta pero viene vacío)
    v_ok_act = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = |no_doc_ext_2| )
        IMPORTING et_msg = t_msg_act ).
    t_msg_exp = VALUE #(
      ( id = cte->c_clase_msgs_def type = 'E' number = '020' message_v1 = cte->c_special_field-doc_ref_externo )
    ).
    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_false ).

    "num doc externo no debe duplicarse
*    v_ok_act = me->o_validator->validate(
*        EXPORTING io_req = NEW lcl_request( iv_case = |bad_doc_ext_1| )
*        IMPORTING et_msg = t_msg_act ).
*    t_msg_exp = VALUE #(
*      ( id = cte->c_clase_msgs_def type = 'E' number = '033' message_v1 = cte->c_special_field-doc_ref_externo )
*    ).
*    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
*    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_false ).

  ENDMETHOD.


  METHOD validate_ok_1.

    DATA t_msg_act TYPE bapiret2_t.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Caso: Estructura ok
    " Se espera: sin mensajes
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(v_ok_act) = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = '' )
        IMPORTING et_msg = t_msg_act ).

    cl_abap_unit_assert=>assert_initial( act = t_msg_act ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_true ).

  ENDMETHOD.

ENDCLASS.
