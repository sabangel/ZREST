""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MOCK REST DAO
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_rest_dao DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_rest_sys_field_dao.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_rest_dao IMPLEMENTATION.

  METHOD zif_rest_sys_field_dao~find_source_fields.

    rt_fields = VALUE #(
( operacion = 'PV' seccion = 'CAB' campo_hol = 'CLASE_DOC_VTAS' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPISDHD1' campo_abap = 'DOC_TYPE' descripcion = 'Clase de documento de ventas' )
( operacion = 'PV' seccion = 'CAB' campo_hol = 'ORG_VENTAS' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPISDHD1' campo_abap = 'SALES_ORG' descripcion = 'Organización de ventas' )
( operacion = 'PV' seccion = 'CAB' campo_hol = 'CANAL_DISTRIB' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPISDHD1' campo_abap = 'DISTR_CHAN' descripcion = 'Canal de distribución' )
( operacion = 'PV' seccion = 'CAB' campo_hol = 'SECTOR' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPISDHD1' campo_abap = 'DIVISION' descripcion = 'Sector' )
( operacion = 'PV' seccion = 'CAB' campo_hol = 'FEC_PREFERENTE_ENTREGA' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPISDHD1' campo_abap = 'REQ_DATE_H' descripcion = 'Fecha preferente de entrega' )
( operacion = 'PV' seccion = 'CAB' campo_hol = 'CLAVE_CONDICION_PAGO' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPISDHD1' campo_abap = 'PMNTTRMS' descripcion = 'Clave de condiciones de pago' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'ALMACEN' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'STORE_LOC' descripcion = 'Almacén' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'CANT_COMPONENTE' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'COMP_QUANT' descripcion = 'Cantidad de componente' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'CENTRO' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'PLANT' descripcion = 'Centro' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'MONEDA' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'CURRENCY' descripcion = 'Moneda de documento comercial' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'NUM_MATERIAL' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'MATERIAL' descripcion = 'Número de material'  )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'NUM_POS' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'ITM_NUMBER' descripcion = 'Posición documento ventas' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'UNID_MED_VTA' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'SALES_UNIT' descripcion = 'Unidad de medida de venta' )
( operacion = 'PV' seccion = 'ITE_CON' campo_hol = 'CAT_COND' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'CONDICIONES' estruc_abap = 'BAPICOND' campo_abap = 'CONDCLASS' descripcion = 'Catgoría de condición' )
( operacion = 'PV' seccion = 'ITE_CON' campo_hol = 'CLASE_COND' id_fuente = 'Q10' opcional = '' seccion_hol = 'CONDICIONES' estruc_abap = 'BAPICOND' campo_abap = 'COND_TYPE' descripcion = 'Clase de condición' )
( operacion = 'PV' seccion = 'ITE_CON' campo_hol = 'IMPTE_COND' id_fuente = 'Q10' opcional = '' seccion_hol = 'CONDICIONES' estruc_abap = 'BAPICOND' campo_abap = 'COND_VALUE' descripcion = 'Impte.condición' )
( operacion = 'PV' seccion = 'ITE_CON' campo_hol = 'MONEDA' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'CONDICIONES' estruc_abap = 'BAPICOND' campo_abap = 'CURRENCY_2' descripcion = 'Moneda de documento comercial' )
( operacion = 'PV' seccion = 'ITE_REP' campo_hol = 'CANTIDAD' id_fuente = 'Q10' opcional = '' seccion_hol = 'REPARTOS' estruc_abap = 'BAPISCHDL' campo_abap = 'REQ_QTY' descripcion = 'Cantidad pedida por el cliente en UMV' )
( operacion = 'PV' seccion = 'ITE_REP' campo_hol = 'NUM_REPARTO' id_fuente = 'Q10' opcional = '' seccion_hol = 'REPARTOS' estruc_abap = 'BAPISCHDL' campo_abap = 'SCHED_LINE' descripcion = 'Nº de reparto' )
( operacion = 'PV' seccion = 'ADI' campo_hol = 'CLAVE' id_fuente = 'Q10' opcional = '' seccion_hol = 'DATOS_ADICIONALES' estruc_abap = 'ZSRESTADICIONALES' campo_abap = 'CLAVE' descripcion = 'Clave' )
( operacion = 'PV' seccion = 'ADI' campo_hol = 'VALOR' id_fuente = 'Q10' opcional = '' seccion_hol = 'DATOS_ADICIONALES' estruc_abap = 'ZSRESTADICIONALES' campo_abap = 'VALOR' descripcion = 'Valor' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'CENTRO_BENEFICIO' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'PROFIT_CTR' descripcion = 'Centro de beneficio' )
( operacion = 'PV' seccion = 'INT' campo_hol = 'FUNCION_INTERLOCUTOR' id_fuente = 'Q10' opcional = '' seccion_hol = 'INTERLOCUTOR' estruc_abap = 'BAPIPARNR' campo_abap = 'PARTN_ROLE' descripcion = 'Función de interlocutor' )
( operacion = 'PV' seccion = 'INT' campo_hol = 'NUM_DEUDOR' id_fuente = 'Q10' opcional = '' seccion_hol = 'INTERLOCUTOR' estruc_abap = 'BAPIPARNR' campo_abap = 'PARTN_NUMB' descripcion = 'Número de deudor' )
( operacion = 'PV' seccion = 'INT' campo_hol = 'NOMBRE_1' id_fuente = 'Q10' opcional = '' seccion_hol = 'INTERLOCUTOR' estruc_abap = 'BAPIPARNR' campo_abap = 'NAME' descripcion = 'Nombre 1' )
( operacion = 'PV' seccion = 'INT' campo_hol = 'NOMBRE_2' id_fuente = 'Q10' opcional = '' seccion_hol = 'INTERLOCUTOR' estruc_abap = 'BAPIPARNR' campo_abap = 'NAME_2' descripcion = 'Nombre 2' )
( operacion = 'PV' seccion = 'INT' campo_hol = 'PAIS' id_fuente = 'Q10' opcional = '' seccion_hol = 'INTERLOCUTOR' estruc_abap = 'BAPIPARNR' campo_abap = 'COUNTRY' descripcion = 'Clave de país' )
( operacion = 'PV' seccion = 'TXT' campo_hol = 'ID_TEXTO' id_fuente = 'Q10' opcional = '' seccion_hol = 'TEXTOS' estruc_abap = 'BAPISDTEXT' campo_abap = 'TEXT_ID' descripcion = 'ID de texto' )
( operacion = 'PV' seccion = 'TXT' campo_hol = 'IDIOMA' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'TEXTOS' estruc_abap = 'BAPISDTEXT' campo_abap = 'LANGU' descripcion = 'Clave de idioma' )
( operacion = 'PV' seccion = 'TXT' campo_hol = 'TEXTO' id_fuente = 'Q10' opcional = '' seccion_hol = 'TEXTOS' estruc_abap = 'BAPISDTEXT' campo_abap = 'TEXT_LINE' descripcion = 'Línea de texto' )
( operacion = 'PV' seccion = 'TXT' campo_hol = 'FUNCION' id_fuente = 'Q10' opcional = '' seccion_hol = 'TEXTOS' estruc_abap = 'BAPISDTEXT' campo_abap = 'FUNCTION' descripcion = 'Función' )
    ).

  ENDMETHOD.

ENDCLASS.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ENTITY MOCK
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_entity DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_rest_entity .
    ALIASES get_string_data FOR if_rest_entity~get_string_data.

    METHODS constructor
      IMPORTING iv_case TYPE string.

  PRIVATE SECTION.
    DATA v_case TYPE string.

    METHODS find_payload
      IMPORTING iv_case       TYPE string
                iv_textname   TYPE tdobname
      RETURNING VALUE(rv_res) TYPE string.

ENDCLASS.

CLASS lcl_entity IMPLEMENTATION.

  METHOD constructor.
    me->v_case = iv_case.
  ENDMETHOD.

  METHOD if_rest_entity~get_string_data.

    rv_data = find_payload( iv_case = me->v_case  iv_textname = 'ZSDCASOBASE' ).

  ENDMETHOD.

  METHOD find_payload.

    DATA t_lines TYPE tline_t.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id       = 'ST'
        name     = iv_textname
        language = 'S'
        object   = 'TEXT'
      TABLES
        lines    = t_lines
      EXCEPTIONS
        OTHERS   = 9.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    LOOP AT t_lines ASSIGNING FIELD-SYMBOL(<s_line>).

      "---------------------------------------------
      "Dañar el payload de acuerdo con el caso
      "---------------------------------------------

      IF iv_case         EQ 'falta_campo_1' AND
         <s_line>-tdline CS 'funcion_interlocutor'.
        "Eliminar el campo función interlocutor
        CONTINUE.
      ENDIF.

      IF   iv_case         EQ 'campo_vacio_1' AND
           <s_line>-tdline CS 'clase_doc_vtas'.
        "Poner el campo clase doc ventas con un valor vacío
        <s_line>-tdline = | "clase_doc_vtas": " ", |.
      ENDIF.

      IF   iv_case         EQ 'campo_sobrante_1' AND
           <s_line>-tdline CS 'cabecera'.
        "Poner en la estructura de cabecera un campo que no está en la configuración
        DATA(v_new_line) = VALUE tline( tdformat = '/'  tdline = | "campo_x": "xxx", | ).
        INSERT v_new_line INTO t_lines INDEX sy-tabix + 2.
      ENDIF.

      rv_res = rv_res && <s_line>-tdline.

    ENDLOOP.

  ENDMETHOD.


ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" REQUEST MOCK
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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

    "Los demás responda uno válido
    rt_segments = VALUE #( ( |{ cte->t_operations[ intern_name = 'PV' ]-extern_name }| ) ).

  ENDMETHOD.

  METHOD if_rest_request~get_header_field.

    "Los demás responda uno válido
    rv_value = |Q10|.

  ENDMETHOD.

  METHOD if_rest_request~get_method.
    rv_method = 'POST'.
  ENDMETHOD.

ENDCLASS.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CLASE DE PRUEBA
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS ltcl_validator_def DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS constructor.

  PRIVATE SECTION.

    DATA o_validator TYPE REF TO zcl_rest_validator_def.
    DATA cte TYPE REF TO zcl_rest_cte.

    METHODS setup.
    METHODS validate_post_customized_ok FOR TESTING RAISING cx_static_check.
    METHODS validate_post_customized_bad FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_validator_def IMPLEMENTATION.

  METHOD constructor.
    me->cte = NEW zcl_rest_cte( ).
  ENDMETHOD.

  METHOD setup.
    me->o_validator = NEW zcl_rest_validator_def( iv_oper = 'PV' io_rest_dao = NEW lcl_rest_dao( ) ).
  ENDMETHOD.

  METHOD validate_post_customized_ok.

    DATA t_msg_act TYPE bapiret2_t.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Caso: ok
    " Se espera: sin mensajes de salida
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(v_ok_act) = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = '' )
        IMPORTING et_msg = t_msg_act ).

    cl_abap_unit_assert=>assert_initial( act = t_msg_act ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_true ).

  ENDMETHOD.

  METHOD validate_post_customized_bad.

    DATA t_msg_act TYPE bapiret2_t.
    DATA t_msg_exp TYPE bapiret2_t.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Caso: falta un campo obligatorio
    " SE espera: mensaje de error
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(v_ok_act) = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = 'falta_campo_1' )
        IMPORTING et_msg = t_msg_act ).

    t_msg_exp = VALUE #(
      ( id = cte->c_clase_msgs_def type = 'E' number = '034' message_v1 = |INTERLOCUTOR-FUNCION_INTERLOCUTOR| )
    ).

    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_false ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Caso: campo obligatorio sin poblar
    " SE espera: mensaje de advertencia
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    v_ok_act = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = 'campo_vacio_1' )
        IMPORTING et_msg = t_msg_act ).

    t_msg_exp = VALUE #(
      ( id = cte->c_clase_msgs_def type = 'W' number = '035' message_v1 = |CABECERA-CLASE_DOC_VTAS| )
    ).

    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_true ). "Warnings no se consideran error

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Caso: campo sobrante
    " SE espera: mensaje de advertencia
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    v_ok_act = me->o_validator->validate(
        EXPORTING io_req = NEW lcl_request( iv_case = 'campo_sobrante_1' )
        IMPORTING et_msg = t_msg_act ).

    t_msg_exp = VALUE #(
      ( id = cte->c_clase_msgs_def type = 'W' number = '036' message_v1 = |CABECERA-CAMPO_X| )
    ).

    cl_abap_unit_assert=>assert_equals( act = t_msg_act  exp = t_msg_exp ).
    cl_abap_unit_assert=>assert_equals( act = v_ok_act   exp = abap_true ). "Warnings no se consideran error

  ENDMETHOD.

ENDCLASS.
