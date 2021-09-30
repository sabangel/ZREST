CLASS ltcl_creator DEFINITION DEFERRED.
CLASS zcl_rest_creator_base DEFINITION LOCAL FRIENDS ltcl_creator.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" REST DAO
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_rest_dao DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_rest_sys_field_dao.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_rest_dao IMPLEMENTATION.

  METHOD zif_rest_sys_field_dao~find_source_fields.

    DATA(fpos) = zcl_rest_cte=>c_special_field-num_posicion.

    rt_fields = VALUE #(
( operacion = 'PV' seccion = 'CAB' campo_hol = 'CLASE_DOC_VTAS' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPISDHD1' campo_abap = 'DOC_TYPE' descripcion = 'Clase de documento de ventas' longitud = '4.0' )
( operacion = 'PV' seccion = 'CAB' campo_hol = 'ORG_VENTAS' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPISDHD1' campo_abap = 'SALES_ORG' descripcion = 'Organización de ventas' longitud = '4.0' )
( operacion = 'PV' seccion = 'CAB' campo_hol = 'CANAL_DISTRIB' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPISDHD1' campo_abap = 'DISTR_CHAN' descripcion = 'Canal de distribución' longitud = '2.0' )
( operacion = 'PV' seccion = 'CAB' campo_hol = 'SECTOR' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPISDHD1' campo_abap = 'DIVISION' descripcion = 'Sector' longitud = '2.0' )
( operacion = 'PV' seccion = 'CAB' campo_hol = 'FEC_PREFERENTE_ENTREGA' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPISDHD1' campo_abap = 'REQ_DATE_H' descripcion = 'Fecha preferente de entrega' longitud = '4.0' )
( operacion = 'PV' seccion = 'CAB' campo_hol = 'CLAVE_CONDICION_PAGO' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPISDHD1' campo_abap = 'PMNTTRMS' descripcion = 'Clave de condiciones de pago' longitud = '4.0' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'ALMACEN' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'STORE_LOC' descripcion = 'Almacén' longitud = '4.0' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'CANT_COMPONENTE' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'COMP_QUANT' descripcion = 'Cantidad de componente' longitud = '13.3' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'CENTRO' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'PLANT' descripcion = 'Centro' longitud = '4.0' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'MONEDA' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'CURRENCY' descripcion = 'Moneda de documento comercial' longitud = '5.0' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'NUM_MATERIAL' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'MATERIAL' descripcion = 'Número de material' longitud = '18.0' )
( operacion = 'PV' seccion = 'ITE' campo_hol = fpos id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'ITM_NUMBER' descripcion = 'Posición documento ventas' longitud = '6.0' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'UNID_MED_VTA' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'SALES_UNIT' descripcion = 'Unidad de medida de venta' longitud = '3.0'  )
( operacion = 'PV' seccion = 'ITE_CON' campo_hol = 'CAT_COND' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'CONDICIONES' estruc_abap = 'BAPICOND' campo_abap = 'CONDCLASS' descripcion = 'Catgoría de condición' longitud = '1.0' )
( operacion = 'PV' seccion = 'ITE_CON' campo_hol = 'CLASE_COND' id_fuente = 'Q10' opcional = '' seccion_hol = 'CONDICIONES' estruc_abap = 'BAPICOND' campo_abap = 'COND_TYPE' descripcion = 'Clase de condición' longitud = '4.0' )
( operacion = 'PV' seccion = 'ITE_CON' campo_hol = 'IMPTE_COND' id_fuente = 'Q10' opcional = '' seccion_hol = 'CONDICIONES' estruc_abap = 'BAPICOND' campo_abap = 'COND_VALUE' descripcion = 'Impte.condición' longitud = '28.2' )
( operacion = 'PV' seccion = 'ITE_CON' campo_hol = 'MONEDA' id_fuente = 'Q10' opcional = '' seccion_hol = 'CONDICIONES' estruc_abap = 'BAPICOND' campo_abap = 'CURRENCY_2' descripcion = 'Moneda de documento comercial' longitud = '5.0' )
( operacion = 'PV' seccion = 'ITE_CON' campo_hol = fpos id_fuente = 'Q10' opcional = '' seccion_hol = 'CONDICIONES' estruc_abap = 'BAPICOND' campo_abap = 'ITM_NUMBER' descripcion = 'Posición documento ventas' longitud = '6.0' )
( operacion = 'PV' seccion = 'ITE_REP' campo_hol = 'CANTIDAD' id_fuente = 'Q10' opcional = '' seccion_hol = 'REPARTOS' estruc_abap = 'BAPISCHDL' campo_abap = 'REQ_QTY' descripcion = 'Cantidad pedida por el cliente en UMV' longitud = '35.0' )
( operacion = 'PV' seccion = 'ITE_REP' campo_hol = fpos id_fuente = 'Q10' opcional = '' seccion_hol = 'REPARTOS' estruc_abap = 'BAPISCHDL' campo_abap = 'ITM_NUMBER' descripcion = 'Posición documento ventas' longitud = '6.0' )
( operacion = 'PV' seccion = 'ITE_REP' campo_hol = 'NUM_REPARTO' id_fuente = 'Q10' opcional = '' seccion_hol = 'REPARTOS' estruc_abap = 'BAPISCHDL' campo_abap = 'SCHED_LINE' descripcion = 'Nº de reparto' longitud = '10.0' )
( operacion = 'PV' seccion = 'ADI' campo_hol = 'CLAVE' id_fuente = 'Q10' opcional = '' seccion_hol = 'DATOS_ADICIONALES' estruc_abap = 'ZSRESTADICIONALES' campo_abap = 'CLAVE' descripcion = 'Clave' longitud = '30.0' )
( operacion = 'PV' seccion = 'ADI' campo_hol = 'VALOR' id_fuente = 'Q10' opcional = '' seccion_hol = 'DATOS_ADICIONALES' estruc_abap = 'ZSRESTADICIONALES' campo_abap = 'VALOR' descripcion = 'Valor' longitud = '99.0' )
( operacion = 'PV' seccion = 'ITE' campo_hol = 'CENTRO_BENEFICIO' id_fuente = 'Q10' opcional = '' seccion_hol = 'ITEMS' estruc_abap = 'BAPISDITM' campo_abap = 'PROFIT_CTR' descripcion = 'Centro de beneficio' longitud = '10.0' )
( operacion = 'PV' seccion = 'INT' campo_hol = 'FUNCION_INTERLOCUTOR' id_fuente = 'Q10' opcional = '' seccion_hol = 'INTERLOCUTOR' estruc_abap = 'BAPIPARNR' campo_abap = 'PARTN_ROLE' descripcion = 'Función de interlocutor' longitud = '2.0' )
( operacion = 'PV' seccion = 'INT' campo_hol = 'NUM_DEUDOR' id_fuente = 'Q10' opcional = '' seccion_hol = 'INTERLOCUTOR' estruc_abap = 'BAPIPARNR' campo_abap = 'PARTN_NUMB' descripcion = 'Número de deudor' longitud = '10.0' )
( operacion = 'PV' seccion = 'INT' campo_hol = 'NOMBRE_1' id_fuente = 'Q10' opcional = '' seccion_hol = 'INTERLOCUTOR' estruc_abap = 'BAPIPARNR' campo_abap = 'NAME' descripcion = 'Nombre 1' longitud = '35.0' )
( operacion = 'PV' seccion = 'INT' campo_hol = 'NOMBRE_2' id_fuente = 'Q10' opcional = '' seccion_hol = 'INTERLOCUTOR' estruc_abap = 'BAPIPARNR' campo_abap = 'NAME_2' descripcion = 'Nombre 2' longitud = '35.0' )
( operacion = 'PV' seccion = 'INT' campo_hol = 'PAIS' id_fuente = 'Q10' opcional = '' seccion_hol = 'INTERLOCUTOR' estruc_abap = 'BAPIPARNR' campo_abap = 'COUNTRY' descripcion = 'Clave de país' longitud = '3.0' )
( operacion = 'PV' seccion = 'TXT' campo_hol = 'ID_TEXTO' id_fuente = 'Q10' opcional = '' seccion_hol = 'TEXTOS' estruc_abap = 'BAPISDTEXT' campo_abap = 'TEXT_ID' descripcion = 'ID de texto' longitud = '4.0' )
( operacion = 'PV' seccion = 'TXT' campo_hol = 'IDIOMA' id_fuente = 'Q10' opcional = '' seccion_hol = 'TEXTOS' estruc_abap = 'BAPISDTEXT' campo_abap = 'LANGU' descripcion = 'Clave de idioma' longitud = '1.0' )
( operacion = 'PV' seccion = 'TXT' campo_hol = 'TEXTO' id_fuente = 'Q10' opcional = '' seccion_hol = 'TEXTOS' estruc_abap = 'BAPISDTEXT' campo_abap = 'TEXT_LINE' descripcion = 'Línea de texto' longitud = '99.0' )
( operacion = 'PV' seccion = 'TXT' campo_hol = 'FUNCION' id_fuente = 'Q10' opcional = '' seccion_hol = 'TEXTOS' estruc_abap = 'BAPISDTEXT' campo_abap = 'FUNCTION' descripcion = 'Función' longitud = '3.0' )
    ).

  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ADAPTER
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_adapter
    DEFINITION CREATE PUBLIC
    INHERITING FROM zcl_rest_adapter_base
    FRIENDS ltcl_creator.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS adapt_customized REDEFINITION.

    METHODS adapt_header
      IMPORTING is_struct_ref TYPE REF TO data.

ENDCLASS.

CLASS lcl_adapter IMPLEMENTATION.

  METHOD adapt_customized.

    LOOP AT it_structs ASSIGNING FIELD-SYMBOL(<s_struct>).

      CASE <s_struct>-struct_name.
        WHEN 'BAPISDHD1'. " Datos de cabecera
          adapt_header( <s_struct>-struct_ref ).
        WHEN 'ZSRESTADICIONALES'. " Datos adicionales
        WHEN 'BAPIPARNR'. " Interlocutor
        WHEN 'BAPISDITM'.  " Datos psiciones
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD adapt_header.

  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" LLAMADOR BAPI
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_bapi_caller DEFINITION
    INHERITING FROM zcl_rest_bapi_caller_base
    CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS call REDEFINITION.
    METHODS get_bapi_name REDEFINITION.

  PROTECTED SECTION.
    METHODS process_bapi_result REDEFINITION.

ENDCLASS.

CLASS lcl_bapi_caller IMPLEMENTATION.

  METHOD call.

  ENDMETHOD.

  METHOD get_bapi_name.

  ENDMETHOD.

  METHOD process_bapi_result.
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" LLAMADOR BAPI
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_register
    DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_rest_register.

    DATA t_registrables TYPE zcl_rest_cte=>tt_registrable.

ENDCLASS.

CLASS lcl_register IMPLEMENTATION.

  METHOD zif_rest_register~add_registrable.
    APPEND VALUE #( o_instance = io_registrable ) TO me->t_registrables.
  ENDMETHOD.

  METHOD zif_rest_register~collect.
  ENDMETHOD.

  METHOD zif_rest_register~do_commit.
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CREATOR -> AUXILIAR PARA PRUEBAS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_creator DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_rest_creator_base.
  		
  PUBLIC SECTION.
    METHODS init_adapter     REDEFINITION.
    METHODS init_bapi_caller REDEFINITION.
    METHODS init_register    REDEFINITION.

  PROTECTED SECTION.
    METHODS init_instances   REDEFINITION.

ENDCLASS.


CLASS lcl_creator IMPLEMENTATION.

  METHOD init_adapter.
    co_adapter = NEW lcl_adapter( iv_oper      = iv_oper
                                  iv_id_fuente = iv_id_fuente
                                  iv_req_id    = iv_req_id
                                  io_serv_dao  = NEW lcl_rest_dao( ) ).
  ENDMETHOD.

  METHOD init_bapi_caller.
    co_caller = NEW lcl_bapi_caller( iv_oper = '' iv_id_fuente = '' iv_req_id = '' ).
  ENDMETHOD.

  METHOD init_register.
    co_reg = NEW lcl_register( ).
  ENDMETHOD.

  METHOD init_instances.
    me->o_rest_dao = NEW lcl_rest_dao( ).
    me->cte = NEW zcl_rest_cte( ).
  ENDMETHOD.

ENDCLASS.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CLASE DE PRUEBAS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS ltcl_creator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES tt_zsrestadicionales TYPE STANDARD TABLE OF zsrestadicionales WITH DEFAULT KEY.
    TYPES tt_bapisdhd1 TYPE STANDARD TABLE OF bapisdhd1 WITH DEFAULT KEY.
    TYPES tt_bapisditm TYPE STANDARD TABLE OF bapisditm WITH DEFAULT KEY.
    TYPES tt_bapicond TYPE STANDARD TABLE OF bapicond WITH DEFAULT KEY.
    TYPES tt_bapischdl TYPE STANDARD TABLE OF bapischdl WITH DEFAULT KEY.
    TYPES tt_bapisdtext TYPE STANDARD TABLE OF bapisdtext WITH DEFAULT KEY.
    TYPES tt_bapiparnr TYPE STANDARD TABLE OF bapiparnr WITH DEFAULT KEY.

    DATA o_creator TYPE REF TO zcl_rest_creator_base.
    DATA cte TYPE REF TO zcl_rest_cte.
    DATA fpos LIKE zcl_rest_cte=>c_special_field-num_posicion.
    DATA v_req_id  TYPE zdrestreqid VALUE '12345'.


    METHODS setup.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " TEST METHODS
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    METHODS create_ok_sd FOR TESTING RAISING cx_static_check.
    METHODS create_bad FOR TESTING RAISING cx_static_check.

    METHODS create_ok_fi. " FOR TESTING RAISING cx_static_check.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    METHODS find_payload
      IMPORTING iv_textname   TYPE tdobname
      RETURNING VALUE(rv_res) TYPE string.

    METHODS exp_zsrestadicionales
      RETURNING VALUE(rt_result) TYPE tt_zsrestadicionales.

    METHODS exp_bapisdhd1
      RETURNING VALUE(rt_result) TYPE tt_bapisdhd1.

    METHODS exp_bapisditm
      RETURNING VALUE(rt_result) TYPE tt_bapisditm.

    METHODS exp_bapicond
      RETURNING VALUE(rt_result) TYPE tt_bapicond.

    METHODS exp_bapischdl
      RETURNING VALUE(rt_result) TYPE tt_bapischdl.

    METHODS exp_bapisdtext
      RETURNING VALUE(rt_result) TYPE tt_bapisdtext.

    METHODS exp_bapiparnr
      RETURNING VALUE(rt_result) TYPE tt_bapiparnr.

ENDCLASS.


CLASS ltcl_creator IMPLEMENTATION.

  METHOD setup.
    me->cte = NEW #( ).
    me->fpos = zcl_rest_cte=>c_special_field-num_posicion.
  ENDMETHOD.

  METHOD create_ok_sd.

    TRY.
        me->o_creator = NEW lcl_creator( iv_oper = zcl_rest_cte=>c_operation-pedido_venta iv_id_fuente = 'Q10' ).
        DATA(v_payload) = find_payload( iv_textname = 'ZSDCASOBASE' ).

        "Evitar el proceso en paralelo
        me->o_creator->v_payload = v_payload.

        me->o_creator->create( iv_payload = v_payload iv_req_id = me->v_req_id ).

        DATA(o_adapter) = CAST lcl_adapter( me->o_creator->o_adapter ).

        "Se espera haber examinado 13 estructuras
        IF lines( o_adapter->t_structs ) NE 13.
          cl_abap_unit_assert=>fail( 'No se generaron todas las estructuras' ).
        ENDIF.

        "Separar cada estructura para comparar
        DATA t_con TYPE zcl_rest_cte=>tt_bapi_struct.
        DATA t_int TYPE zcl_rest_cte=>tt_bapi_struct.
        DATA t_rep TYPE zcl_rest_cte=>tt_bapi_struct.
        DATA t_cab TYPE zcl_rest_cte=>tt_bapi_struct.
        DATA t_ite TYPE zcl_rest_cte=>tt_bapi_struct.
        DATA t_txt TYPE zcl_rest_cte=>tt_bapi_struct.
        DATA t_adi TYPE zcl_rest_cte=>tt_bapi_struct.

        LOOP AT o_adapter->t_structs ASSIGNING FIELD-SYMBOL(<s_structs>).

          CASE <s_structs>-struct_name.
            WHEN 'BAPISDHD1'.
              "cabecera
              INSERT <s_structs> INTO TABLE t_cab.
            WHEN 'BAPIPARNR'.
              "interlocutores
              INSERT <s_structs> INTO TABLE t_int.
            WHEN 'BAPISDITM'.
              "items
              INSERT <s_structs> INTO TABLE t_ite.
            WHEN 'BAPICOND'.
              "condiciones
              INSERT <s_structs> INTO TABLE t_con.
            WHEN 'BAPISCHDL'.
              "repartos
              INSERT <s_structs> INTO TABLE t_rep.
            WHEN 'BAPISDTEXT'.
              "textos
              INSERT <s_structs> INTO TABLE t_txt.
            WHEN 'ZSRESTADICIONALES'.
              "adicionales
              INSERT <s_structs> INTO TABLE t_adi.
          ENDCASE.

        ENDLOOP.

        "----------------------------------------------------------------------
        "Resultado esperado de proceso ADAPT
        "----------------------------------------------------------------------
        DATA(t_adi_exp) = exp_zsrestadicionales( ).
        DATA(t_cab_exp) = exp_bapisdhd1( ).
        DATA(t_ite_exp) = exp_bapisditm( ).
        DATA(t_con_exp) = exp_bapicond( ).
        DATA(t_rep_exp) = exp_bapischdl( ).
        DATA(t_txt_exp) = exp_bapisdtext( ).
        DATA(t_int_exp) = exp_bapiparnr( ).

        DATA r_struct TYPE REF TO data.
        FIELD-SYMBOLS <t_struct> TYPE any.

        r_struct = t_cab[ 1 ]-struct_ref. ASSIGN r_struct->* TO <t_struct>.
        cl_abap_unit_assert=>assert_equals( act = <t_struct>  exp = t_cab_exp[ 1 ] ).
        LOOP AT t_int ASSIGNING FIELD-SYMBOL(<wa>).
          ASSIGN <wa>-struct_ref->* TO <t_struct>.
          cl_abap_unit_assert=>assert_table_contains( line = <t_struct> table = t_int_exp ).
        ENDLOOP.
        LOOP AT t_ite ASSIGNING <wa>.
          ASSIGN <wa>-struct_ref->* TO <t_struct>.
          cl_abap_unit_assert=>assert_table_contains( line = <t_struct> table = t_ite_exp ).
        ENDLOOP.
        LOOP AT t_con ASSIGNING <wa>.
          ASSIGN <wa>-struct_ref->* TO <t_struct>.
          cl_abap_unit_assert=>assert_table_contains( line = <t_struct> table = t_con_exp ).
        ENDLOOP.
        LOOP AT t_rep ASSIGNING <wa>.
          ASSIGN <wa>-struct_ref->* TO <t_struct>.
          cl_abap_unit_assert=>assert_table_contains( line = <t_struct> table = t_rep_exp ).
        ENDLOOP.
        LOOP AT t_txt ASSIGNING <wa>.
          ASSIGN <wa>-struct_ref->* TO <t_struct>.
          cl_abap_unit_assert=>assert_table_contains( line = <t_struct> table = t_txt_exp ).
        ENDLOOP.
        LOOP AT t_adi ASSIGNING <wa>.
          ASSIGN <wa>-struct_ref->* TO <t_struct>.
          cl_abap_unit_assert=>assert_table_contains( line = <t_struct> table = t_adi_exp ).
        ENDLOOP.

      CATCH zcx_rest_ex INTO DATA(o_ex).
        cl_abap_unit_assert=>fail( |Exepción: { o_ex->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD create_bad.

    "----------------------------------------------
    " Caso objeto resp no inicializado
    " Se espera: status error interno
    "----------------------------------------------
    DATA o_resp TYPE REF TO cl_rest_response.
    "creator->zif_rest_processor~create( io_req = NEW lcl_request( ) eo_resp = o_resp ).

    cl_abap_unit_assert=>assert_not_bound( o_resp ).

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
      rv_res = rv_res && <s_line>-tdline.
    ENDLOOP.

  ENDMETHOD.

  METHOD exp_zsrestadicionales.
    rt_result = VALUE #(
      ( clave = 'tipo_id' valor =  'CC' )
      ( clave = 'num_id'  valor = '123654789' )
    ).
  ENDMETHOD.

  METHOD exp_bapisdhd1.
    rt_result = VALUE #(
      ( distr_chan = 'VW' doc_type = 'ZPVB' pmnttrms = '90' req_date_h = '20210522' sales_org = 'EMPR' division = 'CM' )
    ).
  ENDMETHOD.

  METHOD exp_bapisditm.
    rt_result = VALUE #(
      ( itm_number = '20' material = '41000865' plant = 'CL01' store_loc = '1030' sales_unit = 'UN' comp_quant = '4' currency = 'COP' profit_ctr = '10100000' )
      ( itm_number = '10' material = '41000865' plant = 'CL01' store_loc = '1030' sales_unit = 'UN' comp_quant = '2' currency = 'COP' profit_ctr = '10100000' )
    ).
  ENDMETHOD.

  METHOD exp_bapicond.
    rt_result = VALUE #(
      ( itm_number = '20' cond_type = 'ZDEL' cond_value = '5000' )
      ( itm_number = '20' cond_type = 'ZPVE' cond_value = '20000' )
      ( itm_number = '10' cond_type = 'ZPVE' cond_value = '10000' )
    ).
  ENDMETHOD.

  METHOD exp_bapischdl.
    rt_result = VALUE #(
      ( itm_number = '20' sched_line = '1'  req_qty = '4' )
      ( itm_number = '10' sched_line = '1'  req_qty = '2' )
    ).
  ENDMETHOD.

  METHOD exp_bapisdtext.
    rt_result = VALUE #(
      ( text_id = '0002' text_line = 'Representacion grafica' function = '009' )
    ).
  ENDMETHOD.

  METHOD exp_bapiparnr.

    rt_result = VALUE #(
      ( partn_role = 'SP' name = ''          name_2 = ''       partn_numb = '0F60000521' country = '' )
      ( partn_role = 'ZC' name = 'acudiente' name_2 = '123456' partn_numb = '0F60000521' country = 'CO' )
    ).

  ENDMETHOD.

  METHOD create_ok_fi.

    TRY.
        me->o_creator = NEW lcl_creator( iv_oper = zcl_rest_cte=>c_operation-pedido_venta iv_id_fuente = 'Q10' ).
        DATA(v_payload) = find_payload( iv_textname = 'ZFICASOBASE' ).

        "Preasignando el payload se evita el proceso en paralelo
        me->o_creator->v_payload = v_payload.

        me->o_creator->create( iv_payload = v_payload ).

      CATCH zcx_rest_ex INTO DATA(o_ex).
        cl_abap_unit_assert=>fail( |Exepción: { o_ex->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
