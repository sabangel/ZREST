CLASS lcl_caller DEFINITION CREATE PUBLIC
    INHERITING FROM zcl_rest_bapi_caller_base.
  		
  PUBLIC SECTION.

    METHODS get_bapi_name REDEFINITION.

  PROTECTED SECTION.

    METHODS process_bapi_result REDEFINITION.

    METHODS register_bapi_return_msges REDEFINITION.

ENDCLASS.

CLASS lcl_caller IMPLEMENTATION.

  METHOD get_bapi_name.
    rv_name = 'BAPI_SALESORDER_CREATEFROMDAT2'.
  ENDMETHOD.


  METHOD process_bapi_result.
    rv_process_ok = abap_true.
  ENDMETHOD.

  METHOD register_bapi_return_msges.

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_bapi_caller DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA o_caller TYPE REF TO lcl_caller.

    DATA t_structs TYPE zcl_rest_cte=>tt_bapi_struct.

    DATA: s_cab     TYPE bapisdhd1,
          s_cond    TYPE bapicond,
          s_cond_2  TYPE bapicond,
          s_cond_3  TYPE bapicond,
          s_text    TYPE bapisdtext,
          s_item    TYPE bapisditm,
          s_item_2  TYPE bapisditm,
          s_partn   TYPE bapiparnr,
          s_partn_2 TYPE bapiparnr,
          s_schdl   TYPE bapischdl,
          s_schdl_2 TYPE bapischdl.


    METHODS setup.

    METHODS call_ok FOR TESTING RAISING cx_static_check.

    METHODS build_structs
      RETURNING VALUE(rt_structs) TYPE zcl_rest_cte=>tt_bapi_struct.

ENDCLASS.


CLASS ltcl_bapi_caller IMPLEMENTATION.

  METHOD setup.

    me->o_caller = NEW lcl_caller( iv_oper = 'PV' iv_id_fuente = 'Q10' iv_req_id = '2' ).

  ENDMETHOD.


  METHOD call_ok.

    build_structs(  ).

    me->o_caller->call( iv_num_doc_ext = '1' it_structs = me->t_structs ).

    cl_abap_unit_assert=>assert_not_initial( act = me->t_structs ).

  ENDMETHOD.

  METHOD build_structs.

* Header
    s_cab = VALUE bapisdhd1(
        doc_type    =   'ZPVB'
        sales_org   =   'EMPR'
        distr_chan  =   'VW'
        division    =   'CM'
        req_date_h  =   '20210522'
        pmnttrms    =   '0090'
        purch_no_c  =   'PRUEBA 4245 Q10'
    ).

* Items
    s_item   = VALUE bapisditm(
        itm_number  =   '000010'
        material    =   '000000000041000865'
        plant       =   'CL01'
        store_loc   =   '1030'
        sales_unit  =   'UN'
        comp_quant  =   '2'
        currency    =   'COP'
        profit_ctr  =   '0010100000' ).

    s_item_2 = VALUE bapisditm(
        itm_number  =   '000020'
        material    =   '000000000041000865'
        plant       =   'CL01'
        store_loc   =   '1030'
        sales_unit  =   'UN'
        comp_quant  =   '2'
        currency    =   'COP' ).

* Partners
    s_partn   = VALUE bapiparnr(
        partn_role =    'AG'
        partn_numb =    '0F10000003' ).

    s_partn_2 = VALUE bapiparnr(
        partn_role =    'ZC'
        partn_numb =    '0F10000003'
        name       =    'Nombre prueba'
        name_2     =    '1234567890'
        country    =    'CO' ).

* Schedule
    s_schdl   = VALUE bapischdl(
         itm_number  =   '000010'
         sched_line  =   '0001'
         req_qty     =   '2' ).
    s_schdl_2 = VALUE bapischdl(
         itm_number  =   '000020'
         sched_line  =   '0001'
         req_qty     =   '4' ).

* Conditions
    s_cond = VALUE bapicond(
        itm_number  =   '000010'
        cond_type   =   'ZPVE'
        cond_value  =   '10000'
    ).

    s_cond_2 = VALUE bapicond(
        itm_number  =   '000020'
        cond_type   =   'ZPVE'
        cond_value  =   '20000'
    ).

    s_cond_3 = VALUE bapicond(
         itm_number  =   '000020'
         cond_type   =   'ZDEL'
         cond_value  =   '5000'
     ).

* Textos
    s_text = VALUE bapisdtext(
            text_id     = '0002'
            langu       = sy-langu
            text_line   = 'Prueba txt Rep.Grafica Q10'
            function    = '009'

    ).

    me->t_structs = VALUE #(
    ( struct_name = 'BAPISDHD1'     struct_ref = REF #( s_cab ) )
    ( struct_name = 'BAPISDTEXT'    struct_ref = REF #( s_text ) )
    ( struct_name = 'BAPICOND'      struct_ref = REF #( s_cond ) )
    ( struct_name = 'BAPICOND'      struct_ref = REF #( s_cond_2 ) )
    ( struct_name = 'BAPICOND'      struct_ref = REF #( s_cond_3 ) )
    ( struct_name = 'BAPISDITM'     struct_ref = REF #( s_item ) )
    ( struct_name = 'BAPISDITM'     struct_ref = REF #( s_item_2 ) )
    ( struct_name = 'BAPIPARNR'     struct_ref = REF #( s_partn ) )
    ( struct_name = 'BAPIPARNR'     struct_ref = REF #( s_partn_2 ) )
    ( struct_name = 'BAPISCHDL'     struct_ref = REF #( s_schdl ) )
    ( struct_name = 'BAPISCHDL'     struct_ref = REF #( s_schdl_2 ) )

    ).

  ENDMETHOD.

ENDCLASS.
