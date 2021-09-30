""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Clase MOCK DAO tabla = ZTREST_LOG
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_finder_dao DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_rest_finder_dao.
    ALIASES find FOR zif_rest_finder_dao~find_data_log.

    METHODS constructor
      IMPORTING iv_case TYPE string OPTIONAL.

  PRIVATE SECTION.
    DATA v_case TYPE string.

ENDCLASS.

CLASS lcl_finder_dao IMPLEMENTATION.

  METHOD constructor.
    me->v_case = iv_case.
  ENDMETHOD.


  METHOD zif_rest_finder_dao~find_data_log.

    IF me->v_case EQ 'OK'.
      ct_result = VALUE #( ( id_fuente = iv_id_fuente
                             req_id = 'AGERPHCwkfqI1np3jNGSLjkzEhL9'
                             num_doc_ext = 'PV12398'
                             status = 'ED'
                             timestamp = '20210825215158'
                             message   = 'Error con el tipo de identificación CO1N y numero 8000418296 no se encontre BP.' ) ).
      RETURN.
    ENDIF.

    IF me->v_case EQ 'NO_LOG'.
      ct_result = VALUE #( ).
      RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Clase MOCK PARAMETROS get
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_get_parm DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS fill_parameters_get
      RETURNING VALUE(rt_parameters) TYPE tihttpnvp.

ENDCLASS.

CLASS lcl_get_parm IMPLEMENTATION.

  METHOD fill_parameters_get.
    rt_parameters = VALUE #( ( name = 'REQ_ID' value = 'AGERPHCwkfqI1np3jNGSLjkzEhL9' ) ).
  ENDMETHOD.

ENDCLASS.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Clase Finder por defecto
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_finder_def DEFINITION
  INHERITING FROM zcl_rest_finder_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_dao  TYPE REF TO zif_rest_finder_dao OPTIONAL
                iv_case TYPE string OPTIONAL.

  PROTECTED SECTION.
    METHODS find_complements REDEFINITION.
    METHODS get_standard_log REDEFINITION.

  PRIVATE SECTION.

    DATA v_case TYPE string.
    METHODS fill_struct     CHANGING co_data TYPE REF TO data.
    METHODS fill_type_table CHANGING co_data TYPE REF TO data.
    METHODS fill_value      CHANGING co_data TYPE REF TO data.

ENDCLASS.

CLASS lcl_finder_def IMPLEMENTATION.

  METHOD constructor.
    super->constructor( io_dao = io_dao ).
    me->v_case = iv_case.
  ENDMETHOD.


  METHOD find_complements.

    CASE me->v_case.
      WHEN 'OK_STRUCT'.
        fill_struct( CHANGING co_data = er_result  ).
      WHEN 'OK_TYPE_TABLE'.
        fill_type_table( CHANGING co_data = er_result ).
      WHEN 'OK_VALUE'.
        fill_value( CHANGING co_data = er_result ).
    ENDCASE.

  ENDMETHOD.

  METHOD fill_struct.

    CREATE DATA co_data TYPE zsrestdocumentosget.
    ASSIGN co_data->* TO FIELD-SYMBOL(<s_result>).
    <s_result> = VALUE zsrestdocumentosget(
        req_id = '10' num_doc_ext = '25' status = 'OK' fecha_creacion = '20210825215158'
        mensajes = VALUE ztrestmensajeget( (  tipo = 'E' texto = 'Lammada MF: Parámetros insuficientes' )
        ( tipo = 'E' texto = 'El documento de venta  no se modifica' ) )
    ).

  ENDMETHOD.

  METHOD fill_type_table.

    CREATE DATA co_data TYPE ztrestdocumentosget.
    ASSIGN co_data->* TO FIELD-SYMBOL(<s_result>).
    <s_result> = VALUE ztrestdocumentosget(
        ( req_id = '10' num_doc_ext = '25' status = 'OK' fecha_creacion = '20210825215158' mensajes = VALUE ztrestmensajeget( ( tipo = 'S' texto = 'Doc creado correctamente' ) ) )
        ( req_id = '20' num_doc_ext = '26' status = 'OK' fecha_creacion = '20210825215158' mensajes = VALUE ztrestmensajeget( ( tipo = 'S' texto = 'Doc creado correctamente' ) ) )
    ).

  ENDMETHOD.

  METHOD fill_value.

    CREATE DATA co_data TYPE zdrestreqid.
    ASSIGN co_data->* TO FIELD-SYMBOL(<s_result>).
    <s_result> = '10'.

  ENDMETHOD.

  METHOD get_standard_log.

    IF me->v_case = 'OK_STRUCT' OR me->v_case = 'OK_TYPE_TABLE' OR   me->v_case = 'OK_VALUE'.
      rt_mensajes = VALUE #(
        ( tipo  = 'E' texto = 'Lammada MF: Parámetros insuficientes' )
        ( tipo  = 'E' texto = 'El documento de venta  no se modifica' )
      ) .
      RETURN.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Clase de test
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS ltcl_finder DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA o_finder_dao          TYPE REF TO lcl_finder_dao.
    DATA o_finder_def          TYPE REF TO lcl_finder_def.
    DATA o_mock_parameters_get TYPE REF TO lcl_get_parm.


    METHODS setup.
    METHODS test_ok_documents  FOR TESTING.
    METHODS test_ok_struct     FOR TESTING.
    METHODS test_ok_type_table FOR TESTING.
    METHODS test_ok_value      FOR TESTING.

ENDCLASS.

CLASS ltcl_finder IMPLEMENTATION.

  METHOD setup.
    me->o_finder_dao          = NEW #( iv_case ='OK' ).
    me->o_mock_parameters_get = NEW #(  ).
  ENDMETHOD.

  METHOD test_ok_documents.

    FIELD-SYMBOLS <s_result_act> TYPE any.
    FIELD-SYMBOLS <s_result_exp> TYPE any.

    FIELD-SYMBOLS <r_documentos> TYPE REF TO data.
    FIELD-SYMBOLS <data>         TYPE zsrestgetresponsedata.

    DATA r_data_act TYPE REF TO data.
    DATA r_data_exp TYPE REF TO data.

    DATA(t_url_attrib) = me->o_mock_parameters_get->fill_parameters_get( ).
    DATA(t_url_attrib_exp) = VALUE tihttpnvp( ( name = 'REQ_ID' value = 'AGERPHCwkfqI1np3jNGSLjkzEhL9'  ) ).

    cl_abap_unit_assert=>assert_equals( act = t_url_attrib exp = t_url_attrib_exp ).

    TRY.

        me->o_finder_def = NEW #( iv_case = '' io_dao = NEW lcl_finder_dao( iv_case = 'OK' ) ).
        DATA(o_ref_data) = me->o_finder_def->find( iv_oper = 'PV' iv_id_fuente = 'Q10' it_url_attrib = VALUE #( ( name = 'REQ_ID' value = 'AGERPHCwkfqI1np3jNGSLjkzEhL9' ) ) ).

        CREATE DATA r_data_act TYPE zsrestgetresponsedata.
        r_data_act = o_ref_data.

        DATA(o_docs_descr) = CAST  cl_abap_tabledescr( cl_abap_typedescr=>describe_by_name( 'ZTRESTDOCUMENTOSGET' ) ).
        DATA(o_docs_line_descr) = CAST cl_abap_structdescr( o_docs_descr->get_table_line_type( ) ).

        DATA(o_type_exp) = cl_abap_structdescr=>create(
            p_components = VALUE #(
              ( name = 'DOCUMENTOS' type = o_docs_descr ) )
        ).

        CREATE DATA r_data_exp TYPE HANDLE o_type_exp.
        ASSIGN r_data_exp->('DOCUMENTOS') TO FIELD-SYMBOL(<t_documentos>).

        FIELD-SYMBOLS <t_documentos_exp> TYPE ANY TABLE.
        DATA r_documentos_s TYPE REF TO data.
        CREATE DATA r_documentos_s TYPE HANDLE o_docs_line_descr.
        ASSIGN r_documentos_s->* TO FIELD-SYMBOL(<s_documentos>).

        ASSIGN r_data_exp->('DOCUMENTOS') TO <t_documentos_exp>.
        ASSIGN COMPONENT 'REQ_ID' OF STRUCTURE <s_documentos> TO FIELD-SYMBOL(<v_value>). <v_value> = 'AGERPHCwkfqI1np3jNGSLjkzEhL9'.
        ASSIGN COMPONENT 'NUM_DOC_EXT' OF STRUCTURE <s_documentos> TO <v_value>. <v_value> = 'PV12398'.
        ASSIGN COMPONENT 'STATUS' OF STRUCTURE <s_documentos> TO <v_value>. <v_value> = 'ED'.
        ASSIGN COMPONENT 'FECHA_CREACION' OF STRUCTURE <s_documentos> TO <v_value>. <v_value> = '20210825215158'.
        INSERT <s_documentos> INTO TABLE <t_documentos_exp>.

        <t_documentos> = <t_documentos_exp>.

        ASSIGN r_data_act->* TO <s_result_act>.
        ASSIGN r_data_exp->* TO <s_result_exp>.

        cl_abap_unit_assert=>assert_equals( act = <s_result_act> exp = <s_result_exp> ).

      CATCH zcx_rest_ex INTO DATA(o_ex).
        cl_abap_unit_assert=>fail( |Exepción: { o_ex->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_ok_struct.

    FIELD-SYMBOLS <s_result_act> TYPE any.
    FIELD-SYMBOLS <s_result_exp> TYPE any.

    FIELD-SYMBOLS <r_datos_complementarios> TYPE REF TO data.
    FIELD-SYMBOLS <data>                    TYPE zsrestdocumentosget.

    DATA r_data_act TYPE REF TO data.
    DATA r_data_exp TYPE REF TO data.

    DATA(t_url_attrib) = me->o_mock_parameters_get->fill_parameters_get( ).
    DATA(t_url_attrib_exp) = VALUE tihttpnvp( ( name = 'REQ_ID' value = 'AGERPHCwkfqI1np3jNGSLjkzEhL9'  ) ).

    cl_abap_unit_assert=>assert_equals( act = t_url_attrib exp = t_url_attrib_exp ).

    TRY.

        me->o_finder_def = NEW #( iv_case = 'OK_STRUCT' io_dao = NEW lcl_finder_dao( iv_case = 'OK' ) ).
        DATA(o_ref_data) = me->o_finder_def->find( iv_oper = 'PV' iv_id_fuente = 'Q10' it_url_attrib = VALUE #( ( name = 'REQ_ID' value = 'AGERPHCwkfqI1np3jNGSLjkzEhL9' ) ) ).

        ASSIGN o_ref_data->('DATOS_COMPLEMENTARIOS') TO <r_datos_complementarios>.
        ASSIGN <r_datos_complementarios>->* TO <data>.

        CREATE DATA r_data_act TYPE zsrestdocumentosget.

        ASSIGN r_data_act->('REQ_ID')         TO FIELD-SYMBOL(<v_value>). <v_value> = <data>-req_id.
        ASSIGN r_data_act->('NUM_DOC_EXT')    TO <v_value>. <v_value> = <data>-num_doc_ext.
        ASSIGN r_data_act->('STATUS')         TO <v_value>. <v_value> = <data>-status.
        ASSIGN r_data_act->('FECHA_CREACION') TO <v_value>. <v_value> = <data>-fecha_creacion.
        ASSIGN r_data_act->('MENSAJES')       TO <v_value>. <v_value> = <data>-mensajes.

        CREATE DATA r_data_exp TYPE zsrestdocumentosget.

        ASSIGN r_data_exp->('REQ_ID')         TO <v_value>. <v_value> = '10'.
        ASSIGN r_data_exp->('NUM_DOC_EXT')    TO <v_value>. <v_value> = '25'.
        ASSIGN r_data_exp->('STATUS')         TO <v_value>. <v_value> = 'OK'.
        ASSIGN r_data_exp->('FECHA_CREACION') TO <v_value>. <v_value> = '20210825215158'.
        ASSIGN r_data_exp->('MENSAJES')       TO <v_value>. <v_value> = VALUE  ztrestmensajeget( (  tipo = 'E' texto = 'Lammada MF: Parámetros insuficientes' )
                                                                                                 (  tipo = 'E' texto = 'El documento de venta  no se modifica' ) ).

        ASSIGN r_data_act->* TO <s_result_act>.
        ASSIGN r_data_exp->* TO <s_result_exp>.

        cl_abap_unit_assert=>assert_equals( act = <s_result_act> exp = <s_result_exp> ).

      CATCH zcx_rest_ex INTO DATA(o_ex).
        cl_abap_unit_assert=>fail( |Exepción: { o_ex->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_ok_type_table.

    FIELD-SYMBOLS <s_result_act> TYPE any.
    FIELD-SYMBOLS <s_result_exp> TYPE any.

    FIELD-SYMBOLS <r_datos_complementarios> TYPE REF TO data.
    FIELD-SYMBOLS <data>                    TYPE ztrestdocumentosget.

    DATA r_data_act   TYPE REF TO data.
    DATA r_data_exp   TYPE REF TO data.
    DATA r_type_table TYPE REF TO data.

    DATA(t_url_attrib) = me->o_mock_parameters_get->fill_parameters_get( ).
    DATA(t_url_attrib_exp) = VALUE tihttpnvp( ( name = 'REQ_ID' value = 'AGERPHCwkfqI1np3jNGSLjkzEhL9'  ) ).

    cl_abap_unit_assert=>assert_equals( act = t_url_attrib exp = t_url_attrib_exp ).

    TRY.
        me->o_finder_def = NEW #( iv_case = 'OK_TYPE_TABLE' io_dao = NEW lcl_finder_dao( iv_case = 'OK' ) ).
        DATA(o_ref_data) = me->o_finder_def->find( iv_oper = 'PV' iv_id_fuente = 'Q10' it_url_attrib = VALUE #( ( name = 'REQ_ID' value = 'AGERPHCwkfqI1np3jNGSLjkzEhL9' ) ) ).

        ASSIGN o_ref_data->('DATOS_COMPLEMENTARIOS') TO <r_datos_complementarios>.
        ASSIGN <r_datos_complementarios>->* TO <data>.

        CREATE DATA r_data_act TYPE ztrestdocumentosget.
        ASSIGN r_data_act->* TO FIELD-SYMBOL(<t_data_act>).
        <t_data_act> = <data>.

        CREATE DATA r_data_exp TYPE ztrestdocumentosget.

        DATA(o_docs_descr) = CAST  cl_abap_tabledescr( cl_abap_typedescr=>describe_by_name( 'ZTRESTDOCUMENTOSGET' ) ).
        DATA(o_docs_line_descr) = CAST cl_abap_structdescr( o_docs_descr->get_table_line_type( ) ).

        FIELD-SYMBOLS <t_documentos_exp> TYPE ANY TABLE.
        DATA r_documentos_s TYPE REF TO data.

        CREATE DATA r_documentos_s TYPE HANDLE o_docs_line_descr.
        ASSIGN r_documentos_s->* TO FIELD-SYMBOL(<s_documentos>).

        ASSIGN r_data_exp->* TO <t_documentos_exp>.
        ASSIGN COMPONENT 'REQ_ID' OF STRUCTURE <s_documentos> TO FIELD-SYMBOL(<v_value>). <v_value> = '10'.
        ASSIGN COMPONENT 'NUM_DOC_EXT' OF STRUCTURE <s_documentos> TO <v_value>.          <v_value> = '25'.
        ASSIGN COMPONENT 'STATUS' OF STRUCTURE <s_documentos> TO <v_value>.               <v_value> = 'OK'.
        ASSIGN COMPONENT 'FECHA_CREACION' OF STRUCTURE <s_documentos> TO <v_value>.       <v_value> = '20210825215158'.
        ASSIGN COMPONENT 'MENSAJES' OF STRUCTURE <s_documentos> TO <v_value>.             <v_value> = VALUE ztrestmensajeget( (  tipo = 'S' texto = 'Doc creado correctamente' ) ).
        INSERT <s_documentos> INTO TABLE <t_documentos_exp>.

        ASSIGN COMPONENT 'REQ_ID' OF STRUCTURE <s_documentos> TO <v_value>.         <v_value> = '20'.
        ASSIGN COMPONENT 'NUM_DOC_EXT' OF STRUCTURE <s_documentos> TO <v_value>.    <v_value> = '26'.
        ASSIGN COMPONENT 'STATUS' OF STRUCTURE <s_documentos> TO <v_value>.         <v_value> = 'OK'.
        ASSIGN COMPONENT 'FECHA_CREACION' OF STRUCTURE <s_documentos> TO <v_value>. <v_value> = '20210825215158'.
        ASSIGN COMPONENT 'MENSAJES' OF STRUCTURE <s_documentos> TO <v_value>.       <v_value> = VALUE ztrestmensajeget( (  tipo = 'S' texto = 'Doc creado correctamente' ) ).
        INSERT <s_documentos> INTO TABLE <t_documentos_exp>.

        <data> = <t_documentos_exp>.

        ASSIGN r_data_exp->* TO FIELD-SYMBOL(<t_data_exp>).
        <t_data_exp> = <data>.

        ASSIGN r_data_act->* TO <s_result_act>.
        ASSIGN r_data_exp->* TO <s_result_exp>.

        cl_abap_unit_assert=>assert_equals( act = <s_result_act> exp = <s_result_exp> ).

      CATCH zcx_rest_ex INTO DATA(o_ex).
        cl_abap_unit_assert=>fail( |Exepción: { o_ex->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_ok_value.

    FIELD-SYMBOLS <s_result_act> TYPE any.
    FIELD-SYMBOLS <s_result_exp> TYPE any.

    FIELD-SYMBOLS <r_datos_complementarios> TYPE REF TO data.
    FIELD-SYMBOLS <data>                    TYPE zdrestreqid.

    DATA r_data_act TYPE REF TO data.
    DATA r_data_exp TYPE REF TO data.

    DATA(t_url_attrib) = me->o_mock_parameters_get->fill_parameters_get( ).
    DATA(t_url_attrib_exp) = VALUE tihttpnvp( ( name = 'REQ_ID' value = 'AGERPHCwkfqI1np3jNGSLjkzEhL9'  ) ).

    cl_abap_unit_assert=>assert_equals( act = t_url_attrib exp = t_url_attrib_exp ).

    TRY.
        me->o_finder_def = NEW #( iv_case = 'OK_VALUE' io_dao = NEW lcl_finder_dao( iv_case = 'OK' ) ).
        DATA(o_ref_data) = me->o_finder_def->find( iv_oper = 'PV' iv_id_fuente = 'Q10' it_url_attrib = VALUE #( ( name = 'REQ_ID' value = 'AGERPHCwkfqI1np3jNGSLjkzEhL9' ) ) ).

        ASSIGN o_ref_data->('DATOS_COMPLEMENTARIOS') TO <r_datos_complementarios>.
        ASSIGN <r_datos_complementarios>->* TO <data>.

        CREATE DATA r_data_act TYPE zdrestreqid.
        ASSIGN r_data_act->*         TO FIELD-SYMBOL(<v_value>). <v_value> = <data>.

        CREATE DATA r_data_exp TYPE zdrestreqid.
        ASSIGN r_data_exp->*        TO <v_value>. <v_value> = '10'.

        ASSIGN r_data_act->* TO <s_result_act>.
        ASSIGN r_data_exp->* TO <s_result_exp>.

        cl_abap_unit_assert=>assert_equals( act = <s_result_act> exp = <s_result_exp> ).

      CATCH zcx_rest_ex INTO DATA(o_ex).
        cl_abap_unit_assert=>fail( |Exepción: { o_ex->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
