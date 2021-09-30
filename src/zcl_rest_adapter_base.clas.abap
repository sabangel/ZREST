CLASS zcl_rest_adapter_base DEFINITION
  INHERITING FROM zcl_rest_processor_base
  PUBLIC
  CREATE PUBLIC
  ABSTRACT.

  PUBLIC SECTION.
    INTERFACES zif_rest_adapter.
    ALIASES adapt FOR zif_rest_adapter~adapt.

    "! Constructor
    "! @parameter io_serv_dao | Fuiente de info para match campos
    METHODS constructor
      IMPORTING iv_oper      TYPE zdrestoperacion
                iv_id_fuente TYPE zdserv_source_id
                iv_req_id    TYPE zdrestreqid
                io_serv_dao  TYPE REF TO zif_rest_sys_field_dao OPTIONAL.

  PROTECTED SECTION.

    DATA cte           TYPE REF TO zcl_rest_cte.
    DATA util          TYPE REF TO zcl_rest_util.
    DATA o_rest_dao    TYPE REF TO zif_rest_sys_field_dao.
    DATA t_conf_fields TYPE zcl_rest_cte=>tt_conf_fields.
    DATA v_num_doc_ext TYPE zdrestnumdocext.
    DATA t_structs     TYPE zcl_rest_cte=>tt_bapi_struct.

    DATA v_id_fuente TYPE zdserv_source_id.
    DATA v_req_id TYPE zdrestreqid.
    DATA v_operacion TYPE zdrestoperacion.


    "! @parameter ir_root | Referencia a la estructura
    "! @parameter iv_hol_name | Nombre de la estructura
    "! @parameter iv_is_new_table_item | Indica que hay que crear una nueva línea en la tala de estructuras
    "! @parameter ir_parent_root | Referencia a la estructura padre. Se usa cuando es una tabla anidada
    "!     nueva posición para las estructuras repetitivas de una tabla
    METHODS adapt_struct
      IMPORTING ir_root              TYPE REF TO data
                iv_hol_name          TYPE string
                iv_is_new_table_item TYPE abap_bool
                ir_parent_root       TYPE REF TO data OPTIONAL
      RAISING   zcx_rest_ex.

    "! Adapta un bloque tipo tabla
    "! @parameter ir_root | Referencia a la tabla
    "! @parameter iv_name | Nombre de la tabla
    "! @parameter ir_parent_root | Referencia a la estructura padre. Se usa cuando es una tabla anidada
    METHODS adapt_table
      IMPORTING ir_root        TYPE REF TO data
                iv_name        TYPE string
                ir_parent_root TYPE REF TO data OPTIONAL
      RAISING   zcx_rest_ex.

    "! Adapta un valor concreto (llega como string)
    "! @parameter iv_value             | Valor a adaptar
    "! @parameter iv_hol_fieldname     | Nombre holistico del campo que se adaptará
    "! @parameter iv_hol_section_name  | Nombre holistico de la sección (estructura en el json) a llenar
    "! @parameter ir_struct_row        | referencia a la línea de la tabla de estructuras que será afectada
    METHODS adapt_value
      IMPORTING iv_value            TYPE any
                iv_hol_fieldname    TYPE string
                iv_hol_section_name TYPE string
                ir_struct_row       TYPE REF TO zcl_rest_cte=>ty_bapi_struct
      RAISING   zcx_rest_ex.

    "! Retorna una referencia a la línea de la estructura que será modificada y si no la encuentra la crea.
    "! @parameter iv_hol_fieldname    | Nombre holistico del campo que se afectará
    "! @parameter iv_hol_section_name | Nombre holistico de la sección a la que pertenece el campo
    "! @parameter iv_create_new       | Obliga a crear una nueva instancia de la estructura
    "! @parameter rr_instance         | Referencia a la línea que contiene la estructura que será modificada
    METHODS search_struct_row
      IMPORTING iv_hol_fieldname    TYPE string
                iv_hol_section_name TYPE string
                iv_create_new       TYPE abap_bool
      RETURNING VALUE(rr_instance)  TYPE REF TO zcl_rest_cte=>ty_bapi_struct
      RAISING   zcx_rest_ex.

    "! Convierte el valor que entra teniendo en cuenta el tipo del campo abap de destino
    "! @parameter iv_value         | Valor a convertir
    "! @parameter iv_type_kind     | Tipo de dato (extraido de descriptor)
    "! @parameter ev_output        | Valor convertido
    "! @parameter rv_err           | Falso si no pudo convertir el valor
    METHODS convert_value
      IMPORTING iv_value      TYPE any
                iv_type_kind  TYPE abap_typekind
      EXPORTING ev_output     TYPE any
      RETURNING VALUE(rv_err) TYPE zcl_rest_cte=>ty_conv_err.

    "! Agregar campo número posición. Es usa para poder "aplanar" posteriormente
    "! @parameter ir_parent_root | Referencia a la estructura padre
    "! @parameter ir_root        | Referencia a la estructura a la que se le agregará el num_pos
    METHODS insert_position_number
      IMPORTING ir_parent_root TYPE REF TO data
      CHANGING  ir_root        TYPE REF TO data
      RAISING   zcx_rest_ex.

    "! Quita caracteres extraños no imprimibles
    METHODS clean_weird_chars
      CHANGING cv_str TYPE any
      RAISING  cx_static_check cx_dynamic_check.

    "! Adaptaciones y operaciones extra posteriores al llenado de las estructuras <br/>
    "! y previos a la creación del documento de negocio en el sistema
    "! @parameter it_structs | Estructuras pre pobladas con la info del payload
    METHODS adapt_customized ABSTRACT
      CHANGING it_structs TYPE zcl_rest_cte=>tt_bapi_struct
      RAISING  zcx_rest_ex.


  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_rest_adapter_base IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    IF io_serv_dao IS NOT SUPPLIED.
      me->o_rest_dao = NEW zcl_rest_sys_field_dao( ).
    ELSE.
      me->o_rest_dao = io_serv_dao.
    ENDIF.

    me->v_operacion = iv_oper.
    me->v_id_fuente = iv_id_fuente.
    me->v_req_id    = iv_req_id.

    me->cte  = NEW #( ).
    me->util = NEW #( ).

  ENDMETHOD.



  METHOD zif_rest_adapter~adapt.

    TRY.
        util->get_val_from_str_ref( EXPORTING ir_ref = ir_doc iv_fldname = CONV #( cte->c_special_field-doc_ref_externo )
                                    IMPORTING ev_val = me->v_num_doc_ext ).
        IF me->v_num_doc_ext IS INITIAL.
          "Obligatorio que tenga el campos num_doc
          RAISE EXCEPTION TYPE zcx_rest_ex
            EXPORTING
              textid = zcx_rest_ex=>no_doc_number
              p1     = cte->c_special_field-doc_ref_externo
              p2     = 'ZCL_REST_ADAPTER_BASE->ADAPT'.
        ENDIF.

        "La tabla de configuración será usada en los procesamientos anidados
        IF me->t_conf_fields IS INITIAL.
          me->t_conf_fields = me->o_rest_dao->find_source_fields(
              iv_operacion = me->v_operacion iv_id_fuente = me->v_id_fuente iv_buf_result = abap_true ).
        ENDIF.

        ASSIGN ir_doc->* TO FIELD-SYMBOL(<s_dref>).

        DATA(o_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <s_dref> ) ).

        CLEAR me->t_structs.

        LOOP AT o_descr->get_components( ) INTO DATA(s_comp).

          " El número de documento en el JSON no es una estructura y no va para las bapis
          IF s_comp-name EQ cte->c_special_field-doc_ref_externo.
            CONTINUE.
          ENDIF.

          ASSIGN COMPONENT s_comp-name OF STRUCTURE <s_dref> TO FIELD-SYMBOL(<r_root>).

          ASSIGN <r_root>->* TO FIELD-SYMBOL(<any_root>).

          CASE cl_abap_datadescr=>get_data_type_kind( <any_root> ).

            WHEN cl_abap_typedescr=>typekind_struct2.
              adapt_struct( ir_root = <r_root> iv_hol_name = s_comp-name iv_is_new_table_item = abap_true ).

            WHEN cl_abap_typedescr=>typekind_table.
              adapt_table( ir_root = <r_root> iv_name = s_comp-name ).

          ENDCASE.

        ENDLOOP.

        adapt_customized( CHANGING it_structs = me->t_structs ).

        rt_structs = me->t_structs.

      CATCH zcx_rest_ex INTO DATA(o_restex).
        RAISE EXCEPTION o_restex.
      CATCH cx_root INTO DATA(o_rootex).
        RAISE EXCEPTION TYPE zcx_rest_ex
          EXPORTING
            previous = o_rootex
            textid   = zcx_rest_ex=>int_err
            p1       = 'ZCL_REST_ADAPTER_BASE->ADAPT'.
    ENDTRY.

  ENDMETHOD.


  METHOD adapt_table.

    FIELD-SYMBOLS <t_root> TYPE ANY TABLE.
    IF ir_root IS INITIAL.
      "Es posible que una tabla llegue vacía. Ignorarla
      RETURN.
    ENDIF.

    TRY.
        ASSIGN ir_root->* TO FIELD-SYMBOL(<any_root>).
        IF cl_abap_datadescr=>get_data_type_kind( <any_root> ) NE cl_abap_typedescr=>typekind_table.
          RETURN.
        ENDIF.

        ASSIGN ir_root->* TO <t_root>.
        LOOP AT <t_root> ASSIGNING FIELD-SYMBOL(<r_new_root>).

          ASSIGN <r_new_root>->* TO <any_root>.

          CASE cl_abap_datadescr=>get_data_type_kind( <any_root> ).

            WHEN cl_abap_typedescr=>typekind_struct2.

              IF ir_parent_root IS SUPPLIED.
                insert_position_number( EXPORTING ir_parent_root = ir_parent_root CHANGING ir_root = <r_new_root> ).
              ENDIF.
              adapt_struct( ir_root = <r_new_root>  iv_hol_name = iv_name  iv_is_new_table_item = abap_true ).

            WHEN cl_abap_typedescr=>typekind_table.

              adapt_table( ir_root = <r_new_root>  iv_name = iv_name ).

          ENDCASE.

        ENDLOOP.

      CATCH zcx_rest_ex INTO DATA(o_rest).
        RAISE EXCEPTION o_rest.
      CATCH cx_root INTO DATA(o_rootex).
        RAISE EXCEPTION TYPE zcx_rest_ex
          EXPORTING
            previous = o_rootex
            textid   = zcx_rest_ex=>int_err
            p1       = 'ZCL_REST_ADAPTER_BASE->ADAPT_TABLE'.
    ENDTRY.

  ENDMETHOD.


  METHOD adapt_struct.

    FIELD-SYMBOLS <dref> TYPE any.
    FIELD-SYMBOLS <ref> TYPE any.
    DATA v_new_item TYPE abap_bool VALUE abap_true.
    DATA v_has_num_pos TYPE abap_bool.

    IF ir_root IS INITIAL.
      "Es posible que una estructura llegue vacía. Ignorarla
      RETURN.
    ENDIF.

    TRY.
        ASSIGN ir_root->* TO FIELD-SYMBOL(<any_root>).
        IF cl_abap_datadescr=>get_data_type_kind( <any_root> ) NE cl_abap_typedescr=>typekind_struct2.
          RETURN.
        ENDIF.

        DATA(o_sdescr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( ir_root ) ).

        "Primero se procesan solo los campos, no estructuras ni tablas anidadas para asegurarse de
        "capturar el número de posición
        LOOP AT o_sdescr->get_components( ) INTO DATA(s_comp).

          ASSIGN ir_root->(s_comp-name) TO <ref>.
          ASSIGN <ref>->* TO <dref>.

          IF cl_abap_datadescr=>get_data_type_kind( <dref> ) NE cl_abap_typedescr=>typekind_string.
            CONTINUE.
          ENDIF.

          "Indicar si tiene número de posición cada que se empiece el procesamiento de un item de tabla
          IF iv_is_new_table_item EQ abap_true AND s_comp-name EQ cte->c_special_field-num_posicion.
            v_has_num_pos = abap_true.
          ENDIF.

          v_new_item = COND #( WHEN v_new_item EQ abap_true THEN iv_is_new_table_item ).
          DATA(r_struct_row) = search_struct_row( iv_hol_section_name = iv_hol_name
                                                  iv_hol_fieldname    = s_comp-name
                                                  iv_create_new       = v_new_item ).
          IF r_struct_row IS INITIAL.
            CONTINUE.
          ENDIF.

          v_new_item = abap_false.

          adapt_value( iv_value             = <dref>
                       iv_hol_fieldname     = s_comp-name
                       iv_hol_section_name  = iv_hol_name
                       ir_struct_row        = r_struct_row ).
        ENDLOOP.

        v_new_item = abap_false.

        "Procesar de último estructuras y tablas anidadas
        LOOP AT o_sdescr->get_components( ) INTO s_comp.

          ASSIGN ir_root->(s_comp-name) TO <ref>.
          ASSIGN <ref>->* TO <dref>.

          IF cl_abap_datadescr=>get_data_type_kind( <dref> ) EQ cl_abap_typedescr=>typekind_string.
            CONTINUE.
          ENDIF.

          CASE cl_abap_datadescr=>get_data_type_kind( <dref> ).

            WHEN cl_abap_typedescr=>typekind_struct2.

              "Estructura anidada dentro de estructura puede ser una posición de una tabla
              IF v_has_num_pos EQ abap_true.
                insert_position_number( EXPORTING ir_parent_root = ir_root CHANGING ir_root = <ref> ).
                v_new_item = abap_true.
              ENDIF.
              adapt_struct( ir_root = <ref>  iv_hol_name = s_comp-name  iv_is_new_table_item = v_new_item ).
              v_new_item = abap_false.

            WHEN cl_abap_typedescr=>typekind_table.

              "se envía ir_parent_root para agregar dinámicamente el número de posición en estructuras anidadas repetitivas
              adapt_table( ir_root = <ref>  iv_name = s_comp-name  ir_parent_root = ir_root ).

          ENDCASE.

        ENDLOOP.

      CATCH zcx_rest_ex INTO DATA(o_rest).
        RAISE EXCEPTION o_rest.
      CATCH cx_root INTO DATA(o_rootex).
        RAISE EXCEPTION TYPE zcx_rest_ex
          EXPORTING
            previous = o_rootex
            textid   = zcx_rest_ex=>int_err
            p1       = 'ZCL_REST_ADAPTER_BASE->ADAPT_STRUCTURE'
            p2       = 'NO se adaptó la estructura'.
    ENDTRY.

  ENDMETHOD.

  METHOD adapt_value.

    FIELD-SYMBOLS <s_struct> TYPE zcl_rest_cte=>ty_bapi_struct.

    TRY.
        "Buscar el campo abap en la tabla de configuración
        READ TABLE me->t_conf_fields ASSIGNING FIELD-SYMBOL(<s_conf>)
            WITH KEY seccion_hol = iv_hol_section_name
                     campo_hol   = iv_hol_fieldname.
        IF sy-subrc NE 0.
          "El campo holistico no se encuentra en la tabla de configuración: se ignora.
          "El validador registra esto como warning
          RETURN.
        ENDIF.

        ASSIGN ir_struct_row->* TO <s_struct>.

        "Poblar tabla anexa con los datos originales del payload para adaptación complemenetaria
        INSERT VALUE #( field_name = iv_hol_fieldname  field_value = iv_value )
            INTO TABLE <s_struct>-payload_data.

        "Evitar caracteres no imprimibles que pueden llegar desde la tabla
        clean_weird_chars( CHANGING cv_str = <s_conf>-campo_abap ).

        DATA(o_struct_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( <s_struct>-struct_ref ) ).

        READ TABLE o_struct_descr->get_components( ) INTO DATA(s_comp) WITH KEY name = <s_conf>-campo_abap.
        "Si no lo encuentra el campo puede estar contenido en una append de la estructura
        IF sy-subrc NE 0.
          LOOP AT o_struct_descr->get_components( ) INTO DATA(s_comp_append)
              WHERE name = space AND as_include = abap_true.
            TRY.
                DATA(o_appended_descr) = CAST cl_abap_structdescr(
                    cl_abap_typedescr=>describe_by_name( s_comp_append-type->absolute_name ) ).
                READ TABLE o_appended_descr->get_components( ) INTO s_comp WITH KEY name = <s_conf>-campo_abap.
                IF sy-subrc EQ 0.
                  EXIT.
                ENDIF.
              CATCH cx_root.
                CONTINUE.
            ENDTRY.
          ENDLOOP.
          IF s_comp IS INITIAL.
            "Error: no encontró el campo en la estructura ABAP
            RAISE EXCEPTION TYPE zcx_rest_ex
              EXPORTING
                textid = zcx_rest_ex=>int_err_campohol
                p1     = CONV #( <s_conf>-campo_abap )
                p2     = CONV #( <s_conf>-estruc_abap ).
          ENDIF.
        ENDIF.

        "Obtener apuntador al valor que será poblado
        ASSIGN <s_struct>-struct_ref->(<s_conf>-campo_abap) TO FIELD-SYMBOL(<v_field_value>).

        DATA(v_conv_err) = convert_value( EXPORTING iv_value     = iv_value
                                                    iv_type_kind = s_comp-type->type_kind
                                          IMPORTING ev_output    = <v_field_value> ).
        IF v_conv_err IS NOT INITIAL.
          CASE v_conv_err.
            WHEN cte->c_conv_err-no_int.
              DATA(v_textid) = zcx_rest_ex=>bad_integer.
            WHEN cte->c_conv_err-no_num.
              v_textid = zcx_rest_ex=>bad_number.
            WHEN cte->c_conv_err-no_date.
              v_textid = zcx_rest_ex=>bad_date.
            WHEN cte->c_conv_err-no_time.
              v_textid = zcx_rest_ex=>bad_time.
          ENDCASE.

          RAISE EXCEPTION TYPE zcx_rest_ex
            EXPORTING
              textid = v_textid
              p1     = |{ iv_hol_section_name }-{ iv_hol_fieldname }|
              p2     = iv_value.
        ENDIF.

      CATCH zcx_rest_ex INTO DATA(o_rest).
        RAISE EXCEPTION o_rest.
      CATCH cx_root INTO DATA(o_rootex).
        RAISE EXCEPTION TYPE zcx_rest_ex
          EXPORTING
            previous = o_rootex
            textid   = zcx_rest_ex=>int_err
            p1       = 'ZCL_REST_ADAPTER_BASE->ADAPT_VALUE'
            p2       = 'No se adapto el valor'.
    ENDTRY.

  ENDMETHOD.


  METHOD convert_value.

    CLEAR ev_output.

    "Entrada vacía es aceptable, posiblemente se llene en la adaptación particular
    IF iv_value IS INITIAL.
      RETURN.
    ENDIF.

    CASE iv_type_kind.

      WHEN cl_abap_typedescr=>typekind_packed.

        "Validar dato de entrada sea un número con o sin decimal positivo o negativo
        IF NEW cl_abap_regex( '^-?\d+(\.\d+)?$' )->create_matcher( text = iv_value )->match( ) EQ abap_false.
          rv_err = cte->c_conv_err-no_num.
          RETURN.
        ENDIF.

        ev_output = iv_value.

      WHEN cl_abap_typedescr=>typekind_num OR
           cl_abap_typedescr=>typekind_int OR
           cl_abap_typedescr=>typekind_int1 OR
           cl_abap_typedescr=>typekind_int2.

        "Valida dato de entrada sea un número entero positivo o negativo
        IF NEW cl_abap_regex( '^-?\d+$' )->create_matcher( text = iv_value )->match( ) EQ abap_false.
          rv_err = cte->c_conv_err-no_int.
          RETURN.
        ENDIF.

        ev_output = iv_value.

      WHEN cl_abap_typedescr=>typekind_date.

        " Tipo fecha adapta la entradas DD/MM/AAAA y AAAAMMDD
        DATA(v_len) = strlen( iv_value ).
        IF v_len EQ 8.
          DATA(v_fec) = CONV d( iv_value ).
        ELSEIF v_len EQ 10.
          v_fec = |{ iv_value+6(4) }{ iv_value+3(2) }{ iv_value+0(2) }|.
        ELSE.
          rv_err = cte->c_conv_err-no_date.
          RETURN.
        ENDIF.

        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                      = v_fec
          EXCEPTIONS
            plausibility_check_failed = 1
            OTHERS                    = 2.
        IF sy-subrc NE 0.
          rv_err = cte->c_conv_err-no_date.
          RETURN.
        ENDIF.

        ev_output = v_fec.

      WHEN cl_abap_typedescr=>typekind_time.

        " Tipo time adapta las entradas HH:MM:SS y HHMMSS
        v_len = strlen( iv_value ).
        IF v_len EQ 6.
          DATA(v_time) = CONV tims( iv_value ).
        ELSEIF v_len EQ 8.
          v_time = |{ iv_value+0(2) }{ iv_value+3(2) }{ iv_value+6(2) }|.
        ELSE.
          rv_err = cte->c_conv_err-no_time.
          RETURN.
        ENDIF.

        CALL FUNCTION 'TIME_CHECK_PLAUSIBILITY'
          EXPORTING
            time                      = v_time
          EXCEPTIONS
            plausibility_check_failed = 1
            OTHERS                    = 2.
        IF sy-subrc NE 0.
          rv_err = cte->c_conv_err-no_time.
          RETURN.
        ENDIF.

        ev_output = v_time.

      WHEN OTHERS.
        " Si es tipo char lo trunca si la entrada es más grande
        " Si es tipo string entra derecho
        ev_output = iv_value.

    ENDCASE.

  ENDMETHOD.


  METHOD insert_position_number.

    DATA r_new_structure TYPE REF TO data.

    IF ir_root IS INITIAL OR ir_parent_root IS INITIAL.
      "Es posible que una estructura llegue vacía. Ignorarla
      RETURN.
    ENDIF.

    TRY.
        DATA(o_rootdescr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( ir_root ) ).
        ASSIGN ir_root->* TO FIELD-SYMBOL(<s_root>).
        "Ignorar si no es una estructura
        IF cl_abap_datadescr=>get_data_type_kind( <s_root> ) NE cl_abap_typedescr=>typekind_struct2.
          RETURN.
        ENDIF.

        "Si la posición ya está poblada desde el payload ignorar
        DATA(t_comps) = o_rootdescr->get_components( ).
        READ TABLE t_comps TRANSPORTING NO FIELDS WITH KEY name = cte->c_special_field-num_posicion.
        IF sy-subrc EQ 0.
          RETURN.
        ENDIF.

        "Obtener el componente asociado a la posición de la estructura padre
        DATA(o_parentrootdescr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( ir_parent_root ) ).
        READ TABLE o_parentrootdescr->get_components( ) INTO DATA(s_pos_comp) WITH KEY name = cte->c_special_field-num_posicion.
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.

        APPEND s_pos_comp TO t_comps.
        DATA(o_newrootdescr) = cl_abap_structdescr=>create( t_comps ).
        CREATE DATA r_new_structure TYPE HANDLE o_newrootdescr.

        "La nueva estructura debe contener lo anterior más el nuevo campo
        ASSIGN r_new_structure TO FIELD-SYMBOL(<r_new_structure>).
        ASSIGN <r_new_structure>->* TO FIELD-SYMBOL(<s_new_structure>).
        MOVE-CORRESPONDING <s_root> TO <s_new_structure>.

        "Valor del campo de posición proveniente del root
        ASSIGN ir_parent_root->(cte->c_special_field-num_posicion) TO FIELD-SYMBOL(<r_root_num_pos>).
        ASSIGN <r_root_num_pos>->* TO FIELD-SYMBOL(<v_root_num_pos>).

        ASSIGN r_new_structure->(cte->c_special_field-num_posicion) TO FIELD-SYMBOL(<r_new_pos>).
        CREATE DATA <r_new_pos> TYPE string.
        ASSIGN <r_new_pos>->* TO FIELD-SYMBOL(<v_new_pos>).
        <v_new_pos> = <v_root_num_pos>.

        FREE ir_root.  "La estructura sin el campo posición ya no es necesaria, liberar memoria
        ir_root = r_new_structure.

      CATCH cx_root INTO DATA(o_rootex).
        RAISE EXCEPTION TYPE zcx_rest_ex
          EXPORTING
            previous = o_rootex
            textid   = zcx_rest_ex=>int_err
            p1       = 'ZCL_REST_ADAPTER_BASE->INSERT_POSITION_NUMBER'
            p2       = 'NO se insertó la posición'.
    ENDTRY.

  ENDMETHOD.

  METHOD clean_weird_chars.

    "Caracter 0xA0 algunas veces viene desde la tabla
    DATA v_xreplaced1(1) TYPE x VALUE 'A0'.
    DATA v_xreplacement(1) TYPE x VALUE '20'.
    DATA v_xstr TYPE xstring.

    DATA(v_str) = CONV string( cv_str ).

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = v_str
      IMPORTING
        buffer = v_xstr.

    REPLACE ALL OCCURRENCES OF v_xreplaced1 IN v_xstr WITH v_xreplacement IN BYTE MODE.
    "Sin reemplazos no es necesario seguir procesando
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    DATA(o_convin) = cl_abap_conv_in_ce=>create( input       = v_xstr
                                                 ignore_cerr = abap_true
                                                 replacement = '' ).
    CLEAR v_str.
    o_convin->read( IMPORTING data = v_str ).
    cv_str = v_str.

  ENDMETHOD.

  METHOD search_struct_row.

    DATA r_data TYPE REF TO data.

    TRY.
        "Buscar el campo abap en la tabla de configuración
        READ TABLE me->t_conf_fields ASSIGNING FIELD-SYMBOL(<s_conf>)
            WITH KEY seccion_hol = iv_hol_section_name
                     campo_hol   = iv_hol_fieldname.
        IF sy-subrc NE 0.
          "El campo holistico no se encuentra en la tabla de configuración: se ignora.
          "El validador previamente registra esto como warning
          RETURN.
        ENDIF.

        "Obtener o crear la estructura abap a la que pertenece el campo
        READ TABLE me->t_structs ASSIGNING FIELD-SYMBOL(<s_struct>)
            WITH KEY struct_name = <s_conf>-estruc_abap.

        IF sy-subrc EQ 0 AND iv_create_new EQ abap_false.
          "Ya existe, obtener la referencia a esta
          rr_instance = REF #( <s_struct> ).
          RETURN.
        ENDIF.

        " No existe aún, crear la estructura en la tabla de salida
        cl_abap_typedescr=>describe_by_name(
          EXPORTING p_name  = <s_conf>-estruc_abap
          RECEIVING p_descr_ref = DATA(o_type)
          EXCEPTIONS OTHERS = 1 ).
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_rest_ex
            EXPORTING
              textid = zcx_rest_ex=>int_err_struct_conf
              p1     = CONV #( <s_conf>-estruc_abap ).
        ENDIF.

        DATA(o_type_descr) = CAST cl_abap_structdescr( o_type ).
        CREATE DATA r_data TYPE HANDLE o_type_descr.
        INSERT VALUE #( struct_name = <s_conf>-estruc_abap struct_ref = r_data )
          INTO TABLE me->t_structs ASSIGNING <s_struct>.

        rr_instance = REF #( <s_struct> ).

      CATCH cx_root INTO DATA(o_rootex).
        RAISE EXCEPTION TYPE zcx_rest_ex
          EXPORTING
            previous = o_rootex
            textid   = zcx_rest_ex=>int_err
            p1       = 'ZCL_REST_ADAPTER_BASE->ADAPT_VALUE'
            p2       = 'No se adapto el valor'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
