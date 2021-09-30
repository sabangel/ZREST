CLASS zcl_rest_validator_def DEFINITION
  INHERITING FROM zcl_rest_validator_base
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_req_fields,
             seccion_hol TYPE zdseccionholi,
             campo_hol   TYPE zfied_holi,
             opcional    TYPE xfeld,
             encontrado  TYPE abap_bool,
           END OF ty_req_fields.

    TYPES tt_req_fields TYPE SORTED TABLE OF ty_req_fields WITH NON-UNIQUE KEY seccion_hol campo_hol.

    METHODS constructor
      IMPORTING iv_oper     TYPE zdrestoperacion
                io_rest_dao TYPE REF TO zif_rest_sys_field_dao OPTIONAL.

  PROTECTED SECTION.

    DATA o_rest_dao TYPE REF TO zif_rest_sys_field_dao.
    DATA t_req_fields TYPE tt_req_fields.
    DATA t_msg TYPE bapiret2_t.

    "! Se usa la tabla de configuración de sistemas para determinar si
    "! se encuentran los campos que llegan por el payload
    METHODS validate_post_customized REDEFINITION.

    METHODS validate_table
      IMPORTING it_table TYPE ANY TABLE
                iv_name  TYPE string.

    METHODS validate_struct
      IMPORTING is_struct TYPE any
                iv_name   TYPE string
                io_descr  TYPE REF TO cl_abap_structdescr.

    METHODS validate_value
      IMPORTING iv_value        TYPE string
                iv_field_name   TYPE string
                iv_section_name TYPE string.

    "! Validar campos de la coniguración que no vienen en la petición
    METHODS validate_not_found.

ENDCLASS.



CLASS zcl_rest_validator_def IMPLEMENTATION.

  METHOD constructor.

    super->constructor( iv_oper = iv_oper ).

    IF io_rest_dao IS NOT SUPPLIED.
      me->o_rest_dao = NEW zcl_rest_sys_field_dao( ).
    ELSE.
      me->o_rest_dao = io_rest_dao.
    ENDIF.

  ENDMETHOD.


  METHOD validate_post_customized.

    CLEAR et_msg.
    CLEAR me->t_msg.

    DATA(t_source_fields) = me->o_rest_dao->find_source_fields(
        iv_operacion = me->v_oper iv_id_fuente = me->v_id_fuente ).

    MOVE-CORRESPONDING t_source_fields TO me->t_req_fields.

    validate_table( it_table = it_docs  iv_name = cte->c_special_field-documentos ).

    validate_not_found( ).

    READ TABLE me->t_msg WITH KEY type = 'E' TRANSPORTING NO FIELDS.

    rv_ok = COND #( WHEN sy-subrc EQ 0 THEN abap_false ELSE abap_true ).

    et_msg = me->t_msg.

  ENDMETHOD.

  METHOD validate_table.

    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<r_line>).

      ASSIGN <r_line>->* TO FIELD-SYMBOL(<s_line>).

      TRY.
          "La línea es una tabla anidada
          CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <s_line> ) ).
          validate_table( it_table = <s_line>  iv_name = iv_name ).
          CONTINUE.
        CATCH cx_sy_move_cast_error.
      ENDTRY.

      TRY.
          "La línea es una estructura anidada
          DATA(o_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <s_line> ) ).
          validate_struct( is_struct = <s_line>  iv_name = iv_name  io_descr = o_descr ).
        CATCH cx_sy_move_cast_error.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

  METHOD validate_struct.

    LOOP AT io_descr->get_components( ) INTO DATA(s_component).

      ASSIGN COMPONENT s_component-name OF STRUCTURE is_struct TO FIELD-SYMBOL(<r_field>).

      "Número documento externo es especial y se valida en la madre
      IF iv_name EQ cte->c_special_field-documentos AND s_component-name EQ cte->c_special_field-doc_ref_externo.
        CONTINUE.
      ENDIF.

      ASSIGN <r_field>->* TO FIELD-SYMBOL(<field>).

      "Es un valor particular
      IF cl_abap_datadescr=>get_data_type_kind( <field> ) EQ cl_abap_typedescr=>typekind_string.
        validate_value( iv_value = <field>  iv_field_name = s_component-name  iv_section_name = iv_name ).
        CONTINUE.
      ENDIF.

      TRY.
          "Es una estructura anidada
          DATA(o_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <field> ) ).
          validate_struct( is_struct = <field>  iv_name = s_component-name  io_descr = o_descr ).
          CONTINUE.
        CATCH cx_sy_move_cast_error.
      ENDTRY.

      TRY.
          "Es una tabla anidada
          CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <field> ) ).
          validate_table( it_table = <field>  iv_name = s_component-name ).
          CONTINUE.
        CATCH cx_sy_move_cast_error.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

  METHOD validate_value.

    READ TABLE me->t_req_fields ASSIGNING FIELD-SYMBOL(<s_field>)
       WITH KEY seccion_hol = CONV #( iv_section_name )  campo_hol = CONV #( iv_field_name ).

    IF sy-subrc NE 0.
      "Llegó un campo que no existe en la configuración
      APPEND VALUE #( id         = cte->c_clase_msgs_def
                      type       = 'W' number = '36'
                      message_v1 = |{ iv_section_name }-{ iv_field_name }| ) TO me->t_msg.
      RETURN.
    ENDIF.

    <s_field>-encontrado = abap_true.

    IF <s_field>-opcional EQ abap_true.
      "El campo es opcional, no validar su valor
      RETURN.
    ENDIF.

    DATA(v_value) = iv_value.
    CONDENSE v_value NO-GAPS.

    IF v_value IS INITIAL.
      "El campo no es opcional y llegó vacío
      APPEND VALUE #( id         = cte->c_clase_msgs_def
                      type       = 'W' number = '035'
                      message_v1 = |{ iv_section_name }-{ iv_field_name }| ) TO me->t_msg.
    ENDIF.

  ENDMETHOD.


  METHOD validate_not_found.

    DELETE me->t_req_fields WHERE encontrado = abap_true.  "#EC CI_SORTSEQ..

    IF me->t_req_fields IS INITIAL.
      RETURN.
    ENDIF.

    "Los campos opcionales no se tienen en cuenta
    DELETE me->t_req_fields WHERE opcional EQ abap_true.   "#EC CI_SORTSEQ..

    LOOP AT me->t_req_fields ASSIGNING FIELD-SYMBOL(<s_req_field>).
      APPEND VALUE #( id         = cte->c_clase_msgs_def
                      type       = 'E' number = '034'
                      message_v1 = |{ <s_req_field>-seccion_hol }-{ <s_req_field>-campo_hol }| ) TO me->t_msg.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
