CLASS zcl_rest_util DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS json_from_bapimsg
      IMPORTING it_msg         TYPE bapiret2_t
                iv_req_id      TYPE zdrestreqid OPTIONAL
      RETURNING VALUE(rv_json) TYPE string.

    CLASS-METHODS json_from_reference
      IMPORTING ir_ref         TYPE REF TO data
      RETURNING VALUE(rv_json) TYPE string.

    "! Obtener el valor de un campo a partir de la referencia a una estructura
    "! @parameter ir_ref | Referencia a la estructura
    "! @parameter iv_fldname | Nombre del campo dentro de la estructura
    "! @parameter ev_val | Valor del campo indicado
    CLASS-METHODS get_val_from_str_ref
      IMPORTING ir_ref     TYPE any
                iv_fldname TYPE fieldname
      EXPORTING ev_val     TYPE data
      RAISING   zcx_rest_ex.

  PRIVATE SECTION.

    CLASS-DATA: s_json_msg TYPE zrestsmensajes.

ENDCLASS.



CLASS zcl_rest_util IMPLEMENTATION.


  METHOD json_from_bapimsg.

    s_json_msg-req_id = iv_req_id.

    LOOP AT it_msg ASSIGNING FIELD-SYMBOL(<s_msg>).

      APPEND INITIAL LINE TO s_json_msg-mensajes ASSIGNING FIELD-SYMBOL(<s_json_msg>).
      <s_json_msg>-tipo  = <s_msg>-type.

      IF <s_msg>-id IS NOT INITIAL.
        MESSAGE ID <s_msg>-id TYPE <s_msg>-type NUMBER <s_msg>-number
            WITH <s_msg>-message_v1 <s_msg>-message_v2 <s_msg>-message_v3 <s_msg>-message_v4 INTO <s_json_msg>-texto.
      ELSE.
        <s_json_msg>-texto = <s_msg>-message.
      ENDIF.

    ENDLOOP.

    " Retornamos JSON con los mensajes de Advertencia o Errores que se encontraron en la validación del payload
    rv_json = /ui2/cl_json=>serialize( data = s_json_msg compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  ENDMETHOD.


  METHOD get_val_from_str_ref.

    FIELD-SYMBOLS <ref>  TYPE any.
    FIELD-SYMBOLS <dref> TYPE data.
    DATA r_ref TYPE REF TO data.

    r_ref = ir_ref.
    ASSIGN r_ref->(iv_fldname) TO <ref>.

    IF <ref> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE zcx_rest_ex
        EXPORTING
          textid = zcx_rest_ex=>int_err_unassigned
          p1     = CONV #( iv_fldname )
          p2     = |met zcl_rest_util=>get_val_from_str_ref|.
    ENDIF.

    ASSIGN <ref>->* TO <dref>.

    TRY.
        ev_val = <dref>.
      CATCH cx_root INTO DATA(o_rootex).
        RAISE EXCEPTION TYPE zcx_rest_ex
          EXPORTING
            textid = zcx_rest_ex=>bad_value
            p1     = <dref>
            p2     = CONV #( iv_fldname ).
    ENDTRY.

  ENDMETHOD.

  METHOD json_from_reference.

    "CREAR JSON DESDE LA REFERENCIA
    rv_json = /ui2/cl_json=>serialize( data = ir_ref compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  ENDMETHOD.

ENDCLASS.
