CLASS ltcl_rest_util DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS get_val_from_str_ref FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_rest_util IMPLEMENTATION.

  METHOD get_val_from_str_ref.

    "----------------------------------------------
    " CASO: referencia no es estructura o campo no existe
    "----------------------------------------------
    TRY.
        DATA x TYPE i VALUE 0.
        zcl_rest_util=>get_val_from_str_ref(
            EXPORTING ir_ref = REF #( x )
                      iv_fldname = 'KEY' ).
      CATCH zcx_rest_ex INTO DATA(o_ex1).
        cl_abap_unit_assert=>assert_equals(
          act = o_ex1->if_t100_message~t100key
          exp = zcx_rest_ex=>int_err_unassigned
        ).
    ENDTRY.

    "----------------------------------------------
    " CASO: entrada no es estructura
    "----------------------------------------------
    TRY.
        zcl_rest_util=>get_val_from_str_ref(
            EXPORTING ir_ref = x
                      iv_fldname = 'KEY' ).
      CATCH zcx_rest_ex INTO o_ex1.
        cl_abap_unit_assert=>assert_equals(
          act = o_ex1->if_t100_message~t100key
          exp = zcx_rest_ex=>int_err_unassigned
        ).
    ENDTRY.



    cl_abap_unit_assert=>fail( 'Implement your first test here' ).
  ENDMETHOD.

ENDCLASS.
