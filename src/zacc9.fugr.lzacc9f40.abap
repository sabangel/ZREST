*----------------------------------------------------------------------*
*   INCLUDE LACC9F40                                                   *
*----------------------------------------------------------------------*


FORM convert_curr_to_internal

  USING r_parameter LIKE accbapifd5-parameter
        r_tabix     LIKE accbapifd5-tabix
        r_waers     LIKE tcurc-waers
        r_currency  LIKE bapiaccr01-currency_iso
        r_external  LIKE bapicurr-bapicurr
        r_internal.

  DATA: ld_length   TYPE i,
        ld_type,
        ld_digits   TYPE i,
        ld_return   LIKE bapireturn.

  CHECK NOT r_external IS INITIAL.

  IF r_waers IS INITIAL.
    PERFORM convert_curr_from_iso
            USING r_parameter
                  r_tabix
                  r_currency
                  r_waers.
  ENDIF.

  DESCRIBE FIELD r_internal LENGTH ld_length IN BYTE MODE
                 TYPE ld_type.
  IF ld_type = 'P'.
    ld_digits = 2 * ld_length - 1.
  ELSE.
    ld_digits = ld_length.
  ENDIF.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
    EXPORTING
      currency             = r_waers
      amount_external      = r_external
      max_number_of_digits = ld_digits
    IMPORTING
      amount_internal      = r_internal
      return               = ld_return.

  IF ld_return-type = 'A' OR
     ld_return-type = 'E'.
    CLEAR gs_return.
    gs_return-type       = ld_return-type.
    gs_return-message    = ld_return-message.
    gs_return-id         = ld_return-code(2).
    gs_return-number     = ld_return-code+2(3).
    gs_return-message_v1 = ld_return-message_v1.
    gs_return-message_v2 = ld_return-message_v2.
    gs_return-message_v3 = ld_return-message_v3.
    gs_return-message_v4 = ld_return-message_v4.
    gs_return-parameter  = r_parameter.
    gs_return-row        = r_tabix.
    APPEND gs_return TO it_return.                     "note 1034551
  ENDIF.

ENDFORM.                               " CONVERT_CURR_TO_INTERNAL

*---------------------------------------------------------------------*
*      Form  CONVERT_CURR_FROM_ISO
*---------------------------------------------------------------------*
FORM convert_curr_from_iso

  USING r_parameter LIKE accbapifd5-parameter
        r_tabix     LIKE accbapifd5-tabix
        r_currency  LIKE tcurc-isocd
        r_waers     LIKE tcurc-waers.

  PERFORM check_if_initial
          USING r_parameter
                r_tabix
                'CURRENCY'
                r_currency.

  CALL FUNCTION 'CURRENCY_CODE_ISO_TO_SAP'
    EXPORTING
      iso_code  = r_currency
    IMPORTING
      sap_code  = r_waers
*     UNIQUE    =
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  IF NOT sy-subrc IS INITIAL.
    PERFORM error_from_system
            USING r_parameter
                  r_tabix
                  'CURRENCY'.
  ENDIF.

ENDFORM.                               " CONVERT_CURR_FROM_ISO

*---------------------------------------------------------------------*
*      Form  CONVERT_UNIT_FROM_ISO
*---------------------------------------------------------------------*
FORM convert_unit_from_iso

  USING r_parameter LIKE accbapifd5-parameter
        r_tabix     LIKE accbapifd5-tabix
        r_unit_iso  LIKE t006-isocode
        r_unit_sap  LIKE t006-msehi.

  CHECK     r_unit_sap IS INITIAL AND
        NOT r_unit_iso IS INITIAL.

  CALL FUNCTION 'UNIT_OF_MEASURE_ISO_TO_SAP'
    EXPORTING
      iso_code      = r_unit_iso
    IMPORTING
      sap_code      = r_unit_sap
    EXCEPTIONS
      error_message = 1
      OTHERS        = 2.

  IF NOT sy-subrc IS INITIAL.
    PERFORM error_from_system
            USING r_parameter
                  r_tabix
                  space.
  ENDIF.

ENDFORM.                               " CONVERT_UNIT_FROM_ISO

*---------------------------------------------------------------------*
*      Form  CONVERT_LAND_FROM_ISO
*---------------------------------------------------------------------*
FORM convert_land_from_iso

  USING r_parameter LIKE accbapifd5-parameter
        r_tabix     LIKE accbapifd5-tabix
        iso_code    LIKE t005-intca
        sap_code    LIKE t005-land1.

  CHECK NOT iso_code IS INITIAL.

  CALL FUNCTION 'COUNTRY_CODE_ISO_TO_SAP'
    EXPORTING
      iso_code  = iso_code
    IMPORTING
      sap_code  = sap_code
*     UNIQUE    =
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  IF NOT sy-subrc IS INITIAL.
    PERFORM error_from_system
            USING r_parameter
                  r_tabix
                  space.
  ENDIF.

ENDFORM.                               " CONVERT_LAND_FROM_ISO

*---------------------------------------------------------------------*
*      Form  CONVERT_INPUT_PSP_ELEMENT
*---------------------------------------------------------------------*
FORM convert_input_psp_element

  USING r_parameter   LIKE accbapifd5-parameter
        r_tabix       LIKE accbapifd5-tabix
        r_wbs_element LIKE bapiacgl04-wbs_element
        r_ps_psp_pnr  LIKE accit-ps_psp_pnr.

  CHECK NOT r_wbs_element IS INITIAL.

  CALL FUNCTION 'PSPNUM_EXTERN_TO_INTERN_CONV'
    EXPORTING
      ext_num   = r_wbs_element
    IMPORTING
      int_num   = r_ps_psp_pnr
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  IF NOT sy-subrc IS INITIAL.
    PERFORM error_from_system
            USING r_parameter
                  r_tabix
                  space.
  ENDIF.

ENDFORM.                               " CONVERT_INPUT_PSP_ELEMENT

*---------------------------------------------------------------------*
*      Form  CONVERT_INPUT_NETWORK
*---------------------------------------------------------------------*
FORM convert_input_network

  USING r_network     LIKE accbapifd5-network
        r_activity    LIKE accbapifd5-activity
        r_aufpl       LIKE accit-aufpl
        r_aplzl       LIKE accit-aplzl.

  CHECK NOT r_network IS INITIAL.

  CALL FUNCTION 'READ_NETWORK_AUFPL_APLZL'
    EXPORTING
      nplnr     = r_network
      vornr     = r_activity
    IMPORTING
      aplzl     = r_aplzl
      aufpl     = r_aufpl
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  IF NOT sy-subrc IS INITIAL.
    CLEAR: r_aplzl,
           r_aufpl.
  ENDIF.

ENDFORM.                               " CONVERT_INPUT_NETWORK

*&---------------------------------------------------------------------*
*&      Form  convert_input_operation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM convert_input_operation

  USING r_orderid     LIKE accbapifd5-orderid
        r_activity    LIKE accbapifd5-activity
        r_aufpl       LIKE accit-aufpl
        r_aplzl       LIKE accit-aplzl.

  CHECK cl_erp_co_olc_sw_check=>erp_co_olc( ) = abap_true.
  CHECK NOT r_orderid IS INITIAL.

  CALL FUNCTION 'READ_NETWORK_AUFPL_APLZL'
    EXPORTING
      nplnr     = r_orderid
      vornr     = r_activity
      autyp_imp = '30'
    IMPORTING
      aplzl     = r_aplzl
      aufpl     = r_aufpl
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  IF NOT sy-subrc IS INITIAL.
    CLEAR: r_aplzl,
           r_aufpl.
  ENDIF.
ENDFORM.                    "convert_input_operation

*---------------------------------------------------------------------*
*       FORM convert_exchange_rate                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  r_parameter                                                   *
*  -->  r_tabix                                                       *
*  -->  r_exch_rate                                                   *
*  -->  r_exch_rate_v                                                 *
*  -->  r_kursf                                                       *
*---------------------------------------------------------------------*
FORM convert_exchange_rate
     USING r_parameter   LIKE accbapifd5-parameter
           r_tabix       LIKE accbapifd6-tabix
           r_exch_rate   LIKE accbapifd6-exch_rate
           r_exch_rate_v LIKE accbapifd6-exch_rate_v
           r_kursf       TYPE kursf.

  CALL FUNCTION 'CONVERT_RATE_TO_INTERNAL'
    EXPORTING
      rate_p = r_exch_rate
      rate_v = r_exch_rate_v
    IMPORTING
      rate   = r_kursf
    EXCEPTIONS
      OTHERS = 1.

  IF NOT sy-subrc IS INITIAL.
    PERFORM append_msg_to_return
            USING 'E'             "TYPE
                  'RW'            "ID
                  '609'           "NUMBER
                  text-w01        "MESSAGE_ONE
                  text-w02        "MESSAGE_TWO
                  r_parameter     "PARAMETER
                  r_tabix         "ROW
                  'EXCH_RATE'.    "FIELD
  ENDIF.

ENDFORM.                    " CONVERT_EXCHANGE_RATE
