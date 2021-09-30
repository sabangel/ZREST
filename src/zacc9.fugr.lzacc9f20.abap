*----------------------------------------------------------------------*
*   INCLUDE LACC9F20                                                   *
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       MACRO SHIFT UP                                                *
*---------------------------------------------------------------------*
*       shifts a CHAR field to upper case                             *
*---------------------------------------------------------------------*
DEFINE SHIFT_UP.
  TRANSLATE &1 TO UPPER CASE.                             "#EC SYNTCHAR
END-OF-DEFINITION.

FORM document_check
     USING p_currency
           p_component.

  IF p_currency = 'X'.
    PERFORM check_rwin
            USING  p_component.

* no additional COBL check for postings from Real Estate
    CHECK gs_aw-awtyp NP 'RE*'.                             "note1060353

    LOOP AT it_return TRANSPORTING NO FIELDS
                      WHERE type = 'E' OR
                            type = 'A'.
      EXIT.
    ENDLOOP.
    CHECK sy-subrc = 0.
* error occurred ? process cobl check additionally to get more details
* in particular line item in which error occurred
    PERFORM check_cobl.
  ELSE.
    PERFORM check_cobl.
  ENDIF.

ENDFORM.                    " DOCUMENT_CHECK

*---------------------------------------------------------------------*
*       FORM fill_acchd                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  contract_header                                               *
*---------------------------------------------------------------------*
FORM fill_acchd USING contract_header TYPE bapiaccahd.

  CLEAR gs_acchd.
  MOVE-CORRESPONDING gs_aw TO gs_acchd.

  gs_acchd-usnam = gs_bapi_acchd-username.
  gs_acchd-awsys = gs_bapi_acchd-obj_sys.
  gs_acchd-bktxt = gs_bapi_acchd-header_txt.
  gs_acchd-glvor = gs_bapi_acchd-bus_act.
  gs_acchd-tcode = sy-tcode.
  gs_acchd-acc_principle =  gs_bapi_acchd-acc_principle.

  IF NOT contract_header IS INITIAL.
    PERFORM fill_acchd_from_fica
            USING contract_header.
  ENDIF.

  APPEND gs_acchd TO it_acchd.

ENDFORM.                    " FILL_ACCHD

*&---------------------------------------------------------------------*
*&      Form  FILL_bapi_accit
*&---------------------------------------------------------------------*
*       Aufbau des RW-Beleges mit externen Feldnamen
*----------------------------------------------------------------------*
FORM fill_bapi_accit
     TABLES account_receivable STRUCTURE bapiacar09
            account_payable    STRUCTURE bapiacap09
            account_gl         STRUCTURE bapiacgl09
            account_tax        STRUCTURE bapiactx09
            currency_amount    STRUCTURE bapiaccr09
            payment_card       STRUCTURE bapiacpc09
            contract_item      STRUCTURE bapiaccait
     USING  document_header    TYPE      bapiache09
            contract_header    TYPE      bapiaccahd
            r_currency.

  FIELD-SYMBOLS: <ar> TYPE bapiacar09,
                 <ap> TYPE bapiacap09,
                 <gl> TYPE bapiacgl09,
                 <tx> TYPE bapiactx09,
                 <cr> TYPE bapiaccr09.

* AR accounts

  LOOP AT account_receivable ASSIGNING <ar>.
    CLEAR gs_bapi_accit.                                    "Note1066476
    MOVE-CORRESPONDING document_header TO gs_bapi_accit.    "Note1066476
    MOVE-CORRESPONDING <ar> TO gs_bapi_accit.
    gs_bapi_accit-parameter = 'ACCOUNTRECEIVABLE'.
    gs_bapi_accit-tabix     = sy-tabix.
*   Zahlungskarten-Informationen
    READ TABLE payment_card WITH KEY
               itemno_acc = gs_bapi_accit-itemno_acc.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING payment_card TO gs_bapi_accit.
    ENDIF.
*   FI-CA Zusatzdaten
    READ TABLE contract_item WITH KEY
               itemno_acc = gs_bapi_accit-itemno_acc.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING contract_item TO gs_bapi_accit.
    ENDIF.
    IF contract_header IS INITIAL.
      gs_bapi_accit-actv_account = '1'.
    ELSE.
      gs_bapi_accit-actv_account = '2'.
    ENDIF.
    SHIFT_UP: gs_bapi_accit-ref_key_1,                     "Note1485060
              gs_bapi_accit-ref_key_2,                     "Note1485060
              gs_bapi_accit-ref_key_3.                     "Note1485060

    APPEND gs_bapi_accit TO it_bapi_accit.
  ENDLOOP.

* AP accounts
  LOOP AT account_payable ASSIGNING <ap>.
    CLEAR gs_bapi_accit.
    MOVE-CORRESPONDING document_header TO gs_bapi_accit.
    MOVE-CORRESPONDING <ap> TO gs_bapi_accit.
    gs_bapi_accit-parameter = 'ACCOUNTPAYABLE'.
    gs_bapi_accit-tabix     = sy-tabix.
    SHIFT_UP: gs_bapi_accit-ref_key_1,                     "Note1485060
              gs_bapi_accit-ref_key_2,                     "Note1485060
              gs_bapi_accit-ref_key_3.                     "Note1485060

    APPEND gs_bapi_accit TO it_bapi_accit.
  ENDLOOP.

* GL accounts
  LOOP AT account_gl ASSIGNING <gl>.
    CLEAR gs_bapi_accit.                                    "Note1066476
    MOVE-CORRESPONDING document_header TO gs_bapi_accit.    "Note1066476
    MOVE-CORRESPONDING <gl> TO gs_bapi_accit.
    gs_bapi_accit-parameter = 'ACCOUNTGL'.
    gs_bapi_accit-tabix     = sy-tabix.
*   Zahlungskarten-Informationen                            "note936432
    READ TABLE payment_card WITH KEY                        "note936432
               itemno_acc = gs_bapi_accit-itemno_acc.       "note936432
    IF sy-subrc IS INITIAL.                                 "note936432
      MOVE-CORRESPONDING payment_card TO gs_bapi_accit.     "note936432
    ENDIF.                                                  "note936432
    SHIFT_UP: gs_bapi_accit-ref_key_1,                     "Note1485060
              gs_bapi_accit-ref_key_2,                     "Note1485060
              gs_bapi_accit-ref_key_3.                     "Note1485060
    IF <gl>-itemno_tax EQ space.                          "note 1932280
      CLEAR: gs_bapi_accit-itemno_tax.                    "note 1932280
    ENDIF.                                                "note 1932280
    APPEND gs_bapi_accit TO it_bapi_accit.
  ENDLOOP.

* Taxes
  LOOP AT account_tax ASSIGNING <tx>.
    CLEAR gs_bapi_accit.
    MOVE-CORRESPONDING document_header TO gs_bapi_accit.
    MOVE-CORRESPONDING <tx> TO gs_bapi_accit.
    gs_bapi_accit-parameter = 'ACCOUNTTAX'.
    gs_bapi_accit-tabix     = sy-tabix.
    IF ( document_header-obj_type EQ 'BEBD'         "n1859478 "n1982803
    OR   document_header-obj_type EQ 'BERD'               "note 1982803
    OR   document_header-obj_type EQ 'BERE'               "note 1982803
    OR   document_header-obj_type EQ 'CRMFM' )            "note 1982803
    AND NOT <tx>-itemno_tax IS INITIAL.                   "Note 1859478
      gs_bapi_accit-itm_number = <tx>-itemno_tax.         "Note 1859478
      IF gd_xtxit IS INITIAL.                             "Note 1859478
        CLEAR: gs_bapi_accit-itemno_tax.                  "Note 1859478
      ENDIF.                                              "Note 1859478
    ENDIF.                                                "Note 1859478
    IF NOT <tx>-direct_tax IS INITIAL                     "Note 1903918
    AND gd_xtxit IS INITIAL                               "Note 1903918
    AND <tx>-taxjurcode_level IS INITIAL.                 "Note 1903918
      gs_bapi_accit-taxjurcode_level = 'D'.               "Note 1903918
    ENDIF.                                                "Note 1903918
    APPEND gs_bapi_accit TO it_bapi_accit.
  ENDLOOP.

* Currency
  CLEAR gs_bapi_acccr.
  LOOP AT currency_amount ASSIGNING <cr>.
    MOVE-CORRESPONDING <cr> TO gs_bapi_acccr.
    gs_bapi_acccr-tabix = sy-tabix.
    APPEND gs_bapi_acccr TO it_bapi_acccr.
  ENDLOOP.

ENDFORM.                    " FILL_bapi_accit


*---------------------------------------------------------------------*
*       FORM process_accit                                            *
*---------------------------------------------------------------------*
*     - Anreicherungen des RW-Beleges
*     - Mapping der Felder
*---------------------------------------------------------------------*
FORM process_accit.

*   Aufruf CO-Translator
  IF gs_bapi_accit-bus_scenario = 'GRANTOR'.
    PERFORM get_account_assignment_grantor.
  ELSE.
    IF gs_bapi_accit-parameter = 'ACCOUNTGL'.
      PERFORM get_account_assignment.
    ENDIF.
  ENDIF.

* Mapping Geschäftspartner-GUID => Debitor/Kreditor
  IF gs_bapi_accit-parameter = 'ACCOUNTRECEIVABLE'.
    PERFORM map_gp_to_kunnr.
  ENDIF.

* Mapping externe => interne Feldnamen, Iso-Konvertierungen
  PERFORM fill_accit_from_bapi_accit.

* default fields from header
  PERFORM fill_line_from_header.

* Vertriebsbelegnummer aus documentheader-ac_doc_no         note 581277
*  IF gs_bapi_acchd-obj_type  = 'BEBD' AND                  "note 581277
*     gs_bapi_accit-parameter = 'ACCOUNTRECEIVABLE'.        "note 581277
  IF ( gs_bapi_acchd-obj_type  = 'BEBD' OR
       gs_bapi_acchd-obj_type  = 'BERD' ) AND
     gs_bapi_accit-parameter = 'ACCOUNTRECEIVABLE'.
    gs_accit-vbeln = gs_accit-belnr.                       "note 581277
  ENDIF.                                                   "note 581277

  IF gs_accit-kstat IS INITIAL.
*   Buchungskreis und Buchungsdatum Pflichtfelder
    PERFORM check_if_initial
            USING gs_bapi_accit-parameter
                  gs_bapi_accit-tabix:
                  'COMP_CODE ' gs_accit-bukrs,
                  'PSTNG_DATE' gs_accit-budat.
  ENDIF.

* Mengeneinheit
  PERFORM check_uom_when_quantity
          USING gs_bapi_accit-parameter
                gs_bapi_accit-tabix.
* check transaction type
  PERFORM rmvct_check USING gs_bapi_accit.

* MM cobl check for plant,material,unit,quantity
  IF gs_bapi_acchd-obj_type  <> 'BEBD'.                    "note 932864
    PERFORM mm_cobl_check USING gs_bapi_accit.             "note 736111
  ENDIF.                                                   "note 932864

* Kontoart setzen, Steuerzeile prüfen
  PERFORM fill_accounttype.

* Level für Taxjurisdictioncode
  PERFORM get_txjcd_level.

* Buchungsschlüssel
  PERFORM fill_acct_key.

* Belegart
  IF gs_accit-blart IS INITIAL.
    gs_accit-blart = 'AB'.              " allgemeiner Buchhaltungsbeleg
  ENDIF.

* Menge: Flag Retourenposition setzen/ Vorzeichen abschneiden
  PERFORM negative_quantity.

  gs_accit-bapi_param   = gs_bapi_accit-parameter.
  gs_accit-bapi_tabix   = gs_bapi_accit-tabix.

ENDFORM.                    " PROCESS_ACCIT


*---------------------------------------------------------------------*
*       FORM check_and_fill_acc_document                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  account_receivable                                            *
*  -->  account_payable                                               *
*  -->  account_gl                                                    *
*  -->  account_tax                                                   *
*  -->  currency_amount                                               *
*  -->  payment_card                                                  *
*  -->  contract_item                                                 *
*  -->  document_header                                               *
*  -->  contract_header                                               *
*  -->  p_currency                                                    *
*---------------------------------------------------------------------*
FORM check_and_fill_acc_document
     TABLES account_receivable  STRUCTURE bapiacar09
            account_payable     STRUCTURE bapiacap09
            account_gl          STRUCTURE bapiacgl09
            account_tax         STRUCTURE bapiactx09
            currency_amount     STRUCTURE bapiaccr09
            payment_card        STRUCTURE bapiacpc09
            contract_item       STRUCTURE bapiaccait
     USING  document_header     TYPE      bapiache09
            contract_header     TYPE      bapiaccahd
            p_currency.

  DATA ld_nvv_nav.

* shift case-insensitive header fields to upper case        "note1485060
  SHIFT_UP: document_header-obj_type,
            document_header-obj_key,
            document_header-obj_sys,
            document_header-bus_act,
            document_header-username,
            document_header-doc_type,
            document_header-obj_key_r,
            document_header-reason_rev,
            document_header-compo_acc,
            document_header-acc_principle,
            document_header-neg_postng,
            document_header-obj_key_inv.               "end note 1485060

* FI Belegnummer als Referenz

  IF document_header-obj_type   = 'BKPFF' OR (
     document_header-obj_type  IS INITIAL AND
     document_header-obj_key   IS INITIAL AND
     document_header-obj_sys   IS INITIAL ).

    PERFORM reference_create_prelim
             CHANGING document_header-obj_type
                      document_header-obj_key
                      document_header-obj_sys.

    IF document_header-bus_act IS INITIAL.                  "note1264422
      document_header-bus_act = 'RFBU'.                    "note1045412
    ENDIF.                                                  "note1264422

    PERFORM check_document_items_initial                  "note 1820221
            TABLES account_receivable                     "note 1820221
                   account_payable                        "note 1820221
                   account_gl                             "note 1820221
                   account_tax.                           "note 1820221
  ENDIF.

* Belegkopf
  PERFORM check_and_fill_header
            TABLES account_receivable
                   account_payable
            USING  document_header
                   contract_header
                   p_currency.

  PERFORM check_value_ranges                           "note1578127
            TABLES account_gl                          "note1578127
                   account_tax                         "note1578127
            USING  document_header.                    "note1578127

* allow postings without positions from Billing engine only
* insert dummy ACCOUNTRECEIVABLE and CURRENCY_AMOUNT,
* will be removed in AC_DOCUMENT_CREATE later

  IF document_header-obj_type EQ 'BEBD' AND
    account_receivable[] IS INITIAL AND
    account_payable[]    IS INITIAL AND
    account_gl[]         IS INITIAL AND
    account_tax[]        IS INITIAL.
    CLEAR account_receivable.
    account_receivable-customer = '*'.
    APPEND account_receivable.
    CLEAR  currency_amount.
    APPEND currency_amount.
  ENDIF.

* IF document_header-obj_type EQ 'BEBD'.                  "Note 1859478 1886654
  PERFORM check_tax_det_by_line                           "Note 1859478
            USING document_header-comp_code.              "Note 1859478
* ENDIF.                                                  "Note 1859478 1886654

* Belegzeilen aufbauen
  PERFORM fill_bapi_accit
          TABLES account_receivable
                 account_payable
                 account_gl
                 account_tax
                 currency_amount
                 payment_card
                 contract_item
          USING  document_header
                 contract_header
                 p_currency.

* Prüfungen und Mapping der Felder auf Zeilenebene
  SORT it_bapi_accit BY itemno_acc.
  SORT it_bapi_acccr BY itemno_acc.

  LOOP AT it_bapi_accit INTO gs_bapi_accit.
* check dates
    PERFORM check_date_fields USING gs_bapi_accit.          "Note1361439
* check cash discount information (days and percentages)
    PERFORM check_cahsdiscount_fields USING gs_bapi_accit.  "Note1412415
* check tax line by line
    PERFORM check_itemno_tax USING document_header             "1982803
                                   gs_bapi_accit.              "1886654
    CLEAR gs_accit.
    MOVE-CORRESPONDING gs_aw TO gs_accit.

    IF p_currency = 'X'.
      PERFORM fill_acccr
              USING  gs_bapi_accit-parameter
                     gs_bapi_accit-tabix
                     gs_bapi_accit-itemno_acc
                     gs_accit-shkzg.
    ENDIF.

*   Mapping der Belegzeilen und Anreicherungen
    PERFORM process_accit.

    IF gs_accit-taxit = 'X' AND
       gs_accit-kstat = 'X'.
      ld_nvv_nav = 'X'.
    ENDIF.

    PERFORM badi_change_it_accit.

    APPEND gs_accit TO it_accit.

  ENDLOOP.

***********************************************************************
* Prüfungen und Anreicherungen auf Belegebene:
***********************************************************************
* Fill ALWAYS tax data in ACCTX                               "N1318938
*  IF NOT ld_nvv_nav IS INITIAL.                              "N1318938
  PERFORM fill_it_acctx_nvv_nav.
*  ENDIF.                                                     "N1318938

  PERFORM check_balance_intercompany.

  PERFORM enrich_cash_discount.                            "note 972734

  PERFORM copy_bupla_korea.                                "note 695878

  FREE: it_bapi_accit,
        it_bapi_acccr.

ENDFORM.                    " CHECK_AND_FILL_ACC_DOCUMENT

*&---------------------------------------------------------------------*
*&      Form  FILL_CPD
*&---------------------------------------------------------------------*
*       CPD-Kunde
*----------------------------------------------------------------------*
FORM fill_cpd
  USING r_customer LIKE ZBAPIACPA09.

  FIELD-SYMBOLS: <accit> TYPE accit.
  DATA: l_debi  LIKE  vf_debi,
        l_kred  LIKE  vf_kred.
  DATA: lt_bnka LIKE  bnka OCCURS 1 WITH HEADER LINE,
        lv_bnka_lines LIKE SY-TABIX.

  CHECK NOT r_customer IS INITIAL.

* BEGIN of BAPI extensions for SEPA enabling

* fill bank key from IBAN/SWIFT_CODE, technical account
* number is handled later in the FI interface
  IF NOT r_customer-swift_code IS INITIAL.
* SWIFT_CODE intended for SEPA, therefore require IBAN,
* leave BANK_NO, BANK_ACCT, BANK_CTRY, BANK_CTRY_ISO empty since these are
* derived from IBAN/SWIFT_CODE
    IF r_customer-iban IS INITIAL.
* Error and exit from further processing : Enter IBAN
      PERFORM append_msg_to_return
        USING 'E' 'RW' '602' 'IBAN' 'CUSTOMERCPD' 'CUSTOMERCPD' 1 'IBAN'.
      EXIT.
    ENDIF.

    IF NOT r_customer-bank_no IS INITIAL.
* Error: Leave  BANK_NO empty !
      PERFORM append_msg_to_return
        USING 'E' 'RW' '640' 'BANK_NO' SPACE 'CUSTOMERCPD' 1 'BANK_NO'.
    ENDIF.

    IF NOT r_customer-bank_acct IS INITIAL.
* Error: Leave  BANK_ACCT empty !
      PERFORM append_msg_to_return
        USING 'E' 'RW' '640' 'BANK_ACCT' SPACE 'CUSTOMERCPD' 1 'BANK_ACCT'.
    ENDIF.

    IF NOT r_customer-bank_ctry IS INITIAL.
* Error: Leave  BANK_CTRY empty !
      PERFORM append_msg_to_return
        USING 'E' 'RW' '640' 'BANK_CTRY' SPACE 'CUSTOMERCPD' 1 'BANK_CTRY'.
    ENDIF.

    IF NOT r_customer-bank_ctry_iso IS INITIAL.
* Error: Leave  BANK_CTRY_ISO empty !
      PERFORM append_msg_to_return
        USING 'E' 'RW' '640' 'BANK_CTRY_ISO' SPACE 'CUSTOMERCPD' 1 'BANK_CTRY_ISO'.
    ENDIF.

* IBAN filled: now search for bank keys for this IBAN/SWIFT_CODE pair
    CALL FUNCTION 'SEARCH_FOR_BANK'
      EXPORTING
        i_iban               = r_customer-iban
        I_SWIFT              = r_customer-swift_code
      TABLES
        T_BNKA               = lt_bnka
      EXCEPTIONS
        error_message        = 4
        OTHERS               = 4.

    IF sy-subrc <> 0.
      PERFORM error_from_system USING 'CUSTOMERCPD' 1 'IBAN'.
    ELSE.
* Check if bank is unique
      DESCRIBE TABLE lt_bnka LINES lv_bnka_lines.
      IF lv_bnka_lines = 1.
        READ TABLE lt_bnka INDEX 1.
            IF r_customer-swift_code = lt_bnka-swift.
* Valid and unique bank key for this IBAN found: enrich CUSTOMERCPD
               r_customer-bank_ctry  = lt_bnka-banks.
               r_customer-bank_no    = lt_bnka-bankl.
            ELSE.
              PERFORM append_msg_to_return
              USING 'E' 'BF00' '018' SPACE SPACE 'CUSTOMERCPD' 1 'IBAN'.
              IF 1 = 0.
                MESSAGE E018(BF00).
              ENDIF.
            ENDIF.
      ELSE.
* Error: no bank found or bank non-unique
        PERFORM append_msg_to_return
          USING 'E' 'BF00' '018' SPACE SPACE 'CUSTOMERCPD' 1 'IBAN'.
        IF 1 = 0.
          MESSAGE E018(BF00).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
* END of BAPI extensions for SEPA enabling

  LOOP AT it_accit ASSIGNING <accit>
          WHERE NOT kunnr IS INITIAL
          OR    NOT lifnr IS INITIAL.

    CASE <accit>-koart.
      WHEN 'D'.
        CALL FUNCTION 'FI_CUSTOMER_DATA'
          EXPORTING
            i_kunnr = <accit>-kunnr
          IMPORTING
            e_debi  = l_debi
          EXCEPTIONS
            OTHERS  = 1.

        CHECK sy-subrc = 0.
        CHECK NOT l_debi-xcpdk IS INITIAL                   "N1237572
           OR NOT l_debi-xzemp IS INITIAL.                  "N1237572

      WHEN 'K'.
        CALL FUNCTION 'FI_VENDOR_DATA'
          EXPORTING
            i_lifnr = <accit>-lifnr
          IMPORTING
            e_kred  = l_kred
          EXCEPTIONS
            OTHERS  = 1.

        CHECK sy-subrc = 0.
        CHECK NOT l_kred-xcpdk IS INITIAL                   "N1237572
           OR NOT l_kred-xzemp IS INITIAL.                  "N1237572
      WHEN OTHERS.
        CONTINUE.

    ENDCASE.

    CLEAR gs_accfi.
    MOVE-CORRESPONDING <accit> TO gs_accfi.

*   BAPIACPA09
    gs_accfi-name1        = r_customer-name                .
    gs_accfi-name2        = r_customer-name_2              .
    gs_accfi-name3        = r_customer-name_3              .
    gs_accfi-name4        = r_customer-name_4              .
    gs_accfi-pstlz        = r_customer-postl_code          .
    gs_accfi-ort01        = r_customer-city                .
    gs_accfi-land1        = r_customer-country             .
*   gs_ACCFI-LAND1        = R_CUSTOMER-COUNTRY_ISO         .
    gs_accfi-stras        = r_customer-street              .
    gs_accfi-pfach        = r_customer-po_box              .
    gs_accfi-pstl2        = r_customer-pobx_pcd            .
    gs_accfi-pskto        = r_customer-pobk_curac          .
    gs_accfi-bankn        = r_customer-bank_acct           .
    gs_accfi-bankl        = r_customer-bank_no             .
    gs_accfi-banks        = r_customer-bank_ctry           .
*   gs_ACCFI-BANKS_ISO    = R_CUSTOMER-BANK_CTRY_ISO       .
    gs_accfi-stcd1        = r_customer-tax_no_1            .
    gs_accfi-stcd2        = r_customer-tax_no_2            .
    gs_accfi-stcd3        = r_customer-tax_no_3            ."Note1686299
    gs_accfi-stcd4        = r_customer-tax_no_4            ."Note1686299
    gs_accfi-stkzu        = r_customer-tax                 .
    gs_accfi-stkza        = r_customer-equal_tax           .
    gs_accfi-regio        = r_customer-region              .
    gs_accfi-bkont        = r_customer-ctrl_key            .
    gs_accfi-dtaws        = r_customer-instr_key           .
    gs_accfi-dtams        = r_customer-dme_ind             .
*   gs_ACCFI-LAISO        = R_CUSTOMER-LANGU_ISO           .
    gs_accfi-iban         = r_customer-iban                .

    gs_accfi-fityp        = r_customer-fityp                .
    gs_accfi-stcdt        = r_customer-stcdt                .
    gs_accfi-stkzn        = r_customer-stkzn                .


    PERFORM convert_land_from_iso
            USING 'CUSTOMERCPD' 0:
      r_customer-country_iso      gs_accfi-land1,
      r_customer-bank_ctry_iso    gs_accfi-banks.

    IF NOT r_customer-langu_iso IS INITIAL.                "note 569859
      CALL FUNCTION 'LANGUAGE_CODE_ISO_TO_SAP'             "note 569859
        EXPORTING                                          "note 569859
          iso_code        = r_customer-langu_iso           "note 569859
        IMPORTING                                          "note 569859
          sap_code        = gs_accfi-spras                 "note 569859
        EXCEPTIONS                                         "note 569859
          not_found       = 1                              "note 569859
          OTHERS          = 2                              "note 569859
          .                                                "note 569859
      IF sy-subrc <> 0.                                    "note 569859
        PERFORM error_from_system                          "note 569859
                USING 'CUSTOMERCPD'                        "note 569859
                      0                                    "note 569859
                      'LANGU_ISO'.                         "note 569859
      ENDIF.                                               "note 569859
    ENDIF.                                                 "note 569859

    APPEND gs_accfi TO it_accfi.

  ENDLOOP.

ENDFORM.                    " FILL_CPD



*---------------------------------------------------------------------*
*       FORM CHECK_RWIN                                               *
*---------------------------------------------------------------------*
*       Aufruf RWIN                                                   *
*---------------------------------------------------------------------*
FORM check_rwin
  USING r_component.

  DATA ld_mh_active.

  IF NOT r_component IS INITIAL.
    PERFORM check_document_allready_posted
            USING r_component.
  ENDIF.

* Modification: Is the document relevant for accounting ?   note 495175
*  PERFORM check_zero_items.                               "note 495175

  LOOP AT it_return TRANSPORTING NO FIELDS
       WHERE type = 'E' OR type = 'A'.
    EXIT.
  ENDLOOP.
  CHECK NOT sy-subrc IS INITIAL.

  PERFORM messages_start USING ld_mh_active.

  IF r_component <> 'R3RB'.
    CALL FUNCTION 'AC_DOCUMENT_CREATE'
      EXPORTING
        i_comp        = r_component
*       I_COMP_CHECK  =
        i_free_table  = ' '
      TABLES
        t_acchd       = it_acchd
        t_accit       = it_accit
        t_acccr       = it_acccr
*       T_ACCDA       =
        t_accfi       = it_accfi
        t_acctx       = it_acctx
*       T_ACCTAX      =
        t_accit_pa    = it_accit_pa
        t_acccr_pa    = it_acccr_pa
        t_accwt       = it_accwt
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

  ELSE.
    CALL FUNCTION 'RKE_RECEIVE_RW_DATA_FROM_CRM'
      EXPORTING
        compo_acc     = r_component
*       COMMIT        = TRUE
      TABLES
        t_acchd       = it_acchd
        t_accit       = it_accit
        t_acccr       = it_acccr
*       T_ACCDA       =
        t_accfi       = it_accfi
        t_acctx       = it_acctx
        t_accit_pa    = it_accit_pa
        t_acccr_pa    = it_acccr_pa
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

  ENDIF.

  IF NOT sy-subrc IS INITIAL.
    PERFORM error_from_system USING space 0 space.
  ENDIF.

  PERFORM messages_stop USING space 0 ld_mh_active.

ENDFORM.                               " CHECK_RWIN


*---------------------------------------------------------------------*
*       FORM CHECK_COBL                                               *
*---------------------------------------------------------------------*
*       Kontenprüfung                                                 *
*---------------------------------------------------------------------*
FORM check_cobl.

  DATA: ld_parameter LIKE accbapifd5-parameter,
        ld_tabix     LIKE accbapifd5-tabix,
        ld_waers     LIKE acccr-waers.
  DATA: ls_x001      LIKE x001.                           "note 1873541

  FIELD-SYMBOLS: <acchd> LIKE acchd,                      "note 1723218
                 <accit> LIKE accit,
                 <acccr> LIKE acccr.

  LOOP AT it_acchd ASSIGNING <acchd>.                     "note 1723218
    LOOP AT it_accit ASSIGNING <accit>
                     WHERE awtyp EQ <acchd>-awtyp         "note 1723218
                     AND   awref EQ <acchd>-awref         "note 1723218
                     AND   aworg EQ <acchd>-aworg.        "note 1723218
      ld_parameter = <accit>-bapi_param.
    ld_tabix     = <accit>-bapi_tabix.
    READ TABLE it_acccr WITH KEY awtyp = <accit>-awtyp
                                 awref = <accit>-awref
                                 aworg = <accit>-aworg
                                 posnr = <accit>-posnr
                                 curtp = '00'
                                 BINARY SEARCH
                                 ASSIGNING <acccr>.
    IF sy-subrc <> 0.
      CLEAR ld_waers.
    ELSE.
      ld_waers = <acccr>-waers.
    ENDIF.

    CASE <accit>-koart.
      WHEN 'K'.
        PERFORM check_fi_account
                USING <accit>-lifnr
                      <accit>-bukrs
                      <accit>-koart
                      ld_waers
                      ld_parameter
                      ld_tabix.
        WHEN 'V'.                                           "Note1232117
      WHEN 'D'.                                             "Note1232117
        PERFORM check_fi_account
                USING <accit>-kunnr
                      <accit>-bukrs
                      <accit>-koart
                      ld_waers
                      ld_parameter
                      ld_tabix.
        WHEN OTHERS.
* Default currency from G/L account to avoid error F5353
* from FI_ACCOUNT_CHECK
        DATA: lv_sako LIKE xsako.
        IF ld_waers IS INITIAL.
          CALL FUNCTION 'FI_GL_ACCOUNT_DATA'
            EXPORTING
              i_bukrs       = <accit>-bukrs
              i_saknr       = <accit>-hkont
            IMPORTING
              E_SAKO        = lv_sako
            EXCEPTIONS
              ERROR_MESSAGE = 1
              OTHERS        = 1.

           IF sy-subrc = 0.
             ld_waers = lv_sako-waers.
           ENDIF.
        ENDIF.

        PERFORM check_fi_account
                USING <accit>-hkont
                      <accit>-bukrs
                      <accit>-koart
                      ld_waers
                      ld_parameter
                      ld_tabix.

        PERFORM check_cobl_accit
                USING <accit>
                      ld_parameter
                      ld_tabix.
    ENDCASE.
* Note 1393645, also check FI validation errors
    DATA: l_bkpf        LIKE bkpf.
    DATA: l_bseg        LIKE bseg.
    DATA: l_bsez_dummy  LIKE bsez.  "Dummy
    DATA: l_cobl        LIKE cobl.


      MOVE-CORRESPONDING: <acchd> TO l_cobl,              "note 1723218
                          <acchd> TO l_bkpf.              "note 1723218

    MOVE-CORRESPONDING: <accit> TO l_cobl,
                        <accit> TO l_bkpf,
                        <accit> TO l_bseg.

* Begin of note 1873541
    IF ls_x001-bukrs <> <accit>-bukrs.
      CALL FUNCTION 'FI_CURRENCY_INFORMATION'
        EXPORTING
          I_BUKRS                      = <accit>-bukrs
       IMPORTING
          E_X001                       = ls_x001.
    ENDIF.

    LOOP AT it_acccr ASSIGNING <acccr>
                     WHERE awtyp = <accit>-awtyp
                       AND awref = <accit>-awref
                       AND aworg = <accit>-aworg
                       AND posnr = <accit>-posnr.
      CASE <acccr>-curtp.
        WHEN '00'.
          IF <accit>-shkzg = 'H'.
            l_bseg-wrbtr = - <acccr>-wrbtr.
            l_bseg-fwbas = - <acccr>-fwbas.
            l_bseg-skfbt = - <acccr>-skfbt.
            l_bseg-wskto = - <acccr>-wskto.
            l_bseg-wmwst = - <acccr>-wmwst.
          ELSE.
            l_bseg-wrbtr = <acccr>-wrbtr.
            l_bseg-fwbas = <acccr>-fwbas.
            l_bseg-skfbt = <acccr>-skfbt.
            l_bseg-wskto = <acccr>-wskto.
            l_bseg-wmwst = <acccr>-wmwst.
          ENDIF.
        WHEN '10'.
          IF <accit>-shkzg = 'H'.
            l_bseg-dmbtr = - <acccr>-wrbtr.
            l_bseg-hwbas = - <acccr>-fwbas.
            l_bseg-sknto = - <acccr>-wskto.
            l_bseg-mwsts = - <acccr>-wmwst.
          ELSE.
            l_bseg-dmbtr = <acccr>-wrbtr.
            l_bseg-hwbas = <acccr>-fwbas.
            l_bseg-sknto = <acccr>-wskto.
            l_bseg-mwsts = <acccr>-wmwst.
          ENDIF.
        WHEN ls_x001-curt2.
          IF <accit>-shkzg = 'H'.
            l_bseg-dmbe2 = - <acccr>-wrbtr.
            l_bseg-txbh2 = - <acccr>-fwbas.
            l_bseg-mwst2 = - <acccr>-wmwst.
          ELSE.
            l_bseg-dmbe2 = <acccr>-wrbtr.
            l_bseg-txbh2 = <acccr>-fwbas.
            l_bseg-mwst2 = <acccr>-wmwst.
          ENDIF.
        WHEN ls_x001-curt3.
          IF <accit>-shkzg = 'H'.
            l_bseg-dmbe3 = - <acccr>-wrbtr.
            l_bseg-txbh3 = - <acccr>-fwbas.
            l_bseg-mwst3 = - <acccr>-wmwst.
          ELSE.
            l_bseg-dmbe3 = <acccr>-wrbtr.
            l_bseg-txbh3 = <acccr>-fwbas.
            l_bseg-mwst3 = <acccr>-wmwst.
          ENDIF.
      ENDCASE.
    ENDLOOP.
* End of note 1873541

    PERFORM kontierungsblock_bseg_fuellen(sapff001) USING l_bkpf
                                                          l_bseg
                                                          l_bsez_dummy
                                                          l_cobl.
    l_bkpf-waers = ld_waers.

    CALL FUNCTION 'FI_VALIDATION_ITEM'
      EXPORTING
        i_bkpf        = l_bkpf
        i_bseg        = l_bseg
      EXCEPTIONS
        error_message = 4
        OTHERS        = 4.

    IF sy-subrc <> 0.
      PERFORM error_from_system USING ld_parameter ld_tabix space.
    ENDIF.
* End of note 1393645
  ENDLOOP.
  ENDLOOP.                                                "note 1723218

ENDFORM.                               " CHECK_COBL


*---------------------------------------------------------------------*
*       FORM check_fi_account                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  r_account                                                     *
*  -->  r_bukrs                                                       *
*  -->  r_koart                                                       *
*  -->  r_parameter                                                   *
*  -->  r_tabix                                                       *
*---------------------------------------------------------------------*
FORM check_fi_account

  USING r_account   LIKE skb1-saknr
        r_bukrs     LIKE accit-bukrs
        r_koart     LIKE accit-koart
        r_waers     LIKE acccr-waers
        r_parameter LIKE accbapifd5-parameter
        r_tabix     LIKE accbapifd5-tabix.

  DATA: ld_field   LIKE bapiret2-field.

  CALL FUNCTION 'FI_ACCOUNT_CHECK'
    EXPORTING
      i_account                 = r_account
*     I_RECACCOUNT              = ' '
      i_bukrs                   = r_bukrs
      i_koart                   = r_koart
*     I_TCODE                   = ' '
      i_waers                   = r_waers
*     X_DIALOG                  = ' '
    EXCEPTIONS
      account_locked            = 1
      currency                  = 2
      no_reconciliation_account = 3
      reconciliation_account    = 4
      error_message             = 5
      OTHERS                    = 6.

  IF NOT sy-subrc IS INITIAL.
    CASE r_koart.
      WHEN 'K'.
        ld_field = 'VENDOR_NO'.
      WHEN 'D' OR 'V'.
        ld_field = 'CUSTOMER '.
      WHEN 'A' OR 'S' OR 'M'.
        ld_field = 'GL_ACCOUNT'.
    ENDCASE.
    PERFORM error_from_system
           USING r_parameter
                 r_tabix
                 ld_field.
  ENDIF.

ENDFORM.                               " CHECK_FI_ACCOUNT


*---------------------------------------------------------------------*
*       FORM check_cobl_accit                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  gs_accit                                                      *
*  -->  r_parameter                                                   *
*  -->  r_tabix                                                       *
*---------------------------------------------------------------------*
FORM check_cobl_accit

  USING gs_accit     LIKE accit
        r_parameter LIKE accbapifd5-parameter
        r_tabix     LIKE accbapifd5-tabix.

  DATA: lt_return    LIKE bapireturn1 OCCURS 0 WITH HEADER LINE,
        ld_cobl      LIKE cobl.

  MOVE-CORRESPONDING gs_accit TO ld_cobl.

  ld_cobl-vorgn   = 'RFBU'.                                 "Note1232217
  ld_cobl-process = 'BELEGPOS'.
  ld_cobl-event   = 'PRUEFEN'.
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      bukrs = ld_cobl-bukrs
      date  = ld_cobl-budat
    IMPORTING
      currm = ld_cobl-monat
      curry = ld_cobl-gjahr.

  CALL FUNCTION 'COBL_CODINGBLOCK_CHECK'
    EXPORTING
      check_cobl = ld_cobl
    TABLES
      t_messages = lt_return.

* no exporting of changes (also not possible with RWIN-call)

  LOOP AT lt_return.
    CLEAR gs_return.
    MOVE-CORRESPONDING lt_return TO gs_return.
    gs_return-parameter  = r_parameter.
    gs_return-row        = r_tabix.
    APPEND gs_return TO it_return.
  ENDLOOP.

ENDFORM.                               " CHECK_COBL_ACCIT


*---------------------------------------------------------------------*
*       FORM check_document_allready_posted                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  r_comp                                                        *
*---------------------------------------------------------------------*
FORM check_document_allready_posted
     USING r_comp       LIKE trwpr-component.

  DATA: ld_gjahr     LIKE bkpf-gjahr,
        lt_documents LIKE acc_doc OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF ld_key,
          awref LIKE acchd-awref,
          aworg LIKE acchd-aworg,
        END   OF ld_key.

  CHECK NOT r_comp IS INITIAL.

  IF r_comp = 'GL  '.
*   special ledger
    CALL FUNCTION 'G_CHECK_DOCUMENT_POSTED'
      TABLES
        t_acchd                 = it_acchd
        t_accit                 = it_accit
      EXCEPTIONS
        no_ledger_assigned      = 1
        document_already_posted = 2
        OTHERS                  = 3.
    IF NOT sy-subrc IS INITIAL.
      PERFORM error_from_system
              USING 'DOCUMENTHEADER' 1 'OBJ_KEY'.
    ENDIF.
  ELSE.
*   other components
    READ TABLE it_acchd INTO gs_acchd INDEX 1.
    ld_gjahr = sy-datum(04).

    CALL FUNCTION 'RWIN_CHECK'
      EXPORTING
        event     = 'RECORD  '
        gjahr     = ld_gjahr
        process   = 'DOCUMENT'
        comp_only = r_comp
      TABLES
        tkomp     = tkomp.

    LOOP AT tkomp.
      CALL FUNCTION tkomp-function
        EXPORTING
          i_awtyp     = gs_acchd-awtyp
          i_awref     = gs_acchd-awref
          i_aworg     = gs_acchd-aworg
          i_awsys     = gs_acchd-awsys
        TABLES
          t_documents = lt_documents.
    ENDLOOP.

    IF NOT lt_documents[] IS INITIAL.
      ld_key-awref = gs_acchd-awref.
      ld_key-aworg = gs_acchd-aworg.
      CLEAR gs_return.
      gs_return-type       = 'E'.
      gs_return-id         = 'RW'.
      gs_return-number     = 623.
      gs_return-message_v1 = gs_acchd-awtyp.
      gs_return-message_v2 = ld_key.
      gs_return-message_v3 = gs_acchd-awsys.
      gs_return-message_v4 = r_comp.
      gs_return-parameter  = 'DOCUMENTHEADER'.
      gs_return-row        = '1'.
      gs_return-field      = 'OBJ_KEY'.
      APPEND gs_return TO it_return.                     "note 1034551
    ENDIF.
  ENDIF.

ENDFORM.                               " CHECK_DOCUMENT_ALLREADY_POSTED

*&---------------------------------------------------------------------*
*&      Form  FILL_ACCIT_FROM_BAPI_ACCIT
*&---------------------------------------------------------------------*
*       Mapping der Felder, ISO-Konvertierungen
*----------------------------------------------------------------------*
FORM fill_accit_from_bapi_accit .

  DATA ls_fica_it TYPE fica_it_type.
  DATA l_docno    LIKE bapiache09-ac_doc_no.              "note 536852

* ACCBAPIFD5
  gs_accit-posnr              = gs_bapi_accit-itemno_acc          .
*                            = gs_bapi_accit-PARAMETER           .
*                            = gs_bapi_accit-TABIX               .
  gs_accit-bukrs              = gs_bapi_accit-comp_code           .
  gs_accit-hkont              = gs_bapi_accit-gl_account          .
  gs_accit-budat              = gs_bapi_accit-pstng_date          .
  gs_accit-bldat              = gs_bapi_accit-doc_date            .
  gs_accit-wwert              = gs_bapi_accit-trans_date          .
  gs_accit-valut              = gs_bapi_accit-value_date          .
  gs_accit-logvo              = gs_bapi_accit-log_proc            .
  gs_accit-kstat              = gs_bapi_accit-stat_con            .

  CALL FUNCTION 'REF_DOC_NO_CONVERSION_INBOUND'
    EXPORTING
      i_ref_doc_no      = gs_bapi_acchd-ref_doc_no
      i_ref_doc_no_long = gs_bapi_acchd-ref_doc_no_long
    IMPORTING
      e_ref_doc_no_long = gs_accit-xblnr.

  gs_accit-belnr              = gs_bapi_accit-ac_doc_no           .
  IF NOT gs_bapi_accit-ac_doc_no IS INITIAL.              "note 536852
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'           "note 536852
      EXPORTING                                           "note 536852
        input         = gs_bapi_accit-ac_doc_no           "note 536852
      IMPORTING                                           "note 536852
        output        = l_docno.                          "note 536852
    IF gs_bapi_accit-ac_doc_no NE l_docno.                "note 536852
      CLEAR gs_return.                                    "note 536852
      gs_return-type       = 'E'.                         "note 536852
      gs_return-id         = 'RW'.                        "note 536852
      gs_return-number     = '609'.                       "note 536852
      gs_return-message_v1 = gs_bapi_acchd-obj_type.     "note 1982803
      gs_return-message_v2 = gs_bapi_accit-ac_doc_no.     "note 536852
      gs_return-message_v3 = gs_bapi_acchd-obj_sys.      "note 1982803
      gs_return-parameter  = gs_bapi_accit-parameter.     "note 536852
      gs_return-field      = 'AC_DOC_NO'.                 "note 536852
      gs_return-row        = gs_bapi_accit-tabix.         "note 536852
      APPEND gs_return TO it_return.                     "note 1034551
    ENDIF.                                                "note 536852
  ENDIF.                                                  "note 536852
  gs_accit-gjahr              = gs_bapi_accit-fisc_year           .
  gs_accit-monat              = gs_bapi_accit-fis_period          .
*  gs_accit-ABPER              = gs_bapi_accit-STLMNT_PER          .
  gs_accit-blart              = gs_bapi_accit-doc_type            .
  gs_accit-xref1              = gs_bapi_accit-ref_key_1           .
  gs_accit-xref2              = gs_bapi_accit-ref_key_2           .
  gs_accit-xref3              = gs_bapi_accit-ref_key_3           .
  gs_accit-ktosl              = gs_bapi_accit-acct_key            .
  gs_accit-koart              = gs_bapi_accit-acct_type           .
  gs_accit-kunnr              = gs_bapi_accit-customer            .
  gs_accit-lifnr              = gs_bapi_accit-vendor_no           .
  gs_accit-sgtxt              = gs_bapi_accit-item_text           .
  gs_accit-gsber              = gs_bapi_accit-bus_area            .
  gs_accit-kostl              = gs_bapi_accit-costcenter          .
  gs_accit-lstar              = gs_bapi_accit-acttype             .
  gs_accit-bpmng              = gs_bapi_accit-po_pr_qnt           .
  gs_accit-bprme              = gs_bapi_accit-po_pr_uom           .
*                            = gs_bapi_accit-PO_PR_UOM_ISO       .
  PERFORM convert_unit_from_iso
          USING gs_bapi_accit-parameter
                gs_bapi_accit-tabix
                gs_bapi_accit-po_pr_uom_iso
                gs_accit-bprme.

* begin of note 1685076
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input         = gs_bapi_accit-orderid
    IMPORTING
      output        = gs_accit-aufnr.
* gs_accit-aufnr              = gs_bapi_accit-orderid             .
* end of note 1685076
  gs_accit-anln1              = gs_bapi_accit-asset_no            .
  gs_accit-anln2              = gs_bapi_accit-sub_number          .
  gs_accit-bzdat              = gs_bapi_accit-asval_date          .
  gs_accit-matnr              = gs_bapi_accit-material            .
  gs_accit-menge              = gs_bapi_accit-quantity            .
  gs_accit-meins              = gs_bapi_accit-base_uom            .
*                            = gs_bapi_accit-BASE_UOM_ISO        .
  PERFORM convert_unit_from_iso
          USING gs_bapi_accit-parameter
                gs_bapi_accit-tabix
                gs_bapi_accit-base_uom_iso
                gs_accit-meins.

  gs_accit-werks              = gs_bapi_accit-plant               .
  gs_accit-pargb              = gs_bapi_accit-tr_part_ba          .
  gs_accit-hrkft              = gs_bapi_accit-orig_group          .
  gs_accit-hkmat              = gs_bapi_accit-orig_mat            .
  gs_accit-kstrg              = gs_bapi_accit-costobject.   "Note769308
  gs_accit-prctr              = gs_bapi_accit-profit_ctr          .
  gs_accit-pprctr             = gs_bapi_accit-part_prctr          .
*                            = gs_bapi_accit-WBS_ELEMENT         .
  PERFORM convert_input_psp_element
          USING gs_bapi_accit-parameter
                gs_bapi_accit-tabix
                gs_bapi_accit-wbs_element
                gs_accit-ps_psp_pnr.

  gs_accit-nplnr              = gs_bapi_accit-network             .
  gs_accit-vornr              = gs_bapi_accit-activity            . "note964414
  gs_accit-aufpl              = gs_bapi_accit-routing_no          .
  gs_accit-aufps              = gs_bapi_accit-order_itno          .
* gs_accit-fipos              = gs_bapi_accit-cmmt_item           .
* new logic CMMT_ITEM_LONG
  CALL FUNCTION 'CMMT_ITEM_CONVERSION_INBOUND'
    EXPORTING
      i_cmmt_item      = gs_bapi_accit-cmmt_item
      i_cmmt_item_long = gs_bapi_accit-cmmt_item_long
    IMPORTING
      e_cmmt_item      = gs_accit-fipos
    EXCEPTIONS
      not_found        = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
    PERFORM error_from_system
            USING gs_bapi_accit-parameter
                  gs_bapi_accit-tabix
                  'CMMT_ITEM_LONG'.
  ENDIF.

* Budget Period EhP4:
  IF cl_psm_core_switch_check=>psm_fm_core_bud_per_rev_1( ) IS NOT INITIAL.
    gs_accit-budget_pd =  gs_bapi_accit-budget_period.
    gs_accit-pbudget_pd = gs_bapi_accit-partner_budget_period.
  ENDIF.

  gs_accit-fistl              = gs_bapi_accit-funds_ctr           .
  gs_accit-geber              = gs_bapi_accit-fund                .
  gs_accit-kdauf              = gs_bapi_accit-sales_ord           .
  gs_accit-kdpos              = gs_bapi_accit-s_ord_item          .
* start of note 1673816
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input         = gs_bapi_accit-trade_id
    IMPORTING
      output        = gs_accit-vbund.
* gs_accit-vbund              = gs_bapi_accit-trade_id            .
* end of note 1673816
  gs_accit-bwkey              = gs_bapi_accit-val_area            .
  gs_accit-bwtar              = gs_bapi_accit-val_type            .
  gs_accit-zekkn              = gs_bapi_accit-serial_no           .
  gs_accit-vptnr              = gs_bapi_accit-part_acct           .
  gs_accit-kzbew              = gs_bapi_accit-mvt_ind             .
  gs_accit-pstyp              = gs_bapi_accit-item_cat            .
  gs_accit-xumbw              = gs_bapi_accit-reval_ind           .
  gs_accit-mtart              = gs_bapi_accit-matl_type           .
  gs_accit-spart              = gs_bapi_accit-division            .
  gs_accit-brgew              = gs_bapi_accit-gross_wt            .
  gs_accit-ntgew              = gs_bapi_accit-net_weight          .
  gs_accit-gewei              = gs_bapi_accit-unit_of_wt          .
* gs_accit-                   = gs_bapi_accit-UNIT_OF_WT_ISO      .
  PERFORM convert_unit_from_iso
          USING gs_bapi_accit-parameter
                gs_bapi_accit-tabix
                gs_bapi_accit-unit_of_wt_iso
                gs_accit-gewei.

  gs_accit-volum              = gs_bapi_accit-volume              .
  gs_accit-voleh              = gs_bapi_accit-volumeunit          .
* gs_accit-                   = gs_bapi_accit-VOLUMEUNIT_ISO      .
  PERFORM convert_unit_from_iso
          USING gs_bapi_accit-parameter
                gs_bapi_accit-tabix
                gs_bapi_accit-volumeunit_iso
                gs_accit-voleh.

  gs_accit-erfmg              = gs_bapi_accit-entry_qnt           .
  gs_accit-erfme              = gs_bapi_accit-entry_uom           .
* gs_accit-                   = gs_bapi_accit-ENTRY_UOM_ISO       .
  PERFORM convert_unit_from_iso
          USING gs_bapi_accit-parameter
                gs_bapi_accit-tabix
                gs_bapi_accit-entry_uom_iso
                gs_accit-erfme.

* gs_accit                    = gs_bapi_accit-ACTIVITY            .
  PERFORM convert_input_network
          USING gs_bapi_accit-network
                gs_bapi_accit-activity
                gs_accit-aufpl
                gs_accit-aplzl.

  PERFORM convert_input_operation
          USING gs_bapi_accit-orderid
                gs_bapi_accit-activity
                gs_accit-aufpl
                gs_accit-aplzl.

  gs_accit-zuonr              = gs_bapi_accit-alloc_nmbr          .
  gs_accit-fkart              = gs_bapi_accit-bill_type           .
  gs_accit-zfbdt              = gs_bapi_accit-bline_date          .
  gs_accit-diekz              = gs_bapi_accit-bllsrv_ind          .
  gs_accit-zaehk              = gs_bapi_accit-cond_count          .
  gs_accit-stunr              = gs_bapi_accit-cond_st_no          .
* gs_accit-kschl              = gs_bapi_accit-cond_type           .
  gs_accit-xskrl              = gs_bapi_accit-cshdis_ind          .
  gs_accit-tbtkz              = gs_bapi_accit-de_cre_ind          .
  gs_accit-vtweg              = gs_bapi_accit-distr_chan          .
  gs_accit-zbd1t              = gs_bapi_accit-dsct_days1          .
  gs_accit-zbd2t              = gs_bapi_accit-dsct_days2          .
  gs_accit-zbd1p              = gs_bapi_accit-dsct_pct1           .
  gs_accit-zbd2p              = gs_bapi_accit-dsct_pct2           .
  gs_accit-mansp              = gs_bapi_accit-dunn_block          .
  gs_accit-mschl              = gs_bapi_accit-dunn_key            .
* gs_accit-fkber              = gs_bapi_accit-func_area           .
* new logic: FUNC_AREA_LONG
  CALL FUNCTION 'FUNC_AREA_CONVERSION_INBOUND'
    EXPORTING
      i_func_area      = gs_bapi_accit-func_area
      i_func_area_long = gs_bapi_accit-func_area_long
    IMPORTING
      e_func_area_long = gs_accit-fkber.

  gs_accit-bvtyp              = gs_bapi_accit-partner_bk          .
* gs_accit-PERNR              = gs_bapi_accit-PERSON_NO           .
  gs_accit-zterm              = gs_bapi_accit-pmnttrms            .
  gs_accit-zlspr              = gs_bapi_accit-pmnt_block          .
  gs_accit-uzawe              = gs_bapi_accit-pmtmthsupl          .
  gs_accit-esrpz              = gs_bapi_accit-po_checkdg          .
  gs_accit-esrre              = gs_bapi_accit-po_ref_no           .
  gs_accit-esrnr              = gs_bapi_accit-po_sub_no           .
  gs_accit-zlsch              = gs_bapi_accit-pymt_meth           .
  gs_accit-eprctr             = gs_bapi_accit-p_el_prctr          .
  gs_accit-vkorg              = gs_bapi_accit-salesorg            .
  gs_accit-vkgrp              = gs_bapi_accit-sales_grp           .
  gs_accit-vkbur              = gs_bapi_accit-sales_off           .
  gs_accit-lzbkz              = gs_bapi_accit-scbank_ind          .
  gs_accit-kunag              = gs_bapi_accit-sold_to             .
  gs_accit-landl              = gs_bapi_accit-supcountry          .
* gs_accit-                   = gs_bapi_accit-SUPCOUNTRY_ISO      .
  PERFORM convert_land_from_iso
          USING gs_bapi_accit-parameter
                gs_bapi_accit-tabix
                gs_bapi_accit-supcountry_iso
                gs_accit-landl.

  gs_accit-txjcd              = gs_bapi_accit-taxjurcode          .
  gs_accit-mwskz              = gs_bapi_accit-tax_code            .
* gs_accit-kschl              = gs_bapi_accit-cond_key            .

  IF     gs_bapi_accit-parameter = 'ACCOUNTGL'.
    gs_accit-kschl            = gs_bapi_accit-cond_type           .
  ELSEIF gs_bapi_accit-parameter = 'ACCOUNTTAX'.
    gs_accit-kschl            = gs_bapi_accit-cond_key            .
  ENDIF.

  IF     gs_bapi_accit-parameter = 'ACCOUNTTAX' AND   "note1010210
         gs_bapi_accit-direct_tax IS INITIAL.         "note1010210
    gs_accit-xauto          = 'X'.                    "note1010210
  ENDIF.                                              "note1010210

* KBETR von typ CURR => Festpunktarithmetik streicht eine Kommastelle!
  gs_accit-kbetr              = gs_bapi_accit-tax_rate * 10       .
  gs_accit-txdat              = gs_bapi_accit-tax_date            .
  gs_accit-stceg              = gs_bapi_accit-vat_reg_no          .
  gs_accit-qsskz              = gs_bapi_accit-w_tax_code          .
  gs_accit-zbd3t              = gs_bapi_accit-netterms            .
  gs_accit-fklmg              = gs_bapi_accit-inv_qty_su          .
  gs_accit-fkimg              = gs_bapi_accit-inv_qty             .
  gs_accit-vrkme              = gs_bapi_accit-sales_unit          .
* gs_accit-                   = gs_bapi_accit-SALES_UNIT_ISO      .
  PERFORM convert_unit_from_iso
          USING gs_bapi_accit-parameter
                gs_bapi_accit-tabix
                gs_bapi_accit-sales_unit_iso
                gs_accit-vrkme.
  gs_accit-prznr              = gs_bapi_accit-co_busproc          .

* Neu zum Plug-In
  gs_accit-audat               = gs_bapi_accit-auth_date          .
  gs_accit-autra               = gs_bapi_accit-auth_refno.
  gs_accit-autim               = gs_bapi_accit-auth_time.
  gs_accit-ccwae               = gs_bapi_accit-currency.
* gs_accit-                    = gs_bapi_accit-currency_iso.

  PERFORM convert_curr_to_internal
          USING gs_bapi_accit-parameter
                gs_bapi_accit-tabix
                gs_accit-ccwae
                gs_bapi_accit-currency_iso
                gs_bapi_accit-authamount
                gs_accit-autwr.

* gs_accit-autwr               = gs_bapi_accit-authamount.
  gs_accit-aunum               = gs_bapi_accit-cc_autth_no.
  gs_accit-ccact               = gs_bapi_accit-cc_glaccount.
  gs_accit-ccname              = gs_bapi_accit-cc_name.
  gs_accit-ccnum               = gs_bapi_accit-cc_number.
  gs_accit-ccfol               = gs_bapi_accit-cc_seq_no.
  gs_accit-ccins               = gs_bapi_accit-cc_type.
  gs_accit-datab               = gs_bapi_accit-cc_valid_f.
  gs_accit-datbi               = gs_bapi_accit-cc_valid_t.
  gs_accit-cctyp               = gs_bapi_accit-cctyp.
  gs_accit-csour               = gs_bapi_accit-dataorigin.
  gs_accit-trmid               = gs_bapi_accit-terminal.
  gs_accit-locid               = gs_bapi_accit-point_of_receipt.
  gs_accit-merch               = gs_bapi_accit-merchidcl.
  gs_accit-kidno               = gs_bapi_accit-paymt_ref.
  gs_accit-dtws1               = gs_bapi_accit-instr1.
  gs_accit-dtws2               = gs_bapi_accit-instr2.
  gs_accit-dtws3               = gs_bapi_accit-instr3.
  gs_accit-dtws4               = gs_bapi_accit-instr4.
  gs_accit-kokrs               = gs_bapi_accit-co_area.
  gs_accit-paobjnr             = gs_bapi_accit-profit_segm_no.
  gs_accit-xmfrw               = gs_bapi_accit-xmfrw.
  gs_accit-txjdp               = gs_bapi_accit-taxjurcode_deep.
  gs_accit-txjlv               = gs_bapi_accit-taxjurcode_level.
  gs_accit-bupla               = gs_bapi_accit-businessplace.
  gs_accit-secco               = gs_bapi_accit-sectioncode.
  gs_accit-filkd               = gs_bapi_accit-branch        .
  gs_accit-pycur               = gs_bapi_accit-pymt_cur      .
* gs_accit-                    = gs_bapi_accit-pymt_cur_iso      .

  PERFORM convert_curr_to_internal
          USING gs_bapi_accit-parameter
                gs_bapi_accit-tabix
                gs_accit-pycur
                gs_bapi_accit-pymt_cur_iso
                gs_bapi_accit-pymt_amt
                gs_accit-pyamt.

* gs_accit-pyamt               = gs_bapi_accit-pymt_amt     .
  gs_accit-kkber               = gs_bapi_accit-c_ctr_area     .
  gs_accit-hbkid               = gs_bapi_accit-bank_id             .
  gs_accit-ebeln               = gs_bapi_accit-po_number             .
  gs_accit-ebelp               = gs_bapi_accit-po_item             .
  gs_accit-fikrs               = gs_bapi_accit-fm_area.
  gs_accit-umskz               = gs_bapi_accit-sp_gl_ind.
  gs_accit-posnr_sd            = gs_bapi_accit-itm_number.
  gs_accit-kntyp               = gs_bapi_accit-cond_category.
  gs_accit-grant_nbr           = gs_bapi_accit-grant_nbr            .
  gs_accit-empfb               = gs_bapi_accit-alt_payee.   "note1048245
  gs_accit-bvtypabw            = gs_bapi_accit-alt_payee_bank       .
  IF NOT gs_bapi_accit-itemno_tax EQ space.                   "N1859478
    gs_accit-taxps             = gs_bapi_accit-itemno_tax.    "N1859478
  ENDIF.                                                      "N1859478
  gs_accit-maber               = gs_bapi_accit-dunn_area.
  gs_accit-rmvct               = gs_bapi_accit-cs_trans_t.
  gs_accit-measure             = gs_bapi_accit-measure.            "RE
  gs_accit-hktid               = gs_bapi_accit-housebankacctid.    "RE
  gs_accit-kblnr               = gs_bapi_accit-res_doc.
  gs_accit-kblpos              = gs_bapi_accit-res_item.
  gs_accit-vatdate             = gs_bapi_accit-vatdate.     "N1232273
* begin of note 1685076
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input         = gs_bapi_accit-segment
    IMPORTING
      output        = gs_accit-segment.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input         = gs_bapi_accit-partner_segment
    IMPORTING
      output        = gs_accit-psegment.
* gs_accit-segment             = gs_bapi_accit-segment.     "note998466
* gs_accit-psegment            = gs_bapi_accit-partner_segment. "note998466
* end of note 1685076
  gs_accit-perop_beg           = gs_bapi_accit-billing_period_start_date.
  gs_accit-perop_end           = gs_bapi_accit-billing_period_end_date.

* Grantor fields
  gs_accit-crm_payment_type    = gs_bapi_accit-payment_type.    " Note 1534918
  gs_accit-crm_expense_type    = gs_bapi_accit-expense_type.    " Note 1534918
  gs_accit-prog_profile        = gs_bapi_accit-program_profile. " Note 1534918

* Decoupling: EA-APPL may not be installed -> no PPA_EX_IND in ACCIT
  FIELD-SYMBOLS:   <fs_ppa_ex_ind> LIKE accbapifd5-ppa_ex_ind.
  ASSIGN COMPONENT 'PPA_EX_IND' OF STRUCTURE gs_accit TO <fs_ppa_ex_ind>.
  IF sy-subrc = 0.
    <fs_ppa_ex_ind>            = gs_bapi_accit-ppa_ex_ind.
  ENDIF.
  gs_accit-reindat             = gs_bapi_accit-invoice_rec_date.
  gs_accit-fastpay             = gs_bapi_accit-fastpay.
  gs_accit-pgeber              = gs_bapi_accit-partner_fund.
  gs_accit-pgrant_nbr          = gs_bapi_accit-partner_grant_nbr.
  gs_accit-case_guid_core      = gs_bapi_accit-case_guid.
  gs_accit-dispute_if_type     = gs_bapi_accit-dispute_if_type.
  gs_accit-pays_prov           = gs_bapi_accit-pays_prov.
  gs_accit-pays_tran           = gs_bapi_accit-pays_tran.

  gs_accit-mndid               = gs_bapi_accit-sepa_mandate_id.  "Note 1815111



* FI-CA Zusatz:
  ls_fica_it-vkont             = gs_bapi_accit-cont_acct.
  ls_fica_it-hvorg             = gs_bapi_accit-main_trans.
  ls_fica_it-tvorg             = gs_bapi_accit-sub_trans.
  ls_fica_it-actv_account      = gs_bapi_accit-actv_account.
  ls_fica_it-vtref             = gs_bapi_accit-vtref.
  ls_fica_it-case_guid         = gs_bapi_accit-case_guid.
  ls_fica_it-reference_no      = gs_bapi_accit-reference_no.
  ls_fica_it-crmobj            = gs_bapi_accit-crmobj.

  MOVE-CORRESPONDING ls_fica_it TO gs_accit.
  MOVE-CORRESPONDING gs_fica_hd TO gs_accit.

ENDFORM.                    " FILL_ACCIT_FROM_BAPI_ACCIT

*---------------------------------------------------------------------*
*       FORM fill_acct_key                                            *
*---------------------------------------------------------------------*
*       Buchungsschlüssel ermitteln                                   *
*---------------------------------------------------------------------*
FORM fill_acct_key.

  DATA: ld_bschh  LIKE accit-bschl,
        ld_bschs  LIKE accit-bschl,
        ld_umskz  LIKE accit-umskz.

  IF NOT gs_accit-ktosl IS INITIAL AND
     gs_accit-kstat IS INITIAL.
    CALL FUNCTION 'FI_GET_POSTING_KEY'
      EXPORTING
        i_ktosl       = gs_accit-ktosl
      IMPORTING
        e_bschs       = ld_bschs
        e_bschh       = ld_bschh
        e_umskz       = ld_umskz
      EXCEPTIONS
        error_message = 1.
    IF sy-subrc IS INITIAL.
      IF gs_accit-shkzg = 'S'.
        gs_accit-bschl = ld_bschs.
      ELSE.
        gs_accit-bschl = ld_bschh.
      ENDIF.
**     SHB-Vorgänge HR-Travel mit Ktosl 'HRV'
*      IF NOT ld_umskz IS INITIAL.
*        gs_accit-umskz =  ld_umskz.
**        keine Ableitung Zahlungsbedingung für SHB-Vorgänge
*        CLEAR gs_accit-zterm.
*      ENDIF.
    ELSE.
      PERFORM error_from_system
              USING gs_bapi_accit-parameter
                    gs_bapi_accit-tabix
                    'ACCT_KEY'.
    ENDIF.
  ENDIF.

  IF gs_accit-bschl IS INITIAL.
    CASE gs_accit-koart.
      WHEN 'D' OR 'V'.
        IF gs_accit-shkzg = 'S'.
          gs_accit-bschl = '01'.
        ELSE.
          gs_accit-bschl = '11'.
        ENDIF.
      WHEN 'K'.
        IF gs_accit-shkzg = 'S'.
          gs_accit-bschl = '21'.
        ELSE.
          gs_accit-bschl = '31'.
        ENDIF.
      WHEN 'S'.
        IF gs_accit-shkzg = 'S'.
          gs_accit-bschl = '40'.
        ELSE.
          gs_accit-bschl = '50'.
        ENDIF.
      WHEN 'A'.
        IF gs_accit-shkzg = 'S'.
          gs_accit-bschl = '70'.
        ELSE.
          gs_accit-bschl = '75'.
        ENDIF.
      WHEN 'M'.
        IF gs_accit-shkzg = 'S'.
          gs_accit-bschl = '40'.
        ELSE.
          gs_accit-bschl = '50'.
        ENDIF.
    ENDCASE.
  ENDIF.

  IF NOT gs_accit-umskz IS INITIAL.
    IF gs_accit-bschl+1 = '1'.
      gs_accit-bschl+1 = '9'.
    ENDIF.
  ENDIF.

ENDFORM.                    " FILL_ACCT_KEY

*---------------------------------------------------------------------*
*       FORM get_txjcd_level                                          *
*---------------------------------------------------------------------*
*       Level und Deep für Taxjurisdictioncode ermitteln,
*       wenn nicht gefüllt.
*---------------------------------------------------------------------*
FORM get_txjcd_level.

  DATA   ld_level  TYPE i.

  CHECK NOT gs_accit-txjcd IS INITIAL.

  IF gs_accit-txjdp IS INITIAL.
    gs_accit-txjdp = gs_accit-txjcd.
  ENDIF.

  CHECK gs_accit-txjlv IS INITIAL.

  CALL FUNCTION 'FI_TAX_GET_TXJCD_LEVELS'
    EXPORTING
      i_bukrs  = gs_accit-bukrs
      i_txjcd  = gs_accit-txjcd
      i_xcheck = 'X'
    IMPORTING
      level    = ld_level
    EXCEPTIONS
      OTHERS   = 1.

  IF sy-subrc IS INITIAL.
    gs_accit-txjlv = ld_level.
  ELSE.
    PERFORM error_from_system
        USING gs_bapi_accit-parameter
              gs_bapi_accit-tabix
              'TAXJURCODE'.
  ENDIF.

ENDFORM.                    " GET_TXJCD_LEVEL

*&---------------------------------------------------------------------*
*&      Form  FILL_LINE_FROM_HEADER
*&---------------------------------------------------------------------*
*       Zeilenfelder werden aus dem Kopf übernommen wenn leer
*----------------------------------------------------------------------*
FORM fill_line_from_header.

  IF     gs_accit-bukrs           IS INITIAL AND
     NOT gs_bapi_acchd-comp_code  IS INITIAL.
    gs_accit-bukrs = gs_bapi_acchd-comp_code.
  ENDIF.
  IF     gs_accit-budat           IS INITIAL AND
     NOT gs_bapi_acchd-pstng_date IS INITIAL.
    gs_accit-budat = gs_bapi_acchd-pstng_date.
  ENDIF.
  IF     gs_accit-blart           IS INITIAL AND
     NOT gs_bapi_acchd-doc_type   IS INITIAL.
    gs_accit-blart = gs_bapi_acchd-doc_type.
  ENDIF.
  IF     gs_accit-belnr           IS INITIAL AND
     NOT gs_bapi_acchd-ac_doc_no  IS INITIAL.
    gs_accit-belnr = gs_bapi_acchd-ac_doc_no.
  ENDIF.
  IF     gs_accit-gjahr           IS INITIAL AND
     NOT gs_bapi_acchd-fisc_year  IS INITIAL.
    gs_accit-gjahr = gs_bapi_acchd-fisc_year.
  ENDIF.
  IF     gs_accit-monat           IS INITIAL AND
     NOT gs_bapi_acchd-fis_period IS INITIAL.
    gs_accit-monat = gs_bapi_acchd-fis_period.
  ENDIF.

  IF     gs_accit-bldat           IS INITIAL AND
     NOT gs_bapi_acchd-doc_date   IS INITIAL.
    gs_accit-bldat = gs_bapi_acchd-doc_date.
  ENDIF.

  IF     gs_accit-wwert           IS INITIAL AND
     NOT gs_bapi_acchd-trans_date IS INITIAL.
    gs_accit-wwert = gs_bapi_acchd-trans_date.
  ENDIF.

  IF     gs_accit-fktyp           IS INITIAL AND
     NOT gs_bapi_acchd-bill_category IS INITIAL.
    gs_accit-fktyp = gs_bapi_acchd-bill_category.
  ENDIF.


  IF     gs_accit-xblnr                  IS INITIAL AND
     NOT ( gs_bapi_acchd-ref_doc_no      IS INITIAL
      OR   gs_bapi_acchd-ref_doc_no_long IS INITIAL ).
    CALL FUNCTION 'REF_DOC_NO_CONVERSION_INBOUND'
      EXPORTING
        i_ref_doc_no      = gs_bapi_acchd-ref_doc_no
        i_ref_doc_no_long = gs_bapi_acchd-ref_doc_no_long
      IMPORTING
        e_ref_doc_no_long = gs_accit-xblnr.
  ENDIF.

  gs_accit-stgrd = gs_bapi_acchd-reason_rev.
  gs_accit-xnegp = gs_bapi_acchd-neg_postng.
  gs_accit-xprev = gs_bapi_acchd-partial_rev.             "Note 1891435

ENDFORM.                    " FILL_LINE_FROM_HEADER

*---------------------------------------------------------------------*
*       FORM process_accit_tx                                         *
*---------------------------------------------------------------------*
*       KTOSL und HKONT verproben                                     *
*---------------------------------------------------------------------*
*  -->  r_tabix                                                       *
*---------------------------------------------------------------------*
FORM process_accit_tx
                USING r_tabix LIKE sy-tabix.

  DATA: lt_t030k LIKE t030k OCCURS 0 WITH HEADER LINE,
        ld_hkont LIKE accit-hkont,
        ld_t007b LIKE t007b.                               "note 668493

  gs_accit-koart = 'S'.
  gs_accit-taxit = 'X'.

  PERFORM check_if_initial
          USING 'ACCOUNTTAX'
                r_tabix
                'TAX_CODE  '
                gs_accit-mwskz.

  CHECK NOT gs_accit-mwskz IS INITIAL.

*   check NAV or NVV ?                                      note 668493
  CALL FUNCTION 'FI_TAX_GET_TAX_PROCESSINGS'             "note 668493
    EXPORTING                                            "note 668493
      i_ktosl         = gs_accit-ktosl                   "note 668493
    IMPORTING                                            "note 668493
      e_t007b         = ld_t007b                         "note 668493
    EXCEPTIONS                                           "note 668493
      ktosl_not_found = 1                                "note 668493
      OTHERS          = 2.                               "note 668493
  IF sy-subrc IS INITIAL AND ld_t007b-stazf = 'X' AND    "note 668493
     ld_t007b-stbkz = '3'.                               "note 838350
*    mark only NVV items as statistical !                "note 838350
    gs_accit-kstat = 'X'.                              "note 668493
    PERFORM check_if_initial                           "note 668493
            USING 'ACCOUNTTAX'                         "note 668493
                  r_tabix:                             "note 668493
                  'COND_KEY  ' gs_accit-kschl.         "note 668493
*                     'TAX_RATE  ' gs_accit-KBETR.         "note 668493
  ELSE.                                                  "note 668493
*   no NVV
    IF gs_accit-awtyp = 'BEBD' OR
       gs_accit-awtyp = 'BERD' OR
       gs_accit-awtyp = 'BERE' OR
       gs_accit-awtyp = 'ENTD'.
*     Logik nur für Billing Engine
      CALL FUNCTION 'FI_TAX_GET_TAX_ACC_ASSIGNMENT'
        EXPORTING
          i_bukrs         = gs_accit-bukrs
          i_mwskz         = gs_accit-mwskz
          i_ktosl         = gs_accit-ktosl
          i_xignore_distr = 'X'
        IMPORTING
          e_t030k         = lt_t030k
*         E_T030B         = T_T030B
        EXCEPTIONS
          parameter_error = 1
          bukrs_not_found = 2
          ktosl_not_found = 8
          entry_not_found = 9
          OTHERS          = 10.
      IF sy-subrc IS INITIAL.
        APPEND lt_t030k.
      ENDIF.
    ELSE.
      CALL FUNCTION 'FI_TAX_GET_TAX_ACC_BY_KSCHL'
        EXPORTING
          i_bukrs       = gs_accit-bukrs
          i_mwskz       = gs_accit-mwskz
          i_txjcd       = gs_accit-txjcd
          i_prsdt       = gs_accit-txdat
          i_kschl       = gs_accit-kschl
          i_xdeep       = 'X'
*         I_XBAPI       =
        TABLES
          t_t030k       = lt_t030k
*         T_T030B       =
*         ERROR_RETURN  =
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
    ENDIF.

    IF sy-subrc IS INITIAL.
      DESCRIBE TABLE lt_t030k LINES sy-tfill.
      IF sy-tfill = 1.
*       unique
        READ TABLE lt_t030k INDEX 1.

*       check KTOSL
        IF gs_accit-ktosl IS INITIAL.
          gs_accit-ktosl = lt_t030k-ktosl.
        ELSE.
          IF gs_accit-ktosl <> lt_t030k-ktosl.
*           ERROR wrong KTOSL
            PERFORM append_msg_to_return
                    USING 'E'             "TYPE
                          'F5'            "ID
                          '741'           "NUMBER
                          gs_accit-mwskz  "MESSAGE_ONE
                          lt_t030k-ktosl  "MESSAGE_TWO
                          'ACCOUNTTAX'    "PARAMETER
                          r_tabix         "ROW
                          'ACCT_KEY'.     "FIELD
          ENDIF.
        ENDIF.

*       check HKONT
        IF gs_accit-shkzg = 'S'.
          ld_hkont = lt_t030k-konts.
        ELSE.
          ld_hkont = lt_t030k-konth.
        ENDIF.

        IF gs_accit-hkont IS INITIAL.
          gs_accit-hkont = ld_hkont.
        ELSE.
          IF gs_accit-hkont <> ld_hkont
          AND gs_accit-xauto = 'X'.                      "Note1455675
*           ERROR wrong HKONT
            PERFORM append_msg_to_return
                    USING 'E'             "TYPE
                          'RW'            "ID
                          '618'           "NUMBER
                          gs_accit-mwskz   "MESSAGE_ONE
                          ld_hkont        "MESSAGE_TWO
                          'ACCOUNTTAX'    "PARAMETER
                          r_tabix         "ROW
                          'GL_ACCOUNT'.   "FIELD
          ENDIF.
        ENDIF.

      ELSE.
*       more than one entry
        IF gs_accit-ktosl IS INITIAL OR
           gs_accit-hkont IS INITIAL.
          IF NOT gs_accit-ktosl IS INITIAL.
            READ TABLE lt_t030k WITH
                 KEY ktosl = gs_accit-ktosl.
            IF sy-subrc IS INITIAL.
              IF gs_accit-shkzg = 'S'.
                gs_accit-hkont = lt_t030k-konts.
              ELSE.
                gs_accit-hkont = lt_t030k-konth.
              ENDIF.
            ELSE.
              PERFORM append_msg_to_return
                      USING 'E'             "TYPE
                            'RW'            "ID
                            '619'           "NUMBER
                            gs_accit-ktosl   "MESSAGE_ONE
                            gs_accit-hkont   "MESSAGE_TWO
                            'ACCOUNTTAX'    "PARAMETER
                            r_tabix         "ROW
                            'GL_ACCOUNT'.   "FIELD
            ENDIF.
          ELSE.
*           system can not determine KTOSL and HKONT => ERROR
            PERFORM check_if_initial
                    USING 'ACCOUNTTAX'
                          r_tabix:
                          'GL_ACCOUNT' gs_accit-hkont,
                          'ACCT_KEY  ' gs_accit-ktosl.
          ENDIF.
        ELSE.
*         check if combination KTOSL and HKONT valid
          IF gs_accit-shkzg = 'S'.
            READ TABLE lt_t030k
                 WITH KEY ktosl = gs_accit-ktosl
                          konts = gs_accit-hkont.
          ELSE.
            READ TABLE lt_t030k
                 WITH KEY ktosl = gs_accit-ktosl
                          konth = gs_accit-hkont.
          ENDIF.
          IF NOT sy-subrc IS INITIAL
          AND gs_accit-xauto = 'X'.                       "Note 1869159
            PERFORM append_msg_to_return
                    USING 'E'             "TYPE
                          'RW'            "ID
                          '619'           "NUMBER
                          gs_accit-ktosl   "MESSAGE_ONE
                          gs_accit-hkont   "MESSAGE_TWO
                          'ACCOUNTTAX'    "PARAMETER
                          r_tabix         "ROW
                          'GL_ACCOUNT'.   "FIELD
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      PERFORM error_from_system
              USING 'ACCOUNTTAX'
                    r_tabix
                    'ACCT_KEY'.
    ENDIF.
  ENDIF.


ENDFORM.                    " PROCESS_ACCIT_TX

*&---------------------------------------------------------------------*
*&      Form  CHECK_UOM_WHEN_QUANTITY
*&---------------------------------------------------------------------*
*       ...
*----------------------------------------------------------------------*
FORM check_uom_when_quantity
     USING r_parameter LIKE accbapifd5-parameter
           r_tabix     LIKE accbapifd5-tabix.

  PERFORM check_uom USING r_parameter
                          r_tabix:
    'PO_PR_UOM          '   gs_accit-bpmng   gs_accit-bprme,
    'BASE_UOM           '   gs_accit-menge   gs_accit-meins,
    'BASE_UOM           '   gs_accit-fklmg   gs_accit-meins,
    'SALES_UNIT         '   gs_accit-fkimg   gs_accit-vrkme,
    'UNIT_OF_WT         '   gs_accit-ntgew   gs_accit-gewei,
    'UNIT_OF_WT         '   gs_accit-brgew   gs_accit-gewei,
    'VOLUMEUNIT         '   gs_accit-volum   gs_accit-voleh,
    'ENTRY_UOM          '   gs_accit-erfmg   gs_accit-erfme.

ENDFORM.                    " CHECK_UOM_WHEN_QUANTITY

*&---------------------------------------------------------------------*
*&      Form  CHECK_UOM
*&---------------------------------------------------------------------*
*       Mengeneinheit prüfen
*----------------------------------------------------------------------*
FORM check_uom
     USING r_parameter LIKE accbapifd5-parameter
           r_tabix     LIKE accbapifd5-tabix
           r_field
           r_quan
           r_unit.

*  IF NOT r_quan IS INITIAL.                               "note 1583327
    IF r_unit IS INITIAL AND NOT r_quan IS INITIAL.        "note 1583327
      PERFORM append_msg_to_return
        USING 'E'             "TYPE
              'RW'            "ID
              '629'           "NUMBER
              r_parameter     "MESSAGE_ONE
              r_field         "MESSAGE_TWO
              r_parameter     "PARAMETER
              r_tabix         "ROW
              r_field.        "FIELD
    ELSEIF NOT r_unit IS INITIAL.                          "note 1583327
      IF t006-msehi <> r_unit.
        SELECT SINGLE * FROM  t006
               WHERE  msehi  = r_unit.
        IF NOT sy-subrc IS INITIAL.
          PERFORM append_msg_to_return
                  USING 'E'             "TYPE
                        'BM'            "ID
                        '001'           "NUMBER
                        'T006'          "MESSAGE_ONE
                        r_unit          "MESSAGE_TWO
                        r_parameter     "PARAMETER
                        r_tabix         "ROW
                        r_field.        "FIELD
        ENDIF.
      ENDIF.
    ENDIF.
*  ENDIF.                                                  "note 1583327

ENDFORM.                    " CHECK_UOM

*---------------------------------------------------------------------*
*       FORM fill_acccr                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  r_parameter                                                   *
*  -->  r_tabix                                                       *
*  -->  r_posnr                                                       *
*  -->  r_shkzg                                                       *
*---------------------------------------------------------------------*
FORM fill_acccr
     USING  r_parameter  LIKE      accbapifd5-parameter
            r_tabix      LIKE      accbapifd5-tabix
            r_posnr      LIKE      bapiaccr01-itemno_acc
            r_shkzg      LIKE      accit-shkzg.

  DATA:  ld_tabix LIKE sy-tabix.

  CLEAR gd_waers.

  READ TABLE it_bapi_acccr WITH KEY itemno_acc = r_posnr
       BINARY SEARCH TRANSPORTING NO FIELDS.
  IF sy-subrc IS INITIAL.
    ld_tabix = sy-tabix.
    LOOP AT it_bapi_acccr INTO gs_bapi_acccr FROM ld_tabix.
      IF gs_bapi_acccr-itemno_acc = r_posnr.
        CLEAR gs_acccr.
        MOVE-CORRESPONDING gs_aw TO gs_acccr.

        PERFORM fill_acccr_from_bapi_acccr
                USING r_parameter.

*       Soll-/Haben-Kennzeichen aus Vorzeichen ermitteln
        PERFORM process_acccr
                USING gs_bapi_acccr-tabix
                      r_parameter
                      r_shkzg.

        APPEND gs_acccr TO it_acccr.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
  ELSE.
    PERFORM append_msg_to_return
            USING 'E'             "TYPE
                  'RW'            "ID
                  '612'           "NUMBER
                  r_posnr         "MESSAGE_ONE
                  ' '             "MESSAGE_TWO
                  r_parameter     "PARAMETER
                  r_tabix         "ROW
                  ' '.        "FIELD
  ENDIF.

ENDFORM.                    " FILL_ACCCR

*---------------------------------------------------------------------*
*       FORM fill_acccr_from_bapi_acccr                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  r_parameter                                                   *
*---------------------------------------------------------------------*
FORM fill_acccr_from_bapi_acccr
     USING r_parameter LIKE accbapifd5-parameter.

* ACCBAPIFD6
  gs_acccr-posnr     = gs_bapi_acccr-itemno_acc    .
*                    = gs_bapi_acccr-TABIX         .
  gs_acccr-curtp     = gs_bapi_acccr-curr_type     .
  gs_acccr-waers     = gs_bapi_acccr-currency      .
*                    = gs_bapi_acccr-CURRENCY_ISO  .
*                    = gs_bapi_acccr-AMT_DOCCUR    .
*                    = gs_bapi_acccr-EXCH_RATE     .
*                    = gs_bapi_acccr-AMT_BASE      .
*                    = gs_bapi_acccr-DISC_BASE     .
*                    = gs_bapi_acccr-EXCH_RATE_V   .
*                    = gs_bapi_acccr-DISC_AMT     .

  PERFORM convert_exchange_rate
          USING 'CURRENCYAMOUNT'
                gs_bapi_acccr-tabix
                gs_bapi_acccr-exch_rate
                gs_bapi_acccr-exch_rate_v
                gs_acccr-kursf.

  PERFORM convert_curr_to_internal
          USING 'CURRENCYAMOUNT'
                gs_bapi_acccr-tabix
                gs_acccr-waers
                gs_bapi_acccr-currency_iso:
    gs_bapi_acccr-amt_doccur    gs_acccr-wrbtr,
    gs_bapi_acccr-amt_base      gs_acccr-fwbas,
    gs_bapi_acccr-disc_base     gs_acccr-skfbt,
    gs_bapi_acccr-disc_amt      gs_acccr-wskto,
    gs_bapi_acccr-tax_amt       gs_acccr-wmwst.

  IF r_parameter = 'ACCOUNTTAX'.
*   check tax base in parameter accounttax
    CHECK NOT gs_bapi_acccr-amt_doccur IS INITIAL.
    PERFORM check_if_initial
            USING 'CURRENCYAMOUNT'
                  gs_bapi_acccr-tabix
                  'AMT_BASE'
                  gs_bapi_acccr-amt_base.
  ENDIF.


ENDFORM.                    " FILL_ACCCR_FROM_BAPI_ACCCR

*&---------------------------------------------------------------------*
*&      Form  PROCESS_ACCCR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_BAPI_ACCCR_TABIX  text
*      -->P_R_PARAMETER  text
*      -->P_R_SHKZG  text
*----------------------------------------------------------------------*
FORM process_acccr
     USING r_tabix     LIKE accbapifd5-tabix
           r_parameter LIKE accbapifd5-parameter
           r_shkzg     LIKE accit-shkzg.

  DATA: ld_value LIKE acccr-wrbtr.

  IF gs_acccr-curtp IS INITIAL.
    gs_acccr-curtp = '00'.
  ENDIF.

  IF gs_acccr-wrbtr = 0    AND
     r_parameter    = 'ACCOUNTTAX'.
*   tax item without AMT_DOCCUR => use AMT_BASE
    ld_value = gs_acccr-fwbas.
  ELSE.
*   use AMT_DOCCUR to determine SHKZG
    ld_value = gs_acccr-wrbtr.
  ENDIF.

  IF r_shkzg IS INITIAL AND NOT ld_value IS INITIAL.
*   Soll/Habenkennzeichen setzen
    IF ld_value > 0.
      r_shkzg = 'S'.
    ELSE.
      r_shkzg = 'H'.
    ENDIF.
  ELSE.
*   Vorzeichen kontrollieren
    IF ( r_shkzg = 'S' AND ld_value < 0 ) OR
       ( r_shkzg = 'H' AND ld_value > 0 ).
      CLEAR gs_return.
      gs_return-type       = 'E'.
      gs_return-id         = 'RW'.
      gs_return-number     = '610'.
      gs_return-message_v1 = gs_acccr-posnr.
      gs_return-message_v2 = gs_acccr-curtp.
      gs_return-parameter  = 'CURRENCYAMOUNT'.
      gs_return-row        = r_tabix.
      gs_return-field      = 'AMT_DOCCUR'.
      APPEND gs_return TO it_return.                     "note 1034551
    ENDIF.
  ENDIF.

  IF gd_waers IS INITIAL.
    gd_waers = gs_acccr-waers.
  ENDIF.

ENDFORM.                    " PROCESS_ACCCR

*&---------------------------------------------------------------------*
*&      Form  FILL_ACCOUNTTYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_accounttype.

  DATA l_koart TYPE koart.

  CASE gs_bapi_accit-parameter.
    WHEN 'ACCOUNTRECEIVABLE'.
      IF gs_accit-kunnr IS INITIAL AND
         NOT gs_accit-hkont IS INITIAL.
*       Buchung auf Verrechnungskonto statt Debitor
        gs_accit-koart = 'S'.
        gs_accit-taxit = ' '.
      ELSE.
        gs_accit-koart = 'D'.
        IF gs_bapi_accit-actv_account = '2'.
          PERFORM koart_v_active(saplfaci) IF FOUND
                  USING    gs_acchd-awtyp
                  CHANGING l_koart.
          IF NOT l_koart IS INITIAL.
*           Setzen Kontoart 'V' für FI-CA
            gs_accit-koart = l_koart.
          ENDIF.
        ENDIF.
        gs_accit-taxit = ' '.
        gs_accit-xfilkd = 'X'.
        PERFORM check_if_initial
                USING gs_bapi_accit-parameter
                      gs_bapi_accit-tabix:
                'CUSTOMER  ' gs_accit-kunnr.
        IF gs_accit-shkzg = 'H' AND                        "note 1596139
           gs_acchd-awtyp <> 'LOANS'.                      "note 1596139
*         Zahlungsbedingungen Gutschrift
          gs_accit-rebzg = 'V'.
        ENDIF.
      ENDIF.

    WHEN 'ACCOUNTPAYABLE'.
      IF gs_accit-lifnr IS INITIAL AND
         NOT gs_accit-hkont IS INITIAL.
*       Buchung auf Verrechnungskonto statt Kreditor
        gs_accit-koart = 'S'.
        gs_accit-taxit = ' '.
      ELSE.
        gs_accit-koart = 'K'.
        gs_accit-taxit = ' '.
        gs_accit-xfilkd = 'X'.
        IF gs_acchd-awtyp <> 'BEBD' AND
           gs_acchd-awtyp <> 'BERD' AND
           gs_acchd-awtyp <> 'BERE' AND
           gs_acchd-awtyp <> 'ENTD'.
          PERFORM check_if_initial
                  USING gs_bapi_accit-parameter
                        gs_bapi_accit-tabix:
                  'VENDOR_NO ' gs_accit-lifnr.
        ENDIF.
        IF gs_accit-shkzg = 'S' AND                        "note 1596139
           gs_acchd-awtyp <> 'LOANS'.                      "note 1596139
*         Zahlungsbedingungen Gutschrift
          gs_accit-rebzg = 'V'.
        ENDIF.
      ENDIF.

    WHEN 'ACCOUNTGL'.
      IF gs_accit-koart IS INITIAL.
*       initial when LREC, HRxx
        gs_accit-koart = 'S'.
      ENDIF.
      gs_accit-taxit = ' '.
      IF gs_accit-kstat IS INITIAL AND
         gs_accit-koart = 'S'.                "Note 1503626
        PERFORM check_if_initial
                USING gs_bapi_accit-parameter
                      gs_bapi_accit-tabix:
                'GL_ACCOUNT' gs_accit-hkont.
      ENDIF.

    WHEN 'ACCOUNTTAX'.
      PERFORM process_accit_tx
              USING gs_bapi_accit-tabix.

    WHEN OTHERS.
      RAISE wrong_linetyp.
  ENDCASE.


ENDFORM.                    " FILL_ACCOUNTTYPE

*&---------------------------------------------------------------------*
*&      Form  CHECK_AND_FILL_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ACCOUNT_RECEIVABLE  text
*      -->P_ACCOUNT_PAYABLE  text
*      -->P_DOCUMENT_HEADER  text
*      -->P_CONTRACT_HEADER  text
*----------------------------------------------------------------------*
FORM check_and_fill_header
     TABLES  account_receivable STRUCTURE bapiacar09
             account_payable    STRUCTURE bapiacap09
     USING   document_header    TYPE      bapiache09
             contract_header    TYPE      bapiaccahd
             p_currency.

* Headerstruktur füllen und Mußfelder im Header prüfen
  PERFORM init_checks USING document_header.

* AWTYP: keine R/3-Core Typen verwenden
  PERFORM check_awtyp
          USING gs_aw-awtyp
                'DOCUMENTHEADER'
                1.

* unterstützten Geschäftsvorfall prüfen
  PERFORM check_glvor
          TABLES account_receivable
                 account_payable
          USING  document_header-bus_act
                 p_currency.

* check date fields valid on header level
  PERFORM check_date_valid USING 'DOCUMENTHEADER' 1 :
          'DOC_DATE'         document_header-doc_date,
          'PSTNG_DATE'       document_header-pstng_date,
          'TRANS_DATE'       document_header-trans_date,
          'VATDATE'          document_header-vatdate,
          'INVOICE_REC_DATE' document_header-invoice_rec_date.

* Kopf
  PERFORM fill_acchd USING contract_header.


ENDFORM.                    " CHECK_AND_FILL_HEADER

*---------------------------------------------------------------------*
*       FORM document_post                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  r_compo                                                       *
*---------------------------------------------------------------------*
FORM document_post
     USING r_compo LIKE bapiache09-compo_acc.

* looking for error messages
  LOOP AT it_return INTO gs_return
          WHERE type = 'A'
          OR    type = 'E'.
    EXIT.
  ENDLOOP.

  CHECK NOT sy-subrc IS INITIAL.
  CHECK NOT r_compo = 'R3RB'.

  CALL FUNCTION 'AC_DOCUMENT_POST'
    EXPORTING
      i_awtyp       = gs_aw-awtyp
      i_awref       = gs_aw-awref
      i_aworg       = gs_aw-aworg
      i_awsys       = gs_aw-awsys
      i_comp        = r_compo
    EXCEPTIONS
      error_message = 1
      OTHERS        = 2.

  IF sy-subrc IS INITIAL.
    IF gs_aw-awkey IS INITIAL.
      gs_aw-awkey+00(10) = gs_aw-awref.                    "note 1584301
      gs_aw-awkey+10(10) = gs_aw-aworg.                    "note 1584301
    ENDIF.
    CLEAR gs_return.
    gs_return-type       = 'S'.
    gs_return-id         = 'RW'.
    gs_return-number     = '605'.
    gs_return-message_v1 = gs_aw-awtyp.
    gs_return-message_v2 = gs_aw-awkey.
    gs_return-message_v3 = gs_aw-awsys.
    INSERT gs_return INTO it_return INDEX 1.
  ELSE.
    PERFORM error_from_system USING space 0 space.
  ENDIF.

ENDFORM.                    "document_post
*&---------------------------------------------------------------------*
*&      Form  fill_it_acctx_nvv_nav
*&---------------------------------------------------------------------*
*   Alle statistisch gekennzeichneten Steuerzeilen werden in Parameter
*   ACCTX (ACCBSET) gefüllt.
*   NAV: Kontierung auf nichtabzugsfähige Steuerzeilen
*   (ACCOUNTGL-Zeile erhält Flag einer Steuerzeile)
*----------------------------------------------------------------------*
FORM fill_it_acctx_nvv_nav .

  DATA: ld_t007b LIKE t007b.

  DATA: BEGIN OF lt_key OCCURS 0,
          hkont LIKE accit-hkont,
          mwskz LIKE accit-mwskz,
          txjcd LIKE accit-txjcd,
          ktosl LIKE accit-ktosl,
        END   OF lt_key.

* move all tax items with amounts in table IT_ACCTX
  LOOP AT it_accit INTO gs_accit WHERE taxit = 'X'.

*   check NAV or NVV ?
    CALL FUNCTION 'FI_TAX_GET_TAX_PROCESSINGS'
      EXPORTING
        i_ktosl         = gs_accit-ktosl
      IMPORTING
        e_t007b         = ld_t007b
      EXCEPTIONS
        ktosl_not_found = 1
        OTHERS          = 2.

    IF NOT sy-subrc IS INITIAL.
      PERFORM error_from_system
              USING 'ACCOUNTTAX'
                    0
                    space.
      CONTINUE.
    ELSE.
      IF ld_t007b-stazf = 'X' AND
         ld_t007b-stbkz = '2'.
*       NAV remember to change GL-accounts to tax-accounts
        MOVE-CORRESPONDING gs_accit TO lt_key.
        COLLECT lt_key.
      ELSEIF ld_t007b-stazf = 'X' AND
             ld_t007b-stbkz = '3'.
*     NVV
      ELSE.
*     ERROR: stat. item but not NAV or NVV
      ENDIF.
    ENDIF.

*   fill tax items in ACCTX
    CLEAR gs_acctx.                                        "Note 1290298
    MOVE-CORRESPONDING gs_accit TO gs_acctx.
    gs_acctx-stbkz = ld_t007b-stbkz.                       "Note 1290298
    LOOP AT it_acccr INTO gs_acccr
                     WHERE awtyp = gs_accit-awtyp
                     AND   awref = gs_accit-awref
                     AND   aworg = gs_accit-aworg
                     AND   posnr = gs_accit-posnr.
      CASE gs_acccr-curtp.
        WHEN '00'.
          IF gs_acctx-shkzg = 'H'.
            gs_acctx-fwste = - gs_acccr-wrbtr.
            gs_acctx-fwbas = - gs_acccr-fwbas.
          ELSE.
            gs_acctx-fwste = gs_acccr-wrbtr.
            gs_acctx-fwbas = gs_acccr-fwbas.
          ENDIF.
        WHEN '10'.
          IF gs_acctx-shkzg = 'H'.
            gs_acctx-hwste = - gs_acccr-wrbtr.
            gs_acctx-hwbas = - gs_acccr-fwbas.
          ELSE.
            gs_acctx-hwste = gs_acccr-wrbtr.
            gs_acctx-hwbas = gs_acccr-fwbas.
          ENDIF.
        WHEN OTHERS.
* fill second and third local currency amounts if supplied
* new with note 1362607
* get FI currency vector for this company code first
          IF gv_x001-bukrs <> gs_accit-bukrs.
            CALL FUNCTION 'FI_CURRENCY_INFORMATION'
              EXPORTING
                i_bukrs = gs_accit-bukrs
              IMPORTING
                e_x001  = gv_x001
              EXCEPTIONS
                OTHERS  = 0.
          ENDIF.
* second local currency amounts supplied ?
          IF gv_x001-bukrs = gs_accit-bukrs  AND
             gv_x001-curt2 = gs_acccr-curtp  AND
             gv_x001-hwae2 = gs_acccr-waers.

            IF gs_acctx-shkzg = 'H'.
              gs_acctx-h2ste = - gs_acccr-wrbtr.
              gs_acctx-h2bas = - gs_acccr-fwbas.
            ELSE.
              gs_acctx-h2ste = gs_acccr-wrbtr.
              gs_acctx-h2bas = gs_acccr-fwbas.
            ENDIF.
          ENDIF.
* third local currency amounts supplied ?
          IF gv_x001-bukrs = gs_accit-bukrs  AND
             gv_x001-curt3 = gs_acccr-curtp  AND
             gv_x001-hwae3 = gs_acccr-waers.

            IF gs_acctx-shkzg = 'H'.
              gs_acctx-h3ste = - gs_acccr-wrbtr.
              gs_acctx-h3bas = - gs_acccr-fwbas.
            ELSE.
              gs_acctx-h3ste = gs_acccr-wrbtr.
              gs_acctx-h3bas = gs_acccr-fwbas.
            ENDIF.
          ENDIF.
      ENDCASE.
      IF NOT gs_accit-kstat IS INITIAL.
*       delete statistic NAV/NVV items in RW document
        DELETE TABLE it_acccr FROM gs_acccr.
      ENDIF.
    ENDLOOP.

    IF NOT gs_accit-kstat IS INITIAL.
*     delete statistic NAV/NVV items in RW document
      DELETE TABLE it_accit FROM gs_accit.

*     PERFORM CHECK_IF_INITIAL
*             USING 'CURRENCYAMOUNT' 0:
*                   'AMT_DOCCUR ' IT_ACCTX-FWSTE,
*                   'AMT_BASE   ' IT_ACCTX-FWBAS.

    ENDIF.
* Do not transfer 0 tax items in ACCTX since corrersponding ACCITs
* are deleted in the accounting interface anyway.
    CHECK NOT gs_acctx-shkzg IS INITIAL.
    APPEND gs_acctx TO it_acctx.
  ENDLOOP.

* change TAXIT in gl-account of NAV => tax-account
  LOOP AT lt_key.
    gs_accit-taxit = 'X'.
    MODIFY it_accit FROM gs_accit
                    TRANSPORTING taxit
                    WHERE hkont = lt_key-hkont
                    AND   mwskz = lt_key-mwskz
                    AND   txjcd = lt_key-txjcd
                    AND   ktosl = lt_key-ktosl
                    AND   koart = 'S'.
  ENDLOOP.

ENDFORM.                    " fill_it_acctx_nvv_nav

*&---------------------------------------------------------------------*
*&      Form  check_balance_intercompany
*&---------------------------------------------------------------------*
*       Prüft auf Saldo = 0 bei buchungskreisübergreifenden Buchungen
*       New with note 420951
*----------------------------------------------------------------------*
FORM check_balance_intercompany.

  DATA: l_bukrs TYPE accit-bukrs,
        l_intercompany,
        BEGIN OF it_balance OCCURS 5,
          awtyp LIKE accit-awtyp,
          awref LIKE accit-awref,
          aworg LIKE accit-aworg,
          curtp LIKE acccr-curtp,
          wrbtr LIKE acccr-wrbtr,
        END OF it_balance.

  FIELD-SYMBOLS: <accit> LIKE accit.

  LOOP AT it_accit ASSIGNING <accit>
          WHERE kstat IS INITIAL AND bstat NE 'S'.
    IF l_bukrs IS INITIAL.
      l_bukrs = <accit>-bukrs.
    ENDIF.
    IF l_bukrs NE <accit>-bukrs.
      l_intercompany = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK l_intercompany = 'X'.

  LOOP AT it_accit ASSIGNING <accit>
          WHERE kstat IS INITIAL AND bstat NE 'S'.
    CLEAR it_balance.
    MOVE-CORRESPONDING <accit> TO it_balance.
    CLEAR gs_acccr.
    READ TABLE it_acccr INTO gs_acccr
               WITH KEY posnr = <accit>-posnr
                        curtp = '00'.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING gs_acccr TO it_balance.
      COLLECT it_balance .
    ENDIF.
  ENDLOOP.

  LOOP AT it_balance WHERE wrbtr NE 0.
    PERFORM append_msg_to_return
            USING 'E'              "TYPE
                  'RW'             "ID
                  '022'            "NUMBER
                  it_balance-wrbtr "MESSAGE_ONE
                  ' '              "MESSAGE_TWO
                  'CURRENCYAMOUNT' "PARAMETER
                  sy-tabix         "ROW
                  'AMT_DOCCUR'.    "FIELD
  ENDLOOP.

ENDFORM.                    " check_balance_intercompany

*&---------------------------------------------------------------------*
*&      Form  check_zero_items
*&---------------------------------------------------------------------*
*&      Checks, wheather the document contains accounting relevant data
*&---------------------------------------------------------------------*
*&      new with note 495175
*&---------------------------------------------------------------------*
FORM check_zero_items.

  DATA: begin_tabix  LIKE sy-tabix,
        end_tabix    LIKE sy-tabix,
        acccr_lines  LIKE sy-tabix,
        nonzero_items.

  SORT it_acchd BY awtyp awref aworg.
  SORT it_accit BY awtyp awref aworg posnr.
  SORT it_acccr BY awtyp awref aworg posnr curtp.

  IF NOT it_acccr IS INITIAL.                              "note 573772
    DESCRIBE TABLE it_acccr LINES acccr_lines.

* For all non-statistic (i.e. FI relevant) items
    LOOP AT it_accit INTO gs_accit WHERE kstat IS INITIAL.
*   Find out if there are dependent amounts with amount or tax base
*   amount <> 0
      READ TABLE it_acccr TRANSPORTING NO FIELDS
                         WITH KEY awtyp = gs_accit-awtyp
                                  awref = gs_accit-awref
                                  aworg = gs_accit-aworg
                                  posnr = gs_accit-posnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.                              "note 573772
*       begin_tabix setzen
        begin_tabix = sy-tabix.
        CLEAR nonzero_items.
        LOOP AT it_acccr INTO gs_acccr FROM begin_tabix.
          IF gs_acccr-awtyp NE gs_accit-awtyp OR
             gs_acccr-awref NE gs_accit-awref OR
             gs_acccr-aworg NE gs_accit-aworg OR
             gs_acccr-posnr NE gs_accit-posnr.
*           end_tabix setzen
            end_tabix = sy-tabix - 1.
            EXIT.
          ENDIF.
          IF sy-tabix = acccr_lines.
            end_tabix = sy-tabix.
          ENDIF.
          CHECK gs_acccr-wrbtr <> 0 OR gs_acccr-fwbas <> 0.
          nonzero_items = 'X'.
        ENDLOOP.

*       If there are no amounts <> 0
        CHECK nonzero_items IS INITIAL.
*       Delete all dependent amounts
        DELETE it_acccr FROM begin_tabix TO end_tabix.
        DESCRIBE TABLE it_acccr LINES acccr_lines.
      ENDIF.                                               "note 573772
*     Delete item
      DELETE it_accit.
    ENDLOOP.
  ELSE.                                                    "note 573772
    CLEAR gs_return.                                       "note 573772
    gs_return-type       = 'E'.                            "note 573772
    gs_return-id         = 'RW'.                           "note 573772
    gs_return-number     = '003'.                          "note 573772
    APPEND gs_return TO it_return.                     "note 1034551
    CLEAR it_accit.                                        "note 573772
  ENDIF.                                                   "note 573772

  IF it_accit IS INITIAL.
    CLEAR gs_return.
    gs_return-type       = 'E'.
    gs_return-id         = 'RW'.
    gs_return-number     = '002'.
*   gs_RETURN-MESSAGE_V1 = R_SEGFLD.
*   gs_RETURN-MESSAGE_V2 = R_PARAMETER.
*   gs_RETURN-PARAMETER  = R_PARAMETER.
*   gs_RETURN-ROW        = R_TABIX.
*   gs_RETURN-FIELD      = R_SEGFLD.
    APPEND gs_return TO it_return.                     "note 1034551
  ENDIF.

ENDFORM.                    " check_zero_items

*&---------------------------------------------------------------------*
*&      Form  reference_create_prelim
*&---------------------------------------------------------------------*
*       OBJ_TYPE: BKPFF
*       OBJ_KEY:  $
*                 Preliminary document reference
*       OBJ_SYS:  local logical system
*&---------------------------------------------------------------------*
FORM reference_create_prelim
     CHANGING e_awtyp TYPE awtyp
              e_awkey TYPE awkey
              e_awsys TYPE awsys.

  DATA: log_sys TYPE logsys.

  e_awtyp = 'BKPFF'.
  e_awkey = '$'.
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = log_sys
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 2.
  IF sy-subrc IS INITIAL.
    e_awsys = log_sys.
  ELSE.
    CLEAR e_awsys.
  ENDIF.

ENDFORM.                    "reference_create_prelim

*&---------------------------------------------------------------------*
*&      Form  reference_create
*&---------------------------------------------------------------------*
FORM reference_create.

  DATA: lt_accdn LIKE accdn OCCURS 1 WITH HEADER LINE,
        lt_bkpf  LIKE bkpf  OCCURS 1 WITH HEADER LINE,
        lt_dummy LIKE bseg  OCCURS 1 WITH HEADER LINE.


  LOOP AT it_return INTO gs_return
                    WHERE type = 'A'
                    OR    type = 'E'.
    EXIT.
  ENDLOOP.
  CHECK NOT sy-subrc IS INITIAL.

  CHECK gs_aw-awtyp = 'BKPFF'.

* check if at least one document was created
* in FI in the leading company
  lt_accdn-awtyp = gs_aw-awtyp.
*   AWREF,AWORG are still initial in XBKPF in case of
*   internal number assignment
* lt_accdn-awref = gs_aw-awref.
* lt_accdn-aworg = gs_aw-aworg.
  lt_accdn-awsys = gs_aw-awsys.
  APPEND lt_accdn.

  CALL FUNCTION 'FI_DOCUMENT_RETURN'
    TABLES
      t_accdn = lt_accdn
      l_bkpf  = lt_bkpf
      l_bseg  = lt_dummy.          "dummy only

* determine leading company just like in FI_DOCUMENT_POST
  SORT lt_bkpf STABLE BY ausbk.
  READ TABLE  lt_bkpf INDEX 1.

  IF  sy-subrc <> 0.                   "no FI document created at all
    CLEAR gs_return.
    gs_return-type       = 'E'.
    gs_return-id         = 'RW'.
    gs_return-number     = '015'.
    INSERT gs_return INTO it_return INDEX 1.
    EXIT.
    IF 1 = 2. "Cross-reference
      MESSAGE e015(rw).
    ENDIF.
  ENDIF.

* set global parameters from document in leading company (lt_bkpf[1])
  gd_bukrs = lt_bkpf-bukrs.
  gd_blart = lt_bkpf-blart.
  gd_budat = lt_bkpf-budat.
  gd_ldgrp = lt_bkpf-ldgrp.
  gd_belnr = lt_bkpf-belnr.
  gd_gjahr = lt_bkpf-gjahr.

  PERFORM fill_obj_key.

ENDFORM.                    " reference_create

*&---------------------------------------------------------------------*
*&      Form  fill_obj_key
*&---------------------------------------------------------------------*
*       OBJ_TYPE: BKPFF
*       OBJ_KEY:  FI document number, CompCode, Year
*       OBJ_SYS:  local logical system
*&---------------------------------------------------------------------*
FORM fill_obj_key.

  DATA: ld_numkr LIKE t003-numkr,
        ls_nriv  TYPE nriv.

  IF NOT gd_ldgrp IS INITIAL.
    DATA: l_rldnr TYPE rldnr,
          ls_t003 LIKE t003.

    CALL FUNCTION 'FAGL_GET_REPRESENTATIVE_LEDGER'
      EXPORTING
        i_ldgrp       = gd_ldgrp
        i_bukrs       = gd_bukrs
      IMPORTING
        e_rldnr       = l_rldnr
      EXCEPTIONS
        error_message = 0
        OTHERS        = 0.


    CALL FUNCTION 'FAGL_GET_NUMBER_RANGE_FOR_PN'
      EXPORTING
        i_rldnr       = l_rldnr
        i_blart       = gd_blart
      IMPORTING
        es_t003       = ls_t003
      EXCEPTIONS
        error_message = 0
        OTHERS        = 0.

    ld_numkr = ls_t003-numkr.
  ENDIF.

  IF ld_numkr IS INITIAL.
    CALL FUNCTION 'FI_DOCUMENT_TYPE_DATA'
      EXPORTING
        i_blart = gd_blart
      IMPORTING
        e_numkr = ld_numkr.
  ENDIF.

  CALL FUNCTION 'NUMBER_GET_INFO'
    EXPORTING
      nr_range_nr        = ld_numkr
      object             = 'RF_BELEG'
      subobject          = gd_bukrs
      toyear             = gd_gjahr
    IMPORTING
      interval           = ls_nriv
    EXCEPTIONS
      interval_not_found = 1
      object_not_found   = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    PERFORM error_from_system
            USING 'DOCUMENTHEADER'
                  0
                  'OBJ_KEY'.
    EXIT.
  ENDIF.

  IF ls_nriv-externind = space.
*   New document number for internal number assignment
    IF gd_belnr(1) <>  '$' AND NOT gd_belnr IS INITIAL.     "note 1588761
      PERFORM append_msg_to_return
        USING 'E' 'F5' '224' SPACE SPACE 'DOCUMENTHEADER' 1 'AC_DOC_NO'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'RF_GET_DOCUMENT_NUMBER'
      EXPORTING
        company          = gd_bukrs
        range            = ld_numkr
        year             = gd_gjahr
      IMPORTING
        document_number  = gd_belnr
      EXCEPTIONS
        duplicate_number = 1
        range_missing    = 2
        error_in_open_fi = 3
          error_message    = 4                             "note 1698282
          OTHERS           = 5.                            "note 1698282
    IF sy-subrc <> 0.
      PERFORM error_from_system
              USING 'DOCUMENTHEADER'
                    0
                    'OBJ_KEY'.
      EXIT.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'FI_REFERENCE_CREATE'
    EXPORTING
      i_awtyp           = gs_aw-awtyp
      i_bukrs           = gd_bukrs
      i_gjahr           = gd_gjahr
      i_belnr           = gd_belnr
    IMPORTING
      e_awtyp           = gs_aw-awtyp
      e_awref           = gs_aw-awref
      e_aworg           = gs_aw-aworg
      e_awsys           = gs_aw-awsys
    EXCEPTIONS
      object_type       = 1
      missing_parameter = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    PERFORM error_from_system
            USING 'DOCUMENTHEADER'
                  0
                  'OBJ_KEY'.
    EXIT.
  ELSE.
    gs_aw-awkey(10)    = gs_aw-awref.
    gs_aw-awkey+10(10) = gs_aw-aworg.
  ENDIF.
ENDFORM.                               " fill_obj_key
*&---------------------------------------------------------------------*
*&      Form  copy_bupla_korea
*&---------------------------------------------------------------------*
*       New with note 695878
*----------------------------------------------------------------------*
FORM copy_bupla_korea .

  DATA ls_t001 TYPE t001.

* Choose first AR line item containing businessplace
  LOOP AT it_accit INTO gs_accit WHERE koart = 'D' AND
                                   NOT bupla IS INITIAL.
    EXIT.
  ENDLOOP.
  CHECK sy-subrc IS INITIAL.

* Check AWTYP
  CHECK gs_accit-awtyp = 'BEBD' OR
        gs_accit-awtyp = 'BERD' OR
        gs_accit-awtyp = 'BERE' OR
        gs_accit-awtyp = 'ENTD'.

* Check Country
  CALL FUNCTION 'FI_COMPANY_CODE_DATA'
    EXPORTING
      i_bukrs      = gs_accit-bukrs
    IMPORTING
      e_t001       = ls_t001
    EXCEPTIONS
      system_error = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
  ENDIF.
  CHECK ls_t001-land1 = 'KR'.

* Check Amount of AR line item <> 0
  READ TABLE it_acccr INTO gs_acccr
             WITH KEY awtyp = gs_accit-awtyp
                      awref = gs_accit-awref
                      aworg = gs_accit-aworg
                      posnr = gs_accit-posnr.
  CHECK sy-subrc IS INITIAL.
  CHECK gs_acccr-wrbtr = 0.

* Copy Businessplace to other line items
  MODIFY it_accit FROM gs_accit TRANSPORTING bupla
         WHERE bupla IS INITIAL.

ENDFORM.                    " copy_bupla_korea
*&---------------------------------------------------------------------*
*&      Form  rmvct_check
*&---------------------------------------------------------------------*
*       New with note 923093
*----------------------------------------------------------------------*
FORM rmvct_check USING p_accit TYPE accbapifd5.

  DATA: ls_t856 TYPE t856.                                "note 1982803

  CHECK p_accit-parameter = 'ACCOUNTGL'.
  CHECK NOT p_accit-cs_trans_t IS INITIAL.

  SELECT SINGLE trtyp FROM t856 INTO ls_t856              "note 1982803
      WHERE trtyp = p_accit-cs_trans_t.                   "note 1982803

  IF sy-subrc <> 0.                                       "note 1982803
    PERFORM append_msg_to_return
      USING 'E'
            'GZ'
            '262'
            p_accit-cs_trans_t
            ''
            p_accit-parameter
            p_accit-tabix
            'CS_TRANS_T'.
  ENDIF.

ENDFORM.                    "rmvct_check
*&---------------------------------------------------------------------*
*&      Form  mm_cobl_check
*&---------------------------------------------------------------------*
*       New with note 736111
*----------------------------------------------------------------------*
FORM mm_cobl_check USING p_accit TYPE accbapifd5.

  DATA: l_cobl LIKE cobl
  .
* only required for G/L items
  CHECK p_accit-parameter = 'ACCOUNTGL'.
* fill only required COBL fields
* set dummy VORGN to enforce COBL check
  l_cobl-vorgn = 'RFBU'.
  l_cobl-werks = p_accit-plant.
  l_cobl-matnr = p_accit-material.
  l_cobl-meins = p_accit-base_uom.
  l_cobl-menge = p_accit-quantity.
* call mm_cobl_check
  CALL FUNCTION 'MM_COBL_CHECK'
    EXPORTING
      i_cobl        = l_cobl
    EXCEPTIONS
      error_message = 1
      OTHERS        = 1.

  IF sy-subrc <> 0.
* write protocol entry in case of any error
    PERFORM error_from_system
       USING
            'ACCOUNTGL'
            p_accit-tabix
           'PLANT|MATERIAL|QUANTITY|UNIT'.
  ENDIF.
ENDFORM.                    " mm_cobl_check
*&---------------------------------------------------------------------*
*&      Form  enrich_cash_discount
*&---------------------------------------------------------------------*
*       Setzen von ACCIT-BUZID und ACCCR-KZBTR, new with note 972734
*----------------------------------------------------------------------*
FORM enrich_cash_discount .

  DATA: ls_acccr TYPE acccr,
  "Währungszeile zum Kreditor in Hausw.
        ld_tabix TYPE sy-tabix.
  FIELD-SYMBOLS: <fs_accit> TYPE accit,
                 <fs_acccr> TYPE acccr.

  CHECK gs_acchd-glvor = 'RMRP' OR                        "note1085328
        gs_acchd-awtyp = 'BKPFF'.                         "note1085328

* Skontoverrechnungszeile
  LOOP AT it_accit  ASSIGNING <fs_accit>
                    WHERE ktosl = 'SKV'.
    <fs_accit>-buzid = 'Z'.                      "für korrektes Ausziffern
  ENDLOOP.
  CHECK sy-subrc = 0.

* Urprünglicher Kürzungsbetrag in Hauswährung
  READ TABLE it_accit WITH KEY koart = 'K'
                      ASSIGNING <fs_accit>.
  READ TABLE it_acccr WITH KEY posnr = <fs_accit>-posnr
                               curtp = '10'
                      ASSIGNING <fs_acccr>.

  IF sy-subrc NE 0.
*   Fehler im Beleg:
*   Für Skontoermittlung bitte Währungszeile mit
*   Buchungskreiswährung (CURR_TYPE = 10) füllen.
    CLEAR gs_return.
    gs_return-type       = 'E'.
    gs_return-id         = 'RW'.
    gs_return-number     = '609'.
    gs_return-message_v1 = text-s01.
    gs_return-message_v2 = text-s02.
    gs_return-parameter  = 'CURRENCYAMOUNT'.
    gs_return-field      = 'CURR_TYPE'.
    APPEND gs_return TO it_return.
  ELSE.
* get zbd1p from payment terms if necessary
    DATA: l_zbd1p LIKE accit-zbd1p.                        "Note 1246803

    ld_tabix = sy-tabix.
    ls_acccr = <fs_acccr>.
    IF NOT <fs_accit>-zterm IS INITIAL AND                 "Note 1246803
           <fs_accit>-zbd1p IS INITIAL.                    "Note 1246803
      CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'     "Note 1246803
        EXPORTING                                     "Note 1246803
          i_bldat               = <fs_accit>-bldat    "Note 1246803
          i_budat               = <fs_accit>-budat    "Note 1246803
          i_zfbdt               = <fs_accit>-zfbdt    "Note 1246803
          i_zterm               = <fs_accit>-zterm    "Note 1246803
        IMPORTING                                     "Note 1246803
          e_zbd1p               = l_zbd1p.            "Note 1246803
    ELSE.                                                  "Note 1246803
      l_zbd1p = <fs_accit>-zbd1p.                          "Note 1246803
    ENDIF.                                                 "Note 1246803


    READ TABLE it_acccr WITH KEY posnr = <fs_accit>-posnr
                                 curtp = '00'
                        ASSIGNING <fs_acccr>.
    ls_acccr-kzbtr = <fs_acccr>-skfbt * l_zbd1p / 100.     "Note 1246803

    CALL FUNCTION 'ROUND_AMOUNT'                           "Note 1246803
      EXPORTING                                            "Note 1246803
        amount_in           = ls_acccr-kzbtr               "Note 1246803
        company             = <fs_accit>-bukrs             "Note 1246803
        currency            = <fs_acccr>-waers             "Note 1246803
      IMPORTING                                            "Note 1246803
        amount_out          = ls_acccr-kzbtr.              "Note 1246803

    ls_acccr-kzbtr = ls_acccr-kzbtr * ls_acccr-wrbtr / <fs_acccr>-wrbtr.
    MODIFY it_acccr FROM ls_acccr INDEX ld_tabix.
  ENDIF.

ENDFORM.                    " enrich_cash_discount

*&---------------------------------------------------------------------*
*&      Form  CHECK_DOCUMENT_ITEMS_INITIAL
*&---------------------------------------------------------------------*
*       At least one valid line item must be submitted to BAPI.
*       Created with note 1820221
*----------------------------------------------------------------------*
FORM check_document_items_initial
     TABLES   account_receivable STRUCTURE bapiacar09
              account_payable    STRUCTURE bapiacap09
              account_gl         STRUCTURE bapiacgl09
              account_tax        STRUCTURE bapiactx09.

  IF  account_receivable[] IS INITIAL
  AND account_payable[] IS INITIAL
  AND account_gl[] IS INITIAL
  AND account_tax[] IS INITIAL.

    PERFORM append_msg_to_return
            USING 'E'             "TYPE
                  'RW'            "ID
                  '002'           "NUMBER
                  SPACE           "MESSAGE_ONE
                  SPACE           "MESSAGE_TWO
                  SPACE           "PARAMETER
                  SPACE           "ROW
                  SPACE.          "FIELD
  ENDIF.


ENDFORM.                    " CHECK_DOCUMENT_INITIAL

*&---------------------------------------------------------------------*
*&      Form  CHECK_TAX_DET_BY_LINE
*&---------------------------------------------------------------------*
FORM check_tax_det_by_line USING i_bukrs TYPE bukrs.

  DATA: ld_jdactive   LIKE bkpf-xusvr,
        ld_xtxit      LIKE ttxd-xtxit,
        ls_j_1iindcus TYPE j_1iindcus.

  CALL FUNCTION 'CHECK_JURISDICTION_ACTIVE'
      EXPORTING
        i_bukrs          = i_bukrs
      IMPORTING
        e_isactive       = ld_jdactive
        e_xtxit          = ld_xtxit
      EXCEPTIONS
        input_incomplete = 1.
  IF sy-subrc = 0 AND ld_xtxit = 'X'.
    gd_xtxit = 'X'.
  ENDIF.

  CHECK gd_xtxit IS INITIAL.

  CALL FUNCTION 'J_1BSA_COMPONENT_ACTIVE'
    EXPORTING
      bukrs                      = i_bukrs
      component                  = 'IN'
    EXCEPTIONS
      component_not_active       = 1
      OTHERS                     = 2.

  IF sy-subrc = 0.
*  Check CIN is active or not
    SELECT SINGLE * FROM  j_1iindcus
                    INTO  ls_j_1iindcus
                    WHERE j_1ibukrs EQ i_bukrs.
    IF sy-subrc EQ 0.
      gd_xtxit = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_TAX_DET_BY_LINE
