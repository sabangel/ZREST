*----------------------------------------------------------------------*
*   INCLUDE LACC9F10                                                   *
*----------------------------------------------------------------------*


FORM init_globals.

  REFRESH: it_return,
           it_accit,
           it_acccr,
           it_accfi,
           it_acchd,
           it_accit_pa,
           it_acccr_pa,
           it_accwt,
           it_acctx,
           it_bapi_acchd,
           it_bapi_accit,
           it_bapi_acccr.

  CLEAR:   gs_return,
           gs_acchd,
           gs_accit,
           gs_acccr,
           gs_accfi,
           gs_accit_pa,
           gs_acccr_pa,
           gs_accwt,
           gs_acctx,
           gs_bapi_accit,
           gs_bapi_acccr,
           gs_bapi_acchd,
           gs_aw,
           gd_waers,
           gd_xtxit.                                      "Note 1859478

  CALL FUNCTION 'EXTERNAL_TAX_DOC_START_PRICING'            "1597161
                   EXPORTING                                "N1715637
                     i_refresh_complete = 'X'.              "N1715637
  CALL FUNCTION 'REFRESH_TAX_TABLES'.                       "1597161

  PERFORM badi_init.

ENDFORM.                    " INIT_GLOBALS


*---------------------------------------------------------------------*
*       FORM init_checks                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  document_header                                               *
*---------------------------------------------------------------------*
FORM init_checks
     USING document_header TYPE bapiache09.

  DATA: BEGIN OF ld_awkey,
          awref LIKE acchd-awref,
          aworg LIKE acchd-aworg,
        END   OF ld_awkey.

  DATA: ld_logsys LIKE tbdls-logsys.

  DATA l_docno LIKE bapiache09-ac_doc_no.                 "note 536852

  IF NOT document_header-ac_doc_no IS INITIAL.            "note 536852
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'           "note 536852
      EXPORTING                                           "note 536852
        INPUT         = document_header-ac_doc_no         "note 536852
      IMPORTING                                           "note 536852
        OUTPUT        = l_docno.                          "note 536852
    IF document_header-ac_doc_no NE l_docno.              "note 536852
      CLEAR gs_return.                                    "note 536852
      gs_return-type       = 'E'.                         "note 536852
      gs_return-id         = 'RW'.                        "note 536852
      gs_return-number     = '609'.                       "note 536852
      gs_return-message_v1 = gs_bapi_acchd-obj_type.     "note 1982803
      gs_return-message_v2 = document_header-ac_doc_no.   "note 536852
      gs_return-message_v3 = gs_bapi_acchd-obj_sys.      "note 1982803
      gs_return-parameter  = 'DOCUMENTHEADER'.            "note 536852
      gs_return-field      = 'AC_DOC_NO'.                 "note 536852
      APPEND gs_return TO it_return.                     "note 1034551
    ENDIF.                                                "note 536852
  ENDIF.                                                  "note 536852

  MOVE-CORRESPONDING document_header TO gs_bapi_acchd.

  CLEAR gs_aw.

* init GD_ACCIT_KEY
  ld_awkey        = gs_bapi_acchd-obj_key.
  gs_aw-mandt     = sy-mandt.
  gs_aw-awtyp     = gs_bapi_acchd-obj_type.
  gs_aw-awref     = ld_awkey-awref.
  gs_aw-aworg     = ld_awkey-aworg.
  gs_aw-awkey     = gs_bapi_acchd-obj_key.
  gs_aw-awsys     = gs_bapi_acchd-obj_sys.

* Storno / Umkehrbuchung
  ld_awkey        = gs_bapi_acchd-obj_key_r.
  gs_aw-awref_rev = ld_awkey-awref.
  gs_aw-aworg_rev = ld_awkey-aworg.


* OBJ_TYPE, OBJ_KEY und USERNAME
  PERFORM check_if_initial
          USING 'DOCUMENTHEADER'
                1 :
                'OBJ_TYPE  ' gs_aw-awtyp,
                'OBJ_KEY   ' gs_aw-awref,
                'USERNAME  ' gs_bapi_acchd-username.

* OBJ_SYS
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = ld_logsys
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 2.

  IF sy-subrc IS INITIAL.
*   logical system defined => check logical system of sender
    PERFORM check_if_initial
            USING 'DOCUMENTHEADER'
                  1
                  'OBJ_SYS   '
                  gs_bapi_acchd-obj_sys.
  ENDIF.

ENDFORM.                    " INIT_CHECKS


*---------------------------------------------------------------------*
*       FORM CHECK_IF_INITIAL                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  R_PARAMETER                                                   *
*  -->  R_TABIX                                                       *
*  -->  R_SEGFLD                                                      *
*  -->  R_FIELD                                                       *
*---------------------------------------------------------------------*
FORM check_if_initial
  USING  r_parameter LIKE accbapifd5-parameter
         r_tabix     LIKE accbapifd5-tabix
         r_segfld
         r_field.

  CHECK r_field IS INITIAL OR r_field = space.

  PERFORM append_msg_to_return
          USING 'E'             "TYPE
                'RW'            "ID
                '602'           "NUMBER
                r_segfld        "MESSAGE_ONE
                r_parameter     "MESSAGE_TWO
                r_parameter     "PARAMETER
                r_tabix         "ROW
                r_segfld.       "FIELD

ENDFORM.                               " CHECK_IF_INITIAL

*---------------------------------------------------------------------*
*       FORM CHECK_DATE_VALID                                         *
*---------------------------------------------------------------------*
*       introduced with note 1361439                                  *
*---------------------------------------------------------------------*
*  -->  R_PARAMETER                                                   *
*  -->  R_TABIX                                                       *
*  -->  R_SEGFLD                                                      *
*  -->  R_FIELD                                                       *
*---------------------------------------------------------------------*

FORM check_date_valid
  USING  r_parameter LIKE accbapifd5-parameter
         r_tabix     LIKE accbapifd5-tabix
         r_segfld
         r_date      LIKE sy-datum.

  CHECK NOT r_date IS INITIAL AND NOT r_date = SPACE.

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      DATE                            = r_date
    EXCEPTIONS
      PLAUSIBILITY_CHECK_FAILED       = 1
      OTHERS                          = 1.

  CHECK SY-SUBRC <> 0.

  PERFORM append_msg_to_return
          USING 'E'             "TYPE
                'RW'            "ID
                '633'           "NUMBER
                r_date          "MESSAGE_ONE
                SPACE           "MESSAGE_TWO
                r_parameter     "PARAMETER
                r_tabix         "ROW
                r_segfld.       "FIELD

ENDFORM.                               " CHECK_DATE_VALID

*---------------------------------------------------------------------*
*       FORM CHECK_DATE_FIELDS                                        *
*---------------------------------------------------------------------*
*       introduced with note 1361439                                  *
*---------------------------------------------------------------------*
*  -->  R_BAPI_ACCIT                                                  *
*---------------------------------------------------------------------*

FORM check_date_fields
  USING  r_bapi_accit LIKE accbapifd5.
  CASE r_bapi_accit-parameter.
  WHEN 'ACCOUNTGL'.
    PERFORM check_date_valid USING r_bapi_accit-parameter
                                   r_bapi_accit-tabix :
     'PSTNG_DATE'                r_bapi_accit-pstng_date,
     'VALUE_DATE'                r_bapi_accit-value_date,
     'ASVAL_DATE'                r_bapi_accit-asval_date,
     'BILLING_PERIOD_START_DATE' r_bapi_accit-billing_period_start_date,
     'BILLING_PERIOD_END_DATE'   r_bapi_accit-billing_period_end_date.

  WHEN 'ACCOUNTPAYABLE' OR 'ACCOUNTRECEIVABLE'.
    PERFORM check_date_valid USING r_bapi_accit-parameter
                                   r_bapi_accit-tabix :
     'BLINE_DATE'                r_bapi_accit-bline_date,
     'TAX_DATE'                  r_bapi_accit-tax_date.

  WHEN 'ACCOUNTTAX'.
   PERFORM check_date_valid USING  r_bapi_accit-parameter
                                   r_bapi_accit-tabix :
     'TAX_DATE'                  r_bapi_accit-tax_date.

  WHEN 'PAYMENTCARD'.
    PERFORM check_date_valid USING r_bapi_accit-parameter
                                   r_bapi_accit-tabix :
     'AUTH_DATE'                  r_bapi_accit-auth_date,
     'CC_VALID_F'                 r_bapi_accit-cc_valid_f,
     'AUTH_DATE'                  r_bapi_accit-cc_valid_t.


  WHEN 'REALESTATE'.
    PERFORM check_date_valid USING  r_bapi_accit-parameter
                                    r_bapi_accit-tabix :
     'REF_DATE'                   r_bapi_accit-ref_date.

  ENDCASE.
ENDFORM.                               " CHECK_DATE_FIELDS

*---------------------------------------------------------------------*
*       FORM check_awtyp                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  r_awtyp                                                       *
*  -->  r_parameter                                                   *
*  -->  r_tabix                                                       *
*---------------------------------------------------------------------*
FORM check_awtyp
  USING r_awtyp     LIKE acchd-awtyp
        r_parameter LIKE accbapifd5-parameter
        r_tabix     LIKE accbapifd5-tabix.

  IF r_awtyp = 'EBKPF' OR              " Buchhaltung extern
     r_awtyp = 'VBRK ' OR              " Faktura
     r_awtyp = 'WBRK ' OR              " Lieferantenfaktura
     r_awtyp = 'UKPF ' OR              " VK-Wertänd.-Beleg
     r_awtyp = 'MLHD ' OR              " Material-Ledger
     r_awtyp = 'BKPF ' OR              " Buchhaltungsbeleg
*    r_awtyp = 'BKPFF' OR              " Buchhaltungsb. Batch-Input
     r_awtyp = 'PRCHG' OR              " Preisänderung
     r_awtyp = 'UKPF ' OR              " VK-Wertänd.-Beleg
     r_awtyp = 'RMRP ' OR              " Rechnungseingang
     r_awtyp = 'MKPF ' OR              " Materialbeleg
     r_awtyp = 'PORD ' OR              " Bestellung
     r_awtyp = 'PREQ ' OR              " Bestellanforderung
     r_awtyp = 'AFRU ' OR              " rückmeldungsbeleg
     r_awtyp = 'AIBU ' OR              " aib abrechnung
     r_awtyp = 'AMBU ' OR              " anlagenbewegung
     r_awtyp = 'AMDP ' OR              " abschreibungsbuchung
     r_awtyp = 'ANLA ' OR              " anlagenbewegung
     r_awtyp = 'AS91 ' OR              " fiaa-altdatenbeweg.
     r_awtyp = 'AUAK ' OR              " abrechnungsbeleg
     r_awtyp = 'BKPFI' OR              " buchh.    blg f.initial.
     r_awtyp = 'CAJO ' OR              " kassenbuchbeleg
     r_awtyp = 'CATS ' OR              " beleg aus cats
     r_awtyp = 'COBK ' OR              " kostenrechnungsbeleg
     r_awtyp = 'COFIS' OR              " abstimmledger
     r_awtyp = 'COPA ' OR              " ergebnisrechnung
     r_awtyp = 'COPCA' OR              " profit-center-beleg
     r_awtyp = 'ECCS ' OR              " konsolidierung
     r_awtyp = 'FEBEP' OR              " kontoauszugsposition
     r_awtyp = 'FMCA ' OR              " haushaltsmgm.-beleg
     r_awtyp = 'FMCJ ' OR              " ps kassenbuch
     r_awtyp = 'FMCO ' OR              " projektecashm.-beleg
     r_awtyp = 'FMPSO' OR              " anordnungsbeleg
     r_awtyp = 'FMRES' OR              " mittelvormerkung
     r_awtyp = 'GLFLX' OR              " flexibles hauptbuch
     r_awtyp = 'GLX  ' OR              " spezielle ledger
     r_awtyp = 'IBKPF' OR              " beleg aus idoc
     r_awtyp = 'KFPK ' OR              " festpreisverrechnung
*    r_awtyp = 'LOANS' OR              " darlehen               "note997448
     r_awtyp = 'PAYRQ' OR              " zahlungsanordnung
     r_awtyp = 'PYMTD' OR              " zahlungsdaten
     r_awtyp = 'REACC' OR              " immobilienbeleg
     r_awtyp = 'RETAX' OR              " immobilienbeleg vst.
*    r_awtyp = 'TR-TM' OR              " treasury-management    "note997448
     r_awtyp = 'VBAK ' OR              " faktura
     r_awtyp = 'VZBL ' .               " zinsbeleg im ps

    PERFORM append_msg_to_return
            USING 'E'             "TYPE
                  'RW'            "ID
                  '628'           "NUMBER
                  r_awtyp         "MESSAGE_ONE
                  r_parameter     "MESSAGE_TWO
                  r_parameter     "PARAMETER
                  r_tabix         "ROW
                  'OBJ_TYPE'.     "FIELD

  ENDIF.


ENDFORM.                    " CHECK_AWTYP



*---------------------------------------------------------------------*
*       FORM FILL_RETURN_TABLE                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  T_RETURN                                                      *
*  -->  R_AWTYP                                                       *
*  -->  R_AWKEY                                                       *
*  -->  R_AWSYS                                                       *
*---------------------------------------------------------------------*
FORM fill_return_table

  TABLES t_return STRUCTURE bapiret2
  USING  r_awtyp  TYPE      awtyp
         r_awkey  TYPE      awkey
         r_awsys  TYPE      awsys.

  DATA:  ld_row   LIKE bapiret2-row,
         ld_type  LIKE bapiret2-type.

  REFRESH t_return.
  CLEAR   t_return.

* looking for error messages
  LOOP AT it_return INTO gs_return
          WHERE type = 'A'
             OR type = 'E'.
    ld_type = gs_return-type.
    IF ld_type = 'A'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF sy-subrc IS INITIAL.
*   error messages
    CLEAR gs_return.
    gs_return-type       = ld_type.
    gs_return-id         = 'RW'.
    gs_return-number     = '609'.
    gs_return-message_v1 = r_awtyp.
    gs_return-message_v2 = r_awkey.
    gs_return-message_v3 = r_awsys.
    INSERT gs_return INTO it_return INDEX 1.
  ELSE.
*   no error messages
    READ TABLE it_return INTO gs_return INDEX 1.
    IF gs_return-type <> 'S'.
*     no success message => insert success message
      CLEAR gs_return.
      gs_return-type       = 'S'.
      gs_return-id         = 'RW'.
      gs_return-number     = '614'.
      gs_return-message_v1 = r_awtyp.
      gs_return-message_v2 = r_awkey.
      gs_return-message_v3 = r_awsys.
      INSERT gs_return INTO it_return INDEX 1.
    ENDIF.
  ENDIF.

* move messages into return parameter
  LOOP AT it_return INTO gs_return.

    ld_row = gs_return-row.
    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type       = gs_return-type
        cl         = gs_return-id
        number     = gs_return-number
        par1       = gs_return-message_v1
        par2       = gs_return-message_v2
        par3       = gs_return-message_v3
        par4       = gs_return-message_v4
        log_no     = gs_return-log_no
        log_msg_no = gs_return-log_msg_no
        parameter  = gs_return-parameter
        row        = ld_row
        field      = gs_return-field
      IMPORTING
        return     = t_return
      EXCEPTIONS
        OTHERS     = 0.

    APPEND t_return.

  ENDLOOP.

  FREE it_return.

  CALL FUNCTION 'MESSAGES_ACTIVE'                          "note 672821
    EXCEPTIONS                                             "note 672821
      not_active = 1.                                      "note 672821
* Message Handler is still active                          "note 672821
  IF sy-subrc IS INITIAL.                                  "note 672821
*   Import Message Handler tables                          "note 672821
    CALL FUNCTION 'MESSAGE_EXPORT_IMPORT'                  "note 672821
      EXPORTING                                            "note 672821
        EX_OR_IF         = 'IF'                            "note 672821
      EXCEPTIONS                                           "note 672821
        EMPTY            = 0                               "note 672821
        OTHERS           = 1.                              "note 672821
  ENDIF.                                                   "note 672821

ENDFORM.                               " FILL_RETURN_TABLE



*---------------------------------------------------------------------*
*      Form  MESSAGES_START
*---------------------------------------------------------------------*
FORM messages_start USING r_mh_active.

  CALL FUNCTION 'MESSAGES_ACTIVE'
    EXCEPTIONS
      not_active = 1.

  IF sy-subrc NE 0.

    CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXCEPTIONS
        OTHERS = 0.
  ELSE.
*   Aufrufer des BAPI hat Messagehandler aktiv
    r_mh_active = 'X'.
*   Export Message Handler tables
    CALL FUNCTION 'MESSAGE_EXPORT_IMPORT'
      EXPORTING
        EX_OR_IF         = 'EX'
      EXCEPTIONS
        EMPTY            = 0
        OTHERS           = 1.
    CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXPORTING
        RESET                  = 'X'.
  ENDIF.

ENDFORM.                               " MESSAGES_START

*---------------------------------------------------------------------*
*      Form  MESSAGES_STOP
*---------------------------------------------------------------------*
FORM messages_stop
  USING r_parameter LIKE accbapifd5-parameter
        r_tabix     LIKE accbapifd5-tabix
        r_mh_active.
  DATA: lt_mesg     LIKE mesg OCCURS 0 WITH HEADER LINE.

  IF r_mh_active IS INITIAL.
* Messagehandler wurde selbst gestartet
    CALL FUNCTION 'MESSAGES_STOP'
      EXCEPTIONS
        OTHERS = 0.
  ENDIF.

  CALL FUNCTION 'MESSAGES_GIVE'
    TABLES
      t_mesg = lt_mesg
    EXCEPTIONS
      OTHERS = 0.

  LOOP AT lt_mesg.
    gs_return-type       = lt_mesg-msgty.
    gs_return-id         = lt_mesg-arbgb.
    gs_return-number     = lt_mesg-txtnr.
    gs_return-message    = lt_mesg-text.
*   gs_RETURN-LOG_NO     =
*   gs_RETURN-LOG_MSG_NO =
    gs_return-message_v1 = lt_mesg-msgv1.
    gs_return-message_v2 = lt_mesg-msgv2.
    gs_return-message_v3 = lt_mesg-msgv3.
    gs_return-message_v4 = lt_mesg-msgv4.
    gs_return-parameter  = r_parameter.
    gs_return-row        = r_tabix.
*   gs_RETURN-FIELD      =
    APPEND gs_return TO it_return.
  ENDLOOP.

ENDFORM.                               " MESSAGES_STOP


*---------------------------------------------------------------------*
*       FORM CALL_CUSTOMER_FUNCTION                                   *
*---------------------------------------------------------------------*
*       Userexit hier, wegen MOVE-CORRESPONDING ACCIT TO ACCIT_PA     *
*---------------------------------------------------------------------*
*  -->  T_EXTENSION                                                   *
*---------------------------------------------------------------------*
FORM call_customer_function
  TABLES t_extension STRUCTURE bapiacextc.

  DATA: lt_return LIKE STANDARD TABLE OF bapiret2 WITH HEADER LINE.

  CHECK NOT t_extension[] IS INITIAL.

  READ TABLE it_acchd INTO gs_acchd INDEX 1.


  CALL FUNCTION 'OPEN_FI_PERFORM_RWBAPI01_P'
    TABLES
      c_accit     = it_accit
      c_acccr     = it_acccr
      c_return    = lt_return
      c_extension = t_extension
      c_accwt     = it_accwt
    CHANGING
      c_acchd     = gs_acchd.

  MODIFY it_acchd FROM gs_acchd INDEX 1.

  MOVE-CORRESPONDING gs_acchd TO gs_aw.
  gs_aw-awkey+00(10) = gs_acchd-awref.                     "note 1584301
  gs_aw-awkey+10(10) = gs_acchd-aworg.                     "note 1584301

  LOOP AT lt_return.
    MOVE-CORRESPONDING lt_return TO gs_return.
    APPEND gs_return TO it_return.
  ENDLOOP.

ENDFORM.                               " CALL_CUSTOMER_FUNCTION

*---------------------------------------------------------------------*
*       FORM append_msg_to_return                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  p_type                                                        *
*  -->  p_id                                                          *
*  -->  p_number                                                      *
*  -->  p_message_v1                                                  *
*  -->  p_message_v2                                                  *
*  -->  p_parameter                                                   *
*  -->  p_row                                                         *
*  -->  p_field                                                       *
*---------------------------------------------------------------------*
FORM append_msg_to_return USING  p_type       LIKE gs_return-type
                                 p_id         LIKE gs_return-id
                                 p_number     LIKE gs_return-number
                                 p_message_v1
                                 p_message_v2
                                 p_parameter
                                 p_row        "LIKE gs_return-row
                                 p_field.     "LIKE gs_return-field.

  CLEAR gs_return.
  gs_return-type       = p_type.
  gs_return-id         = p_id.
  gs_return-number     = p_number.
  gs_return-message_v1 = p_message_v1.
  gs_return-message_v2 = p_message_v2.
  gs_return-parameter  = p_parameter.
  gs_return-row        = p_row.
  gs_return-field      = p_field.

  APPEND gs_return TO it_return.

ENDFORM.                    " APPEND_MSG_TO_RETURN

*&---------------------------------------------------------------------*
*&      Form  CHECK_GLVOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_glvor
     TABLES acc_rec STRUCTURE bapiacar09
            acc_pay STRUCTURE bapiacap09
     USING  r_glvor LIKE      bapiache09-bus_act
            r_currency.


  IF r_glvor IS INITIAL.
    PERFORM check_if_initial
            USING 'DOCUMENTHEADER'
                  1
                  'BUS_ACT   '
                  r_glvor.
  ELSE.

    CASE r_glvor.

*     Sachkontenbuchung
      WHEN 'RFBU'.

*     Faktura: Debitorzeile Pflicht
      WHEN 'SD00'.
        CHECK gs_bapi_acchd-obj_type NE 'ACE'.             "note 601778
        CHECK gs_bapi_acchd-obj_type NE 'BEBD'.            "note 932864
        CHECK gs_bapi_acchd-obj_type NE 'CRMFM'.           "note1485545

        IF acc_rec[] IS INITIAL AND
           r_currency = 'X'.

          PERFORM append_msg_to_return
                 USING 'E'                 "TYPE
                       'RW'                "ID
                       '611'               "NUMBER
                       'ACCOUNTRECEIVABLE' "MESSAGE_ONE
                       r_glvor             "MESSAGE_TWO
                       'ACCOUNTRECEIVABLE' "PARAMETER
                       ' ' "r_tabix        "ROW
                       'BUS_ACT'.          "FIELD
        ENDIF.

*     Warenbewegungen
      WHEN 'RMWA' OR 'RMWE' OR 'RMWF' OR 'RMWI'
        OR 'RMWL' OR 'RMWQ' OR 'RMWU'.

*     Rechnungseingang: Kreditorzeile Pflicht
      WHEN 'RMRP'.
        CHECK gs_bapi_acchd-obj_type NE 'BEBD'.            "note 932864

        IF acc_pay[] IS INITIAL AND
           r_currency = 'X'.

          PERFORM append_msg_to_return
                 USING 'E'                 "TYPE
                       'RW'                "ID
                       '611'               "NUMBER
                       'ACCOUNTPAYABLE'    "MESSAGE_ONE
                       r_glvor             "MESSAGE_TWO
                       'ACCOUNTPAYABLE'    "PARAMETER
                       ' ' "r_tabix             "ROW
                       'BUS_ACT'.          "FIELD
        ENDIF.

*     Geschäftsvorfall nicht unterstützt
      WHEN OTHERS.
        PERFORM append_msg_to_return
                USING 'E'              "TYPE
                      'RW'             "ID
                      '613'            "NUMBER
                      r_glvor          "MESSAGE_ONE
                      'DOCUMENTHEADER' "MESSAGE_TWO
                      'DOCUMENTHEADER' "PARAMETER
                      ' ' "r_tabix          "ROW
                      'BUS_ACT'.       "FIELD

    ENDCASE.
  ENDIF.

ENDFORM.                    " CHECK_GLVOR

*&---------------------------------------------------------------------*
*&      Form  ERROR_FROM_SYSTEM
*&---------------------------------------------------------------------*
*       SY-Felder in den Message-Handler übergeben
*----------------------------------------------------------------------*
FORM error_from_system
     USING r_parameter LIKE accbapifd5-parameter
           r_tabix     LIKE accbapifd5-tabix
           r_field.

  CLEAR gs_return.
  IF NOT sy-msgty IS INITIAL.
    gs_return-type     = sy-msgty.
  ELSE.
    gs_return-type     = 'E'.
  ENDIF.
  gs_return-id         = sy-msgid.
  gs_return-number     = sy-msgno.
  gs_return-message_v1 = sy-msgv1.
  gs_return-message_v2 = sy-msgv2.
  gs_return-message_v3 = sy-msgv3.
  gs_return-message_v4 = sy-msgv4.
  gs_return-parameter  = r_parameter.
  gs_return-row        = r_tabix.
  gs_return-field      = r_field.
  APPEND gs_return TO it_return.

ENDFORM.                               " ERROR_FROM_SYSTEM
*&---------------------------------------------------------------------*

*&      Form  CHECK_CAHSDISCOUNT_FIELDS
*&---------------------------------------------------------------------*
*       created by note 1412415                                        *
*----------------------------------------------------------------------*

FORM CHECK_CAHSDISCOUNT_FIELDS  USING  r_bapi_accit LIKE accbapifd5.

DATA: l_sklin TYPE sklin.                                  "note 1604539

CHECK r_bapi_accit-parameter = 'ACCOUNTPAYABLE'
    OR r_bapi_accit-parameter = 'ACCOUNTRECEIVABLE'.

PERFORM check_cashdiscountdays_valid USING r_bapi_accit-parameter
                                           r_bapi_accit-tabix :
    'DSCT_DAYS1'             r_bapi_accit-dsct_days1,
    'DSCT_DAYS2'             r_bapi_accit-dsct_days2,
    'NETTERMS'               r_bapi_accit-netterms.        "note 1604539


PERFORM check_cashdiscountperc_valid USING r_bapi_accit-parameter
                                           r_bapi_accit-tabix :
    'DSCT_PCT1'             r_bapi_accit-dsct_pct1,
    'DSCT_PCT2'             r_bapi_accit-dsct_pct2.


*** begin of note 1604539 ***
l_sklin-ztag1 = r_bapi_accit-dsct_days1.
l_sklin-ztag2 = r_bapi_accit-dsct_days2.
l_sklin-ztag3 = r_bapi_accit-netterms.
l_sklin-zprz1 = r_bapi_accit-dsct_pct1.
l_sklin-zprz2 = r_bapi_accit-dsct_pct2.

PERFORM check_cashdiscountcond_valid USING r_bapi_accit-parameter
                                           r_bapi_accit-tabix
                                           l_sklin.
***  end of note 1604539  ***

ENDFORM.                    " CHECK_CAHSDISCOUNT_FIELDS

*&---------------------------------------------------------------------*
*&      Form  CHECK_CAHSDISCOUNT_VALID
*&---------------------------------------------------------------------*
*       created by note 1412415                                        *
*----------------------------------------------------------------------*

FORM CHECK_CASHDISCOUNTDAYS_VALID
    USING  r_parameter           LIKE accbapifd5-parameter
           r_tabix               LIKE accbapifd5-tabix
           r_segfld
           r_cashdiscount_day    LIKE accit-zbd1t.

  CHECK r_cashdiscount_day < 0.

  PERFORM append_msg_to_return
          USING 'E'             "TYPE
                '00'            "ID
                '126'           "NUMBER
                r_cashdiscount_day    "MESSAGE_ONE
                SPACE           "MESSAGE_TWO
                r_parameter     "PARAMETER
                r_tabix         "ROW
                r_segfld.       "FIELD

ENDFORM.                    " CHECK_CAHSDISCOUNTDAYS_VALID

*&---------------------------------------------------------------------*
*&      Form  CHECK_CASHDISCOUNTPERC_VALID
*&---------------------------------------------------------------------*
*       created by note 1412415                                        *
*----------------------------------------------------------------------*
FORM CHECK_CASHDISCOUNTPERC_VALID
    USING  r_parameter           LIKE accbapifd5-parameter
           r_tabix               LIKE accbapifd5-tabix
           r_segfld
           r_cashdiscount_percentage    LIKE accit-zbd1p.

  CHECK r_cashdiscount_percentage < 0.

  PERFORM append_msg_to_return
          USING 'E'             "TYPE
                '00'            "ID
                '126'           "NUMBER
                r_cashdiscount_percentage    "MESSAGE_ONE
                SPACE           "MESSAGE_TWO
                r_parameter     "PARAMETER
                r_tabix         "ROW
                r_segfld.       "FIELD

ENDFORM.                    " CHECK_CASHDISCOUNTPERC_VALID

*&---------------------------------------------------------------------*
*&      Form  CHECK_VALUE_RANGES
*&---------------------------------------------------------------------*
*       created by note 1578127
*----------------------------------------------------------------------*
*      -->ACCOUNT_GL
*      -->ACCOUNT_TAX
*      -->DOCUMENT_HEADER
*----------------------------------------------------------------------*
FORM CHECK_VALUE_RANGES
    TABLES   account_gl         STRUCTURE bapiacgl09
             account_tax        STRUCTURE bapiactx09
    USING    document_header    TYPE      bapiache09.

  PERFORM check_xfield
          USING document_header-neg_postng
                'DOCUMENTHEADER'
                1
                'NEG_POSTNG'.

  LOOP AT account_gl.
    PERFORM check_xfield
            USING account_gl-cshdis_ind
                  'ACCOUNTGL'
                  sy-tabix
                  'CSHDIS_IND'.

    PERFORM check_xfield
            USING account_gl-xmfrw
                  'ACCOUNTGL'
                  sy-tabix
                  'XMFRW'.

    PERFORM check_xfield
            USING account_gl-reval_ind
                  'ACCOUNTGL'
                  sy-tabix
                  'REVAL_IND'.
  ENDLOOP.

  LOOP AT account_tax.
    PERFORM check_xfield
            USING account_tax-direct_tax
                  'ACCOUNTTAX'
                  sy-tabix
                  'DIRECT_TAX'.
  ENDLOOP.


ENDFORM.                    " CHECK_VALUE_RANGES

*&---------------------------------------------------------------------*
*&      Form  CHECK_XFIELD
*&---------------------------------------------------------------------*
*       created by note 1578127
*----------------------------------------------------------------------*
FORM CHECK_XFIELD
    USING    r_value
             r_parameter
             r_tabix
             r_segfld.

    CHECK r_value IS NOT INITIAL AND r_value <> 'X'.

    PERFORM append_msg_to_return
            USING 'E'             "TYPE
                  '00'            "ID
                  '491'           "NUMBER
                  SPACE           "MESSAGE_ONE
                  SPACE           "MESSAGE_TWO
                  r_parameter     "PARAMETER
                  r_tabix         "ROW
                  r_segfld.       "FIELD

ENDFORM.                    " CHECK_XFIELD
*&---------------------------------------------------------------------*
*&      Form  CHECK_CASHDISCOUNTCOND_VALID
*&---------------------------------------------------------------------*
*       created by note 1604539
*----------------------------------------------------------------------*
FORM CHECK_CASHDISCOUNTCOND_VALID  USING    R_PARAMETER
                                            R_TABIX
                                            R_SKLIN.

  DATA: rc LIKE sy-subrc.

  CALL FUNCTION 'FI_TERMS_OF_PAYMENT_CHECKING'
    EXPORTING
      i_sklin = r_sklin
    IMPORTING
      e_rcode = rc
    EXCEPTIONS
      OTHERS  = 1.

  IF rc <> 0.
    PERFORM append_msg_to_return
          USING 'E'             "TYPE
                'F5'            "ID
                '158'           "NUMBER
                SPACE           "MESSAGE_ONE
                SPACE           "MESSAGE_TWO
                r_parameter     "PARAMETER
                r_tabix         "ROW
                SPACE.          "FIELD
  ENDIF.

ENDFORM.                    " CHECK_CASHDISCOUNTCOND_VALID

*&---------------------------------------------------------------------*
*&      Form  CHECK_AWTYP_REV
*&---------------------------------------------------------------------*
*       created with note 1746760
*----------------------------------------------------------------------*
*      -->r_awtyp
*      -->r_parameter
*      -->r_tabix
*----------------------------------------------------------------------*
FORM CHECK_AWTYP_REV  USING r_awtyp     LIKE acchd-awtyp
                            r_parameter LIKE accbapifd5-parameter
                            r_tabix     LIKE accbapifd5-tabix.

* Check all AWTYPes which are blocked for posting
* Document reversal on those should not be possible aswell
  PERFORM check_awtyp
          USING r_awtyp
                r_parameter
                r_tabix.

* List of AWTYPes which are allowed for posting, but do not
* support real reversal procedure
  IF r_awtyp = 'HRPAY' OR               " Personalabrechnung
     r_awtyp = 'TRAVL'.                 " Reisekostenabrechnung
    PERFORM append_msg_to_return
            USING 'E'             "TYPE
                  'RW'            "ID
                  '628'           "NUMBER
                  r_awtyp         "MESSAGE_ONE
                  r_parameter     "MESSAGE_TWO
                  r_parameter     "PARAMETER
                  r_tabix         "ROW
                  'OBJ_TYPE'.     "FIELD
  ENDIF.
ENDFORM.                    " CHECK_AWTYP_REV

*&---------------------------------------------------------------------*
*&      Form  FILL_RETURN_TO_ECS
*&---------------------------------------------------------------------*
*       ECS uses ROW field of return structure to assign error
*       messages to the corresponding ECS items. Instead of the
*       BAPI_TABIX the POSNR is used within ECS, so this has to be
*       temporarily mapped to the return structure prior processing
*       of the ECS.
*----------------------------------------------------------------------*
FORM fill_return_to_ecs.
  DATA: ls_accit TYPE accit.

  LOOP AT it_return INTO gs_return WHERE row IS NOT INITIAL.
    READ TABLE it_accit WITH KEY bapi_param = gs_return-parameter
                                 bapi_tabix = gs_return-row
                            INTO ls_accit.
    IF sy-subrc = 0.
      gs_return-row = ls_accit-posnr.
      MODIFY it_return FROM gs_return TRANSPORTING row.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FILL_RETURN_TO_ECS

*&---------------------------------------------------------------------*
*&      Form  FILL_RETURN_FROM_ECS
*&---------------------------------------------------------------------*
*       ECS uses ROW field of return structure to assign error
*       messages to the corresponding ECS items. Instead of the
*       BAPI_TABIX the POSNR is used within ECS. The ROW parameter
*       of the return table from ECS must therefore be converted back
*       to BAPI_TABIX.
*----------------------------------------------------------------------*
FORM fill_return_from_ecs .

  DATA: ls_accit TYPE accit.

  LOOP AT it_return INTO gs_return WHERE row IS NOT INITIAL.
    READ TABLE it_accit WITH KEY posnr = gs_return-row
                            INTO ls_accit.
    IF sy-subrc = 0.
      gs_return-row = ls_accit-bapi_tabix.
      MODIFY it_return FROM gs_return TRANSPORTING row.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FILL_RETURN_FROM_ECS
*&---------------------------------------------------------------------*
*&      Form  CHECK_TAXPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_BAPI_ACCIT  text
*----------------------------------------------------------------------*
FORM check_itemno_tax USING p_document_header TYPE bapiache09  "1982803
                            p_gs_bapi_accit TYPE accbapifd5.   "1886654

 DATA: l_msgty TYPE MSGTS.

* Field ITEMNO_TAX not available in GL acconut structure on CRM side
* therefor TAXPS cannot be set in GL line items. It will be derived
* later in FI interface.
 CHECK p_document_header-obj_type <> 'BEBD'               "note 1982803
 AND   p_document_header-obj_type <> 'BERD'               "note 1982803
 AND   p_document_header-obj_type <> 'BERE'               "note 1982803
 AND   p_document_header-obj_type <> 'CRMFM'.             "note 1982803

 CHECK p_gs_bapi_accit-parameter <> 'ACCOUNTPAYABLE' AND
       p_gs_bapi_accit-parameter <> 'ACCOUNTRECEIVABLE' AND
       NOT p_gs_bapi_accit-tax_code IS INITIAL.

 IF gd_xtxit IS INITIAL.
   IF NOT p_gs_bapi_accit-itemno_tax IS INITIAL.
     CALL FUNCTION 'READ_CUSTOMIZED_MESSAGE'
       EXPORTING
         i_arbgb       = 'FF'
         i_dtype       = 'E'
         i_msgnr       = '817'
       IMPORTING
         e_msgty       = l_msgty.
     IF l_msgty <> '-'.
       PERFORM append_msg_to_return
            USING 'E'             "TYPE
                  'FF'            "ID
                  '817'           "NUMBER
                  p_gs_bapi_accit-taxjurcode_deep
                  p_gs_bapi_accit-tax_code
                  p_gs_bapi_accit-itemno_tax
                  0
                  'ITEMNO_TAX'.
     ENDIF.
   ENDIF.
 ELSE.
   IF p_gs_bapi_accit-itemno_tax IS INITIAL.
     CALL FUNCTION 'READ_CUSTOMIZED_MESSAGE'
       EXPORTING
         i_arbgb       = 'FF'
         i_dtype       = 'E'
         i_msgnr       = '818'                               "1943169
       IMPORTING
         e_msgty       = l_msgty.
     IF l_msgty <> '-'.
       PERFORM append_msg_to_return
            USING 'E'             "TYPE
                  'FF'            "ID
                  '818'           "NUMBER                    "1943169
                  p_gs_bapi_accit-taxjurcode_deep
                  p_gs_bapi_accit-tax_code
                  p_gs_bapi_accit-itemno_tax
                  0
                  'ITEMNO_TAX'.
     ENDIF.
   ENDIF.
 ENDIF.
ENDFORM.                    " CHECK_TAXPS
