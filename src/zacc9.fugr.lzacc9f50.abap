*----------------------------------------------------------------------*
*   INCLUDE LACC9F50                                                   *
*----------------------------------------------------------------------*

FORM get_account_assignment.

  DATA: ls_acc_assignment TYPE iaom_account_assignment,
        ls_add_acc_info   TYPE iaom_add_acc_info,
        lt_sub_obj_attrs  type iaomt_sub_object_attribute_tab,
        ld_uom            TYPE meins.

*Hier wird nicht unterschieden, ob die Faktura CO-relevant ist, der
*Translator wird immer aufgrufen

  IF NOT gs_bapi_accit-ext_object_id IS INITIAL AND
     NOT gs_bapi_accit-bus_scenario  IS INITIAL.

* Geschäftsjahr
    PERFORM fill_fisc_year.

* Verkaufsmengeneinheit vs. Basismengeneinheit
    IF gs_bapi_accit-base_uom IS INITIAL.
      ld_uom = gs_bapi_accit-sales_unit.
    ELSE.
      ld_uom = gs_bapi_accit-base_uom.
    ENDIF.

    PERFORM check_if_initial
            USING gs_bapi_accit-parameter
                  gs_bapi_accit-tabix :
*                 'BASE_UOM/SALES_UNIT' ld_uom,            "note 601778
                  'FISC_YEAR ' gs_bapi_accit-fisc_year.

***************************************************************
****Changes due to Intercompany Case for CRM Sales - Begin*****
***************************************************************

    data lv_subkey type string.
    data lv_time_stamp type timestampl.
    GET TIME STAMP FIELD lv_time_stamp.
    lv_subkey = lv_time_stamp.
    log-point id IAOM_CRM_ACC_DOC
              subkey lv_subkey
              fields gs_bapi_acchd-REF_DOC_NO
                     gs_bapi_accit-bus_scenario
                     gs_bapi_accit-ext_object_id
                     gs_bapi_accit-bill_category.

    case gs_bapi_accit-bus_scenario.

      when 'CRM_SALES'.

        IF gs_bapi_accit-bill_category = 'A' AND gs_bapi_acchd-bus_act = 'SD00'.

*         Integration of Inter-company billing document into A/R
          CALL FUNCTION 'IAOM_GET_ACCOUNT_ASSIGNMENT'
            EXPORTING
              i_bus_scenario_id          = gs_bapi_accit-bus_scenario
              i_ext_object_id            = gs_bapi_accit-ext_object_id
              i_base_uom                 = ld_uom
              i_posting_year             = gs_bapi_accit-fisc_year
            IMPORTING
              e_account_assignment       = ls_acc_assignment
              e_add_acc_info             = ls_add_acc_info
            EXCEPTIONS
              bus_scenario_unknown       = 1
              ext_object_id_unknown      = 2
              ext_object_not_co_relevant = 0
              prof_segment_error         = 3
              other_error                = 4
              OTHERS                     = 5.

        else.

          lt_sub_obj_attrs = cl_service_aam_crm_sales=>add_ics_sub_obj_attrs_for_obj( ).

*         Customer billing document in the inter-company scenario
*         and Inter-company billing document into A/P
          CALL FUNCTION 'IAOM_GET_ACCOUNT_ASSIGNMENT'
            EXPORTING
              i_bus_scenario_id          = gs_bapi_accit-bus_scenario
              i_ext_object_id            = gs_bapi_accit-ext_object_id
              i_sub_obj_attrs            = lt_sub_obj_attrs
              i_base_uom                 = ld_uom
              i_posting_year             = gs_bapi_accit-fisc_year
            IMPORTING
              e_account_assignment       = ls_acc_assignment
              e_add_acc_info             = ls_add_acc_info
            EXCEPTIONS
              bus_scenario_unknown       = 1
              ext_object_id_unknown      = 2
              ext_object_not_co_relevant = 0
              prof_segment_error         = 3
              other_error                = 4
              OTHERS                     = 5.

           if sy-subrc = 2.
             clear: ls_acc_assignment, ls_add_acc_info.

*            Customer billing document for none inter-company scenario
             CALL FUNCTION 'IAOM_GET_ACCOUNT_ASSIGNMENT'
               EXPORTING
                 i_bus_scenario_id          = gs_bapi_accit-bus_scenario
                 i_ext_object_id            = gs_bapi_accit-ext_object_id
                 i_base_uom                 = ld_uom
                 i_posting_year             = gs_bapi_accit-fisc_year
               IMPORTING
                 e_account_assignment       = ls_acc_assignment
                 e_add_acc_info             = ls_add_acc_info
               EXCEPTIONS
                 bus_scenario_unknown       = 1
                 ext_object_id_unknown      = 2
                 ext_object_not_co_relevant = 0
                 prof_segment_error         = 3
                 other_error                = 4
                 OTHERS                     = 5.
           endif.
        ENDIF.

      when others.

*       External billing document
        CALL FUNCTION 'IAOM_GET_ACCOUNT_ASSIGNMENT'
          EXPORTING
            i_bus_scenario_id          = gs_bapi_accit-bus_scenario
            i_ext_object_id            = gs_bapi_accit-ext_object_id
            i_base_uom                 = ld_uom
            i_posting_year             = gs_bapi_accit-fisc_year
          IMPORTING
            e_account_assignment       = ls_acc_assignment
            e_add_acc_info             = ls_add_acc_info
          EXCEPTIONS
            bus_scenario_unknown       = 1
            ext_object_id_unknown      = 2
            ext_object_not_co_relevant = 0
            prof_segment_error         = 3
            other_error                = 4
            OTHERS                     = 5.

    endcase.

***************************************************************
****Changes due to Intercompany Case for CRM Sales - End*******
***************************************************************


    IF sy-subrc IS INITIAL.
      gs_bapi_accit-orderid         = ls_acc_assignment-order_no.
      gs_bapi_accit-wbs_element     = ls_acc_assignment-wbs_element.
      gs_bapi_accit-sales_ord       = ls_acc_assignment-sales_document.
      gs_bapi_accit-s_ord_item   = ls_acc_assignment-sales_document_pos.
      gs_bapi_accit-profit_segm_no  = ls_acc_assignment-profit_segm_no.
      gs_bapi_accit-profit_ctr      = ls_acc_assignment-profit_centre.
      gs_bapi_accit-costcenter      = ls_acc_assignment-cost_center.

*Aktualisiere Org-Felder, die zum Erg.obj / Innenauftrag gehören
      IF NOT ls_add_acc_info IS INITIAL.
        IF gs_bapi_accit-profit_ctr IS INITIAL.            "note 617210
          gs_bapi_accit-profit_ctr = ls_add_acc_info-profit_centre.
        ENDIF.
        gs_bapi_accit-co_area    = ls_add_acc_info-co_area.
        gs_bapi_accit-comp_code  = ls_add_acc_info-company_code.
        gs_bapi_accit-bus_area   = ls_add_acc_info-business_area.
*       gs_bapi_accit-func_area  = ls_add_acc_info-functional_area. "note 946821
      ENDIF.
* Begin of note 1917331
    ELSEIF sy-subrc = 2.  "object unknown in CO
      PERFORM append_msg_to_return
        USING 'E'
              'CRMSERV_ACC_EXEC'
              '005'
              gs_bapi_accit-ext_object_id
              gs_bapi_accit-itm_number
              gs_bapi_accit-parameter
              gs_bapi_accit-tabix
              'EXT_OBJECT_ID'.
* End of note 1917331
    ELSE.
*Fehler im Translator-Aufruf
      PERFORM error_from_system
              USING gs_bapi_accit-parameter
                    gs_bapi_accit-tabix
                    'EXT_OBJECT_ID'.
    ENDIF.

  ELSEIF NOT gs_bapi_accit-ext_object_id IS INITIAL
      OR NOT gs_bapi_accit-bus_scenario  IS INITIAL.
* Nur ein Feld gefüllt
    PERFORM check_if_initial
            USING gs_bapi_accit-parameter
                  gs_bapi_accit-tabix:
                  'EXT_OBJECT_ID' gs_bapi_accit-ext_object_id,
                  'BUS_SCENARIO'  gs_bapi_accit-bus_scenario.
  ENDIF.

ENDFORM.                    " GET_ACCOUNT_ASSIGNMENT

*---------------------------------------------------------------------*
*       FORM fill_acchd_from_fica                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  contract_header                                               *
*---------------------------------------------------------------------*
FORM fill_acchd_from_fica
     USING contract_header TYPE bapiaccahd.

  gs_acchd-subset = 'R001'.

  CLEAR gs_fica_hd.
  gs_fica_hd-opbel     = contract_header-doc_no.
  gs_fica_hd-blart_fkk = contract_header-doc_type_ca.
  gs_fica_hd-resky     = contract_header-res_key.
  gs_fica_hd-rfzas     = contract_header-payment_form_ref.
  gs_fica_hd-fikey     = contract_header-fikey.

* Structure gs_fica_hd will be moved to ACCIT

ENDFORM.                    "fill_acchd_from_fica

*&---------------------------------------------------------------------*
*&      Form  MAP_GP_TO_KUNNR
*&---------------------------------------------------------------------*
*       Geschäftspartner-GUID => Kundennummer
*----------------------------------------------------------------------*
FORM map_gp_to_kunnr.

  DATA: ld_customer TYPE accit-kunnr,
        ld_p_guid   TYPE bu_partner_guid,
        ls_but000   TYPE but000.

  CHECK NOT gs_bapi_accit-partner_guid IS INITIAL.

  CASE gs_bapi_accit-actv_account.

*   FI-CA
    WHEN '2'.
      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = func_fica_partner
        EXCEPTIONS
          function_not_exist = 1
          OTHERS             = 2.
      IF NOT sy-subrc IS INITIAL.
        PERFORM append_msg_to_return
               USING 'E'                 "TYPE
                     'RW'                "ID
                     '609'               "NUMBER
                     text-001            "MESSAGE_ONE
                     text-002            "MESSAGE_TWO
                     'ACCOUNTRECEIVABLE' "PARAMETER
                     ' '                 "ROW
                     'PARTNER_GUID'.    "FIELD

      ELSE.
*       func_fica_partner = BUP_BUT000_SELECT_WITH_GUID
*       MOVE char 32 GUID to raw 16
        ld_p_guid = gs_bapi_accit-partner_guid.
        CALL FUNCTION func_fica_partner
          EXPORTING
            i_partner_guid = ld_p_guid
          IMPORTING
            e_but000       = ls_but000
          EXCEPTIONS
            not_found      = 1
            OTHERS         = 2.
        IF sy-subrc IS INITIAL.
          gs_bapi_accit-customer = ls_but000-partner.
        ENDIF.
      ENDIF.
*   FI-AP/AR
    WHEN OTHERS.
      CALL FUNCTION 'PI_BP_CRM_MAP_BP_TO_KUNNR'
        EXPORTING
          iv_partner         = gs_bapi_accit-partner_guid
        IMPORTING
          ev_customer        = ld_customer
        EXCEPTIONS
          customer_not_found = 1
          OTHERS             = 2.
      IF sy-subrc IS INITIAL.
        CASE gs_bapi_accit-parameter.
          WHEN 'ACCOUNTRECEIVABLE'.
            gs_bapi_accit-customer  = ld_customer.
          WHEN 'ACCOUNTPAYABLE'.
            gs_bapi_accit-vendor_no = ld_customer.
        ENDCASE.
      ENDIF.
  ENDCASE.

  IF NOT sy-subrc IS INITIAL.
    PERFORM error_from_system
            USING gs_bapi_accit-parameter
                  gs_bapi_accit-tabix
                  'PARTNER_GUID'.
  ENDIF.

ENDFORM.                    " MAP_GP_TO_KUNNR

*&---------------------------------------------------------------------*
*&      Form  FILL_FISC_YEAR
*&---------------------------------------------------------------------*
*       Geschäftsjahr für CO-Translator ermitteln
*----------------------------------------------------------------------*
FORM fill_fisc_year.

  CHECK gs_bapi_accit-fisc_year IS INITIAL.

  IF gs_bapi_acchd-fisc_year IS INITIAL.

    IF gs_bapi_accit-comp_code IS INITIAL.
      gs_bapi_accit-comp_code = gs_bapi_acchd-comp_code.
    ENDIF.
    IF gs_bapi_accit-pstng_date IS INITIAL.
      gs_bapi_accit-pstng_date = gs_bapi_acchd-pstng_date.
    ENDIF.

    CALL FUNCTION 'FI_PERIOD_DETERMINE'
      EXPORTING
        i_budat        = gs_bapi_accit-pstng_date
        i_bukrs        = gs_bapi_accit-comp_code
      IMPORTING
        e_gjahr        = gs_bapi_accit-fisc_year
      EXCEPTIONS
        fiscal_year    = 1
        period         = 2
        period_version = 3
        posting_period = 4
        special_period = 5
        version        = 6
        posting_date   = 7
        OTHERS         = 8.
    IF NOT sy-subrc IS INITIAL.
      PERFORM error_from_system
              USING gs_bapi_accit-parameter
                    gs_bapi_accit-tabix
                    'FISC_YEAR'.
    ENDIF.
  ELSE.
    gs_bapi_accit-fisc_year = gs_bapi_acchd-fisc_year.
  ENDIF.

ENDFORM.                    " FILL_FISC_YEAR
*&---------------------------------------------------------------------*
*&      Form  call_badi
*&---------------------------------------------------------------------*
*       Customer Exit ( BadI )
*----------------------------------------------------------------------*
FORM call_badi
     TABLES p_extension STRUCTURE bapiparex.

* CHECK NOT p_extension[] IS INITIAL.

  DATA: ls_acchd  TYPE acchd,
        lt_return TYPE TABLE OF bapiret2.

  CHECK NOT g_exit IS INITIAL.

  READ TABLE it_acchd INTO ls_acchd INDEX 1.

  CALL METHOD g_exit->change
    EXPORTING
      flt_val      = gs_aw-awtyp
    CHANGING
      c_acchd      = ls_acchd
      c_accit      = it_accit
      c_acccr      = it_acccr
      c_accwt      = it_accwt
      c_acctx      = it_acctx
      c_extension2 = p_extension[]
      c_return     = lt_return.

  MODIFY it_acchd FROM ls_acchd INDEX 1.

  MOVE-CORRESPONDING ls_acchd TO gs_aw.
  gs_aw-awkey+00(10) = ls_acchd-awref.                     "note 1584301
  gs_aw-awkey+10(10) = ls_acchd-aworg.                     "note 1584301

  CLEAR gs_return.
  LOOP AT lt_return INTO gs_return.
    APPEND gs_return TO it_return.
  ENDLOOP.

ENDFORM.                    " call_badi

*&---------------------------------------------------------------------*
*&      Form  NEGATIVE_QUANTITY
*&---------------------------------------------------------------------*
*       Bei negativer fakturierter Menge wird das Retourenflag
*       gesetzt und das Vorzeichen der Mengen umgedreht
*----------------------------------------------------------------------*
FORM negative_quantity.

  IF gs_accit-fkimg < 0 AND NOT gs_accit-xmfrw IS INITIAL. "Note 1255534
    gs_accit-shkzg_va = 'X'.
  ENDIF.

  gs_accit-fkimg = abs( gs_accit-fkimg ). "INV_QTY
  gs_accit-fklmg = abs( gs_accit-fklmg ). "INV_QTY_SU
  gs_accit-menge = abs( gs_accit-menge ). "QUANTITY
  gs_accit-ntgew = abs( gs_accit-ntgew ). "NET_WEIGHT
  gs_accit-brgew = abs( gs_accit-brgew ). "GROSS_WT
  gs_accit-volum = abs( gs_accit-volum ). "VOLUME

ENDFORM.                    " NEGATIVE_QUANTITY

*&---------------------------------------------------------------------*
*&      Form  try_real_reversal
*&---------------------------------------------------------------------*
*       Versuche Echten Storno
*----------------------------------------------------------------------*
FORM try_reversal
     TABLES   it_accountgl         STRUCTURE bapiacgl09
              it_accountreceivable STRUCTURE bapiacar09
              it_accountpayable    STRUCTURE bapiacap09
     USING    i_documentheader     TYPE      bapiache09
              i_contractheader     TYPE      bapiaccahd
     CHANGING e_reversed           TYPE      flag.

  DATA: ls_reverse TYPE bapiacrev,
        ls_accrev  TYPE accrev,
        ld_awkey_rev TYPE awkey,
        mh_active  TYPE flag.

  DATA: BEGIN OF ls_fica_rev,
         blart_fkk TYPE acpi_doc_type_ca,     "Belegart
         fikey     TYPE acpi_fikey,           "Abstimmschlüssel
         opbel     TYPE acpi_doc_no,          "Externe Belegnummer
        END OF ls_fica_rev.

  DATA: lr_acc_document_reversal TYPE REF TO acc_document_reversal.

  FIELD-SYMBOLS <fs> TYPE ANY.

* Try Reversal by Reference only for Billing Engine and Real Estate
  CHECK i_documentheader-obj_type = 'BEBD'  OR
        i_documentheader-obj_type = 'BERD'  OR
        i_documentheader-obj_type = 'BERE'  OR
        i_documentheader-obj_type = 'ENTD'  OR
        i_documentheader-obj_type = 'REACI' OR
        i_documentheader-obj_type = 'CRMFM'.



  CHECK NOT i_documentheader-obj_key_r IS INITIAL.
  CHECK     i_documentheader-compo_acc IS INITIAL.
  CHECK     i_documentheader-partial_rev IS INITIAL.      "Note 1875644

  MOVE-CORRESPONDING i_documentheader TO ls_reverse.


  PERFORM messages_start USING mh_active.

* Map to interal structure
  PERFORM check_and_fill_accrev
          USING  ls_reverse
                 ls_accrev
                 i_documentheader-neg_postng                "Note1143408
                 i_documentheader-ref_doc_no.               "Note1478260


  READ TABLE it_accountgl INDEX 1.
  IF sy-subrc = 0.
    ls_accrev-fkart = it_accountgl-bill_type.
  ENDIF.
  ls_accrev-glvor = i_documentheader-bus_act.

  IF NOT i_contractheader IS INITIAL.
*   Look for FI-CA extension
    ASSIGN COMPONENT 'FIKEY' OF STRUCTURE ls_accrev TO <fs>.
    IF sy-subrc IS INITIAL.
      ls_fica_rev-opbel     = i_contractheader-doc_no.
      ls_fica_rev-blart_fkk = i_contractheader-doc_type_ca.
      ls_fica_rev-fikey     = i_contractheader-fikey.
      MOVE-CORRESPONDING ls_fica_rev TO ls_accrev.
      ls_accrev-subset      = 'R001'.                  "note_925555
    ELSE.
      CLEAR e_reversed.
      EXIT.
    ENDIF.
  ENDIF.


  TRY.
    GET BADI lr_acc_document_reversal.
  CATCH cx_badi_not_implemented .                       "#EC NO_HANDLER
  ENDTRY.

  IF lr_acc_document_reversal IS BOUND.
    CALL BADI lr_acc_document_reversal->no_reversal_by_reference
         EXPORTING   i_documentheader     = i_documentheader
                     i_contractheader     = i_contractheader
                     i_reversal           = ls_accrev
                     it_accountgl         = it_accountgl[]
                     it_accountreceivable = it_accountreceivable[]
                     it_accountpayable    = it_accountpayable[].
  ENDIF.

* call AC_DOCUMENT_REVERSE
  PERFORM reverse_check
          USING  ls_accrev.

  PERFORM messages_stop
          USING 'REVERSAL' '1' mh_active.

  LOOP AT it_return INTO gs_return
          WHERE type = 'A'
             OR type = 'E'.
    EXIT.
  ENDLOOP.
  IF sy-subrc IS INITIAL.
    CLEAR e_reversed.
    CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXPORTING
        reset = 'X'.
    CLEAR it_return.
    ld_awkey_rev(10)     = ls_accrev-awref_rev.
    ld_awkey_rev+10(10)  = ls_accrev-aworg_rev.
    CLEAR gs_return.
    gs_return-type       = 'I'.
    gs_return-id         = 'RW'.
    gs_return-number     = '632'.
    gs_return-message_v1 = ls_accrev-awtyp.
    gs_return-message_v2 = ld_awkey_rev.
    gs_return-message_v3 = ls_accrev-awsys.
    INSERT gs_return INTO it_return INDEX 1.
  ELSE.
*   Reversal by Reference successful !
    e_reversed = 'X'.
    gs_aw-awtyp = ls_accrev-awtyp.
    gs_aw-awref = ls_accrev-awref.
    gs_aw-aworg = ls_accrev-aworg.
    gs_aw-awsys = ls_accrev-awsys.
  ENDIF.

ENDFORM.                    " try_reversal
*&---------------------------------------------------------------------*
*&      Form  badi_change_it_accit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM badi_change_it_accit .
  DATA: ls_acchd  TYPE acchd.

  CHECK NOT g_exit IS INITIAL.

  READ TABLE it_acchd INTO ls_acchd INDEX 1.

  CALL METHOD g_exit->fill_accit
    EXPORTING
      flt_val      = gs_aw-awtyp
      i_acchd      = ls_acchd
    CHANGING
      c_bapi_accit = gs_bapi_accit
      c_accit      = gs_accit.

ENDFORM.                    " badi_change_it_accit
*&---------------------------------------------------------------------*
*&      Form  badi_init
*&---------------------------------------------------------------------*
*       get reference to BADI implementation, if any
*----------------------------------------------------------------------*
FORM badi_init .

  CALL METHOD cl_exithandler=>get_instance
    EXPORTING
      exit_name              = 'ACC_DOCUMENT'
      null_instance_accepted = 'X'
    CHANGING
      instance               = g_exit.

ENDFORM.                    " badi_init
*&---------------------------------------------------------------------*
*&      Form  get_account_assignment_grantor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_account_assignment_grantor .
  DATA: l_item_guid TYPE iaom_gtr_record-item_guid,
        mycx TYPE REF TO cx_iaom_gtr_services,
        ls_attributes TYPE iaom_gtr_record.
  DATA: ls_acc_assignment TYPE iaom_account_assignment,
        ls_add_acc_info   TYPE iaom_add_acc_info,
        ld_uom            TYPE meins.
  FIELD-SYMBOLS: <mess> TYPE iaom_message.


  CHECK: gs_bapi_accit-parameter = 'ACCOUNTGL' OR
         gs_bapi_accit-parameter = 'ACCOUNTRECEIVABLE'.

  IF gs_bapi_accit-ext_object_id IS NOT INITIAL.

* Read Grantor CO-Translator entry
    l_item_guid = gs_bapi_accit-ext_object_id.
    TRY.
        ls_attributes = cl_iaom_gtr_services=>read( l_item_guid ).
      CATCH cx_iaom_gtr_services INTO mycx.
        LOOP AT mycx->iaom_messages ASSIGNING <mess>.
          PERFORM append_msg_to_return
                  USING <mess>-msgty                "TYPE
                        <mess>-msgid                "ID
                        <mess>-msgno                "NUMBER
                        <mess>-msgv1                "MESSAGE_ONE
                        <mess>-msgv1                "MESSAGE_TWO
                        gs_bapi_accit-parameter     "PARAMETER
                        gs_bapi_accit-tabix         "ROW
                        'GRANTOR CO-TRANSLATOR'.    "FIELD
        ENDLOOP.
    ENDTRY.

* Move Account Assignments from CO-Translator to ACCIT
    gs_bapi_accit-crmobj  = l_item_guid.
    gs_bapi_accit-comp_code  = ls_attributes-bukrs.
    if gs_bapi_accit-parameter = 'ACCOUNTGL'.
      gs_bapi_accit-gl_account  = ls_attributes-saknr.
    endif.
    CALL FUNCTION 'RK_KOKRS_FIND'
      EXPORTING
        bukrs                  = gs_bapi_accit-comp_code
      IMPORTING
        kokrs                  = gs_bapi_accit-co_area
      EXCEPTIONS
        assignment_not_allowed = 1
        insufficient_input     = 2
        no_kokrs_assigned      = 3
        no_kokrs_for_bukrs     = 4
        no_kokrs_for_bu_gb     = 5
        wrong_kokrs_for_bukrs  = 6
        wrong_kokrs_for_bu_gb  = 7
        OTHERS                 = 8.
    IF sy-subrc <> 0.
      PERFORM error_from_system
              USING gs_bapi_accit-parameter
                    gs_bapi_accit-tabix
                    'EXT_OBJECT_ID'.
    ENDIF.

    gs_bapi_accit-bus_area  = ls_attributes-gsber.
    gs_bapi_accit-cmmt_item_long = ls_attributes-fipex.
    gs_bapi_accit-funds_ctr = ls_attributes-fistl.
    gs_bapi_accit-fund      = ls_attributes-geber.
    gs_bapi_accit-func_area_long = ls_attributes-fkber.
    gs_bapi_accit-grant_nbr = ls_attributes-grant_nbr.
    gs_bapi_accit-measure = ls_attributes-measure.
    gs_bapi_accit-main_trans = ls_attributes-main_trans.
    gs_bapi_accit-sub_trans = ls_attributes-sub_trans.
    gs_bapi_accit-orderid = ls_attributes-aufnr.
    gs_bapi_accit-costcenter = ls_attributes-kostl.
    gs_bapi_accit-wbs_element = ls_attributes-ps_posid.
    gs_bapi_accit-network = ls_attributes-nplnr.
    gs_bapi_accit-activity = ls_attributes-vornr.
****GJAHR
****HEADER_GUID
****HEADER_ID
****REF_PRED_ITEM_GUID
****REF_PRED_HEADER_GUID

* Geschäftsjahr
    PERFORM fill_fisc_year.

* Verkaufsmengeneinheit vs. Basismengeneinheit
    IF gs_bapi_accit-base_uom IS INITIAL.
      ld_uom = gs_bapi_accit-sales_unit.
    ELSE.
      ld_uom = gs_bapi_accit-base_uom.
    ENDIF.

    PERFORM check_if_initial
            USING gs_bapi_accit-parameter
                  gs_bapi_accit-tabix:
*                 'BASE_UOM/SALES_UNIT' ld_uom,            "note 601778
                  'FISC_YEAR ' gs_bapi_accit-fisc_year.

  ELSE.
* Only one field filled
    PERFORM check_if_initial
            USING gs_bapi_accit-parameter
                  gs_bapi_accit-tabix:
                  'EXT_OBJECT_ID' gs_bapi_accit-ext_object_id,
                  'BUS_SCENARIO'  gs_bapi_accit-bus_scenario.
  ENDIF.

ENDFORM.                    " get_account_assignment_grantor
