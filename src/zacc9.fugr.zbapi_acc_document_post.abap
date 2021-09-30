FUNCTION ZBAPI_ACC_DOCUMENT_POST.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(DOCUMENTHEADER) LIKE  BAPIACHE09 STRUCTURE  BAPIACHE09
*"     VALUE(CUSTOMERCPD) LIKE  ZBAPIACPA09 STRUCTURE  ZBAPIACPA09
*"       OPTIONAL
*"     VALUE(CONTRACTHEADER) LIKE  BAPIACCAHD STRUCTURE  BAPIACCAHD
*"       OPTIONAL
*"  EXPORTING
*"     VALUE(OBJ_TYPE) LIKE  BAPIACHE09-OBJ_TYPE
*"     VALUE(OBJ_KEY) LIKE  BAPIACHE09-OBJ_KEY
*"     VALUE(OBJ_SYS) LIKE  BAPIACHE09-OBJ_SYS
*"  TABLES
*"      ACCOUNTGL STRUCTURE  BAPIACGL09 OPTIONAL
*"      ACCOUNTRECEIVABLE STRUCTURE  BAPIACAR09 OPTIONAL
*"      ACCOUNTPAYABLE STRUCTURE  BAPIACAP09 OPTIONAL
*"      ACCOUNTTAX STRUCTURE  BAPIACTX09 OPTIONAL
*"      CURRENCYAMOUNT STRUCTURE  BAPIACCR09
*"      CRITERIA STRUCTURE  BAPIACKEC9 OPTIONAL
*"      VALUEFIELD STRUCTURE  BAPIACKEV9 OPTIONAL
*"      EXTENSION1 STRUCTURE  BAPIACEXTC OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2
*"      PAYMENTCARD STRUCTURE  BAPIACPC09 OPTIONAL
*"      CONTRACTITEM STRUCTURE  BAPIACCAIT OPTIONAL
*"      EXTENSION2 STRUCTURE  BAPIPAREX OPTIONAL
*"      REALESTATE STRUCTURE  BAPIACRE09 OPTIONAL
*"      ACCOUNTWT STRUCTURE  BAPIACWT09 OPTIONAL
*"----------------------------------------------------------------------

  DATA: reversed TYPE flag.

  PERFORM init_globals.

  PERFORM try_reversal
          TABLES   accountgl
                   accountreceivable
                   accountpayable
          USING    documentheader
                   contractheader
          CHANGING reversed.

  IF reversed IS INITIAL.
    PERFORM check_and_fill_acc_document
            TABLES accountreceivable
                   accountpayable
                   accountgl
                   accounttax
                   currencyamount
                   paymentcard
                   contractitem
            USING  documentheader
                   contractheader
                   'X'.

    PERFORM fill_cpd
            USING customercpd.

    PERFORM fill_real_estate
            TABLES realestate.

    PERFORM fill_with_tax
           TABLES accountwt.

    PERFORM call_customer_function
            TABLES extension1.

    PERFORM call_badi
            TABLES extension2.

    PERFORM fill_and_check_copa
            TABLES criteria
                   valuefield.

    IF documentheader-ecs_env IS NOT INITIAL
      AND cl_fagl_switch_check=>fagl_fin_err_corr( ) IS NOT INITIAL.
      PERFORM fill_return_to_ecs.                         "Note 1851146
      CALL FUNCTION 'GLE_ECS_SINGLE_ACC_DOCUMENT'
        EXPORTING
          i_documentheader = documentheader
*         i_testrun        = ' '
        CHANGING
          cs_acchd         = gs_acchd
          ct_accit         = it_accit
          ct_acccr         = it_acccr
          ct_acctx         = it_acctx
          ct_accwt         = it_accwt
          ct_accfi         = it_accfi
          ct_accit_pa      = it_accit_pa
          ct_acccr_pa      = it_acccr_pa
          ct_return        = it_return
        EXCEPTIONS
          error_message    = 1.
      IF sy-subrc <> 0.
        CLEAR gs_return.
        gs_return-type       = sy-msgty.
        gs_return-id         = sy-msgid.
        gs_return-number     = sy-msgno.
        gs_return-message_v1 = sy-msgv1.
        gs_return-message_v2 = sy-msgv2.
        gs_return-message_v3 = sy-msgv3.
        gs_return-message_v4 = sy-msgv4.
        INSERT gs_return INTO it_return INDEX 1.
      ELSE.
        MOVE-CORRESPONDING gs_acchd TO gs_aw.
        CONCATENATE gs_acchd-awref gs_acchd-aworg INTO gs_aw-awkey.
      ENDIF.
      PERFORM fill_return_from_ecs.                       "Note 1851146
    ELSE.
      PERFORM document_check
            USING 'X'
                  documentheader-compo_acc.

      PERFORM reference_create.
    ENDIF.
  ENDIF.

  IF documentheader-ecs_env IS INITIAL
      OR cl_fagl_switch_check=>fagl_fin_err_corr( ) IS INITIAL.
    PERFORM document_post
          USING  documentheader-compo_acc.
  ENDIF.

  obj_type = gs_aw-awtyp.
  obj_key  = gs_aw-awkey.
  obj_sys  = gs_aw-awsys.

  PERFORM fill_return_table
          TABLES return
          USING  gs_aw-awtyp
                 gs_aw-awkey
                 gs_aw-awsys.

ENDFUNCTION.
