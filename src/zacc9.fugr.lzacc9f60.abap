*----------------------------------------------------------------------*
***INCLUDE LACC9F60 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  REVERSE
*&---------------------------------------------------------------------*
FORM reverse
     TABLES r_return  STRUCTURE bapiret2
     USING  r_reverse LIKE bapiacrev
            r_glvor   LIKE acchd-glvor
            r_modus.

  DATA: ls_accrev    LIKE accrev,
        ld_awkey_rev LIKE bapiacrev-obj_key_r,
        mh_active.

  CLEAR   gs_return.
  REFRESH it_return.
  CLEAR   gs_bapi_acchd.                                    "Note1144749

  PERFORM messages_start USING mh_active.

  PERFORM check_and_fill_accrev
          USING  r_reverse
                 ls_accrev
                 space                                      "Note1143408
                 space.                                     "Note1478260

  ls_accrev-glvor = r_glvor.

  PERFORM reverse_check
          USING  ls_accrev.

  PERFORM messages_stop
          USING 'REVERSAL' '1' mh_active.

  IF r_modus = 'POST'.
    CLEAR gs_aw.
    MOVE-CORRESPONDING ls_accrev TO gs_aw.

  data: lt_acchd LIKE ACCHD OCCURS 1 WITH HEADER LINE,
        lt_accit LIKE ACCIT OCCURS 1 WITH HEADER LINE,
        lt_acccr LIKE ACCCR OCCURS 1 WITH HEADER LINE.


    IF gs_aw-awtyp    = 'BKPFF'.

      CALL FUNCTION 'FI_ACC_GET'
        TABLES
          T_ACCHD       = lt_acchd[]
          T_ACCIT       = lt_accit[]
          T_ACCCR       = lt_acccr[].


      READ TABLE lt_acchd INDEX 1.
*     READ TABLE lt_accit INDEX 1.
*  pick up a line item of ACCIT in the leading company code
      READ TABLE lt_accit WITH KEY                          "note1071105
                          bukrs = gs_aw-aworg_rev(4).       "note1071105

      IF sy-subrc = 0.
*   FORM fill_obj_key needs the following global data
        gd_budat = lt_accit-budat.
        gd_bukrs = lt_accit-bukrs.
        gd_blart = lt_accit-blart.
        gd_belnr = ls_accrev-belnr.
        gd_ldgrp = lt_acchd-ldgrp.
        gd_gjahr = lt_accit-gjahr.
        PERFORM fill_obj_key.
        r_reverse-obj_key           = gs_aw-awref.
        r_reverse-obj_key+10        = gs_aw-aworg.
      ENDIF.
    ENDIF.
    PERFORM document_post                                   "note1073689
             USING space.                                   "note1073689
  ENDIF.

* looking for error messages
  LOOP AT it_return INTO gs_return
          WHERE type = 'A'
             OR type = 'E'.
    EXIT.
  ENDLOOP.
* error messages
  IF sy-subrc IS INITIAL.
    ld_awkey_rev+00(10) = ls_accrev-awref_rev.              "note1584301
    ld_awkey_rev+10(10) = ls_accrev-aworg_rev.              "note1584301
    CLEAR gs_return.
    gs_return-type       = 'E'.
    gs_return-id         = 'RW'.
    gs_return-number     = '632'.
    gs_return-message_v1 = ls_accrev-awtyp.
    gs_return-message_v2 = ld_awkey_rev.
    gs_return-message_v3 = ls_accrev-awsys.
    INSERT gs_return INTO it_return INDEX 1.
  ENDIF.

  PERFORM fill_return_table
          TABLES r_return
          USING  ls_accrev-awtyp
                 r_reverse-obj_key
                 ls_accrev-awsys.

ENDFORM.                    " REVERSE

*&---------------------------------------------------------------------*
*&      Form  check_and_fill_accrev
*&---------------------------------------------------------------------*
*       Mussfelder und Mapping
*----------------------------------------------------------------------*
FORM check_and_fill_accrev
     USING  r_reverse LIKE bapiacrev
            r_accrev  LIKE accrev
            r_xnegp   LIKE bapiache09-neg_postng        "Note1143408
            r_xblnr   LIKE bapiache09-ref_doc_no.       "Note1478260

  DATA: BEGIN OF ld_awkey,
          awref LIKE acchd-awref,
          aworg LIKE acchd-aworg,
        END   OF ld_awkey.

* Key fields are obligatory
  PERFORM check_if_initial
          USING  'REVERSAL' '1':
    'OBJ_TYPE'  r_reverse-obj_type,
    'OBJ_KEY_R' r_reverse-obj_key_r.

  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 2.

  IF sy-subrc IS INITIAL AND  r_reverse-obj_type <> 'BKPFF'."Note1069413
    PERFORM check_if_initial
            USING  'REVERSAL' '1'
                   'OBJ_SYS'
                   r_reverse-obj_sys.
  ENDIF.

* move fields
  CLEAR r_accrev.

  r_accrev-awtyp     = r_reverse-obj_type.
  r_accrev-awsys     = r_reverse-obj_sys.

  ld_awkey           = r_reverse-obj_key.
  r_accrev-awref     = ld_awkey-awref.
  r_accrev-aworg     = ld_awkey-aworg.

  ld_awkey           = r_reverse-obj_key_r.
  r_accrev-awref_rev = ld_awkey-awref.
  r_accrev-aworg_rev = ld_awkey-aworg.

* r_accrev-BUDAT_REV =
  r_accrev-budat     = r_reverse-pstng_date.
  r_accrev-monat     = r_reverse-fis_period.
  r_accrev-belnr     = r_reverse-ac_doc_no.
  r_accrev-stgrd     = r_reverse-reason_rev.
  r_accrev-XNEGP     = r_xnegp.                             "Note1143408

  IF r_reverse-obj_type = 'BEBD' AND r_xblnr NE space.      "Note1478260
    r_accrev-xblnr = r_xblnr.                               "Note1478260
  ENDIF.                                                    "Note1478260

* r_accrev-FKART     =
  r_accrev-bukrs     = r_reverse-comp_code.
* r_accrev-GLVOR     =
* r_accrev-CPUDT     = sy-cpudt.
* r_accrev-CPUTM     = sy-cputm.
* r_accrev-SUBSET    =
  r_accrev-acc_principle = r_reverse-acc_principle.

  IF r_reverse-obj_type = 'BKPFF'.
    PERFORM reference_create_prelim
              CHANGING r_reverse-obj_type
                       r_reverse-obj_key
                       r_reverse-obj_sys.

     r_accrev-awsys =  r_reverse-obj_sys.                   "Note1069413
     ld_awkey       =  r_reverse-obj_key.
     r_accrev-awref =  ld_awkey-awref.
     r_accrev-aworg =  ld_awkey-aworg.
* for reversal doc types with external number assignment
    IF NOT r_reverse-ac_doc_no IS INITIAL.
      r_accrev-awref =  r_reverse-ac_doc_no.
      r_accrev-aworg =  r_accrev-aworg_rev.
    ENDIF.
  ELSE.                                                    "Note 1746760
* AWTYP: keine R/3-Core Typen verwenden                    "Note 1746760
    PERFORM check_awtyp_rev                                "Note 1746760
            USING r_reverse-obj_type                       "Note 1746760
                  'REVERSAL'                               "Note 1746760
                  1.                                       "Note 1746760
  ENDIF.

ENDFORM.                    " check_and_fill_accrev

*&---------------------------------------------------------------------*
*&      Form  reverse_check
*&---------------------------------------------------------------------*
*       Storno anstossen / möglich ?
*----------------------------------------------------------------------*
FORM reverse_check
     USING r_accrev LIKE accrev.

  CHECK it_return[] IS INITIAL.

  CALL FUNCTION 'AC_DOCUMENT_REVERSE'
    EXPORTING
      i_accrev                 = r_accrev
*     I_COMP                   =
    EXCEPTIONS
      reverse_impossible       = 1
      error_message            = 2
      OTHERS                   = 3 .

  IF NOT sy-subrc IS INITIAL.
    PERFORM error_from_system
            USING 'REVERSAL' '1' ' '.
  ENDIF.

ENDFORM.                    " reverse_check
