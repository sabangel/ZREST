*----------------------------------------------------------------------*
***INCLUDE LACC9F70 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  fill_real_estate
*&---------------------------------------------------------------------*
*       Anreichern der Belegzeilen um RE Kontierungen
*----------------------------------------------------------------------*
FORM fill_real_estate
     TABLES real_estate STRUCTURE bapiacre09.

  DATA: l_accitpos TYPE sy-tabix,
        ld_imkey   TYPE imkey,
        ld_intreno TYPE vvintreno.

  FIELD-SYMBOLS: <re> LIKE bapiacre09.

  CHECK NOT real_estate[] IS INITIAL.

  SORT it_accit BY posnr.

  LOOP AT real_estate ASSIGNING <re>.
    CLEAR gs_accit.
    READ TABLE it_accit INTO gs_accit
               WITH KEY posnr = <re>-itemno_acc
               BINARY SEARCH.
    CHECK sy-subrc IS INITIAL.

    l_accitpos = sy-tabix.
    CLEAR: ld_imkey, ld_intreno.
*     IMKEY und INTRENO nachlesen
    IF NOT gs_accit-umskz IS INITIAL
       OR  gs_accit-koart = 'S'.
      PERFORM get_imkey_intreno
              USING    <re>
                       gs_accit-hkont
              CHANGING ld_imkey
                       ld_intreno.
    ENDIF.

*   ACCIT Zeilen updaten
    CASE gs_accit-koart.
      WHEN 'D' OR 'K'.
*     Debitor-/Kreditorzeile
        gs_accit-intreno = ld_intreno.
*        IF NOT gs_accit-umskz IS INITIAL.                  note 597281
**       mit Sonderhauptbuch-Kennzeichen                    note 597281
*          gs_accit-imkey   = ld_imkey.                     note 597281
*        ENDIF.                                             note 597281
      WHEN 'S'.
*     Sachkontenzeile
        gs_accit-imkey   = ld_imkey.
        gs_accit-intreno = ld_intreno.
        gs_accit-dabrz   = <re>-ref_date.
    ENDCASE.
    gs_accit-vertn = <re>-contract_no.
    IF NOT <re>-contract_no IS INITIAL.
*     Vertragsart: Allgemeiner Vertrag - Immobilien
      gs_accit-vertt = '9'.
    ENDIF.
    gs_accit-vbewa = <re>-flow_type.

    MODIFY it_accit FROM gs_accit INDEX l_accitpos.

  ENDLOOP.

ENDFORM.                    " fill_real_estate
*&---------------------------------------------------------------------*
*&      Form  get_imkey_intreno
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_imkey_intreno
     USING    ls_realestate TYPE bapiacre09
              i_hkont       TYPE hkont
     CHANGING c_imkey TYPE imkey
              c_intreno TYPE vvintreno.


*** check if any RE account assignment passed in ls_realestate
  CHECK NOT ls_realestate-business_entity  IS INITIAL OR   "note 1079413
        NOT ls_realestate-property         IS INITIAL OR   "note 1079413
        NOT ls_realestate-building         IS INITIAL OR   "note 1079413
        NOT ls_realestate-rental_object    IS INITIAL OR   "note 1079413
        NOT ls_realestate-serv_charge_key  IS INITIAL OR   "note 1079413
        NOT ls_realestate-settlement_unit  IS INITIAL OR   "note 1079413
        NOT ls_realestate-contract_no      IS INITIAL OR   "note 1079413
        NOT ls_realestate-corr_item        IS INITIAL.     "note 1079413

***
*** Prüfung, ob IMKEY ermittelt werden soll !
***

*-----------------------------------------------------------------------
  INCLUDE ifre_begin_of_re_classic.
*-----------------------------------------------------------------------

  CALL FUNCTION 'REMD_ENCODE_IMKEY'
    EXPORTING
      i_bukrs                       = gs_accit-bukrs
      i_swenr                       = ls_realestate-business_entity
      i_sgrnr                       = ls_realestate-property
      i_sgenr                       = ls_realestate-building
      i_smenr                       = ls_realestate-rental_object
      i_snksl                       = ls_realestate-serv_charge_key
      i_sempsl                      = ls_realestate-settlement_unit
      i_dabrbez                     = ls_realestate-ref_date
      i_recnnr                      = ls_realestate-contract_no
      i_sberi                       = ls_realestate-corr_item
    IMPORTING
      e_imkey                       = c_imkey
      e_intreno                     = c_intreno
*       E_OBJNR                       =
    EXCEPTIONS
      object_key_inconsistent       = 1
      object_does_not_exist         = 2
      no_imkey                      = 3
      activity_not_allowed          = 4
      no_objnr                      = 5
      other_error                   = 6
      OTHERS                        = 7.
  IF sy-subrc <> 0.
*     imkey is requested; if imkey is initial exception NO_IMKEY will
*     be raised
    PERFORM error_from_system USING 'REALESTATE' sy-tabix ' '.
    RETURN.
  ENDIF.

  INCLUDE ifre_end_of_re_classic.

*-----------------------------------------------------------------------
  INCLUDE ifre_begin_of_re_ea_fin.
*-----------------------------------------------------------------------

* begin note 668168
  CALL METHOD ('CL_REEX_CALLBACK_ACCT_ASS_FI')=>get_acct_assignment
    EXPORTING
      id_bukrs            = gs_accit-bukrs
      id_swenr            = ls_realestate-business_entity
      id_sgrnr            = ls_realestate-property
      id_sgenr            = ls_realestate-building
      id_smenr            = ls_realestate-rental_object
      id_snksl            = ls_realestate-serv_charge_key
      id_sempsl           = ls_realestate-settlement_unit
      id_dabrz            = ls_realestate-ref_date
      id_recnnr           = ls_realestate-contract_no
      id_sberi            = ls_realestate-corr_item
      id_glaccount        = i_hkont
    IMPORTING
      ed_intreno          = c_intreno
      ed_imkey            = c_imkey
    EXCEPTIONS
      error               = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
    PERFORM error_from_system USING 'REALESTATE' sy-tabix ' '.
    RETURN.
  ENDIF.
* end note 668168

  INCLUDE ifre_end_of_re_ea_fin.

ENDFORM.                    " get_imkey_intreno
