*----------------------------------------------------------------------*
***INCLUDE LACC9F80 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  fill_with_tax
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ACCOUNTWT  text
*----------------------------------------------------------------------*
FORM fill_with_tax  TABLES   ACCOUNTWT STRUCTURE bapiacwt09.

  FIELD-SYMBOLS: <wt> LIKE bapiacwt09,
                 <it> like accit,
                 <cr> like acccr.

  DATA: save_bukrs LIKE bkpf-bukrs,
        lc         LIKE tcurc-waers,
        lc2        LIKE tcurc-waers,
        lc3        LIKE tcurc-waers,
        wt_tabix   LIKE sy-tabix,
        it_tabix   LIKE sy-tabix.

  CHECK NOT ACCOUNTWT[] IS INITIAL.

  SORT it_accit BY posnr.
  SORT it_acccr BY posnr curtp.                             "Note1348253

  LOOP AT ACCOUNTWT ASSIGNING <wt>.
    wt_tabix = sy-tabix.


    READ TABLE it_accit assigning <it>
                   WITH KEY posnr = <wt>-itemno_acc
                   BINARY SEARCH.
    CHECK sy-subrc IS INITIAL
    and  <it>-koart CA 'DK'.
    it_tabix = sy-tabix.

    READ TABLE it_acccr assigning <cr>
                   WITH KEY posnr = <wt>-itemno_acc
                            curtp = '00'
                   BINARY SEARCH.


    IF <it>-bukrs <> save_bukrs.
      PERFORM get_parallel_currencies
          USING <it>-bukrs
          CHANGING lc lc2 lc3.
      save_bukrs = <it>-bukrs.
    ENDIF.

    CLEAR gs_accwt.
    gs_accwt-WITHT     = <wt>-WT_TYPE.
    gs_accwt-WT_WITHCD = <wt>-WT_CODE.
    gs_accwt-WT_BASMAN = <wt>-BAS_AMT_IND.
    gs_accwt-WT_AMNMAN = <wt>-MAN_AMT_IND.

    perform convert_curr_to_internal USING
      'ACCOUNTWT' wt_tabix:

      <cr>-waers space <wt>-BAS_AMT_TC  gs_accwt-WT_QSSHB,
      lc         space <wt>-BAS_AMT_LC  gs_accwt-WT_QSSHH,
      lc2        space <wt>-BAS_AMT_L2  gs_accwt-WT_QSSH2,
      lc3        space <wt>-BAS_AMT_L3  gs_accwt-WT_QSSH3,

      <cr>-waers space <wt>-MAN_AMT_TC  gs_accwt-WT_QBUIHB,
      lc         space <wt>-MAN_AMT_LC  gs_accwt-WT_QBUIHH,
      lc2        space <wt>-MAN_AMT_L2  gs_accwt-WT_QBUIH2,
      lc3        space <wt>-MAN_AMT_L3  gs_accwt-WT_QBUIH3,

      <cr>-waers space <wt>-AWH_AMT_TC  gs_accwt-WT_WWRBTR,
      lc         space <wt>-AWH_AMT_LC  gs_accwt-WT_WDMBTR,
      lc2        space <wt>-AWH_AMT_L2  gs_accwt-WT_WDMBT2,
      lc3        space <wt>-AWH_AMT_L3  gs_accwt-WT_WDMBT3.


    gs_accwt-wt_key = it_tabix.
    append gs_accwt to it_accwt.

*   update ACCIT
    <it>-wt_key =  gs_accwt-wt_key.
  ENDLOOP.
ENDFORM.                    " fill_with_tax

*&---------------------------------------------------------------------*
*&      Form  get_parallel_currencies
*&---------------------------------------------------------------------*
*       determine additional currencies for a company code
*----------------------------------------------------------------------*
*      -->p_comp  company code
*      <--p_lc    first local
*         p_lc2   second local
*         p_lc3   third local currency
*----------------------------------------------------------------------*
FORM get_parallel_currencies
                    USING    p_comp TYPE bukrs
                    CHANGING p_lc   TYPE waers
                             p_lc2  TYPE waers
                             p_lc3  TYPE waers.

  DATA: l_t001 LIKE t001,
        l_x001 LIKE x001.

  CLEAR: p_lc, p_lc2, p_lc3.

* get first local currency

  CALL FUNCTION 'FI_COMPANY_CODE_DATA'
    EXPORTING
      I_BUKRS            = p_comp
    IMPORTING
      E_T001             = l_t001
    EXCEPTIONS
      SYSTEM_ERROR       = 0
      OTHERS             = 0.

  p_lc =  l_t001-waers.

* get additional local currencies

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      I_BUKRS                      = p_comp
    IMPORTING
      E_X001                       = l_x001
    EXCEPTIONS
      OTHERS                       = 0.

  p_lc2 = l_x001-hwae2.
  p_lc3 = l_x001-hwae3.

ENDFORM.                    " get_parallel_currencies
