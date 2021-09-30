*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGRESTCAMPOS
*   generation date: 29.07.2021 at 10:01:54
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGRESTCAMPOS       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
