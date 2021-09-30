*----------------------------------------------------------------------*
*   INCLUDE LACC9F30                                                   *
*----------------------------------------------------------------------*




FORM fill_and_check_copa

  TABLES t_criteria   STRUCTURE bapiackec9
         t_valuefield STRUCTURE bapiackev9.

  CHECK: NOT t_criteria[]   IS INITIAL OR
         NOT t_valuefield[] IS INITIAL.

  SORT it_accit BY awtyp awref aworg posnr.                "note 660591

  PERFORM fill_accit_pa
          TABLES t_criteria
                 t_valuefield.

  PERFORM fill_acccr_pa
          TABLES t_valuefield.

  PERFORM check_copa.

ENDFORM.                               " FILL_AND_CHECK_COPA


*---------------------------------------------------------------------*
*       FORM FILL_ACCIT_PA                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  T_CRITERIA                                                    *
*  -->  T_VALUEFIELD                                                  *
*---------------------------------------------------------------------*
FORM fill_accit_pa
  TABLES t_criteria   STRUCTURE bapiackec9
         t_valuefield STRUCTURE bapiackev9.

  DATA: ld_tabix     LIKE sy-tabix,
        ld_tabix_pa  LIKE sy-tabix,
        ld_fieldname LIKE bapiackeva-fieldname,
        ld_cest1     LIKE cest1,
        ld_new.

  CLEAR gs_accit.

  FIELD-SYMBOLS: <value>.

  LOOP AT t_criteria.
    ld_tabix = sy-tabix.
*   check ITEMNO_ACC
    IF t_criteria-itemno_acc IS INITIAL.
      PERFORM append_msg_to_return
        USING 'E'             "TYPE
              'RW'            "ID
              '602'           "NUMBER
              'ITEMNO_ACC'    "MESSAGE_ONE
              ' '             "MESSAGE_TWO
              'CRITERIA'      "PARAMETER
              ld_tabix        "ROW
              'ITEMNO_ACC'.   "FIELD
      CONTINUE.
    ENDIF.
*   check FIELDNAME
    IF t_criteria-fieldname IS INITIAL.
      PERFORM append_msg_to_return
        USING 'E'             "TYPE
              'RW'            "ID
              '602'           "NUMBER
              'FIELDNAME'     "MESSAGE_ONE
              ' '             "MESSAGE_TWO
              'CRITERIA'      "PARAMETER
              ld_tabix        "ROW
              'FIELDNAME'.    "FIELD
      CONTINUE.
    ENDIF.

    IF t_criteria-itemno_acc <> gs_accit-posnr.
*     check new item in table ACCIT
      READ TABLE it_accit INTO gs_accit
           WITH KEY posnr = t_criteria-itemno_acc
           BINARY SEARCH.
      IF NOT sy-subrc IS INITIAL.
        PERFORM append_msg_to_return
          USING 'E'             "TYPE
                'RW'            "ID
                '621'           "NUMBER
                t_criteria-itemno_acc  "MESSAGE_ONE
                ' '             "MESSAGE_TWO
                'CRITERIA'      "PARAMETER
                ld_tabix        "ROW
                ' '.    "FIELD
      ENDIF.

      IF gs_accit-bapi_param <> 'ACCOUNTGL' AND             "Note1259543
         gs_accit-bapi_param <> 'ACCOOUNTTAX'.              "Note1259543
         CONTINUE.                                          "Note1259543
      ENDIF.                                                "Note1259543

    ENDIF.

    READ TABLE it_accit_pa INTO gs_accit_pa
         WITH KEY posnr = t_criteria-itemno_acc
         BINARY SEARCH.

    ld_tabix_pa = sy-tabix.
    IF sy-subrc IS INITIAL.
      ld_new = space.
    ELSE.
      CLEAR gs_accit_pa.
      MOVE-CORRESPONDING: gs_accit TO ld_cest1.
      MOVE: gs_accit-kunag TO ld_cest1-kndnr,
            gs_accit-matnr TO ld_cest1-artnr.
      MOVE-CORRESPONDING: ld_cest1         TO gs_accit_pa,
                          gs_aw           TO gs_accit_pa.
      gs_accit_pa-posnr = t_criteria-itemno_acc.
      ld_new = 'X'.
    ENDIF.

*   move values
    ASSIGN COMPONENT t_criteria-fieldname OF
           STRUCTURE gs_accit_pa TO <value>.
    IF sy-subrc IS INITIAL.
      <value> = t_criteria-character.
    ELSE.
      PERFORM append_msg_to_return
        USING 'E'             "TYPE
              'RW'            "ID
              '615'           "NUMBER
              t_criteria-fieldname "MESSAGE_ONE
              t_criteria-itemno_acc "MESSAGE_TWO
              'CRITERIA'      "PARAMETER
              ld_tabix        "ROW
              'FIELDNAME'.    "FIELD
    ENDIF.

    IF ld_new = 'X'.
      INSERT gs_accit_pa INTO it_accit_pa INDEX ld_tabix_pa.
    ELSE.
      MODIFY it_accit_pa FROM gs_accit_pa INDEX ld_tabix_pa.
    ENDIF.

  ENDLOOP.

  LOOP AT t_valuefield
       WHERE NOT base_uom     IS INITIAL
       OR    NOT base_uom_iso IS INITIAL.
    ld_tabix = sy-tabix.
    IF t_valuefield-itemno_acc IS INITIAL.
      PERFORM append_msg_to_return
        USING 'E'             "TYPE
              'RW'            "ID
              '602'           "NUMBER
              'ITEMNO_ACC'    "MESSAGE_ONE
              ' '             "MESSAGE_TWO
              'VALUEFIELD'    "PARAMETER
              ld_tabix        "ROW
              'ITEMNO_ACC'.   "FIELD
      CONTINUE.
    ENDIF.
    IF t_valuefield-fieldname IS INITIAL.
      PERFORM append_msg_to_return
        USING 'E'             "TYPE
              'RW'            "ID
              '602'           "NUMBER
              'FIELDNAME'     "MESSAGE_ONE
              ' '             "MESSAGE_TWO
              'VALUEFIELD'    "PARAMETER
              ld_tabix        "ROW
              'FIELDNAME'.    "FIELD
      CONTINUE.
    ENDIF.
    IF t_valuefield-base_uom IS INITIAL.
*     convert unit from ISO to SAP
      PERFORM convert_unit_from_iso
              USING 'VALUEFIELD'
                    ld_tabix
                    t_valuefield-base_uom_iso
                    t_valuefield-base_uom.
    ENDIF.

*   READ TABLE IT_ACCIT
*        WITH KEY POSNR = T_CRITERIA-ITEMNO_ACC
*        BINARY SEARCH TRANSPORTING NO FIELDS.
*   MESSAGE E617.

    READ TABLE it_accit_pa INTO gs_accit_pa
         WITH KEY posnr = t_valuefield-itemno_acc
         BINARY SEARCH.

    ld_tabix_pa = sy-tabix.
    IF sy-subrc IS INITIAL.
      ld_new = space.
    ELSE.
      CLEAR gs_accit_pa.
      MOVE-CORRESPONDING gs_aw TO gs_accit_pa.
      gs_accit_pa-posnr = t_valuefield-itemno_acc.
      ld_new = 'X'.
    ENDIF.

*   process criteria from quantityfields
    ld_fieldname       = t_valuefield-fieldname.
    ld_fieldname+25(3) = '_ME'.
    CONDENSE ld_fieldname NO-GAPS.
    ASSIGN COMPONENT ld_fieldname OF
           STRUCTURE gs_accit_pa TO <value>.
    IF sy-subrc IS INITIAL.
      <value> = t_valuefield-base_uom.
    ELSE.
      PERFORM append_msg_to_return
        USING 'E'             "TYPE
              'RW'            "ID
              '615'           "NUMBER
              t_valuefield-fieldname "MESSAGE_ONE
              t_valuefield-itemno_acc "MESSAGE_TWO
              'VALUEFIELD'    "PARAMETER
              ld_tabix        "ROW
              'FIELDNAME'.    "FIELD
    ENDIF.

    IF ld_new = 'X'.
      INSERT gs_accit_pa INTO it_accit_pa INDEX ld_tabix_pa.
    ELSE.
      MODIFY it_accit_pa FROM gs_accit_pa INDEX ld_tabix_pa.
    ENDIF.

  ENDLOOP.

ENDFORM.                               " FILL_ACCIT_PA

*---------------------------------------------------------------------*
*      Form  FILL_ACCCR_PA
*---------------------------------------------------------------------*
FORM fill_acccr_pa

  TABLES t_valuefield STRUCTURE bapiackeva.

  DATA: ld_tabix     LIKE sy-tabix,
        ld_tabix_pa  LIKE sy-tabix,
        ld_new.

  FIELD-SYMBOLS: <value>.

  LOOP AT t_valuefield.
    ld_tabix = sy-tabix.
    IF t_valuefield-itemno_acc IS INITIAL.
      PERFORM append_msg_to_return
        USING 'E'             "TYPE
              'RW'            "ID
              '602'           "NUMBER
              'ITEMNO_ACC'    "MESSAGE_ONE
              ' '             "MESSAGE_TWO
              'VALUEFIELD'    "PARAMETER
              ld_tabix        "ROW
              'ITEMNO_ACC'.    "FIELD
      CONTINUE.
    ENDIF.
    IF t_valuefield-fieldname IS INITIAL.
      PERFORM append_msg_to_return
        USING 'E'             "TYPE
              'RW'            "ID
              '602'           "NUMBER
              'FIELDNAME'     "MESSAGE_ONE
              ' '             "MESSAGE_TWO
              'VALUEFIELD'    "PARAMETER
              ld_tabix        "ROW
              'FIELDNAME'.    "FIELD
      CONTINUE.
    ENDIF.
    IF ( NOT t_valuefield-currency     IS INITIAL OR
         NOT t_valuefield-currency_iso IS INITIAL )  AND
       ( NOT t_valuefield-base_uom     IS INITIAL OR
         NOT t_valuefield-base_uom_iso IS INITIAL ).
      PERFORM append_msg_to_return
        USING 'E'             "TYPE
              'RW'            "ID
              '622'           "NUMBER
              t_valuefield-itemno_acc "MESSAGE_ONE
              ' '             "MESSAGE_TWO
              'VALUEFIELD'    "PARAMETER
              ld_tabix        "ROW
              ' '.            "FIELD
      CONTINUE.
    ENDIF.

    IF t_valuefield-curr_type IS INITIAL.
      t_valuefield-curr_type = '00'.
    ENDIF.

*   READ TABLE IT_ACCIT
*        WITH KEY POSNR = T_CRITERIA-ITEMNO_ACC
*        BINARY SEARCH TRANSPORTING NO FIELDS.
*   MESSAGE E617.

    READ TABLE it_acccr_pa INTO gs_acccr_pa
         WITH KEY posnr = t_valuefield-itemno_acc
                  curtp = t_valuefield-curr_type
         BINARY SEARCH.

    ld_tabix_pa = sy-tabix.
    IF sy-subrc IS INITIAL.
      ld_new = space.
    ELSE.
      CLEAR gs_acccr_pa.
      MOVE-CORRESPONDING gs_aw TO gs_acccr_pa.
      gs_acccr_pa-posnr = t_valuefield-itemno_acc.
      gs_acccr_pa-curtp = t_valuefield-curr_type.
      ld_new = 'X'.
    ENDIF.

    ASSIGN COMPONENT t_valuefield-fieldname OF
           STRUCTURE gs_acccr_pa TO <value>.
    IF sy-subrc IS INITIAL.

      IF NOT t_valuefield-currency     IS INITIAL OR
         NOT t_valuefield-currency_iso IS INITIAL.
*       process currencfields
        PERFORM convert_curr_to_internal
                USING 'VALUEFIELD'
                      ld_tabix
                      t_valuefield-currency
                      t_valuefield-currency_iso
                      t_valuefield-amt_valcom
                      <value>.
        IF gs_acccr_pa-waers IS INITIAL.
          gs_acccr_pa-waers = t_valuefield-currency.
        ELSEIF gs_acccr_pa-waers <> t_valuefield-currency.
          PERFORM append_msg_to_return
            USING 'E'             "TYPE
                  'RW'            "ID
                  '620'           "NUMBER
                  t_valuefield-currency "MESSAGE_ONE
                  gs_acccr_pa-waers "MESSAGE_TWO
                  'VALUEFIELD'    "PARAMETER
                  ld_tabix        "ROW
                  'CURRENCY'.     "FIELD
        ENDIF.
      ELSEIF NOT t_valuefield-base_uom     IS INITIAL OR
             NOT t_valuefield-base_uom_iso IS INITIAL.
*       process quantityfields
        <value> = t_valuefield-qua_valcom.
      ELSE.
        PERFORM append_msg_to_return
          USING 'E'             "TYPE
                'RW'            "ID
                '602'           "NUMBER
                'CURRENCY'      "MESSAGE_ONE
                ' '             "MESSAGE_TWO
                'VALUEFIELD'    "PARAMETER
                ld_tabix        "ROW
                'CURRENCY'.     "FIELD
      ENDIF.
    ELSE.
*     message e616.
      PERFORM append_msg_to_return
        USING 'E'             "TYPE
              'RW'            "ID
              '616'           "NUMBER
              t_valuefield-fieldname "MESSAGE_ONE
              t_valuefield-itemno_acc "MESSAGE_TWO
              'VALUEFIELD'    "PARAMETER
              ld_tabix        "ROW
              'FIELDNAME'.     "FIELD
    ENDIF.

    IF ld_new = 'X'.
      INSERT gs_acccr_pa INTO it_acccr_pa INDEX ld_tabix_pa.
    ELSE.
      MODIFY it_acccr_pa FROM gs_acccr_pa INDEX ld_tabix_pa.
    ENDIF.

  ENDLOOP.

ENDFORM.                               " FILL_ACCCR_PA


*---------------------------------------------------------------------*
*       FORM CHECK_COPA                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM check_copa.

  DATA: ld_criteria_f LIKE cest1,
        ld_criteria_v LIKE copacrit,
        ld_tabix_acc  LIKE sy-tabix,
        ld_tabix_copa LIKE sy-tabix.

  FIELD-SYMBOLS <accit_pa> LIKE accit_pa.                  "note 582802

  LOOP AT it_acccr_pa INTO gs_acccr_pa WHERE curtp = '00'.

    READ TABLE it_accit INTO gs_accit WITH KEY
               awtyp = gs_acccr_pa-awtyp
               awref = gs_acccr_pa-awref
               aworg = gs_acccr_pa-aworg
               posnr = gs_acccr_pa-posnr
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      ld_tabix_acc = sy-tabix.
      MOVE-CORRESPONDING gs_accit TO ld_criteria_f.
    ELSE.
      PERFORM append_msg_to_return
        USING 'E'             "TYPE
              'RW'            "ID
              '617'           "NUMBER
              gs_acccr_pa-posnr "MESSAGE_ONE
              ' '             "MESSAGE_TWO
              'VALUEFIELD'    "PARAMETER
              ' '             "ROW
              ' '.            "FIELD
      CONTINUE.
    ENDIF.

    READ TABLE it_accit_pa INTO gs_accit_pa WITH KEY
               awtyp = gs_acccr_pa-awtyp
               awref = gs_acccr_pa-awref
               aworg = gs_acccr_pa-aworg
               posnr = gs_acccr_pa-posnr
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      ld_tabix_copa = sy-tabix.
      MOVE-CORRESPONDING gs_accit_pa TO ld_criteria_f.
      MOVE-CORRESPONDING gs_accit_pa TO ld_criteria_v.
    ELSE.
*     nur Standardmerkmale
      CLEAR: ld_tabix_copa,
             ld_criteria_v.
    ENDIF.

    CALL FUNCTION 'COPA_DOCUMENT_EXTERNAL_CHECK'
         EXPORTING
              i_cest1       = ld_criteria_f
              i_criteria_v  = ld_criteria_v
         IMPORTING
              e_paobjnr     = gs_accit-paobjnr
              e_cest1       = ld_criteria_f
              e_criteria_v  = ld_criteria_v
         EXCEPTIONS
              error_message = 1
              OTHERS        = 2.

    IF sy-subrc IS INITIAL.
*     insert PAOBJNR in ACCIT
      MODIFY it_accit FROM gs_accit INDEX ld_tabix_acc.
      IF NOT ld_tabix_copa IS INITIAL.
        MOVE-CORRESPONDING: ld_criteria_f TO gs_accit_pa,
                            ld_criteria_v TO gs_accit_pa.
        gs_accit_pa-awtyp = gs_acccr_pa-awtyp.
        gs_accit_pa-awref = gs_acccr_pa-awref.
        gs_accit_pa-aworg = gs_acccr_pa-aworg.
        gs_accit_pa-posnr = gs_acccr_pa-posnr.
        MODIFY it_accit_pa FROM gs_accit_pa INDEX ld_tabix_copa.
      ENDIF.
    ELSE.
      PERFORM error_from_system
              USING 'CRITERIA' ld_tabix_copa space.        "note1052410
    ENDIF.

  ENDLOOP.

* Keine Wertfelder übergeben => nur Ableitung des Ergebnisobjekts
  IF NOT sy-subrc IS INITIAL.                              "note 582802
    LOOP AT it_accit_pa ASSIGNING <accit_pa>.              "note 582802
      ld_tabix_copa = sy-tabix.                            "note1052410
      READ TABLE it_accit INTO gs_accit WITH KEY           "note 582802
                 awtyp = <accit_pa>-awtyp                  "note 582802
                 awref = <accit_pa>-awref                  "note 582802
                 aworg = <accit_pa>-aworg                  "note 582802
                 posnr = <accit_pa>-posnr                  "note 582802
                 BINARY SEARCH.                            "note 582802
      IF sy-subrc IS INITIAL.                              "note 582802
        ld_tabix_acc = sy-tabix.                           "note 582802
        MOVE-CORRESPONDING gs_accit   TO ld_criteria_f.    "note 582802
        MOVE-CORRESPONDING <accit_pa> TO ld_criteria_f.    "note 582802
        MOVE-CORRESPONDING <accit_pa> TO ld_criteria_v.    "note 582802
        CALL FUNCTION 'COPA_DOCUMENT_EXTERNAL_CHECK'       "note 582802
           EXPORTING                                       "note 582802
              i_cest1       = ld_criteria_f                "note 582802
              i_criteria_v  = ld_criteria_v                "note 582802
           IMPORTING                                       "note 582802
              e_paobjnr     = gs_accit-paobjnr             "note 582802
           EXCEPTIONS                                      "note 582802
              error_message = 1                            "note 582802
              OTHERS        = 2.                           "note 582802
        IF sy-subrc IS INITIAL.                            "note 582802
          MODIFY it_accit FROM gs_accit INDEX ld_tabix_acc."note 582802
        ELSE.                                              "note 582802
          PERFORM error_from_system                        "note 582802
                  USING 'CRITERIA' ld_tabix_copa space.    "note1052410
        ENDIF.                                             "note 582802
      ENDIF.                                               "note 582802
    ENDLOOP.                                               "note 582802
  ENDIF.                                                   "note 582802

ENDFORM.                               " CHECK_COPA
