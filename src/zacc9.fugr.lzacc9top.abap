FUNCTION-POOL ZACC9.                         "MESSAGE-ID ..

***************************************************************
**********************Intercompany Case Begin******************
***************************************************************
TYPE-POOLS iaomt.
***************************************************************
**********************Intercompany Case End********************
***************************************************************


TYPES: BEGIN OF return_type,
        type        LIKE bapiret2-type,
        id          LIKE bapiret2-id,
        number      LIKE bapiret2-number,
        message     LIKE bapiret2-message,
        log_no      LIKE bapiret2-log_no,
        log_msg_no  LIKE bapiret2-log_msg_no,
        message_v1  LIKE bapiret2-message_v1,
        message_v2  LIKE bapiret2-message_v2,
        message_v3  LIKE bapiret2-message_v3,
        message_v4  LIKE bapiret2-message_v4,
        parameter   LIKE bapiret2-parameter,
        row(10)     TYPE n,
        field       LIKE bapiret2-field,
        system      LIKE bapiret2-system,
      END OF return_type.

TYPES: BEGIN OF fica_hd_type,
           opbel(12)    TYPE c,
           blart_fkk(2) TYPE c,
           resky(30)    TYPE c,
           rfzas(30)    TYPE c,
           fikey(12)    TYPE c,
       END OF fica_hd_type,

       BEGIN OF fica_it_type,
            vkont(12) TYPE c,
            hvorg(4)  TYPE c,
            tvorg(4)  TYPE c,
            actv_account,
            vtref(20)    TYPE c,
            case_guid TYPE scmg_case_guid,
            reference_no(16) TYPE c,
            crmobj TYPE crmt_object_guid,
       END OF fica_it_type.


*Globale Datendefinitionen

DATA: gs_return TYPE bapiret2, "return_type,
      it_return TYPE STANDARD TABLE OF bapiret2, "return_type,

      it_acchd      TYPE STANDARD TABLE OF acchd,      "Belegkopf
      it_accit      TYPE STANDARD TABLE OF accit,      "Belegzeile
      it_acccr      TYPE STANDARD TABLE OF acccr,      "Währung
      it_accfi      TYPE STANDARD TABLE OF accfi,      "CPD
      it_acctx      TYPE STANDARD TABLE OF accbset,    "Steuern
      it_accit_pa   TYPE STANDARD TABLE OF accit_pa,   "CO-PA Merkmale
      it_acccr_pa   TYPE STANDARD TABLE OF acccr_pa,   "CO-PA Wertkomp
      it_accwt      TYPE STANDARD TABLE OF accit_wt,   "Quellensteuer
      it_bapi_acchd TYPE STANDARD TABLE OF accbapifd4, "Übermenge Kopf
      it_bapi_accit TYPE STANDARD TABLE OF accbapifd5, "Übermenge Zeile
      it_bapi_acccr TYPE STANDARD TABLE OF accbapifd6, "Übermenge Währ


      gs_acchd     TYPE acchd,
      gs_accit     TYPE accit,
      gs_acccr     TYPE acccr,
      gs_accfi     TYPE accfi,
      gs_acctx     TYPE accbset,
      gs_accit_pa  TYPE accit_pa,
      gs_acccr_pa  TYPE acccr_pa,
      gs_accwt     TYPE accit_wt,
      gs_fica_hd   TYPE fica_hd_type,
      gs_bapi_acchd TYPE accbapifd4,
      gs_bapi_accit TYPE accbapifd5,
      gs_bapi_acccr TYPE accbapifd6.

DATA  gv_x001  LIKE x001.                                   "Note1362607

DATA: BEGIN OF gs_aw,
        mandt     LIKE acchd-mandt,
        awtyp     LIKE acchd-awtyp,
        awkey     LIKE bapiache09-obj_key,
        awref     LIKE acchd-awref,
        aworg     LIKE acchd-aworg,
        awsys     LIKE acchd-awsys,
        awref_rev LIKE accit-awref_rev,
        aworg_rev LIKE accit-aworg_rev,
      END   OF gs_aw.


DATA: gd_waers LIKE acccr-waers.
DATA  gd_ldgrp LIKE acchd-ldgrp.
DATA  gd_bukrs LIKE accit-bukrs.
DATA  gd_blart LIKE accit-blart.
DATA  gd_budat LIKE accit-budat.
DATA  gd_belnr LIKE accit-belnr.
DATA  gd_gjahr LIKE accit-gjahr.

DATA: gd_xtxit LIKE ttxd-xtxit.                           "Note 1859478

INCLUDE: is01ad00.
TABLES: t006.

CONSTANTS func_fica_partner TYPE edfunction
                            VALUE 'BUP_BUT000_SELECT_WITH_GUID'.

CLASS cl_exithandler DEFINITION LOAD.
DATA: g_exit TYPE REF TO if_ex_acc_document.




**************************ALT:************************************

*INCLUDE: rkporvtp,
*
*CONSTANTS: con_process_default LIKE trwpr-process VALUE space,
*           con_process_banf    LIKE trwpr-process VALUE 'BANF    ',
*           con_process_best    LIKE trwpr-process VALUE 'BEST    '.
