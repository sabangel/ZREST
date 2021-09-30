*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 29.07.2021 at 10:01:54
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTRESTCAMPOSHOL.................................*
DATA:  BEGIN OF STATUS_ZTRESTCAMPOSHOL               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRESTCAMPOSHOL               .
CONTROLS: TCTRL_ZTRESTCAMPOSHOL
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZTRESTCAMPOSSIST................................*
DATA:  BEGIN OF STATUS_ZTRESTCAMPOSSIST              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRESTCAMPOSSIST              .
CONTROLS: TCTRL_ZTRESTCAMPOSSIST
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZTRESTSECCIONHOL................................*
DATA:  BEGIN OF STATUS_ZTRESTSECCIONHOL              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRESTSECCIONHOL              .
CONTROLS: TCTRL_ZTRESTSECCIONHOL
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZTRESTCAMPOSHOL               .
TABLES: *ZTRESTCAMPOSSIST              .
TABLES: *ZTRESTSECCIONHOL              .
TABLES: ZTRESTCAMPOSHOL                .
TABLES: ZTRESTCAMPOSSIST               .
TABLES: ZTRESTSECCIONHOL               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
