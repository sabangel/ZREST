*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 29.06.2021 at 15:06:11
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTRESTEQUIVALENC................................*
DATA:  BEGIN OF STATUS_ZTRESTEQUIVALENC              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRESTEQUIVALENC              .
CONTROLS: TCTRL_ZTRESTEQUIVALENC
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZTRESTEQUIVALENC              .
TABLES: ZTRESTEQUIVALENC               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
