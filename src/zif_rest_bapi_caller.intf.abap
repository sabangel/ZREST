INTERFACE zif_rest_bapi_caller
  PUBLIC .

  "! Llamar una BAPI
  "! @parameter iv_num_doc_ext | id del documento externo
  "! @parameter it_structs     | Lista de estructuras para mapear en la bapi
  METHODS call
    IMPORTING iv_num_doc_ext TYPE zdrestnumdocext
              it_structs     TYPE zcl_rest_cte=>tt_bapi_struct
    RETURNING VALUE(rv_process_ok) TYPE abap_bool
    RAISING   zcx_rest_ex.

ENDINTERFACE.
