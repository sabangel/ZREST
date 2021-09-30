INTERFACE zif_rest_creator
  PUBLIC .

  METHODS create
    IMPORTING iv_payload TYPE string
              iv_req_id  TYPE zdrestreqid OPTIONAL
    RAISING   zcx_rest_ex.

ENDINTERFACE.
