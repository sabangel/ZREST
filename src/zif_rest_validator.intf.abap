INTERFACE zif_rest_validator
  PUBLIC.

  "! Valida el request
  "! @parameter io_req | Request
  "! @parameter iv_oper | Operación a realizar
  "! @parameter et_msg | Mensajes de la validación
  "! @parameter rv_ok | Indica si validación ok o no
  METHODS validate
    IMPORTING io_req       TYPE REF TO if_rest_request
    EXPORTING et_msg       TYPE bapiret2_t
    RETURNING VALUE(rv_ok) TYPE abap_bool.

ENDINTERFACE.
