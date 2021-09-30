INTERFACE zif_rest_registrable
  PUBLIC.

  "! Obtener registros generados por el proceso BAPI
  METHODS get_regs
    RETURNING VALUE(rt_regs) TYPE zcl_rest_cte=>tt_rest_reg.


ENDINTERFACE.
