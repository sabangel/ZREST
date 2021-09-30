INTERFACE zif_rest_adapter
  PUBLIC .

  "! Modificar, homologar, completar el documento entrante y entregar las estructuras
  "! @parameter ir_doc     | Referencia a estructura de documento
  "! @parameter ro_structs | Estructuras de salida
  METHODS adapt
    IMPORTING ir_doc            TYPE REF TO data
    RETURNING VALUE(rt_structs) TYPE zcl_rest_cte=>tt_bapi_struct
    RAISING   zcx_rest_ex.

ENDINTERFACE.
