INTERFACE zif_rest_finder
  PUBLIC .

  "! Consultar los objetos creados por una petición a la integración erpdoc
  "! @parameter it_url_attrib | Atributos provenientes de la petición get. Son los parámetros de la consulta
  "! @parameter r_result | retultado genérico de la consulta
  METHODS find
    IMPORTING iv_oper         TYPE zdrestoperacion
              iv_id_fuente    TYPE zdserv_source_id
              it_url_attrib   TYPE tihttpnvp
    RETURNING VALUE(r_result) TYPE REF TO data
    RAISING   zcx_rest_ex.

ENDINTERFACE.
