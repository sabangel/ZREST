INTERFACE zif_rest_finder_dao
  PUBLIC .

  "! Consultar los objetos creados por una petición a la integración erpdoc
  "! @parameter it_url_attrib | Atributos provenientes de la petición get. Son los parámetros de la consulta
  "! @parameter r_result | retultado genérico de la consulta
  METHODS find_data_log
    IMPORTING iv_oper        TYPE zdrestoperacion
              iv_id_fuente   TYPE zdserv_source_id
              ir_req_id      TYPE etrange_string_tab OPTIONAL
              ir_num_doc_ext TYPE etrange_string_tab OPTIONAL
              ir_fecha       TYPE etrange_string_tab OPTIONAL
              ir_status      TYPE etrange_string_tab OPTIONAL
    CHANGING  ct_result      TYPE zcl_rest_cte=>tt_datos_log
    RAISING   zcx_rest_ex.

ENDINTERFACE.
