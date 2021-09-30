INTERFACE zif_rest_sys_field_dao
  PUBLIC .

  "! Consultar configuración de campos REST
  "! @parameter iv_operacion  | id operación
  "! @parameter iv_fuente     | id fuente peticiones
  "! @parameter iv_buf_result | Bufferear el resultado de la consulta en variable estática
  METHODS find_source_fields
    IMPORTING iv_operacion     TYPE zdrestoperacion
              iv_id_fuente     TYPE zdserv_source_id
              iv_buf_result    TYPE abap_bool DEFAULT abap_false
    RETURNING VALUE(rt_fields) TYPE zcl_rest_cte=>tt_conf_fields.

  METHODS find_duplicate_documents IMPORTING iv_id_fuente        TYPE zdserv_source_id
                                             iv_operacion        TYPE zdrestoperacion
                                             it_doc_ext          TYPE /bofu/t_fbi_value_range
                                   RETURNING VALUE(rt_duplicate) TYPE /eacc/fobu_t_string.

  METHODS find_source_fields_get
    IMPORTING iv_operacion     TYPE zdrestoperacion
              iv_id_fuente     TYPE zdserv_source_id
              iv_seccion       TYPE zfied_secc
              iv_buf_result    TYPE abap_bool DEFAULT abap_false
    RETURNING VALUE(rt_fields) TYPE zcl_rest_cte=>tt_conf_fields.

  METHODS find_document_data_log IMPORTING iv_id_fuente       TYPE zdserv_source_id
                                           iv_operacion       TYPE zdrestoperacion
                                           iv_num_doc_externo TYPE zdrestnumdocext
                                 RETURNING VALUE(rt_data_log) TYPE zcl_rest_cte=>tt_ztrest_log.

ENDINTERFACE.
