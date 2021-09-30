CLASS zcl_rest_sys_field_dao DEFINITION
  INHERITING FROM zcl_fw_dao
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_rest_sys_field_dao.
    ALIASES find_source_fields     FOR zif_rest_sys_field_dao~find_source_fields.
    ALIASES find_source_fields_get FOR zif_rest_sys_field_dao~find_source_fields_get.
    ALIASES find_document_data_log FOR zif_rest_sys_field_dao~find_document_data_log.

  PROTECTED SECTION.

    METHODS register_bobjs REDEFINITION.

  PRIVATE SECTION.

    CLASS-DATA t_fields TYPE zcl_rest_cte=>tt_conf_fields.

ENDCLASS.



CLASS zcl_rest_sys_field_dao IMPLEMENTATION.

  METHOD zif_rest_sys_field_dao~find_source_fields.

    IF iv_buf_result EQ abap_true AND t_fields IS NOT INITIAL.
      rt_fields = t_fields.
      RETURN.
    ENDIF.

    SELECT
      c~operacion,
      c~seccion,
      c~campo_holistico,
      c~id_fuente,
      c~opcional,
      s~nombre_hol,
      estruc_abap,
      campo_abap,
      h~descripcion,
      h~longitud
    FROM ztrestcampossist AS c
    INNER JOIN ztrestcamposhol  AS h
    ON c~operacion       EQ h~operacion AND
       c~seccion         EQ h~seccion   AND
       c~campo_holistico EQ h~campo_holistico
    INNER JOIN ztrestseccionhol AS s
    ON c~operacion       EQ s~operacion AND
       c~seccion         EQ s~seccion
    INTO TABLE @rt_fields
    WHERE c~operacion EQ @iv_operacion
      AND c~id_fuente EQ @iv_id_fuente.

    "Tratamiento bufereo
    t_fields = COND #( WHEN iv_buf_result EQ abap_true THEN rt_fields ).

  ENDMETHOD.

  METHOD register_bobjs.

    add_bobj( iv_bobj = 'REST'  ).

  ENDMETHOD.

  METHOD zif_rest_sys_field_dao~find_duplicate_documents.

    SELECT num_doc_ext
           FROM ztrest_log
           INTO TABLE rt_duplicate
           WHERE id_fuente   =  iv_id_fuente AND
                 num_doc_ext IN it_doc_ext AND
                 operacion   =  iv_operacion AND
                 status      =  zcl_rest_cte=>c_status_doc-ok.

    IF sy-subrc EQ 0.
    ENDIF.
  ENDMETHOD.

  METHOD zif_rest_sys_field_dao~find_source_fields_get.

    IF iv_buf_result EQ abap_true AND t_fields IS NOT INITIAL.
      rt_fields = t_fields.
      RETURN.
    ENDIF.

    SELECT
      c~operacion,
      c~seccion,
      c~campo_holistico,
      c~id_fuente,
      c~opcional,
      s~nombre_hol,
      estruc_abap,
      campo_abap,
      h~descripcion,
      h~longitud
    FROM ztrestcampossist AS c
    INNER JOIN ztrestcamposhol  AS h
    ON c~operacion       EQ h~operacion AND
       c~seccion         EQ h~seccion   AND
       c~campo_holistico EQ h~campo_holistico
    INNER JOIN ztrestseccionhol AS s
    ON c~operacion       EQ s~operacion AND
       c~seccion         EQ s~seccion
    INTO TABLE @rt_fields
    WHERE c~operacion EQ @iv_operacion
      AND c~seccion   EQ @iv_seccion
      AND c~id_fuente EQ @iv_id_fuente.

    IF sy-subrc EQ 0.
      t_fields = COND #( WHEN iv_buf_result EQ abap_true THEN rt_fields ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_rest_sys_field_dao~find_document_data_log.

    SELECT *
       FROM ztrest_log
       INTO TABLE rt_data_log
       WHERE id_fuente   = iv_id_fuente AND
             num_doc_ext = iv_num_doc_externo AND
             operacion   = iv_operacion.

    IF sy-subrc EQ 0.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
