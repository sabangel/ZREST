CLASS zcl_rest_finder_dao DEFINITION
  INHERITING FROM zcl_fw_dao
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_rest_finder_dao.
    ALIASES find_data_log FOR zif_rest_finder_dao~find_data_log.

  PROTECTED SECTION.

    METHODS register_bobjs REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_rest_finder_dao IMPLEMENTATION.

  METHOD zif_rest_finder_dao~find_data_log.

    DATA t_data_log TYPE zcl_rest_cte=>tt_datos_log.

    SELECT id_fuente req_id num_doc_ext status num_doc timestamp message
          FROM ztrest_log
          INTO TABLE t_data_log
          WHERE id_fuente   = iv_id_fuente    AND
                operacion   = iv_oper         AND
                req_id      IN ir_req_id      AND
                num_doc_ext IN ir_num_doc_ext AND
                status      IN ir_status      AND
                timestamp   IN ir_fecha.

    IF sy-subrc EQ 0.
      ct_result = t_data_log.
    ENDIF.

  ENDMETHOD.

  METHOD register_bobjs.

    add_bobj( iv_bobj = 'REST'  ).

  ENDMETHOD.

ENDCLASS.
