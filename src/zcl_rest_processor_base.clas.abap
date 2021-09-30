CLASS zcl_rest_processor_base DEFINITION
  PUBLIC
  CREATE PUBLIC
  ABSTRACT.

  PUBLIC SECTION.
    INTERFACES zif_rest_registrable.
    ALIASES get_regs FOR zif_rest_registrable~get_regs.

    METHODS constructor.

  PROTECTED SECTION.

    DATA t_regs    TYPE zcl_rest_cte=>tt_rest_reg.
    DATA o_logger  TYPE REF TO zif_logger.
    DATA o_sys_dao TYPE REF TO zif_rest_sys_field_dao.

    "! Agregar un registro tipo log. <br/>
    "! id_fuente y req_id son obligatorios para poder loguear en la tabla zrest_log pero se puede dejar su llenado <br/>
    "! a la clase que se encarga de procesar los registros (tipo zif_rest_register)
    METHODS append_reg
      IMPORTING iv_id_fuente      TYPE zdserv_source_id
                iv_req_id         TYPE zdrestreqid
                iv_num_doc_ext    TYPE zdrestnumdocext
                iv_status         TYPE zdreststatus
                iv_operacion      TYPE zdrestoperacion
                iv_num_doc        TYPE zdrestnumdoc     OPTIONAL
                iv_num_doc_origen TYPE zdrestnumdocorig OPTIONAL
                iv_ejercicio      TYPE gjahr            OPTIONAL
                iv_ind_anula      TYPE zdrestindanula   OPTIONAL
                iv_message        TYPE bapi_msg         OPTIONAL.

    METHODS register_bapi_return_msges IMPORTING iv_id_fuente   TYPE zdserv_source_id OPTIONAL
                                                 iv_req_id      TYPE zdrestreqid      OPTIONAL
                                                 iv_num_doc_ext TYPE zdrestnumdocext
                                                 it_msges       TYPE bapiret2_t.

ENDCLASS.



CLASS zcl_rest_processor_base IMPLEMENTATION.

  METHOD constructor.
    o_sys_dao = NEW zcl_rest_sys_field_dao( ).
  ENDMETHOD.

  METHOD zif_rest_registrable~get_regs.
    rt_regs = me->t_regs.
  ENDMETHOD.

  METHOD append_reg.

    DATA v_icon TYPE icon_d.
    DATA t_ztrest_log TYPE zcl_rest_cte=>tt_ztrest_log.

    GET TIME STAMP FIELD DATA(v_ts).

    IF iv_status = zcl_rest_cte=>c_status_doc-err_datos OR iv_status = zcl_rest_cte=>c_status_doc-err_interno.
      v_icon = icon_display.
    ELSEIF iv_status = zcl_rest_cte=>c_status_doc-ok.
      v_icon = icon_okay.

      " Si llega en la integración un registro que se creo correctamente en SAP,
      " se valida que no existan num_doc_externos con errores anteriores
      " para modificar el icono a verde para que en la visualización del monitoreo
      " solo queden los que estan en error o que faltan por reprocesar
      DATA(t_data_num_doc_externos) = me->o_sys_dao->find_document_data_log(
        EXPORTING
          iv_id_fuente         = iv_id_fuente
          iv_operacion         = iv_operacion
          iv_num_doc_externo   = iv_num_doc_ext
      ).

      LOOP AT t_data_num_doc_externos ASSIGNING FIELD-SYMBOL(<s_data_num_doc_externos>).
        <s_data_num_doc_externos>-icon = icon_okay.
        APPEND <s_data_num_doc_externos> TO t_ztrest_log.
      ENDLOOP.

    ENDIF.

    DATA(s_reg) = VALUE ztrest_log(
    id_fuente      = iv_id_fuente
    req_id         = iv_req_id
    num_doc_ext    = iv_num_doc_ext
    icon           = v_icon
    status         = iv_status
    operacion      = iv_operacion
    num_doc        = iv_num_doc
    num_doc_origen = iv_num_doc_origen
    ejercicio      = iv_ejercicio
    ind_anula      = iv_ind_anula
    timestamp      = v_ts
    message        = iv_message
  ).

    " Añadimos registro nuevo para ser almacenado posteriormente en la tabla de BD ztrest_log.
    APPEND s_reg TO t_ztrest_log.

    MODIFY ztrest_log FROM TABLE t_ztrest_log.

    COMMIT WORK.

  ENDMETHOD.

  METHOD register_bapi_return_msges.

    " Crear log estandar SGL1
    o_logger = zcl_logger_factory=>create_log(
           object    = zcl_rest_cte=>c_object_log-operacionrestservice " OPERACIONRESTSERVICE
           subobject = iv_id_fuente
           desc      =  |{ iv_req_id }-{ iv_num_doc_ext } |
       ).

    LOOP AT it_msges ASSIGNING FIELD-SYMBOL(<s_msges>).

      CASE <s_msges>-type.

          " Se almacena en el LOG estandar solo los mensajes de error o warning
        WHEN zcl_rest_cte=>c_tipo_mensaje_bapi-e OR zcl_rest_cte=>c_tipo_mensaje_bapi-w.

          " Añadir registros en el log estandar SGL1
          o_logger->add( obj_to_log    = <s_msges>-message
                         type          = <s_msges>-type ).

          " Guardar registros en el log estandar SGL1
          o_logger->save( ).

        WHEN OTHERS.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
