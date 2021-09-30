CLASS zcl_rest_cte DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " TIPOS
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TYPES ty_url_section     TYPE string.
    TYPES ty_json_field_name TYPE string.
    TYPES ty_res_id          TYPE char16.

    TYPES tt_rest_reg TYPE SORTED TABLE OF ztrest_log
        WITH NON-UNIQUE KEY id_fuente req_id.

    TYPES: BEGIN OF ty_conf_fields,
             operacion   TYPE zdrestoperacion,
             seccion     TYPE zfied_secc,
             campo_hol   TYPE zfied_holi,
             id_fuente   TYPE zdserv_source_id,
             opcional    TYPE xfeld,
             seccion_hol TYPE zdseccionholi,
             estruc_abap TYPE zdreststrucabap,
             campo_abap  TYPE zdrestfieldabap,
             descripcion TYPE zdrestdatadescr,
             longitud    TYPE zfied_size,
           END OF ty_conf_fields.

    TYPES tt_conf_fields TYPE STANDARD TABLE OF ty_conf_fields WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_factories,
             res_id    TYPE ty_res_id,
             id_fuente TYPE zdserv_source_id,
             factory   TYPE classname,
           END OF ty_factories.

    TYPES tt_factories TYPE STANDARD TABLE OF ty_factories.

    TYPES: BEGIN OF ty_operations,
             extern_name TYPE ty_res_id,
             intern_name TYPE zdrestoperacion,
           END OF ty_operations.

    TYPES: tt_operations TYPE STANDARD TABLE OF ty_operations WITH DEFAULT KEY.

    " Errores conversión
    TYPES ty_conv_err(1) TYPE c.

    "-----------------------------------------------------------------
    " Estructura poblada por el adapter
    "-----------------------------------------------------------------
    TYPES: BEGIN OF ty_payload_data,
             field_name  TYPE ty_json_field_name,
             field_value TYPE string,
           END OF ty_payload_data.

    TYPES tt_payload_data TYPE SORTED TABLE OF ty_payload_data WITH UNIQUE KEY field_name.

    "-----------------------------------------------------------------
    " Estructura poblada por el adapter. Contendrá la info deserializada
    "-----------------------------------------------------------------
    TYPES: BEGIN OF ty_bapi_struct,
             "! Nombre de la estructura
             struct_name  TYPE zdreststrucabap,
             "! Referencia a la estructura
             struct_ref   TYPE REF TO data,
             "! Nombre externo
             payload_data TYPE tt_payload_data,
           END OF ty_bapi_struct.

    TYPES tt_bapi_struct TYPE SORTED TABLE OF ty_bapi_struct WITH NON-UNIQUE KEY struct_name.

    TYPES: BEGIN OF ty_registrable,
             o_instance TYPE REF TO zif_rest_registrable,
           END OF ty_registrable.

    TYPES tt_registrable TYPE STANDARD TABLE OF ty_registrable.

    "-----------------------------------------------------------------
    " Lista de fuentes
    "-----------------------------------------------------------------
    TYPES: BEGIN OF ty_sources,
             id TYPE zdserv_source_id,
           END OF ty_sources.

    TYPES tt_sources TYPE STANDARD TABLE OF ty_sources WITH DEFAULT KEY.

    TYPES:BEGIN OF ty_datos_log,
            id_fuente   TYPE zdserv_source_id,
            req_id      TYPE zdrestreqid,
            num_doc_ext TYPE zdrestnumdocext,
            status      TYPE zdreststatus,
            num_doc     TYPE zdrestnumdoc,
            timestamp   TYPE qentstzeitutc,
            message     TYPE bapi_msg,
          END OF ty_datos_log,

          tt_datos_log TYPE STANDARD TABLE OF ty_datos_log.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Tipo tabla datos log
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TYPES tt_ztrest_log TYPE STANDARD TABLE OF ztrest_log WITH DEFAULT KEY.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Tipo tabla reproceso
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TYPES tt_reproceso TYPE STANDARD TABLE OF ztrest_reproceso WITH DEFAULT KEY.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " CONSTANTES
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS c_clase_msgs_def TYPE arbgb VALUE 'ZREST'.

    CONSTANTS: BEGIN OF c_urlparams,
                 "! Nombre recurso (TODO: BORRAR)
                 res_name        TYPE ty_url_section VALUE 'fidocgrp',
                 "! Identificador recurso (TODO: BORRAR)
                 res_id          TYPE ty_url_section VALUE 'fidocgrp',
                 "! Numéro de documento en el sistema externo
                 num_doc_externo TYPE ty_url_section VALUE 'num_doc_ext',
               END OF c_urlparams.

    " Lista de identificadores de recursos
    CONSTANTS: BEGIN OF c_resource_id,
                 "! Grupo documentos FI
                 doc_group     TYPE ty_res_id VALUE 'fidocgrp',
                 "! Documento FI
                 doc           TYPE ty_res_id VALUE 'fidocs',
                 "! Grupo órdenes de compra
                 order_group   TYPE ty_res_id VALUE 'ordengrp',
                 "! Orden de compra
                 order         TYPE ty_res_id VALUE 'orden',
                 "! Pedido de venta
                 pedidos_venta TYPE ty_res_id VALUE 'pedsventa',
                 "! Nota crédito
                 nota_credito  TYPE ty_res_id VALUE 'notcredito',
                 "! Anulaciones
                 anulaciones   TYPE ty_res_id VALUE 'anulaciones',
               END OF c_resource_id.

    " Nombres de clase tipo resource que serán usadas
    CONSTANTS: BEGIN OF c_resource_class,
                 "! Recurso peticiones contabilización
                 c_fidocs_classname TYPE classname VALUE 'ZCL_FI_DOCS_RESOURCE',
               END OF c_resource_class.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Nombres de encabezado. Agregar nombres de headers de petición abajo.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS: BEGIN OF c_special_field,
                 "! Nombre header sistema fuente
                 fuente          TYPE ty_json_field_name VALUE 'ID_FUENTE',
                 num_posicion    TYPE ty_json_field_name VALUE 'NUMERO_POSICION',
                 informacion_msg TYPE ty_json_field_name VALUE 'INFO_MENSAJE',
                 id_peticion     TYPE ty_json_field_name VALUE 'REQ_ID',
                 version_serv    TYPE ty_json_field_name VALUE 'VERSION',
                 documentos      TYPE ty_json_field_name VALUE 'DOCUMENTOS',
                 doc_ref_externo TYPE ty_json_field_name VALUE 'NUM_DOC_EXTERNO',
                 fecha           TYPE ty_json_field_name VALUE 'FECHA_INI',
                 fecha_fin       TYPE ty_json_field_name VALUE 'FECHA_FIN',
                 status          TYPE ty_json_field_name VALUE 'STATUS',
                 complement_data TYPE ty_json_field_name VALUE 'DATOS_COMPLEMENTARIOS',
               END OF c_special_field.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Valores de versiones
    " Agregar nuevas si hay cambios significativos en la estructura
    " general del servicio de ERP-DOCS
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS: BEGIN OF c_versions,
                 current  TYPE string VALUE '1.0',
                 stable_1 TYPE string VALUE '1.0',
               END OF c_versions.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " OPERACIONES A REALIZAR
    " Agregar las operaciones que se realizarán
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS: BEGIN OF c_operation,
                 contabiliza  TYPE zdrestoperacion VALUE 'CONT',
                 pedido_venta TYPE zdrestoperacion VALUE 'PV',
                 nota_credito TYPE zdrestoperacion VALUE 'NC',
                 anulacion    TYPE zdrestoperacion VALUE 'ANUL',
               END OF c_operation.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " CONSTANTES ERRORES DE CONVERSION
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS: BEGIN OF c_conv_err,
                 no_num  TYPE ty_conv_err VALUE 'n',
                 no_int  TYPE ty_conv_err VALUE 'i',
                 no_date TYPE ty_conv_err VALUE 'd',
                 no_time TYPE ty_conv_err VALUE 't',
               END OF c_conv_err.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " STATUS CREACIÓN DOC
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS: BEGIN OF c_status_doc,
                 ok          TYPE zdreststatus VALUE 'OK',
                 err_interno TYPE zdreststatus VALUE 'EI',
                 err_datos   TYPE zdreststatus VALUE 'ED',
               END OF c_status_doc.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " TIPO MENSAJE RETURN BAPI
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS: BEGIN OF c_tipo_mensaje_bapi,
                 s TYPE bapi_mtype VALUE 'S',
                 e TYPE bapi_mtype VALUE 'E',
                 w TYPE bapi_mtype VALUE 'W',
               END OF c_tipo_mensaje_bapi.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Constante Tabla de equivalencia
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS:BEGIN OF c_param_source,
                equivalencia TYPE vim_name VALUE 'ZTRESTEQUIVALENC',
              END OF c_param_source.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Constante Object log estandar
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS: BEGIN OF c_object_log,
                 operacionrestservice TYPE balobj_d VALUE 'OPERACIONRESTSERVICE',
               END OF c_object_log.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Constante Tipo identificación
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS: BEGIN OF c_tipo_identificacion,
                 cc TYPE bptaxtype VALUE 'CO1C',
               END OF c_tipo_identificacion.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Nombre de la estructura RETURN en las BAPIS de SD - FI
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS: BEGIN OF c_name_struct,
                 return TYPE zdreststrucabap VALUE 'BAPIRET2',
               END OF c_name_struct.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Mensajes de retorno JSON
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS:BEGIN OF c_mensajes,
                mensajes TYPE string VALUE 'mensajes',
              END OF   c_mensajes.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adición para BP
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS: c_bp TYPE c LENGTH 2 VALUE '0F'.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Contantes metodos integración
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONSTANTS:BEGIN OF c_metodos,
                post TYPE zfied_secc VALUE 'POST',
                get  TYPE zfied_secc VALUE 'GET',
              END OF c_metodos.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " CONSTANTES INICIALIZADAS POR PROCESO
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA t_factories TYPE tt_factories.

    DATA t_operations TYPE tt_operations.

    DATA t_sources TYPE tt_sources.

    "! Inicializa constantes
    METHODS constructor.

ENDCLASS.



CLASS zcl_rest_cte IMPLEMENTATION.


  METHOD constructor.

    "-----------------------------------------------------------------
    " LISTA DE OPERACIONES
    " Agregar un registro para cada operación a procesar
    "-----------------------------------------------------------------
    me->t_operations = VALUE #(
      ( extern_name = 'fidocs'     intern_name = 'CONT' )
      ( extern_name = 'pedsventa'  intern_name = 'PV' )
      ( extern_name = 'notcredito' intern_name = 'NC' )
      ( extern_name = 'anulaciones' intern_name = 'ANUL' )
    ).

    "-----------------------------------------------------------------
    " LISTA DE SISTEMAS FUENTE
    " Agregar un registro para cada sistema fuente
    "-----------------------------------------------------------------
    me->t_sources = VALUE #(
        ( id = 'Q10' )
    ).

    "-----------------------------------------------------------------
    " EQUIVALENCIA FACTORIES CONTRA RESOURCES Y CASOS
    " Agregar más registros a medida que se incluyan servicios y sistemas externos
    "-----------------------------------------------------------------
    me->t_factories = VALUE #(
      ( res_id = c_resource_id-doc           id_fuente = 'Q10' factory = 'ZCL_FI_ACC_REST_FACTORY' )
      ( res_id = c_resource_id-pedidos_venta id_fuente = 'Q10' factory = 'ZCL_SD_ORDER_REST_FACTORY' )
      ( res_id = c_resource_id-nota_credito  id_fuente = 'Q10' factory = 'ZCL_SD_NC_REST_FACTORY' )
      ( res_id = c_resource_id-anulaciones   id_fuente = 'Q10' factory = 'ZCL_FI_REV_REST_FACTORY' )
    ).

  ENDMETHOD.
ENDCLASS.
