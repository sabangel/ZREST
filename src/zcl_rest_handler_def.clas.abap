CLASS zcl_rest_handler_def DEFINITION
  INHERITING FROM cl_rest_http_handler
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    METHODS constructor.

    METHODS if_rest_application~get_root_handler REDEFINITION.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  PROTECTED SECTION.

    METHODS handle_csrf_token REDEFINITION.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  PRIVATE SECTION.

    DATA cte TYPE REF TO zcl_rest_cte.

ENDCLASS.



CLASS zcl_rest_handler_def IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    me->cte = NEW #( ).

  ENDMETHOD.


  METHOD if_rest_application~get_root_handler.

    DATA(o_handler) = NEW cl_rest_router( ).

    "Se acepta "/" al final como para crear
    o_handler->attach( iv_template      = '/'
                       iv_handler_class = 'ZCL_REST_RESOURCE_DEF'  ).

    o_handler->attach( iv_template      = |/\{{ cte->c_resource_id-pedidos_venta }\}|
                       iv_handler_class = 'ZCL_REST_RESOURCE_DEF' ).

    o_handler->attach( iv_template      = |/\{{ cte->c_resource_id-nota_credito }\}|
                       iv_handler_class = 'ZCL_REST_RESOURCE_DEF' ).

    ro_root_handler = o_handler.

  ENDMETHOD.

  METHOD handle_csrf_token.

    "TODO: Decidir si validar o no el token
*    super->handle_csrf_token(
*        io_csrf_handler = io_csrf_handler
*        io_request      = io_request
*        io_response     = io_response ).

  ENDMETHOD.

ENDCLASS.
