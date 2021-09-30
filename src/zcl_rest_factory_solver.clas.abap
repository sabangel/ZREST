CLASS zcl_rest_factory_solver DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Devuelve un rest factory dependiendo de info contenida en el request
    "! @parameter io_req    | request
    "! @parameter ev_status | estatus de la operación. Se puede usar si no se encontró un
    "!                        factory para el request entregado
    CLASS-METHODS solve
      IMPORTING io_req               TYPE REF TO if_rest_request
      CHANGING  co_factory           TYPE REF TO zif_rest_factory
      RETURNING VALUE(rv_err_status) TYPE i.

  PRIVATE SECTION.

    CLASS-DATA cte TYPE REF TO zcl_rest_cte.

ENDCLASS.



CLASS zcl_rest_factory_solver IMPLEMENTATION.

  METHOD solve.

    TRY.
        cte = NEW zcl_rest_cte( ).

        DATA(v_source) = io_req->get_header_field( cte->c_special_field-fuente ).

        DATA(t_uri_segs) = io_req->get_uri_segments( ).

        "Busca el nombre del procesador en la estructura constante zcl_fi_acc_rest_cte=>c_processor
        DATA(v_oper) = CONV cte->ty_res_id( t_uri_segs[ 1 ] ).
        DATA(v_factory_name) = cte->t_factories[ res_id = v_oper id_fuente = v_source ]-factory.

        CREATE OBJECT co_factory TYPE (v_factory_name).

      CATCH cx_sy_itab_line_not_found.
        "No encontró un nombre de factory para el resource y la fuente entrantes
        rv_err_status = cl_rest_status_code=>gc_client_error_bad_request.

      CATCH cx_sy_create_object_error INTO DATA(o_ex).
        "No pudo inicializar el factory. Es un error interno
        rv_err_status = cl_rest_status_code=>gc_server_error_internal.

      CATCH cx_dynamic_check INTO DATA(o_dinex).
        rv_err_status = cl_rest_status_code=>gc_server_error_internal.

      CATCH cx_sy_no_handler INTO DATA(o_syex).
        rv_err_status = cl_rest_status_code=>gc_server_error_internal.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
