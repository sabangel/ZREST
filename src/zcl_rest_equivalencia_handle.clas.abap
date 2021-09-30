class ZCL_REST_EQUIVALENCIA_HANDLE definition
  public
  create protected .

public section.

  types:
    BEGIN OF ty_result,
            operacion                    TYPE zdrestoperacion,
            id_fuente                    TYPE zdserv_source_id,
            estruc_abap                  TYPE zdreststrucabap,
            campo_abap                   TYPE zdrestfieldabap,
            dependencia_1                TYPE zddependencia_1,
            valor_inferior_dependnecia_1 TYPE zdval_dependencia_inf_1,
            valor_superior_dependnecia_1 TYPE zdval_dependencia_sup_1,
            dependencia_2                TYPE zddependencia_2,
            valor_inferior_dependnecia_2 TYPE zdval_dependencia_inf_2,
            valor_superior_dependnecia_2 TYPE zdval_dependencia_sup_2,
            equivalencia                 TYPE zdequivalencia,
          END OF ty_result .
  types:
    tt_result TYPE SORTED TABLE OF ty_result WITH UNIQUE KEY operacion id_fuente estruc_abap campo_abap dependencia_1 valor_inferior_dependnecia_1.

  class-methods FIND
    importing
      !IV_SOURCE type VIM_NAME
      !IV_OPERACION type ZDRESTOPERACION
      !IV_FUENTE type ZDSERV_SOURCE_ID
      !IV_DEPENDENCIA type ZDDEPENDENCIA_1
      !IV_VALOR_INF_DEPENCIA_1 type ZDVAL_DEPENDENCIA_INF_1
      !IV_DEPENDENCIA_2 type ZDDEPENDENCIA_2 optional
      !IV_VALOR_INF_DEPENCIA_2 type ZDVAL_DEPENDENCIA_INF_2 optional
    returning
      value(RV_VAL) type TT_RESULT .
protected section.

  types:
    BEGIN OF ty_param,
            source    TYPE vim_name,
            operacion TYPE ZDRESTOPERACION,
            ID_FUENTE TYPE ZDSERV_SOURCE_ID,
            ESTRUC_ABAP TYPE ZDRESTSTRUCABAP,
            CAMPO_ABAP TYPE ZDRESTFIELDABAP,
            DEPENDENCIA_1 TYPE ZDDEPENDENCIA_1,
            VALOR_INFERIOR_DEPENDNECIA_1 TYPE ZDVAL_DEPENDENCIA_INF_1,
            VALOR_SUPERIOR_DEPENDNECIA_1 TYPE ZDVAL_DEPENDENCIA_SUP_1,
            DEPENDENCIA_2 TYPE ZDDEPENDENCIA_2,
            VALOR_INFERIOR_DEPENDNECIA_2 TYPE ZDVAL_DEPENDENCIA_INF_2,
            VALOR_SUPERIOR_DEPENDNECIA_2 TYPE ZDVAL_DEPENDENCIA_SUP_2,
            EQUIVALENCIA TYPE ZDEQUIVALENCIA,
         END OF ty_param .
  types:
    tt_param TYPE SORTED TABLE OF ty_param WITH UNIQUE KEY source operacion ID_FUENTE ESTRUC_ABAP
                                                           CAMPO_ABAP  DEPENDENCIA_1 VALOR_INFERIOR_DEPENDNECIA_1
                                                           VALOR_SUPERIOR_DEPENDNECIA_1.

  class-data O_REST_EQUI type ref to ZCL_REST_EQUIVALENCIA_HANDLE .
  class-data T_PARAMS_BUFFER type TT_PARAM .
  class-data:
    T_PARAM_SOURCE_RANGE TYPE RANGE OF vim_name .

  methods FIND_PARAMS
    importing
      !IV_SOURCE type VIM_NAME
      !IV_OPERACION type ZDRESTOPERACION
      !IV_FUENTE type ZDSERV_SOURCE_ID
    returning
      value(RT_RESULT) type TT_PARAM .
private section.
ENDCLASS.



CLASS ZCL_REST_EQUIVALENCIA_HANDLE IMPLEMENTATION.


  METHOD find.

    DATA: t_params TYPE tt_param,
          s_val TYPE ty_result..

    "Bufferear parámetros a demanda
    IF t_param_source_range IS INITIAL OR iv_source NOT IN t_param_source_range.

      IF o_rest_equi IS INITIAL.
        o_rest_equi = NEW zcl_rest_equivalencia_handle( ).
      ENDIF.

      TRY.
          t_params = o_rest_equi->find_params(
               iv_source    = iv_source
               iv_operacion = iv_operacion
               iv_fuente    = iv_fuente ).

        CATCH cx_dynamic_check INTO DATA(o_dyn_ex).

      ENDTRY.

      "Consignar el nombre de la tabla de parámetros ya buffereada
      TRY.
          zcl_range_util=>append_to_default_range(
              EXPORTING iv_value = iv_source
              CHANGING  ct_range = t_param_source_range ).

        CATCH zcx_util_excep INTO DATA(ex1).

      ENDTRY.

      "Agregar a la pila de parámetros
      DATA s_buffer LIKE LINE OF t_params_buffer.

      LOOP AT t_params ASSIGNING FIELD-SYMBOL(<s_params>).

        s_buffer-source                       = iv_source.
        s_buffer-operacion                    = <s_params>-operacion.
        s_buffer-id_fuente                    = <s_params>-id_fuente.
        s_buffer-estruc_abap                  = <s_params>-estruc_abap.
        s_buffer-campo_abap                   = <s_params>-campo_abap.

        s_buffer-dependencia_1                = <s_params>-dependencia_1.
        s_buffer-valor_inferior_dependnecia_1 = <s_params>-valor_inferior_dependnecia_1.
        s_buffer-valor_superior_dependnecia_1 = <s_params>-valor_superior_dependnecia_1.

        s_buffer-dependencia_2                = <s_params>-dependencia_2.
        s_buffer-valor_inferior_dependnecia_2 = <s_params>-valor_inferior_dependnecia_2.
        s_buffer-valor_superior_dependnecia_2 = <s_params>-valor_superior_dependnecia_2.

        s_buffer-equivalencia                 = <s_params>-equivalencia.

        INSERT s_buffer INTO TABLE t_params_buffer.

      ENDLOOP.

    ENDIF.

    LOOP AT t_params_buffer ASSIGNING FIELD-SYMBOL(<s_param>) WHERE source = iv_source AND
                                                                    operacion = iv_operacion AND
                                                                    id_fuente = iv_fuente  AND
                                                                    dependencia_1 = iv_dependencia AND
                                                                    valor_inferior_dependnecia_1 = iv_valor_inf_depencia_1.

      s_val-operacion                    = <s_param>-operacion.
      s_val-id_fuente                    = <s_param>-id_fuente.
      s_val-estruc_abap                  = <s_param>-estruc_abap.
      s_val-campo_abap                   = <s_param>-campo_abap.
      s_val-dependencia_1                = <s_param>-dependencia_1.
      s_val-valor_inferior_dependnecia_1 = <s_param>-valor_inferior_dependnecia_1.
      s_val-valor_superior_dependnecia_1 = <s_param>-valor_superior_dependnecia_1.

      s_val-dependencia_2                = <s_param>-dependencia_2.
      s_val-valor_inferior_dependnecia_2 = <s_param>-valor_inferior_dependnecia_2.
      s_val-valor_superior_dependnecia_2 = <s_param>-valor_superior_dependnecia_2.

      s_val-equivalencia                 = <s_param>-equivalencia.

      INSERT s_val INTO TABLE rv_val.

    ENDLOOP.

  ENDMETHOD.


  METHOD find_params.

    SELECT *
      FROM (iv_source)
      INTO CORRESPONDING FIELDS OF TABLE rt_result
      WHERE operacion EQ iv_operacion AND
            id_fuente EQ iv_fuente.

  ENDMETHOD.
ENDCLASS.
