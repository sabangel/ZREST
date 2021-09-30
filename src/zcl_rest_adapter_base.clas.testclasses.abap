CLASS ltcl_adapter DEFINITION DEFERRED.
CLASS zcl_rest_adapter_base DEFINITION LOCAL FRIENDS ltcl_adapter.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" REST DAO
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_rest_dao DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_rest_sys_field_dao.

ENDCLASS.

CLASS lcl_rest_dao IMPLEMENTATION.



  METHOD zif_rest_sys_field_dao~find_source_fields.

    DATA(fpos) = zcl_rest_cte=>c_special_field-num_posicion.

    rt_fields = VALUE #(
( operacion = 'CONT' seccion = 'CAB' campo_hol = 'CLASE_DOC' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPIACHE09' campo_abap = 'DOC_TYPE' )
( operacion = 'CONT' seccion = 'CAB' campo_hol = 'EJERCICIO' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPIACHE09' campo_abap = 'FISC_YEAR' )
( operacion = 'CONT' seccion = 'CAB' campo_hol = 'FEC_CONTABIZA_DOC' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPIACHE09' campo_abap = 'PSTNG_DATE' )
( operacion = 'CONT' seccion = 'CAB' campo_hol = 'FEC_DOC' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPIACHE09' campo_abap = 'DOC_DATE' )
( operacion = 'CONT' seccion = 'AGL' campo_hol = 'CENTRO_COSTE' id_fuente = 'Q10' opcional = '' seccion_hol = 'CTAS_DE_MAYOR' estruc_abap = 'BAPIACGL09' campo_abap = 'COSTCENTER' )
( operacion = 'CONT' seccion = 'CAB' campo_hol = 'NUM_DOC_REF' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPIACHE09' campo_abap = 'REF_DOC_NO' )
( operacion = 'CONT' seccion = 'CAB' campo_hol = 'SOCIEDAD' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPIACHE09' campo_abap = 'COMP_CODE' )
( operacion = 'CONT' seccion = 'CAB' campo_hol = 'TXT_CAB' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPIACHE09' campo_abap = 'HEADER_TXT' )
( operacion = 'CONT' seccion = 'AGL' campo_hol = 'CLV_OPER' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'CTAS_DE_MAYOR' estruc_abap = 'BAPIACGL09' campo_abap = 'ACCT_KEY' )
( operacion = 'CONT' seccion = 'AGL' campo_hol = 'CLV_REF_KEY1_BP' id_fuente = 'Q10' opcional = '' seccion_hol = 'CTAS_DE_MAYOR' estruc_abap = 'BAPIACGL09' campo_abap = 'REF_KEY_1' )
( operacion = 'CONT' seccion = 'AGL' campo_hol = 'CTA_MAYOR_PPAL' id_fuente = 'Q10' opcional = '' seccion_hol = 'CTAS_DE_MAYOR' estruc_abap = 'BAPIACGL09' campo_abap = 'GL_ACCOUNT' )
( operacion = 'CONT' seccion = 'AGL' campo_hol = 'FEC_VALOR' id_fuente = 'Q10' opcional = '' seccion_hol = 'CTAS_DE_MAYOR' estruc_abap = 'BAPIACGL09' campo_abap = 'VALUE_DATE' )
( operacion = 'CONT' seccion = 'AGL' campo_hol = 'NUM_ASIGNACION' id_fuente = 'Q10' opcional = '' seccion_hol = 'CTAS_DE_MAYOR' estruc_abap = 'BAPIACGL09' campo_abap = 'ALLOC_NMBR' )
( operacion = 'CONT' seccion = 'AGL' campo_hol = fpos id_fuente = 'Q10' opcional = '' seccion_hol = 'CTAS_DE_MAYOR' estruc_abap = 'BAPIACGL09' campo_abap = 'ITEMNO_ACC' )
( operacion = 'CONT' seccion = 'AGL' campo_hol = 'TXT_POS' id_fuente = 'Q10' opcional = '' seccion_hol = 'CTAS_DE_MAYOR' estruc_abap = 'BAPIACGL09' campo_abap = 'ITEM_TEXT' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'CODIGO_POSTAL_APARTADO' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'POBX_PCD' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'IND_CME' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'SP_GL_IND' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'NUM_ASIGNACION' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'ALLOC_NMBR' )
( operacion = 'CONT' seccion = 'AGL' campo_hol = 'CLV_REF_KEY3_POS_DOC' id_fuente = 'Q10' opcional = '' seccion_hol = 'CTAS_DE_MAYOR' estruc_abap = 'BAPIACGL09' campo_abap = 'REF_KEY_3' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = fpos id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'ITEMNO_ACC' )
( operacion = 'CONT' seccion = 'AGL_CAM' campo_hol = 'IMPORTE_MONEDA_DOCUMENTO' id_fuente = 'Q10' opcional = '' seccion_hol = 'IMPORTES_CTAS_DE_MAYOR' estruc_abap = 'BAPIACCR09' campo_abap = 'AMT_DOCCUR' )
( operacion = 'CONT' seccion = 'ARE_CAM' campo_hol = 'IMPORTE_MONEDA_DOCUMENTO' id_fuente = 'Q10' opcional = '' seccion_hol = 'IMPORTES_DEUDORES' estruc_abap = 'BAPIACCR09' campo_abap = 'AMT_DOCCUR' )
( operacion = 'CONT' seccion = 'CAB' campo_hol = 'PERIODO' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPIACHE09' campo_abap = 'FIS_PERIOD' )
( operacion = 'CONT' seccion = 'CAB' campo_hol = 'NOMBRE_USUARIO' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'CABECERA' estruc_abap = 'BAPIACHE09' campo_abap = 'USERNAME' )
( operacion = 'CONT' seccion = 'AGL_CAM' campo_hol = fpos id_fuente = 'Q10' opcional = 'X' seccion_hol = 'IMPORTES_CTAS_DE_MAYOR' estruc_abap = 'BAPIACCR09' campo_abap = 'ITEMNO_ACC' )
( operacion = 'CONT' seccion = 'AGL' campo_hol = 'CLV_REF_KEY2_BP' id_fuente = 'Q10' opcional = '' seccion_hol = 'CTAS_DE_MAYOR' estruc_abap = 'BAPIACGL09' campo_abap = 'REF_KEY_2' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'CLAVE_CONTABILIZACION' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'BSCHL' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'CLAVE_CONTABILIZACION' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'BSCHL' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'TXT_POS' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'ITEM_TEXT' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'IND_IVA' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'TAX_CODE' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'CLV_COND_PAGO' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'PMNTTRMS' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'CEBE' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'PROFIT_CTR' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'VIA_PAGO' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'PYMT_METH' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'CLV_BLOQUEO_PAGO' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'PMNT_BLOCK' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'CLV_REF_KEY1_BP' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'REF_KEY_1' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'CLV_REF_KEY2_BP' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'REF_KEY_2' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'CLV_REF_KEY3_POS_DOC' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'REF_KEY_3' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'CEBE' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'PROFIT_CTR' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'CLV_BLOQUEO_PAGO' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'PMNT_BLOCK' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'CLV_COND_PAGO' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'PMNTTRMS' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'CLV_REF_KEY1_BP' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'REF_KEY_1' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'CLV_REF_KEY2_BP' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'REF_KEY_2' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'CLV_REF_KEY3_POS_DOC' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'REF_KEY_3' )
( operacion = 'CONT' seccion = 'CAB' campo_hol = 'MONEDA_DOCUMENTO' id_fuente = 'Q10' opcional = '' seccion_hol = 'CABECERA' estruc_abap = 'BAPIACHE09' campo_abap = 'CURRENCY' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'IND_CME' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'SP_GL_IND' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'IND_IVA' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'TAX_CODE' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'NUM_ASIGNACION' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'ALLOC_NMBR' )
( operacion = 'CONT' seccion = 'APA' campo_hol = fpos id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'ITEMNO_ACC' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'TXT_POS' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'ITEM_TEXT' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'VIA_PAGO' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'PYMT_METH' )
( operacion = 'CONT' seccion = 'CAB' campo_hol = 'TASA_CAMBIO' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'CABECERA' estruc_abap = 'BAPIACHE09' campo_abap = 'EXCH_RATE' )
( operacion = 'CONT' seccion = 'APA_CAM' campo_hol = 'IMPORTE_MONEDA_DOCUMENTO' id_fuente = 'Q10' opcional = '' seccion_hol = 'IMPORTES_ACREEDORES' estruc_abap = 'BAPIACCR09' campo_abap = 'AMT_DOCCUR' )
( operacion = 'CONT' seccion = 'APA_CAM' campo_hol = fpos id_fuente = 'Q10' opcional = 'X' seccion_hol = 'IMPORTES_ACREEDORES' estruc_abap = 'BAPIACCR09' campo_abap = 'ITEMNO_ACC' )
( operacion = 'CONT' seccion = 'ARE_CAM' campo_hol = fpos id_fuente = 'Q10' opcional = 'X' seccion_hol = 'IMPORTES_DEUDORES' estruc_abap = 'BAPIACCR09' campo_abap = 'ITEMNO_ACC' )
( operacion = 'CONT' seccion = 'AGL' campo_hol = 'CLAVE_CONTABILIZACION' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'CTAS_DE_MAYOR' estruc_abap = 'BAPIACGL09' campo_abap = 'BSCHL' )
( operacion = 'CONT' seccion = 'AGL_ADI' campo_hol = 'CLAVE' id_fuente = 'Q10' opcional = '' seccion_hol = 'ADIC_CTAS_DE_MAYOR' estruc_abap = 'ZSRESTADICIONALES_POSICION' campo_abap = 'CLAVE' )
( operacion = 'CONT' seccion = 'AGL_ADI' campo_hol = 'VALOR' id_fuente = 'Q10' opcional = '' seccion_hol = 'ADIC_CTAS_DE_MAYOR' estruc_abap = 'ZSRESTADICIONALES_POSICION' campo_abap = 'VALOR' )
( operacion = 'CONT' seccion = 'ARE_ADI' campo_hol = 'CLAVE' id_fuente = 'Q10' opcional = '' seccion_hol = 'ADIC_DEUDORES' estruc_abap = 'ZSRESTADICIONALES_POSICION' campo_abap = 'CLAVE' )
( operacion = 'CONT' seccion = 'ARE_ADI' campo_hol = 'VALOR' id_fuente = 'Q10' opcional = '' seccion_hol = 'ADIC_DEUDORES' estruc_abap = 'ZSRESTADICIONALES_POSICION' campo_abap = 'VALOR' )
( operacion = 'CONT' seccion = 'APA_ADI' campo_hol = 'CLAVE' id_fuente = 'Q10' opcional = '' seccion_hol = 'ADIC_ACREEDORES' estruc_abap = 'ZSRESTADICIONALES_POSICION' campo_abap = 'CLAVE' )
( operacion = 'CONT' seccion = 'APA_ADI' campo_hol = 'VALOR' id_fuente = 'Q10' opcional = '' seccion_hol = 'ADIC_ACREEDORES' estruc_abap = 'ZSRESTADICIONALES_POSICION' campo_abap = 'VALOR' )
( operacion = 'CONT' seccion = 'AGL_ADI' campo_hol = fpos id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ADIC_CTAS_DE_MAYOR' estruc_abap = 'ZSRESTADICIONALES_POSICION' campo_abap = zcl_rest_cte=>c_special_field-num_posicion )
( operacion = 'CONT' seccion = 'APA_ADI' campo_hol = fpos id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ADIC_ACREEDORES' estruc_abap = 'ZSRESTADICIONALES_POSICION' campo_abap = zcl_rest_cte=>c_special_field-num_posicion )
( operacion = 'CONT' seccion = 'ARE_ADI' campo_hol = fpos id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ADIC_DEUDORES' estruc_abap = 'ZSRESTADICIONALES_POSICION' campo_abap = zcl_rest_cte=>c_special_field-num_posicion )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'NAME1' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'NAME' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'NAME2' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'NAME_2' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'NAME3' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'NAME_3' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'NAME4' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'NAME_4' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'CODIGO_POSTAL' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'POSTL_CODE' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'POBLACION' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'CITY' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'PAIS' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'COUNTRY' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'PAIS_ISO' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'COUNTRY_ISO' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'CALLE' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'STREET' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'APARTADO' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'PO_BOX' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'CUENTA_CORRIENTE' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'POBK_CURAC' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'CUENTA_BANCARIA' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'BANK_ACCT' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'CLAVE_BCO' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'BANK_NO' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'PAIS_BCO' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'BANK_CTRY' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'PAIS_BCO_ISO' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'BANK_CTRY_ISO' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'N_IDENTIFICACION' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'TAX_NO_1' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'N_IDENTIFICACION2' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'TAX_NO_2' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'IVA' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'TAX' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'INDICADOR_IVA' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'EQUAL_TAX' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'REGION' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'REGION' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'CONTROL_BANCO' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'CTRL_KEY' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'CLAVE_INSTRUCCION_ISD' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'INSTR_KEY' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'CLAVE_NOTIF_ISD' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'DME_IND' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'IDIOMA_ISO' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'LANGU_ISO' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'IBAN' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'IBAN' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'CODIGO_SWIFT' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'SWIFT_CODE' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'N_IDENTIFICACION3' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'TAX_NO_3' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'N_IDENTIFICACION4' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'TAX_NO_4' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'CLASE_IMPUESTO' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'FITYP' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'TIPO_NIF' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'STCDT' )
( operacion = 'CONT' seccion = 'APA_CPD' campo_hol = 'PERSONA_FISICA' id_fuente = 'Q10' opcional = 'X' seccion_hol = 'ACREEDOR_ESPORADICO' estruc_abap = 'BAPIACPA09' campo_abap = 'STKZN' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'CTA_MAYOR_PPAL' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'GL_ACCOUNT' )
( operacion = 'CONT' seccion = 'APA' campo_hol = 'FECHA_BASE' id_fuente = 'Q10' opcional = '' seccion_hol = 'ACREEDORES' estruc_abap = 'BAPIACAP09' campo_abap = 'BLINE_DATE' )
( operacion = 'CONT' seccion = 'ARE' campo_hol = 'FECHA_BASE' id_fuente = 'Q10' opcional = '' seccion_hol = 'DEUDORES' estruc_abap = 'BAPIACAR09' campo_abap = 'BLINE_DATE' )
( operacion = 'CONT' seccion = 'AGL' campo_hol = 'CEBE' id_fuente = 'Q10' opcional = '' seccion_hol = 'CTAS_DE_MAYOR' estruc_abap = 'BAPIACGL09' campo_abap = 'PROFIT_CTR' )
    ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_dao DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_rest_sys_field_dao.
ENDCLASS.

CLASS lcl_dao IMPLEMENTATION.
ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" LLAMADOR BAPI
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_bapi_caller DEFINITION
    INHERITING FROM zcl_rest_bapi_caller_base
    CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS call REDEFINITION.
    METHODS get_bapi_name REDEFINITION.

  PROTECTED SECTION.
    METHODS process_bapi_result REDEFINITION.

ENDCLASS.

CLASS lcl_bapi_caller IMPLEMENTATION.

  METHOD call.
  ENDMETHOD.

  METHOD get_bapi_name.
  ENDMETHOD.

  METHOD process_bapi_result.
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" LLAMADOR BAPI
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_register
    DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_rest_register.

    DATA t_registrables TYPE zcl_rest_cte=>tt_registrable.

ENDCLASS.

CLASS lcl_register IMPLEMENTATION.

  METHOD zif_rest_register~add_registrable.
    APPEND VALUE #( o_instance = io_registrable ) TO me->t_registrables.
  ENDMETHOD.

  METHOD zif_rest_register~collect.
  ENDMETHOD.

  METHOD zif_rest_register~do_commit.
  ENDMETHOD.

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ADAPTER
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_adapter
    DEFINITION CREATE PUBLIC
    INHERITING FROM zcl_rest_adapter_base
    FRIENDS ltcl_adapter.

  PROTECTED SECTION.
    METHODS adapt_customized REDEFINITION.

    METHODS adapt_header
      IMPORTING is_struct_ref TYPE REF TO data.

ENDCLASS.

CLASS lcl_adapter IMPLEMENTATION.

  METHOD adapt_customized.
  ENDMETHOD.

  METHOD adapt_header.
  ENDMETHOD.

ENDCLASS.



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CREATOR -> AUXILIAR PARA PRUEBAS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_creator DEFINITION CREATE PUBLIC
  INHERITING FROM zcl_rest_creator_base
  FRIENDS ltcl_adapter.

  PUBLIC SECTION.
    METHODS init_adapter     REDEFINITION.
    METHODS init_bapi_caller REDEFINITION.
    METHODS init_register    REDEFINITION.

    "Auxiliar para evitar paralelismo
    METHODS set_payload
      IMPORTING iv_payload TYPE string.

  PROTECTED SECTION.
    METHODS init_instances   REDEFINITION.

ENDCLASS.


CLASS lcl_creator IMPLEMENTATION.

  METHOD init_adapter.
    co_adapter = NEW lcl_adapter( iv_oper      = iv_oper
                                  iv_id_fuente = iv_id_fuente
                                  iv_req_id    = iv_req_id
                                  io_serv_dao  = NEW lcl_rest_dao( ) ).
  ENDMETHOD.

  METHOD init_bapi_caller.
    co_caller = NEW lcl_bapi_caller( iv_oper = '' iv_id_fuente = '' iv_req_id = '' ).
  ENDMETHOD.

  METHOD init_register.
    co_reg = NEW lcl_register( ).
  ENDMETHOD.

  METHOD init_instances.
    super->init_instances( ).
  ENDMETHOD.

  METHOD set_payload.
    me->v_payload = iv_payload.
  ENDMETHOD.

ENDCLASS.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CLASE DE PRUEBAS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS ltcl_adapter DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA o_setup TYPE REF TO lcl_adapter.
    DATA cte TYPE REF TO zcl_rest_cte.

    METHODS setup.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " TEST METHODS
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    METHODS adapt_fi_ok FOR TESTING RAISING cx_static_check.
    METHODS check_convert FOR TESTING RAISING cx_static_check.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    METHODS find_payload
      IMPORTING iv_textname   TYPE tdobname
      RETURNING VALUE(rv_res) TYPE string.

    METHODS check_cab
      IMPORTING it_data TYPE zcl_rest_cte=>tt_bapi_struct
      RAISING   cx_static_check.

    METHODS check_deu
      IMPORTING it_data TYPE zcl_rest_cte=>tt_bapi_struct.

    METHODS check_imp
      IMPORTING it_data TYPE zcl_rest_cte=>tt_bapi_struct.

    METHODS check_adi
      IMPORTING it_data TYPE zcl_rest_cte=>tt_bapi_struct.

ENDCLASS.


CLASS ltcl_adapter IMPLEMENTATION.

  METHOD setup.
    me->cte = NEW #( ).
  ENDMETHOD.

  METHOD adapt_fi_ok.

    DATA t_cab TYPE zcl_rest_cte=>tt_bapi_struct.
    DATA t_deu TYPE zcl_rest_cte=>tt_bapi_struct.
    DATA t_imp TYPE zcl_rest_cte=>tt_bapi_struct.
    DATA t_adi TYPE zcl_rest_cte=>tt_bapi_struct.

    TRY.
        DATA(o_creator) = NEW lcl_creator( iv_oper = zcl_rest_cte=>c_operation-contabiliza iv_id_fuente = 'Q10' ).
        DATA(v_payload) = find_payload( iv_textname = 'ZFICASOBASE' ).

        "Evitar el proceso en paralelo
        o_creator->set_payload( v_payload ).

        o_creator->create( iv_payload = v_payload ).

        me->o_setup = CAST #( o_creator->o_adapter ).

        LOOP AT me->o_setup->t_structs ASSIGNING FIELD-SYMBOL(<s_struct>).

          CASE <s_struct>-struct_name.

              "Cabecera
            WHEN 'BAPIACHE09'.
              APPEND <s_struct> TO t_cab.

              "Deudores
            WHEN 'BAPIACAR09'.
              APPEND <s_struct> TO t_deu.

              "Importes
            WHEN 'BAPIACCR09'.
              APPEND <s_struct> TO t_imp.

            WHEN 'ZSRESTADICIONALES_POSICION'.
              "Adicionales posición
              APPEND <s_struct> TO t_adi.

          ENDCASE.

        ENDLOOP.

        check_cab( t_cab ).
        check_deu( t_deu ).
        check_imp( t_imp ).
        check_adi( t_adi ).

        "----------------------------------------------------------------------
        "Resultado esperado de proceso ADAPT
        "----------------------------------------------------------------------
      CATCH zcx_rest_ex INTO DATA(o_ex).
        cl_abap_unit_assert=>fail( |Exepción: { o_ex->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

  METHOD check_cab.

    DATA r_cab_exp TYPE REF TO data.
    DATA r_cab_act TYPE REF TO data.

    FIELD-SYMBOLS <v_value> TYPE any.

    TRY.
        "Cantidad datos esperada
        cl_abap_unit_assert=>assert_equals( act = lines( it_data ) exp = 1 ).

        "CREATE DATA r_json_exp TYPE bapiache09.
        DATA(t_payload_exp) = VALUE cte->tt_payload_data(
          ( field_name = 'SOCIEDAD'          field_value = 'CF01' )
          ( field_name = 'CLASE_DOC'         field_value = 'RI' )
          ( field_name = 'EJERCICIO'         field_value = '2021' )
          ( field_name = 'PERIODO'           field_value = '04' )
          ( field_name = 'FEC_CONTABIZA_DOC' field_value = '20210405' )
          ( field_name = 'FEC_DOC'           field_value = '20210405' )
          ( field_name = 'NUM_DOC_REF'       field_value = 'Prueba' )
          ( field_name = 'TXT_CAB'           field_value = 'Recaudo anticipado' )
        ).

        "Datos que ingresan
        DATA(t_payload_act) = it_data[ 1 ]-payload_data.
        cl_abap_unit_assert=>assert_equals( act = t_payload_act  exp = t_payload_exp ).

        "Estructura dinámica esperada
        CREATE DATA r_cab_exp TYPE bapiache09.
        ASSIGN r_cab_exp->('COMP_CODE')  TO <v_value>. <v_value> = 'CF01'.
        ASSIGN r_cab_exp->('DOC_TYPE')   TO <v_value>. <v_value> = 'RI'.
        ASSIGN r_cab_exp->('FISC_YEAR')  TO <v_value>. <v_value> = '2021'.
        ASSIGN r_cab_exp->('FIS_PERIOD') TO <v_value>. <v_value> = '04'.
        ASSIGN r_cab_exp->('PSTNG_DATE') TO <v_value>. <v_value> = '20210405'.
        ASSIGN r_cab_exp->('DOC_DATE')   TO <v_value>. <v_value> = '20210405'.
        ASSIGN r_cab_exp->('REF_DOC_NO') TO <v_value>. <v_value> = 'Prueba'.
        ASSIGN r_cab_exp->('HEADER_TXT') TO <v_value>. <v_value> = 'Recaudo anticipado'.

        r_cab_act = it_data[ 1 ]-struct_ref.
        cl_abap_unit_assert=>assert_equals( act = r_cab_act  exp = r_cab_exp ).

      CATCH cx_static_check INTO DATA(o_stex).
        RAISE EXCEPTION o_stex.
      CATCH cx_root INTO DATA(o_ex).
        cl_abap_unit_assert=>fail( |Exepción: { o_ex->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD check_deu.

    DATA r_abap_struct_exp TYPE REF TO data.
    DATA r_abap_struct_act TYPE REF TO data.
    DATA(fpos) = zcl_rest_cte=>c_special_field-num_posicion.


    FIELD-SYMBOLS <v_value> TYPE any.

    "Cantidad datos esperada
    cl_abap_unit_assert=>assert_equals( act = lines( it_data ) exp = 4 ).

    "----------------------------------------------
    " Check posición 1 del payload (viene poblado en la pos 4 de las abap)
    "----------------------------------------------
    DATA(t_payload_exp) = VALUE cte->tt_payload_data(
      ( field_name = 'CEBE'                 field_value = '15404113' )
      ( field_name = 'IND_CME'              field_value = 'Y' )
      ( field_name = 'NUM_ASIGNACION'       field_value = '202103' )
      ( field_name = fpos field_value = '3' )
      ( field_name = 'TXT_POS'              field_value = 'txt pos 03 AR' )
    ).

    "Datos que ingresan
    DATA(t_payload_act) = it_data[ 4 ]-payload_data.
    cl_abap_unit_assert=>assert_equals( act = t_payload_act  exp = t_payload_exp ).

    "Estructura dinámica esperada
    CREATE DATA r_abap_struct_exp TYPE bapiacar09.
    ASSIGN r_abap_struct_exp->('PROFIT_CTR') TO <v_value>. <v_value> = '15404113'.
    ASSIGN r_abap_struct_exp->('SP_GL_IND')  TO <v_value>. <v_value> = 'Y'.
    ASSIGN r_abap_struct_exp->('ALLOC_NMBR') TO <v_value>. <v_value> = '202103'.
    ASSIGN r_abap_struct_exp->('ITEMNO_ACC') TO <v_value>. <v_value> = '3'.
    ASSIGN r_abap_struct_exp->('ITEM_TEXT')  TO <v_value>. <v_value> = 'txt pos 03 AR'.

    r_abap_struct_act = it_data[ 4 ]-struct_ref.
    cl_abap_unit_assert=>assert_equals( act = r_abap_struct_act  exp = r_abap_struct_exp ).

    "----------------------------------------------
    " Check posición 3 del payload (viene poblado en la pos 2 de las abap)
    "----------------------------------------------
    t_payload_exp = VALUE cte->tt_payload_data(
      ( field_name = 'CEBE'                 field_value = '15404115' )
      ( field_name = 'IND_CME'              field_value = 'Y' )
      ( field_name = 'NUM_ASIGNACION'       field_value = '202105' )
      ( field_name = fpos  field_value = '5' )
      ( field_name = 'TXT_POS'              field_value = 'txt pos 05 AR' )
    ).

    "Datos que ingresan
    t_payload_act = it_data[ 2 ]-payload_data.
    cl_abap_unit_assert=>assert_equals( act = t_payload_act  exp = t_payload_exp ).

    "Estructura dinámica esperada
    CREATE DATA r_abap_struct_exp TYPE bapiacar09.
    ASSIGN r_abap_struct_exp->('PROFIT_CTR') TO <v_value>. <v_value> = '15404115'.
    ASSIGN r_abap_struct_exp->('SP_GL_IND')  TO <v_value>. <v_value> = 'Y'.
    ASSIGN r_abap_struct_exp->('ALLOC_NMBR') TO <v_value>. <v_value> = '202105'.
    ASSIGN r_abap_struct_exp->('ITEMNO_ACC') TO <v_value>. <v_value> = '5'.
    ASSIGN r_abap_struct_exp->('ITEM_TEXT')  TO <v_value>. <v_value> = 'txt pos 05 AR'.

    r_abap_struct_act = it_data[ 2 ]-struct_ref.
    cl_abap_unit_assert=>assert_equals( act = r_abap_struct_act  exp = r_abap_struct_exp ).

  ENDMETHOD.


  METHOD check_imp.

    DATA r_abap_struct_exp TYPE REF TO data.
    DATA r_abap_struct_act TYPE REF TO data.
    DATA(fpos) = zcl_rest_cte=>c_special_field-num_posicion.


    FIELD-SYMBOLS <v_value> TYPE any.

    "Cantidad datos esperada
    cl_abap_unit_assert=>assert_equals( act = lines( it_data ) exp = 7 ).

    "----------------------------------------------
    " Check posición 1 del payload (viene poblado en la pos 4 de las abap)
    "----------------------------------------------
    DATA(t_payload_exp) = VALUE cte->tt_payload_data(
      ( field_name = 'IMPORTE_MONEDA_DOCUMENTO' field_value = '450003.00' )
      ( field_name = fpos       field_value = '3' )
    ).

    "Datos que ingresan
    DATA(t_payload_act) = it_data[ 4 ]-payload_data.
    cl_abap_unit_assert=>assert_equals( act = t_payload_act  exp = t_payload_exp ).

    "Estructura dinámica esperada
    CREATE DATA r_abap_struct_exp TYPE bapiaccr09.
    ASSIGN r_abap_struct_exp->('AMT_DOCCUR') TO <v_value>. <v_value> = '450003.00'.
    ASSIGN r_abap_struct_exp->('ITEMNO_ACC') TO <v_value>. <v_value> = '3'.

    r_abap_struct_act = it_data[ 4 ]-struct_ref.
    cl_abap_unit_assert=>assert_equals( act = r_abap_struct_act  exp = r_abap_struct_exp ).

    "----------------------------------------------
    " Check posición 7 del payload (viene poblado en la pos 7 de las abap)
    "----------------------------------------------
    t_payload_exp = VALUE cte->tt_payload_data(
      ( field_name = 'IMPORTE_MONEDA_DOCUMENTO' field_value = '450007.00' )
      ( field_name = fpos      field_value = '7' )
    ).

    "Datos que ingresan
    t_payload_act = it_data[ 7 ]-payload_data.
    cl_abap_unit_assert=>assert_equals( act = t_payload_act  exp = t_payload_exp ).

    "Estructura dinámica esperada
    CREATE DATA r_abap_struct_exp TYPE bapiaccr09.
    ASSIGN r_abap_struct_exp->('AMT_DOCCUR') TO <v_value>. <v_value> = '450007'.
    ASSIGN r_abap_struct_exp->('ITEMNO_ACC') TO <v_value>. <v_value> = '7'.

    r_abap_struct_act = it_data[ 7 ]-struct_ref.
    cl_abap_unit_assert=>assert_equals( act = r_abap_struct_act  exp = r_abap_struct_exp ).

  ENDMETHOD.


  METHOD check_adi.

    DATA r_abap_struct_exp TYPE REF TO data.
    DATA r_abap_struct_act TYPE REF TO data.
    DATA(fpos) = zcl_rest_cte=>c_special_field-num_posicion.

    FIELD-SYMBOLS <v_value> TYPE any.

    "Cantidad datos esperada
    cl_abap_unit_assert=>assert_equals( act = lines( it_data ) exp = 15 ).

    "----------------------------------------------
    " Check 2 adicionales de cuentas de mayor (pos 11 y 10 la estructura bapi)
    "----------------------------------------------
    DATA(t_payload_exp) = VALUE cte->tt_payload_data(
      ( field_name = 'CLAVE'   field_value = 'num_id_agl_2' )
      ( field_name = 'VALOR'   field_value = '123654782' )
      ( field_name = fpos field_value = '2' )
    ).

    "Datos que ingresan
    DATA(t_payload_act) = it_data[ 11 ]-payload_data.
    cl_abap_unit_assert=>assert_equals( act = t_payload_act  exp = t_payload_exp ).

    "Estructura dinámica esperada
    CREATE DATA r_abap_struct_exp TYPE zsrestadicionales_posicion.
    ASSIGN r_abap_struct_exp->('CLAVE') TO <v_value>. <v_value> = 'num_id_agl_2'.
    ASSIGN r_abap_struct_exp->('VALOR') TO <v_value>. <v_value> = '123654782'.
    ASSIGN r_abap_struct_exp->(fpos) TO <v_value>. <v_value> = '2'.

    r_abap_struct_act = it_data[ 11 ]-struct_ref.
    cl_abap_unit_assert=>assert_equals( act = r_abap_struct_act  exp = r_abap_struct_exp ).

    t_payload_exp = VALUE cte->tt_payload_data(
      ( field_name = 'CLAVE'   field_value = 'tipo_id_agl_2' )
      ( field_name = 'VALOR'   field_value = 'CC2' )
      ( field_name = fpos field_value = '2' )
    ).

    "Datos que ingresan
    t_payload_act = it_data[ 10 ]-payload_data.
    cl_abap_unit_assert=>assert_equals( act = t_payload_act  exp = t_payload_exp ).

    "Estructura dinámica esperada
    CREATE DATA r_abap_struct_exp TYPE zsrestadicionales_posicion.
    ASSIGN r_abap_struct_exp->('CLAVE') TO <v_value>. <v_value> = 'tipo_id_agl_2'.
    ASSIGN r_abap_struct_exp->('VALOR') TO <v_value>. <v_value> = 'CC2'.
    ASSIGN r_abap_struct_exp->(fpos) TO <v_value>. <v_value> = '2'.

    r_abap_struct_act = it_data[ 10 ]-struct_ref.
    cl_abap_unit_assert=>assert_equals( act = r_abap_struct_act  exp = r_abap_struct_exp ).

    "----------------------------------------------
    " Check adicional 7, la de acreedores (pos 14 de estructuras bapi)
    "----------------------------------------------
    t_payload_exp = VALUE cte->tt_payload_data(
      ( field_name = 'CLAVE'   field_value = 'tipo_id_ap_7' )
      ( field_name = 'VALOR'   field_value = 'CC7' )
      ( field_name = fpos      field_value = '7' )
    ).

    "Datos que ingresan
    t_payload_act = it_data[ 14 ]-payload_data.
    cl_abap_unit_assert=>assert_equals( act = t_payload_act  exp = t_payload_exp ).

    "Estructura dinámica esperada
    CREATE DATA r_abap_struct_exp TYPE zsrestadicionales_posicion.
    ASSIGN r_abap_struct_exp->('CLAVE') TO <v_value>. <v_value> = 'tipo_id_ap_7'.
    ASSIGN r_abap_struct_exp->('VALOR') TO <v_value>. <v_value> = 'CC7'.
    ASSIGN r_abap_struct_exp->(fpos) TO <v_value>. <v_value> = '7'.

    r_abap_struct_act = it_data[ 14 ]-struct_ref.
    cl_abap_unit_assert=>assert_equals( act = r_abap_struct_act  exp = r_abap_struct_exp ).

  ENDMETHOD.


  METHOD check_convert.

    DATA(o_adapter) = NEW lcl_adapter( io_serv_dao = NEW lcl_dao( ) iv_oper = 'CONT' iv_id_fuente = 'Q10' iv_req_id = '000001' ).

    DATA(o_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( 'ZSRESTTEST' ) ).
    DATA s_act_ok TYPE zsresttest.

    "--------------------------------------------------------------------------------
    " CASO OK: datos entran con el formato correcto
    "--------------------------------------------------------------------------------

    DATA(s_exp_ok) = VALUE zsresttest(
        char_type = 'ABCDE'              "Se espera que trunque la última
        str_type  = 'ABCDEF'             "Se espera entre igual
        num_type  = 1234                 "Se espera entre igual
        dec_type  = '1234.57'            "Se espera redondee
        int_type  = 12345                "Se espera entre igual
        date_type = '20211231'           "Se espera convierta de DD/MM/AAAA
        time_type = '132010'             "Se espera convierta de HH:MM:SS
    ).

    o_adapter->convert_value(
       EXPORTING iv_value        = 'ABCDEF'
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-char_type )->type_kind
       IMPORTING ev_output       = s_act_ok-char_type ).

    o_adapter->convert_value(
       EXPORTING iv_value        = 'ABCDEF'
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-str_type )->type_kind
       IMPORTING ev_output       = s_act_ok-str_type ).

    o_adapter->convert_value(
       EXPORTING iv_value        = '1234'
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-num_type )->type_kind
       IMPORTING ev_output       = s_act_ok-num_type ).

    o_adapter->convert_value(
       EXPORTING iv_value        = '001234.567'   "Se redondea
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-dec_type )->type_kind
       IMPORTING ev_output       = s_act_ok-dec_type ).

    o_adapter->convert_value(
       EXPORTING iv_value        = '12345'
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-int_type )->type_kind
       IMPORTING ev_output       = s_act_ok-int_type ).

    o_adapter->convert_value(
       EXPORTING iv_value        = '31/12/2021'
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-date_type )->type_kind
       IMPORTING ev_output       = s_act_ok-date_type ).

    DATA(v_err) = o_adapter->convert_value(
       EXPORTING iv_value        = '13:20:10'
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-time_type )->type_kind
       IMPORTING ev_output       = s_act_ok-time_type ).

    cl_abap_unit_assert=>assert_equals( act = s_act_ok exp = s_exp_ok ).
    cl_abap_unit_assert=>assert_equals( act = v_err exp = '' ).

    "Fecha y hora sin separadores
    o_adapter->convert_value(
       EXPORTING iv_value        = '20211231'      "AAAAMMDD
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-date_type )->type_kind
       IMPORTING ev_output       = s_act_ok-date_type ).

    o_adapter->convert_value(
       EXPORTING iv_value        = '132010'        "HHMMSS
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-time_type )->type_kind
       IMPORTING ev_output       = s_act_ok-time_type ).

    cl_abap_unit_assert=>assert_equals( act = s_act_ok exp = s_exp_ok ).

    "Prueba con negativo
    o_adapter->convert_value(
       EXPORTING iv_value        = '-001234.567'   "Se redondea
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-dec_type )->type_kind
       IMPORTING ev_output       = s_act_ok-dec_type ).

    cl_abap_unit_assert=>assert_equals( act = s_act_ok-dec_type exp = '-001234.57' ).


    "--------------------------------------------------------------------------------
    " CASO MAL: datos entran con fallas de formato
    "--------------------------------------------------------------------------------
    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '10-'              "Debe entrar un entero
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-num_type )->type_kind
       IMPORTING ev_output       = s_act_ok-num_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_int ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = 'ABCD'              "Debe entrar un entero
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-num_type )->type_kind
       IMPORTING ev_output       = s_act_ok-num_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_int ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '12.18'              "Debe entrar un entero
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-num_type )->type_kind
       IMPORTING ev_output       = s_act_ok-num_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_int ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '00123X.567'
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-dec_type )->type_kind
       IMPORTING ev_output       = s_act_ok-dec_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_num ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '001.23.567'
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-dec_type )->type_kind
       IMPORTING ev_output       = s_act_ok-dec_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_num ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '00123.'
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-dec_type )->type_kind
       IMPORTING ev_output       = s_act_ok-dec_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_num ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '00-123.56'
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-dec_type )->type_kind
       IMPORTING ev_output       = s_act_ok-dec_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_num ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = 'bad'
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-int_type )->type_kind
       IMPORTING ev_output       = s_act_ok-int_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_int ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '90.2'                    "Debe entrar un entero
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-int_type )->type_kind
       IMPORTING ev_output       = s_act_ok-int_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_int ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '50/12/2021'   "bad
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-date_type )->type_kind
       IMPORTING ev_output       = s_act_ok-date_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_date ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '50/12/20210'   "bad
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-date_type )->type_kind
       IMPORTING ev_output       = s_act_ok-date_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_date ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '20211301'    "bad
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-date_type )->type_kind
       IMPORTING ev_output       = s_act_ok-date_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_date ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '202112011'    "bad por exceso
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-date_type )->type_kind
       IMPORTING ev_output       = s_act_ok-date_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_date ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '28:20:00'      "bad por contenido
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-time_type )->type_kind
       IMPORTING ev_output       = s_act_ok-time_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_time ).

    v_err = o_adapter->convert_value(
       EXPORTING iv_value        = '2010300'        "bad por exceso
                 iv_type_kind    = cl_abap_typedescr=>describe_by_data( s_act_ok-time_type )->type_kind
       IMPORTING ev_output       = s_act_ok-time_type ).

    cl_abap_unit_assert=>assert_equals( act = v_err exp = zcl_rest_cte=>c_conv_err-no_time ).

  ENDMETHOD.

  METHOD find_payload.

    DATA t_lines TYPE tline_t.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id       = 'ST'
        name     = iv_textname
        language = 'S'
        object   = 'TEXT'
      TABLES
        lines    = t_lines
      EXCEPTIONS
        OTHERS   = 9.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    LOOP AT t_lines ASSIGNING FIELD-SYMBOL(<s_line>).
      rv_res = rv_res && <s_line>-tdline.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
