*&---------------------------------------------------------------------*
*& Include          ZSDI_DIA_ANALISE_FISCAL_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_seleciona_dados.

  DATA: lt_ztsd_br147 TYPE STANDARD TABLE OF ty_ztsd_br147,
        lt_dados      TYPE STANDARD TABLE OF ty_dados.

  "Selecionando dados......Por favor Aguarde!
  PERFORM zf_status_message USING TEXT-006.

  SELECT zdia_a03
         zdia_a04
         zdia_a05
         zdia_a06
         zdia_b04
         zdia_c02
         zdia_b03
         zdia_b05
         zdia_cst
         zdia_c03
         zdia_c06
         zdia_c07
         zdia_d01
    FROM ztsd_br147
    INTO TABLE gt_ztsd_br147
   WHERE zdia_a05 EQ p_ano
     AND zdia_a06 EQ p_mes.
  IF sy-subrc EQ 0.

    SORT gt_ztsd_br147 BY zdia_a04
                          zdia_a05
                          zdia_a06
                          zdia_c03.

*    SELECT *
*     FROM ztsd_br148
*     INTO TABLE gt_ztsd_br148
*      FOR ALL ENTRIES IN gt_ztsd_br147
*    WHERE zdia_a03 EQ gt_ztsd_br147-zdia_a03
*      AND zdia_a04 EQ gt_ztsd_br147-zdia_a04
*      AND zdia_a05 EQ gt_ztsd_br147-zdia_a05
*      AND zdia_a06 EQ gt_ztsd_br147-zdia_a06
*      AND zdia_b04 EQ gt_ztsd_br147-zdia_b04
*      AND zdia_c02 EQ gt_ztsd_br147-zdia_c02.
*
*    IF sy-subrc EQ 0.
*
*      SORT gt_ztsd_br148 BY zdia_a03
*                            zdia_a04
*                            zdia_a05
*                            zdia_a06
*                            zdia_b04
*                            zdia_c02.
*
*    ENDIF.

    LOOP AT gt_ztsd_br147 ASSIGNING FIELD-SYMBOL(<fs_ztsd_br147>).

      MOVE: <fs_ztsd_br147>-zdia_b04(2)    TO <fs_ztsd_br147>-regio,
            <fs_ztsd_br147>-zdia_b04+2(2)  TO <fs_ztsd_br147>-nfyear,
            <fs_ztsd_br147>-zdia_b04+4(2)  TO <fs_ztsd_br147>-nfmonth,
            <fs_ztsd_br147>-zdia_b04+6(14) TO <fs_ztsd_br147>-stcd1,
            <fs_ztsd_br147>-zdia_b04+20(2) TO <fs_ztsd_br147>-model,
            <fs_ztsd_br147>-zdia_b04+22(3) TO <fs_ztsd_br147>-serie,
            <fs_ztsd_br147>-zdia_b04+25(9) TO <fs_ztsd_br147>-nfnum9,
            <fs_ztsd_br147>-zdia_b04+34(9) TO <fs_ztsd_br147>-docnum9,
            <fs_ztsd_br147>-zdia_b04+43(1) TO <fs_ztsd_br147>-cdv.

    ENDLOOP.

    lt_ztsd_br147[] = gt_ztsd_br147[].

    DELETE ADJACENT DUPLICATES FROM lt_ztsd_br147
                          COMPARING zdia_b04.

    SELECT ref~docnum
           ref~regio
           ref~nfyear
           ref~nfmonth
           ref~stcd1
           ref~model
           ref~serie
           ref~nfnum9
           ref~docnum9
           ref~cdv
           doc~nftype
           doc~direct
           doc~bukrs
           doc~branch
           doc~docdat
           doc~pstdat
           doc~parid
           doc~nftot
           doc~nfenum
           doc~name1
           doc~cgc
           lin~itmnum
           lin~matnr
           lin~maktx
           lin~cfop
           lin~matorg
           lin~matuse
           lin~netwr
      INTO TABLE gt_dados
      FROM j_1bnfe_active AS ref
      INNER JOIN j_1bnfdoc AS doc
         ON ref~docnum EQ doc~docnum
      INNER JOIN j_1bnflin AS lin
         ON lin~docnum EQ doc~docnum
      FOR ALL ENTRIES IN lt_ztsd_br147
      WHERE ref~regio   EQ lt_ztsd_br147-regio
        AND ref~nfyear  EQ lt_ztsd_br147-nfyear
        AND ref~nfmonth EQ lt_ztsd_br147-nfmonth
        AND ref~stcd1   EQ lt_ztsd_br147-stcd1
        AND ref~model   EQ lt_ztsd_br147-model
        AND ref~serie   EQ lt_ztsd_br147-serie
        AND ref~nfnum9  EQ lt_ztsd_br147-nfnum9
        AND ref~docnum9 EQ lt_ztsd_br147-docnum9
        AND ref~cdv     EQ lt_ztsd_br147-cdv
        AND doc~doctyp  NE '5'
        AND doc~direct  EQ '1'
        AND doc~bukrs   EQ p_bukrs
        AND doc~branch  EQ p_branch
        AND doc~cancel  NE 'X'
        AND doc~ie_bupla EQ p_in_es.

    IF sy-subrc EQ 0.

      SORT gt_dados BY regio
                       nfyear
                       nfmonth
                       stcd1
                       model
                       serie
                       nfnum9
                       docnum9
                       cdv
                       itmnum.

      lt_dados[] = gt_dados[].
      DELETE ADJACENT DUPLICATES FROM lt_dados
                            COMPARING bukrs
                                      branch
                                      cfop
                                      matuse
                                      matorg.

      DELETE lt_dados[] WHERE bukrs IS INITIAL
                           OR branch IS INITIAL
                           OR cfop IS INITIAL
                           OR matuse IS INITIAL
                           OR matorg IS INITIAL.

      IF lt_dados IS NOT INITIAL.

        SELECT bukrs
               branch
               cfop
               matuse
               matorg
               multipl
               zdia_c06
          FROM ztsd_dia_codtrib
          INTO TABLE gt_ztsd_dia_codtrib
           FOR ALL ENTRIES IN lt_dados
         WHERE bukrs  EQ lt_dados-bukrs
           AND branch EQ lt_dados-branch
           AND cfop   EQ lt_dados-cfop
           AND matuse EQ lt_dados-matuse
           AND matorg EQ lt_dados-matorg.
        IF sy-subrc EQ 0.

          SORT gt_ztsd_dia_codtrib BY bukrs
                                      branch
                                      cfop
                                      matuse
                                      matorg.

        ENDIF.

      ENDIF.

      CLEAR: lt_dados[].
      lt_dados[] = gt_dados[].
      DELETE ADJACENT DUPLICATES FROM lt_dados
                            COMPARING bukrs.

      SELECT bukrs
             nome_resp
             tel_resp
             email_resp
        FROM ztsd_dia_resp
        INTO TABLE gt_ztsd_dia_resp
         FOR ALL ENTRIES IN lt_dados
       WHERE bukrs EQ lt_dados-bukrs.
      IF sy-subrc EQ 0.
        SORT gt_ztsd_dia_resp BY bukrs.
      ENDIF.

    ENDIF.

  ELSE.

    "Dados não encontrados para a seleção informada!
    MESSAGE TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_MONTA_RELATORIO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_monta_relatorio .

  DATA: lv_indice TYPE sy-tabix,
        lv_itmnum TYPE j_1bnflin-itmnum.

  "Montando Relatório......Por favor Aguarde!
  PERFORM zf_status_message USING TEXT-007.

  LOOP AT gt_ztsd_br147 INTO DATA(ls_ztsd_br147).

    MOVE: ls_ztsd_br147-zdia_a03 TO gs_final-zdia_a03,
          ls_ztsd_br147-zdia_a04 TO gs_final-zdia_a04,
          ls_ztsd_br147-zdia_a05 TO gs_final-zdia_a05,
          ls_ztsd_br147-zdia_a06 TO gs_final-zdia_a06,
          ls_ztsd_br147-zdia_b04 TO gs_final-zdia_b04,
          ls_ztsd_br147-zdia_c02 TO gs_final-zdia_c02,
          ls_ztsd_br147-zdia_b03 TO gs_final-zdia_b03,
          ls_ztsd_br147-zdia_b05 TO gs_final-zdia_b05,
          ls_ztsd_br147-zdia_c07 TO gs_final-zdia_c07,
          ls_ztsd_br147-zdia_d01 TO gs_final-zdia_d01,
          ls_ztsd_br147-zdia_cst TO gs_final-zdia_cst.

    MOVE: sy-datum TO gs_final-erdat,
          sy-uzeit TO gs_final-erzet,
          sy-uname TO gs_final-ernam.

    CONCATENATE ls_ztsd_br147-zdia_c06(4)
                '.'
                ls_ztsd_br147-zdia_c06+4(2)
                '.'
                ls_ztsd_br147-zdia_c06+6(2)
           INTO gs_final-zdia_c06.

    MOVE: 'S'  TO gs_final-zdia_conf,
          '1'  TO gs_final-zdia_ind,
          '00' TO gs_final-zdia_prod_acab.

*    READ TABLE gt_ztsd_br148 INTO DATA(ls_ztsd_br148)
*                          WITH KEY  zdia_a03  = ls_ztsd_br147-zdia_a03
*                                    zdia_a04  = ls_ztsd_br147-zdia_a04
*                                    zdia_a05  = ls_ztsd_br147-zdia_a05
*                                    zdia_a06  = ls_ztsd_br147-zdia_a06
*                                    zdia_b04  = ls_ztsd_br147-zdia_b04
*                                    zdia_c02  = ls_ztsd_br147-zdia_c02
*                                    BINARY SEARCH.
*
*
*    IF sy-subrc EQ 0.

    lv_itmnum = ls_ztsd_br147-zdia_c02 * 10.

    READ TABLE gt_dados INTO DATA(ls_data_aux)
                        WITH KEY regio   = ls_ztsd_br147-regio
                                 nfyear  = ls_ztsd_br147-nfyear
                                 nfmonth = ls_ztsd_br147-nfmonth
                                 stcd1   = ls_ztsd_br147-stcd1
                                 model   = ls_ztsd_br147-model
                                 serie   = ls_ztsd_br147-serie
                                 nfnum9  = ls_ztsd_br147-nfnum9
                                 docnum9 = ls_ztsd_br147-docnum9
                                 cdv     = ls_ztsd_br147-cdv
                                 itmnum  = lv_itmnum
                                 BINARY SEARCH.
    IF sy-subrc EQ 0.

      lv_indice = sy-tabix.

      LOOP AT gt_dados INTO DATA(ls_data) FROM lv_indice.

        IF ls_data-regio   NE ls_data_aux-regio OR
           ls_data-nfyear  NE ls_data_aux-nfyear OR
           ls_data-nfmonth NE ls_data_aux-nfmonth OR
           ls_data-stcd1   NE ls_data_aux-stcd1 OR
           ls_data-model   NE ls_data_aux-model OR
           ls_data-serie   NE ls_data_aux-serie OR
           ls_data-nfnum9  NE ls_data_aux-nfnum9 OR
           ls_data-docnum9 NE ls_data_aux-docnum9 OR
           ls_data-cdv     NE ls_data_aux-cdv OR
           ls_data-itmnum  NE lv_itmnum.

          EXIT.

        ENDIF.

        MOVE: icon_green_light TO gs_final-status.

        MOVE: ls_data-bukrs  TO gs_final-bukrs,
              ls_data-branch TO gs_final-branch,
              ls_data-docdat TO gs_final-docdat,
              ls_data-pstdat TO gs_final-pstdat,
              ls_data-docnum TO gs_final-docnum,
              ls_data-nftype TO gs_final-nftype,
              ls_data-direct TO gs_final-direct,
              ls_data-nfenum TO gs_final-nfenum,
              ls_data-cfop   TO gs_final-cfop,
              ls_data-itmnum TO gs_final-itmnum,
              ls_data-matnr  TO gs_final-zdia_c03,
              ls_data-maktx  TO gs_final-maktx,
              ls_data-matuse TO gs_final-matuse,
              ls_data-matorg TO gs_final-matorg,
              ls_data-parid  TO gs_final-parid,
              ls_data-name1  TO gs_final-name1,
              ls_data-regio  TO gs_final-regio,
              ls_data-netwr  TO gs_final-netwr,
              ls_data-nftot  TO gs_final-nftot.

        WRITE ls_data-cgc USING EDIT MASK
        '__.___.___/____-__'  TO gs_final-cgc.

        READ TABLE gt_ztsd_dia_codtrib INTO DATA(ls_ztsd_dia_codtrib)
                                       WITH KEY bukrs  = ls_data-bukrs
                                                branch = ls_data-branch
                                                cfop   = ls_data-cfop
                                                matuse = ls_data-matuse
                                                matorg = ls_data-matorg
                                                BINARY SEARCH.
        IF sy-subrc EQ 0.

          MOVE: ls_ztsd_dia_codtrib-zdia_c06 TO gs_final-zcod_trib,
                ls_ztsd_dia_codtrib-multipl  TO gs_final-zmultipl.

          gs_final-zdia_c09 = ( gs_final-zdia_c07 / 100 ) * ls_ztsd_dia_codtrib-multipl.

          CLEAR: ls_ztsd_dia_codtrib.
        ENDIF.

        READ TABLE gt_ztsd_dia_resp INTO DATA(ls_ztsd_dia_resp)
                                    WITH KEY bukrs = ls_data-bukrs
                                    BINARY SEARCH.
        IF sy-subrc EQ 0.

          MOVE: ls_ztsd_dia_resp-nome_resp  TO gs_final-zdia_a07,
                ls_ztsd_dia_resp-tel_resp   TO gs_final-zdia_a08,
                ls_ztsd_dia_resp-email_resp TO gs_final-zdia_a09.

          CLEAR: ls_ztsd_dia_resp.
        ENDIF.

        APPEND gs_final TO gt_final.

        CLEAR: gs_final-cgc,
               gs_final-zdia_a07,
               gs_final-zdia_a08,
               gs_final-zdia_a09,
               gs_final-zdia_c09,
               gs_final-bukrs,
               gs_final-branch,
               gs_final-docdat,
               gs_final-pstdat,
               gs_final-docnum,
               gs_final-nftype,
               gs_final-direct,
               gs_final-nfenum,
               gs_final-cfop,
               gs_final-itmnum,
               gs_final-zdia_c03,
               gs_final-maktx,
               gs_final-matuse,
               gs_final-matorg,
               gs_final-parid,
               gs_final-name1,
               gs_final-regio,
               gs_final-netwr,
               gs_final-nftot,
               gs_final-status,
               gs_final-zcod_trib,
               gs_final-zmultipl.

        CLEAR: ls_data.

      ENDLOOP.

      CLEAR: ls_data_aux.

    ELSE.

      MOVE: icon_red_light TO gs_final-status.

      APPEND gs_final TO gt_final.
      CLEAR: gs_final.

    ENDIF.

    CLEAR: ls_ztsd_br147,
           ls_data_aux.

  ENDLOOP.

  SORT gt_final BY zdia_b03
                   zdia_b04
                   zdia_c02.

  PERFORM zf_atualiza_txt_mat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_EXIBI_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_exibi_alv .

  CALL SCREEN 0100.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_FIELDCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_fieldcat.

  PERFORM zf_fill_catalog_column USING:

    1  '' 'STATUS' 'GT_FINAL' '' 'Status'  'Status'     'Status',
    2  '' 'ZDIA_A03' 'GT_FINAL' 'ZTSD_BR148' 'V. Lay Arq'  'Versão Layout Arq.'     'Versão Layout do Arquivo',
    3  '' 'ZDIA_A04' 'GT_FINAL' 'ZTSD_BR148' 'Ins. Est.'  'Ins Est Contr Decl'  'Ins Est Contr Declarante',
    4  '' 'ZDIA_A05' 'GT_FINAL' 'ZTSD_BR148' 'AnoApDANFE'  'Ano Apres.DANFE'     'Ano de Apresentação DANFE',
    5  '' 'ZDIA_A06' 'GT_FINAL' 'ZTSD_BR148' 'MêsApDANFE'  'Mês Apres.DANFE'     'Mês de Apresentação DANFE',
    6  '' 'ZDIA_B04' 'GT_FINAL' 'ZTSD_BR148' 'ChavAc NFe'  'Chave de Acesso NFe'     'Chave de Acesso NFe',
    7  '' 'ZDIA_C02' 'GT_FINAL' 'ZTSD_BR148' 'Item NFe'  'Item da NFe'     'Item da NFe',
    8  'X' 'BUKRS' 'GT_FINAL' 'ZTSD_BR148' 'Empresa'  'Empresa'     'Empresa',
    9  'X' 'BRANCH' 'GT_FINAL' 'ZTSD_BR148' 'Loc. Neg.'  'Loc Negócios'     'Local de negócios',
    10 'X' 'DOCDAT' 'GT_FINAL' 'J_1BNFDOC' 'Data doc.'  'Data doc.'     'Data do Documento',
    11 'X' 'PSTDAT' 'GT_FINAL' 'J_1BNFDOC' 'Dt.lçto.'  'Dt.lçto.'     'Data de Lançamento',
    12 'X' 'DOCNUM' 'GT_FINAL' 'ZTSD_BR148' 'Nº Doc.'  'Nº Documento'     'Nº documento',
    13 '' 'ZDIA_B03' 'GT_FINAL' 'ZTSD_BR148' 'NºOrd Nota'  'Nº de Ord da Nota'     'Nº de Ordem da Nota',
    14 '' 'ZDIA_CONF' 'GT_FINAL' 'ZTSD_BR148' 'ConfRecNFe'  'Conf. Rec. da NFe'     'Confirmação Receb. da NFe',
    15 '' 'ZDIA_B05' 'GT_FINAL' 'ZTSD_BR148' 'NºItensNFe'  'Nº Itens da NFe'     'Número de Itens da NFe',
    16 'X' 'MATNR' 'GT_FINAL'   'J_1BNFLIN' 'CódIntProd'  'Cód Interno Prod.'     'Código Interno do Produto',
    17 '' 'ZDIA_IND' 'GT_FINAL' 'ZTSD_BR148' 'Índ CGProd'  'Índ. Cód Geral Prod.'     'Índice Cód. Geral Prod.',
    18 '' 'ZDIA_C06' 'GT_FINAL' 'ZTSD_BR148' 'Cód G.Prod'  'Cód Geral Prod.'     'Código Geral do Produto',
    19 '' 'ZCOD_TRIB' 'GT_FINAL' 'ZTSD_BR148' 'Cód T.Trib'  'Cód. Tipo Tributação'     'Código Tipo de Tributação',
    20 '' 'ZDIA_C07' 'GT_FINAL' 'ZTSD_BR148' 'Vl ItemNFe'  'Valor Item NFe'     'Valor Item da NFe',
    21 '' 'ZMULTIPL' 'GT_FINAL' 'ZTSD_BR148' 'Vlr Mult.'  'Vlr Mult.'     'Valor Multiplicador',
    22 '' 'ZDIA_C09' 'GT_FINAL' 'ZTSD_BR148' 'Vlr Decl.'  'Vlr Imp. Declarado'     'Valor Imposto Declarado',
    23 '' 'ZDIA_PROD_ACAB' 'GT_FINAL' 'ZTSD_BR148' 'Prod.Acab.'  'Prod. Acabado'     'Produto Acabado',
    24 '' 'ZDIA_D01' 'GT_FINAL' 'ZTSD_BR148' 'Nº NT. Arq'  'Núm. Notas Arq'     'Número de Notas no Arquivo',
    25 'X' 'NFTYPE' 'GT_FINAL' 'ZTSD_BR148' 'Ctg. de NF'  'Ctg.de nota fiscal'     'Ctg.de nota fiscal',
    26 'X' 'DIRECT' 'GT_FINAL' 'ZTSD_BR148' 'Dir.M.Merc'  'Direção Mov. Merc.'     'Direção do Mov. de Mercadorias',
    27 'X' 'NFENUM' 'GT_FINAL' 'ZTSD_BR148' 'Número NFe'  'Número NFe'     'Nº NF Eletrônica',
    28 'X' 'CFOP' 'GT_FINAL' 'ZTSD_BR148' 'C.CFOP.Ext'  'Cód. CFOP e Extensão'     'Código CFOP e Extensão',
    29 'X' 'ITMNUM' 'GT_FINAL' 'ZTSD_BR148' 'Nº ItemDoc'  'Nº Item do Doc'     'Nº Item do Documento',
    30 ' ' 'MAKTX' 'GT_FINAL' 'ZTSD_BR148' 'Txt Mat.'  'Texto Breve Mat.'     'Texto breve de material',
    31 'X' 'MATUSE' 'GT_FINAL' 'ZTSD_BR148' 'Util. Mat.'  'Util. de Mat.'     'Utilização de Material',
    32 'X' 'MATORG' 'GT_FINAL' 'ZTSD_BR148' 'Orig. Mat.'  'Origem de Mat.'     'Origem de Mat.',
    33 '' 'ZDIA_CST' 'GT_FINAL' 'ZTSD_BR148' 'Nº de CST'  'Número de CST'     'Número de CST',
    34 'X' 'PARID' 'GT_FINAL' 'ZTSD_BR148' 'ID Parc.'  'Identificação Parc.'     'Identificação do Parceiro ',
    35 'X' 'CGC' 'GT_FINAL' 'ZTSD_BR148' 'Code CGC'  'Code CGC'     'Code CGC',
    36 'X' 'NAME1' 'GT_FINAL' 'ZTSD_BR148' 'Nome 1'  'Nome 1'     'Nome 1',
    37 'X' 'REGIO' 'GT_FINAL' 'ZTSD_BR148' 'Região'  'Região'     'Região',
    38 'X' 'NETWR' 'GT_FINAL' 'ZTSD_BR148' 'Vlr Líq.'  'Valor Líquido'     'Valor Líquido',
    39 'X' 'NFTOT' 'GT_FINAL' 'ZTSD_BR148' 'Vl.T.c/Imp'  'Valor Total c/ Imp.'     'Valor Total Incluindo os Impostos',
    40 '' 'ZDIA_A07' 'GT_FINAL' 'ZTSD_BR148' 'Nome Resp.'  'Nom.Resp. p/ Env.DIA'     'Nome Resp. p/ Envio da DIA',
    41 '' 'ZDIA_A08' 'GT_FINAL' 'ZTSD_BR148' 'Tel.Resp.'  'Tel.Resp. p/ Env.DIA'     'Tel.Resp. p/ Envio DIA',
    42 '' 'ZDIA_A09' 'GT_FINAL' 'ZTSD_BR148' 'Email.Resp'  'Email Resp Env.DIA'     'E-mail do Resp. p/ Env.DIA',
    43 '' 'ERDAT'    'GT_FINAL' 'ZTSD_BR148' 'Dt Cri Reg'  'Data Criação Reg.'      'Data de Criação do Registro',
    44 '' 'ERZET'    'GT_FINAL' 'ZTSD_BR148' 'Hora Reg.'   'Hora do Registro'    'Hora do Registro',
    45 '' 'ERNAM'    'GT_FINAL' 'ZTSD_BR148' 'Nome Resp.'  'Nome Resp p/ Reg'  'Nome Resp pelo Reg.',
    46 '' 'MANDT'    'GT_FINAL' 'ZTSD_BR148'      ''      ''      ''.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_FILL_CATALOG_COLUMN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_fill_catalog_column  USING u_pos        TYPE sy-cucol
                                   u_edit       TYPE c
                                   u_field      TYPE slis_fieldname
                                   u_tabname    TYPE slis_tabname
                                   u_ddictab    TYPE slis_tabname
                                   u_short_text TYPE dd03p-scrtext_s
                                   u_med_text   TYPE dd03p-scrtext_m
                                   u_long_text  TYPE dd03p-scrtext_l.

  DATA: lw_fieldcat TYPE lvc_s_fcat.

  CLEAR lw_fieldcat.

  lw_fieldcat-col_pos   = u_pos.
  lw_fieldcat-fieldname = u_field.
  lw_fieldcat-scrtext_l  = u_long_text.
  lw_fieldcat-scrtext_m  = u_med_text.
  lw_fieldcat-scrtext_s  = u_short_text.
  lw_fieldcat-selddictxt = 'L'.
  lw_fieldcat-ref_field  = u_field.
  lw_fieldcat-ref_table  = u_ddictab.
  lw_fieldcat-edit = u_edit.

  IF p_show EQ abap_true
     AND gv_modify EQ abap_false.

    CLEAR: lw_fieldcat-edit.

  ENDIF.

  IF lw_fieldcat-fieldname EQ 'MANDT'.
    lw_fieldcat-no_out = 'X'.
  ENDIF.

  IF lw_fieldcat-fieldname EQ 'ZDIA_PROD_ACAB'.
    lw_fieldcat-datatype = 'CHAR'.
    lw_fieldcat-outputlen = 2.
    lw_fieldcat-just = 'R'.
  ENDIF.

  IF lw_fieldcat-fieldname EQ 'ZDIA_C07' OR
     lw_fieldcat-fieldname EQ 'ZDIA_C09' OR
     lw_fieldcat-fieldname EQ 'NETWR' OR
     lw_fieldcat-fieldname EQ 'NFTOT'.

    lw_fieldcat-datatype = 'CURR'.

  ENDIF.

*Ronaldo Alves - Correção DUMP ao filtrar por NFE - 19.02.2019.
  IF lw_fieldcat-fieldname EQ 'ZDIA_B04'.
    CLEAR:lw_fieldcat-ref_table,
          lw_fieldcat-ref_field.
    lw_fieldcat-datatype = 'CHAR'.
    lw_fieldcat-outputlen = 44.
  ENDIF.
*Fim.

  APPEND lw_fieldcat TO gt_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_EXCLUI_BOTOES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_exclui_botoes .

  DATA: ls_exclude TYPE ui_func.

*  ls_exclude = cl_gui_alv_grid=>mc_fc_auf.                   APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_average.               APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_back_classic.          APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_call_abc.              APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_call_chain.            APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_call_crbatch.          APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_call_crweb.            APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_call_lineitems.        APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_call_master_data.      APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_call_more.             APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_call_report.           APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_call_xint.             APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_call_xml_export.       APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_call_xxl.              APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_check.                 APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_col_invisible.         APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_col_optimize.          APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_count.                 APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_current_variant.       APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_data_save.             APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_delete_filter.         APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_deselect_all.          APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_detail.                APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_expcrdata.             APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_expcrdesig.            APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_expcrtempl.            APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_expmdb.                APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_extend.                APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_f4.                    APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_filter.                APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_find.                  APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_find_more.             APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_fix_columns.           APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.                 APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_help.                  APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_html.                  APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_info.                  APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_load_variant.          APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.        APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.              APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.          APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.               APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.        APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.        APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.          APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.             APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.     APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.              APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_maintain_variant.      APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_maximum.               APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_minimum.               APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_pc_file.               APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_print.                 APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_print_back.            APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_print_prev.            APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.               APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_reprep.                APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_save_variant.          APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_select_all.            APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_send.                  APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_sort.                  APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_asc.              APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_dsc.              APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_subtot.                APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_sum.                   APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_to_office.             APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_to_rep_tree.           APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_unfix_columns.         APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard. APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_variant_admin.         APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_views.                 APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_view_crystal.          APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_view_excel.            APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_view_grid.             APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_view_lotus.            APPEND ls_exclude TO gt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_word_processor.        APPEND ls_exclude TO gt_exclude.

  SET PF-STATUS 'GUI_0100' EXCLUDING gt_exclude.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_VALIDA_EMPRESA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_valida_empresa  USING u_bukrs
                              u_row_id
                     CHANGING c_erro.

  DATA: lv_bukrs TYPE t001-bukrs,
        lv_msg2  TYPE symsgv.

  SELECT SINGLE bukrs
    FROM t001
    INTO lv_bukrs
   WHERE bukrs EQ u_bukrs.
  IF sy-subrc IS NOT INITIAL.

    lv_msg2 = u_bukrs.

    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
        titel = 'ATENÇÃO!'
        msgid = 'ZSD'
        msgty = 'E'
        msgno = '000'
        msgv1 = 'Código da Empresa'
        msgv2 = lv_msg2
        msgv3 = 'Inválido!'.

    MOVE: abap_true TO c_erro.

  ELSE.

    IF u_row_id IS NOT INITIAL.

      READ TABLE gt_final ASSIGNING FIELD-SYMBOL(<fs_data>)
                                           INDEX u_row_id.
      IF sy-subrc EQ 0.

        SELECT SINGLE nome_resp
                      tel_resp
                      email_resp
          FROM ztsd_dia_resp
          INTO (<fs_data>-zdia_a07,
                <fs_data>-zdia_a08,
                <fs_data>-zdia_a09)
         WHERE bukrs EQ u_bukrs.
        IF sy-subrc EQ 0.

          CALL METHOD o_alv->refresh_table_display
            EXCEPTIONS
              finished = 1
              OTHERS   = 2.

          CALL METHOD cl_gui_cfw=>flush.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_VALIDA_LOC_NEG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_valida_loc_neg  USING u_value
                              u_row_id
                     CHANGING c_erro.

  DATA: lv_branch TYPE j_1bbranch-branch,
        lv_msg2   TYPE symsgv.

  READ TABLE gt_final INTO DATA(ls_final) INDEX u_row_id.
  IF sy-subrc EQ 0.

    IF ls_final-bukrs IS INITIAL.

      CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
        EXPORTING
          titel = 'ATENÇÃO!'
          msgid = 'ZSD'
          msgty = 'E'
          msgno = '000'
          msgv1 = 'Informar o'
          msgv2 = 'Código da Empresa!'.

      MOVE: abap_true TO c_erro.

    ELSE.

      SELECT SINGLE branch
        FROM j_1bbranch
        INTO lv_branch
       WHERE bukrs EQ ls_final-bukrs
         AND branch EQ u_value.
      IF sy-subrc NE 0.

        lv_msg2 = u_value.

        CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
          EXPORTING
            titel = 'ATENÇÃO!'
            msgid = 'ZSD'
            msgty = 'E'
            msgno = '000'
            msgv1 = 'Local de Negócio'
            msgv2 = lv_msg2
            msgv3 = 'Inválido!'.

        MOVE: abap_true TO c_erro.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_VALIDA_TABELA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_valida_tabela .

  DATA: lv_index   TYPE sy-tabix,
        lv_erro,
        l_valid(1) TYPE c,
        vx_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'ATENÇÃO!'
      text_question         = 'Salvar Alterações?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      display_cancel_button = 'X'
    IMPORTING
      answer                = vx_answer.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  IF vx_answer EQ '1'.

    IF NOT o_alv IS INITIAL.
      CALL METHOD o_alv->check_changed_data
        IMPORTING
          e_valid = l_valid.
    ENDIF.

    LOOP AT gt_final INTO DATA(ls_final).

      lv_index = sy-tabix.

      IF ls_final-bukrs IS NOT INITIAL.

        PERFORM zf_valida_empresa USING ls_final-bukrs
                                        ''
                               CHANGING lv_erro.

        IF lv_erro IS NOT INITIAL.
          EXIT.
        ENDIF.

      ENDIF.

      IF ls_final-branch IS NOT INITIAL.

        PERFORM zf_valida_loc_neg USING ls_final-branch
                                        lv_index
                               CHANGING lv_erro.

        IF lv_erro IS NOT INITIAL.
          EXIT.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_ATUALIZA_TXT_MAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_atualiza_txt_mat.

  TYPES: BEGIN OF ty_makt,
           matnr TYPE makt-matnr,
           maktx TYPE makt-maktx,
         END OF ty_makt.

  DATA: lt_final TYPE STANDARD TABLE OF ty_final,
        lt_makt  TYPE STANDARD TABLE OF ty_makt.

  LOOP AT gt_final ASSIGNING FIELD-SYMBOL(<fs_final_1>).

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = <fs_final_1>-zdia_c03
      IMPORTING
        output = <fs_final_1>-matnr.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDLOOP.

  lt_final[] = gt_final[].

  DELETE ADJACENT DUPLICATES FROM lt_final
                        COMPARING zdia_c03.

  DELETE lt_final WHERE zdia_c03 IS INITIAL.

  IF lt_final IS NOT INITIAL.

    SELECT matnr
           maktx
      FROM makt
      INTO TABLE lt_makt
       FOR ALL ENTRIES IN lt_final
     WHERE matnr EQ lt_final-matnr
       AND spras EQ sy-langu.
    IF sy-subrc EQ 0.

      SORT lt_makt BY matnr.

      LOOP AT gt_final ASSIGNING FIELD-SYMBOL(<fs_final_2>).

        READ TABLE lt_makt INTO DATA(ls_makt)
                            WITH KEY matnr = <fs_final_2>-matnr
                            BINARY SEARCH.
        IF sy-subrc EQ 0.
          MOVE: ls_makt-maktx TO <fs_final_2>-maktx.
        ENDIF.

        CLEAR: ls_makt.

      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_ATUALIZA_CAMPOS_ALV
*&---------------------------------------------------------------------*
*& Atualiza campos:
*& Valor Mult / Cod. T. Trib / Valor Imposto Declarado
*&---------------------------------------------------------------------*
FORM zf_atualiza_campos_alv USING u_row_id
                                  u_fieldname
                                  u_value.

  READ TABLE gt_final ASSIGNING FIELD-SYMBOL(<fs_data>)
                                       INDEX u_row_id.
  IF sy-subrc EQ 0.

    CASE u_fieldname.
      WHEN 'BUKRS'.
        <fs_data>-bukrs = u_value.
      WHEN 'BRANCH'.
        <fs_data>-branch = u_value.
      WHEN 'CFOP'.
        <fs_data>-cfop = u_value.
      WHEN 'MATUSE'.
        <fs_data>-matuse = u_value.
      WHEN 'MATORG'.
        <fs_data>-matorg = u_value.
      WHEN OTHERS.
    ENDCASE.

    IF <fs_data>-bukrs IS NOT INITIAL
      AND <fs_data>-branch IS NOT INITIAL
      AND <fs_data>-cfop IS NOT INITIAL
      AND <fs_data>-matuse IS NOT INITIAL
      AND <fs_data>-matorg IS NOT INITIAL
      AND <fs_data>-zdia_c07 IS NOT INITIAL.

      SELECT SINGLE multipl
                    zdia_c06
        FROM ztsd_dia_codtrib
        INTO (<fs_data>-zmultipl, <fs_data>-zcod_trib)
       WHERE bukrs  EQ <fs_data>-bukrs
         AND branch EQ <fs_data>-branch
         AND cfop   EQ <fs_data>-cfop
         AND matuse EQ <fs_data>-matuse
         AND matorg EQ <fs_data>-matorg.
      IF sy-subrc EQ 0.

        <fs_data>-zdia_c09 = ( <fs_data>-zdia_c07 / 100 ) * <fs_data>-zmultipl.

        CALL METHOD o_alv->refresh_table_display
          EXCEPTIONS
            finished = 1
            OTHERS   = 2.

        CALL METHOD cl_gui_cfw=>flush.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_SALVA_DADOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_salva_dados.

  LOOP AT gt_final INTO gs_final.

    MOVE-CORRESPONDING gs_final TO gs_ztsd_br148.
    gs_ztsd_br148-mandt = sy-mandt.

    IF gs_ztsd_br148-cgc IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_INPUT'
        EXPORTING
          input     = gs_ztsd_br148-cgc
        IMPORTING
          output    = gs_ztsd_br148-cgc
        EXCEPTIONS
          not_valid = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDIF.

    APPEND gs_ztsd_br148 TO gt_ztsd_br148.
    CLEAR: gs_final.

  ENDLOOP.

  MODIFY ztsd_br148 FROM TABLE gt_ztsd_br148.
  COMMIT WORK.

  CLEAR: gt_ztsd_br148[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_GERAR_XML
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_gerar_xml .

  DATA: lo_xml TYPE REF TO cl_xml_document.
  DATA: ixml TYPE REF TO if_ixml.
  DATA: ls_table TYPE ztsd_xml_suframa.
  DATA: encoding TYPE REF TO if_ixml_encoding.
  DATA: stream_factory TYPE REF TO if_ixml_stream_factory.
  DATA: document TYPE REF TO if_ixml_document.
  DATA: iterator TYPE REF TO if_ixml_node_iterator.
  DATA: node TYPE REF TO if_ixml_node.
  DATA: lt_chave_acesso TYPE zcsd_pim_suframa_nf.
  DATA: lv_xml TYPE xstring.
  DATA: lv_chaveacesso TYPE string.
  DATA: lv_retcode TYPE sy-subrc.
  DATA: lv_file TYPE localfile.
  DATA: lv_path TYPE string.

  DATA: lv_ie_cont_dec           TYPE char9,
        lv_anoapres              TYPE string,
        lv_mesapres              TYPE string,
        lv_nomeresp              TYPE string,
        lv_foneresp              TYPE string,
        lv_emailresp             TYPE string,
        lv_num_nf_arq            TYPE string,
        lv_versao                TYPE string,
        lv_notas                 TYPE string,
        lv_numordemnota          TYPE string,
        lv_chave_nfe             TYPE string,
        lv_reconhece_nfe         TYPE string,
        lv_notafiscal            TYPE string,
        lv_xml_final             TYPE string,
        lv_num_itens             TYPE string,
        lv_zdia_b05              TYPE string,
        lv_produto               TYPE string,
        lv_numitemnfe            TYPE string,
        lv_codinternoproduto     TYPE string,
        lv_indicecodgeralproduto TYPE string,
        lv_codgeralproduto       TYPE string,
        lv_codtipotributacao     TYPE string,
        lv_valorbasecalculoitem  TYPE string,
        lv_valormultiplicador    TYPE string,
        lv_vlr_imp_dec           TYPE string,
        lv_zdia_conf             TYPE string.

  CONSTANTS: lc_barra TYPE c VALUE '\'.
  CONSTANTS: lc_true(4) TYPE c VALUE 'true'.
  CONSTANTS: lc_null(4) TYPE c VALUE 'NULL'.

  DATA: ls_final_aux TYPE ty_final.
  DATA: ls_final_1 TYPE ty_final.
  DATA: lt_final_aux TYPE STANDARD TABLE OF ty_final.
  DATA: lv_indice TYPE sy-tabix,
        lv_flag   TYPE c.

  lt_final_aux[] = gt_final[].

  DELETE ADJACENT DUPLICATES FROM lt_final_aux
                        COMPARING zdia_b03
                                  zdia_b04.

  LOOP AT lt_final_aux INTO ls_final_1.

    ls_final_aux = ls_final_1.

    AT FIRST.

      MOVE: ls_final_aux-zdia_a04+6(9) TO lv_ie_cont_dec,
            ls_final_aux-zdia_a05 TO lv_anoapres,
            ls_final_aux-zdia_a06 TO lv_mesapres,
            ls_final_aux-zdia_a07 TO lv_nomeresp,
            ls_final_aux-zdia_a08 TO lv_foneresp,
            ls_final_aux-zdia_a09 TO lv_emailresp,
            ls_final_aux-zdia_conf TO lv_zdia_conf,
            '01' TO lv_versao.

      PACK: ls_final_aux-zdia_d01 TO lv_num_nf_arq.

    ENDAT.

    READ TABLE gt_final INTO gs_final
                        WITH KEY zdia_b03 = ls_final_1-zdia_b03
                                 zdia_b04 = ls_final_1-zdia_b04
                                 BINARY SEARCH.
    IF sy-subrc EQ 0.

      lv_indice = sy-tabix.

      CLEAR: lv_numordemnota,
             lv_notafiscal,
             lv_num_itens,
             lv_chave_nfe,
             lv_reconhece_nfe.

      PACK: ls_final_aux-zdia_b05 TO lv_zdia_b05.

      MOVE: ls_final_aux-zdia_b03 TO lv_numordemnota.
      SHIFT lv_numordemnota LEFT DELETING LEADING '0'.

      lv_notafiscal = |<listaNotasFiscais> <notaFiscal numOrdemNota="{ lv_numordemnota }">|.
      lv_num_itens = |<numItens>{ lv_zdia_b05 }</numItens>|.
      lv_chave_nfe = |{ lv_notafiscal }<chaveNFe>{ ls_final_aux-zdia_b04 }</chaveNFe>|.
      lv_reconhece_nfe = |{ lv_chave_nfe }| &&
                         |<reconheceNFe>{ lv_zdia_conf }</reconheceNFe>| &&
                         |<nfeInformadaPeloContribuinte>{ 'N' }</nfeInformadaPeloContribuinte>| &&
                         |{ lv_num_itens }|.

      LOOP AT gt_final INTO ls_final_aux FROM lv_indice.

        IF ls_final_aux-zdia_b03 NE ls_final_1-zdia_b03 OR
           ls_final_aux-zdia_b04 NE ls_final_1-zdia_b04.

          EXIT.

        ENDIF.

        "Detalhes do item:
        MOVE: ls_final_aux-zdia_c06 TO lv_codgeralproduto,
              ls_final_aux-zdia_ind TO lv_indicecodgeralproduto,
              ls_final_aux-zdia_c03 TO lv_codinternoproduto,
              ls_final_aux-zcod_trib TO lv_codtipotributacao,
              ls_final_aux-zdia_c07 TO lv_valorbasecalculoitem,
              ls_final_aux-zmultipl TO lv_valormultiplicador,
              ls_final_aux-zdia_c09 TO lv_vlr_imp_dec.

        PACK: ls_final_aux-zdia_c02 TO lv_numitemnfe.

        IF lv_flag IS INITIAL.

          lv_produto = |{ lv_reconhece_nfe }| &&
                       |<produto><numItemNfe>{ lv_numitemnfe }</numItemNfe>| &&
                       |<codInternoProduto>{ lv_codinternoproduto }</codInternoProduto>| &&
                       |<indiceCodGeralProduto>{ lv_indicecodgeralproduto }</indiceCodGeralProduto>| &&
                       |<codGeralProduto>{ lv_codgeralproduto }</codGeralProduto>| &&
                       |<codTipoTributacao>{ lv_codtipotributacao }</codTipoTributacao>| &&
                       |<valorBaseCalculoItem>{ lv_valorbasecalculoitem }</valorBaseCalculoItem>| &&
                       |<valorMultiplicador>{ lv_valormultiplicador }</valorMultiplicador>| &&
                       |<valorImpostoDeclarado>{ lv_vlr_imp_dec }</valorImpostoDeclarado>| &&
                       |<prodAcabado>{ '00' }</prodAcabado>| &&
                       |</produto>|.

          MOVE: abap_true TO lv_flag.

        ELSE.

          lv_produto = |{ lv_produto }<produto><numItemNfe>{ lv_numitemnfe }</numItemNfe>| &&
                       |<codInternoProduto>{ lv_codinternoproduto }</codInternoProduto>| &&
                       |<indiceCodGeralProduto>{ lv_indicecodgeralproduto }</indiceCodGeralProduto>| &&
                       |<codGeralProduto>{ lv_codgeralproduto }</codGeralProduto>| &&
                       |<codTipoTributacao>{ lv_codtipotributacao }</codTipoTributacao>| &&
                       |<valorBaseCalculoItem>{ lv_valorbasecalculoitem }</valorBaseCalculoItem>| &&
                       |<valorMultiplicador>{ lv_valormultiplicador }</valorMultiplicador>| &&
                       |<valorImpostoDeclarado>{ lv_vlr_imp_dec }</valorImpostoDeclarado>| &&
                       |<prodAcabado>{ '00' }</prodAcabado>| &&
                       |</produto>|.

        ENDIF.

        CLEAR: ls_final_aux.
      ENDLOOP.

      CLEAR: gs_final.
    ENDIF.

    AT LAST.
      lv_notas = |{ lv_produto }</notaFiscal></listaNotasFiscais>|.
      lv_xml_final  = |<infDeclaracaoMensal versao="{ lv_versao }">| &&
                      |<ieContribuinteDeclarante>{ lv_ie_cont_dec }</ieContribuinteDeclarante>| &&
                      |<anoApresentacao>{ lv_anoapres }</anoApresentacao>| &&
                      |<mesApresentacao>{ lv_mesapres }</mesApresentacao>| &&
                      |<nomeResponsavel>{ lv_nomeresp }</nomeResponsavel>| &&
                      |<foneResponsavel>{ lv_foneresp }</foneResponsavel>| &&
                      |<emailResponsavel>{ lv_emailresp }</emailResponsavel>| &&
*                      |{ lv_notas }</infDeclaracaoMensal>|.
                      |{ lv_notas }|.

    ENDAT.

    CLEAR: ls_final_1.
  ENDLOOP.

  lv_xml = cl_abap_codepage=>convert_to(
*  |<?xml version="1.0" encoding="UTF-8" ?>| &&
  |<enviDeclaracaoAutodesembaraco | &&
  |xsi:schemaLocation="http://www.sefaz.am.gov.br/autodesembaraco enviDeclaracaoMensalAuto_v1.02.xsd" | &&
  |xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" | &&
  |xmlns="http://www.sefaz.am.gov.br/autodesembaraco">| &&
  |{ lv_xml_final }| &&
  |<numNotasArquivo>{ lv_num_nf_arq }</numNotasArquivo>| &&
  |</infDeclaracaoMensal>| &&
  |</enviDeclaracaoAutodesembaraco>| ).

  ixml = cl_ixml=>create( ).

  stream_factory = ixml->create_stream_factory( ).

  document = ixml->create_document( ).

  IF ixml->create_parser( document       = document
                          stream_factory = stream_factory
                          istream = stream_factory->create_istream_xstring( string = lv_xml ) )->parse( ) <> 0.

  ENDIF.

  iterator = document->create_iterator( ).

  DO.
    node = iterator->get_next( ).
    IF node IS INITIAL.
      EXIT.
    ENDIF.
    IF node->get_type( ) = if_ixml_node=>co_node_text.
      node->set_value( to_upper( node->get_value( ) ) ).
    ENDIF.
  ENDDO.

  CLEAR lv_xml.

  document->render( ostream = ixml->create_stream_factory( )->create_ostream_xstring( string = lv_xml ) ).

  CREATE OBJECT lo_xml.

  CALL METHOD lo_xml->parse_xstring
    EXPORTING
      stream  = lv_xml
    RECEIVING
      retcode = lv_retcode.

  CALL METHOD lo_xml->set_encoding
    EXPORTING
      charset = 'UTF-8'.

  CALL METHOD lo_xml->display.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_EXIBI_XML
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_exibi_xml.

  DATA: lo_xml TYPE REF TO cl_xml_document.
  DATA: ixml TYPE REF TO if_ixml.
  DATA: encoding TYPE REF TO if_ixml_encoding.
  DATA: stream_factory TYPE REF TO if_ixml_stream_factory.
  DATA: document TYPE REF TO if_ixml_document.
  DATA: iterator TYPE REF TO if_ixml_node_iterator.
  DATA: node TYPE REF TO if_ixml_node.
  DATA: lv_xml TYPE xstring.
  DATA: lv_chaveacesso TYPE string.
  DATA: lv_retcode TYPE sy-subrc.

  DATA: lv_ie_cont_dec           TYPE char9,
        lv_anoapres              TYPE string,
        lv_mesapres              TYPE string,
        lv_nomeresp              TYPE string,
        lv_foneresp              TYPE string,
        lv_emailresp             TYPE string,
        lv_num_nf_arq            TYPE string,
        lv_versao                TYPE string,
        lv_notas                 TYPE string,
        lv_numordemnota          TYPE string,
        lv_chave_nfe             TYPE string,
        lv_reconhece_nfe         TYPE string,
        lv_notafiscal            TYPE string,
        lv_xml_final             TYPE string,
        lv_num_itens             TYPE string,
        lv_zdia_b05              TYPE string,
        lv_produto               TYPE string,
        lv_numitemnfe            TYPE string,
        lv_codinternoproduto     TYPE string,
        lv_indicecodgeralproduto TYPE string,
        lv_codgeralproduto       TYPE string,
        lv_codtipotributacao     TYPE string,
        lv_valorbasecalculoitem  TYPE string,
        lv_valormultiplicador    TYPE string,
        lv_vlr_imp_dec           TYPE string,
        lv_zdia_conf             TYPE string.

  DATA: ls_final_aux TYPE ty_final.
  DATA: ls_final_1 TYPE ty_final.
  DATA: lt_final_aux TYPE STANDARD TABLE OF ty_final.
  DATA: lv_indice TYPE sy-tabix,
        lv_flag   TYPE c.

  CONSTANTS: lc_barra TYPE c VALUE '\'.
  CONSTANTS: lc_true(4) TYPE c VALUE 'true'.
  CONSTANTS: lc_null(4) TYPE c VALUE 'NULL'.

  lt_final_aux[] = gt_final[].

  DELETE ADJACENT DUPLICATES FROM lt_final_aux
                        COMPARING zdia_b03
                                  zdia_b04.

  LOOP AT lt_final_aux INTO ls_final_1.

    ls_final_aux = ls_final_1.

    AT FIRST.

      MOVE: ls_final_aux-zdia_a04+6(9) TO lv_ie_cont_dec,
            ls_final_aux-zdia_a05 TO lv_anoapres,
            ls_final_aux-zdia_a06 TO lv_mesapres,
            ls_final_aux-zdia_a07 TO lv_nomeresp,
            ls_final_aux-zdia_a08 TO lv_foneresp,
            ls_final_aux-zdia_a09 TO lv_emailresp,
            ls_final_aux-zdia_conf TO lv_zdia_conf,
            '01' TO lv_versao.

      PACK: ls_final_aux-zdia_d01 TO lv_num_nf_arq.

    ENDAT.

    READ TABLE gt_final INTO gs_final
                        WITH KEY zdia_b03 = ls_final_1-zdia_b03
                                 zdia_b04 = ls_final_1-zdia_b04
                                 BINARY SEARCH.
    IF sy-subrc EQ 0.

      lv_indice = sy-tabix.
      CLEAR lv_flag.

      CLEAR: lv_numordemnota,
             lv_notafiscal,
             lv_num_itens,
             lv_chave_nfe,
             lv_reconhece_nfe.

      PACK: ls_final_aux-zdia_b05 TO lv_zdia_b05.

      MOVE: ls_final_aux-zdia_b03 TO lv_numordemnota.
      SHIFT lv_numordemnota LEFT DELETING LEADING '0'.

      lv_notafiscal = |<notaFiscal numOrdemNota="{ lv_numordemnota }">|.
      lv_num_itens = |<numItens>{ lv_zdia_b05 }</numItens>|.
      lv_chave_nfe = |{ lv_notafiscal }<chaveNFe>{ ls_final_aux-zdia_b04 }</chaveNFe>|.
      lv_reconhece_nfe = |{ lv_chave_nfe }| &&
                         |<reconheceNFe>{ lv_zdia_conf }</reconheceNFe>| &&
                         |<nfeInformadaPeloContribuinte>{ 'N' }</nfeInformadaPeloContribuinte>| &&
                         |{ lv_num_itens }|.

      LOOP AT gt_final INTO ls_final_aux FROM lv_indice.

        IF ls_final_aux-zdia_b03 NE ls_final_1-zdia_b03 OR
           ls_final_aux-zdia_b04 NE ls_final_1-zdia_b04.

          EXIT.

        ENDIF.

        "Detalhes do item:
        MOVE: ls_final_aux-zdia_c06 TO lv_codgeralproduto,
              ls_final_aux-zdia_ind TO lv_indicecodgeralproduto,
              ls_final_aux-zdia_c03 TO lv_codinternoproduto,
              ls_final_aux-zcod_trib TO lv_codtipotributacao,
              ls_final_aux-zdia_c07 TO lv_valorbasecalculoitem,
              ls_final_aux-zmultipl TO lv_valormultiplicador,
              ls_final_aux-zdia_c09 TO lv_vlr_imp_dec.

        PACK: ls_final_aux-zdia_c02 TO lv_numitemnfe.

        IF lv_flag IS INITIAL.

          IF lv_produto IS INITIAL.

            lv_produto = |{ lv_reconhece_nfe }| &&
                         |<produto><numItemNfe>{ lv_numitemnfe }</numItemNfe>| &&
                         |<codInternoProduto>{ lv_codinternoproduto }</codInternoProduto>| &&
                         |<indiceCodGeralProduto>{ lv_indicecodgeralproduto }</indiceCodGeralProduto>| &&
                         |<codGeralProduto>{ lv_codgeralproduto }</codGeralProduto>| &&
                         |<codTipoTributacao>{ lv_codtipotributacao }</codTipoTributacao>| &&
                         |<valorBaseCalculoItem>{ lv_valorbasecalculoitem }</valorBaseCalculoItem>| &&
                         |<valorMultiplicador>{ lv_valormultiplicador }</valorMultiplicador>| &&
                         |<valorImpostoDeclarado>{ lv_vlr_imp_dec }</valorImpostoDeclarado>| &&
                         |<prodAcabado>{ '00' }</prodAcabado>| &&
                         |</produto>|.

          ELSE.

            lv_produto = |{ lv_produto }{ lv_reconhece_nfe }| &&
                         |<produto><numItemNfe>{ lv_numitemnfe }</numItemNfe>| &&
                         |<codInternoProduto>{ lv_codinternoproduto }</codInternoProduto>| &&
                         |<indiceCodGeralProduto>{ lv_indicecodgeralproduto }</indiceCodGeralProduto>| &&
                         |<codGeralProduto>{ lv_codgeralproduto }</codGeralProduto>| &&
                         |<codTipoTributacao>{ lv_codtipotributacao }</codTipoTributacao>| &&
                         |<valorBaseCalculoItem>{ lv_valorbasecalculoitem }</valorBaseCalculoItem>| &&
                         |<valorMultiplicador>{ lv_valormultiplicador }</valorMultiplicador>| &&
                         |<valorImpostoDeclarado>{ lv_vlr_imp_dec }</valorImpostoDeclarado>| &&
                         |<prodAcabado>{ '00' }</prodAcabado>| &&
                         |</produto>|.

          ENDIF.

          MOVE: 'X' TO lv_flag.

        ELSE.

          lv_produto = |{ lv_produto }<produto><numItemNfe>{ lv_numitemnfe }</numItemNfe>| &&
                       |<codInternoProduto>{ lv_codinternoproduto }</codInternoProduto>| &&
                       |<indiceCodGeralProduto>{ lv_indicecodgeralproduto }</indiceCodGeralProduto>| &&
                       |<codGeralProduto>{ lv_codgeralproduto }</codGeralProduto>| &&
                       |<codTipoTributacao>{ lv_codtipotributacao }</codTipoTributacao>| &&
                       |<valorBaseCalculoItem>{ lv_valorbasecalculoitem }</valorBaseCalculoItem>| &&
                       |<valorMultiplicador>{ lv_valormultiplicador }</valorMultiplicador>| &&
                       |<valorImpostoDeclarado>{ lv_vlr_imp_dec }</valorImpostoDeclarado>| &&
                       |<prodAcabado>{ '00' }</prodAcabado>| &&
                       |</produto>|.

        ENDIF.

        CLEAR: ls_final_aux.
      ENDLOOP.

      CLEAR: gs_final.
    ENDIF.

    lv_produto = |{ lv_produto }</notaFiscal>|.

    AT LAST.
      lv_notas = |{ lv_produto }|.
      lv_xml_final  = |<infDeclaracaoMensal versao="{ lv_versao }">| &&
                      |<ieContribuinteDeclarante>{ lv_ie_cont_dec }</ieContribuinteDeclarante>| &&
                      |<anoApresentacao>{ lv_anoapres }</anoApresentacao>| &&
                      |<mesApresentacao>{ lv_mesapres }</mesApresentacao>| &&
                      |<nomeResponsavel>{ lv_nomeresp }</nomeResponsavel>| &&
                      |<foneResponsavel>{ lv_foneresp }</foneResponsavel>| &&
                      |<emailResponsavel>{ lv_emailresp }</emailResponsavel>| &&
                      |<listaNotasFiscais>{ lv_notas }</listaNotasFiscais>|.

    ENDAT.

    CLEAR: ls_final_1.
  ENDLOOP.

  lv_xml = cl_abap_codepage=>convert_to(
*  |<?xml version="1.0" encoding="UTF-8" ?>| &&
  |<enviDeclaracaoAutodesembaraco | &&
  |xsi:schemaLocation="http://www.sefaz.am.gov.br/autodesembaraco enviDeclaracaoMensalAuto_v1.02.xsd" | &&
  |xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" | &&
  |xmlns="http://www.sefaz.am.gov.br/autodesembaraco">| &&
  |{ lv_xml_final }| &&
  |<numNotasArquivo>{ lv_num_nf_arq }</numNotasArquivo>| &&
  |</infDeclaracaoMensal>| &&
  |</enviDeclaracaoAutodesembaraco>| ).

  ixml = cl_ixml=>create( ).

  stream_factory = ixml->create_stream_factory( ).

  document = ixml->create_document( ).

  IF ixml->create_parser( document       = document
                          stream_factory = stream_factory
                          istream = stream_factory->create_istream_xstring( string = lv_xml ) )->parse( ) <> 0.

  ENDIF.

  iterator = document->create_iterator( ).

  DO.
    node = iterator->get_next( ).
    IF node IS INITIAL.
      EXIT.
    ENDIF.
    IF node->get_type( ) = if_ixml_node=>co_node_text.
      node->set_value( to_upper( node->get_value( ) ) ).
    ENDIF.
  ENDDO.

  CLEAR lv_xml.

  document->render( ostream = ixml->create_stream_factory( )->create_ostream_xstring( string = lv_xml ) ).

  CREATE OBJECT lo_xml.

  CALL METHOD lo_xml->parse_xstring
    EXPORTING
      stream  = lv_xml
    RECEIVING
      retcode = lv_retcode.

  CALL METHOD lo_xml->set_encoding
    EXPORTING
      charset = 'UTF-8'.

  CALL METHOD lo_xml->display.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_DOWNLOAD_XML
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_download_xml .

  DATA: lo_xml TYPE REF TO cl_xml_document.
  DATA: ixml TYPE REF TO if_ixml.
  DATA: ls_table TYPE ztsd_xml_suframa.
  DATA: encoding TYPE REF TO if_ixml_encoding.
  DATA: stream_factory TYPE REF TO if_ixml_stream_factory.
  DATA: document TYPE REF TO if_ixml_document.
  DATA: iterator TYPE REF TO if_ixml_node_iterator.
  DATA: node TYPE REF TO if_ixml_node.
  DATA: lt_chave_acesso TYPE zcsd_pim_suframa_nf.
  DATA: lv_xml TYPE xstring.
  DATA: lv_chaveacesso TYPE string.
  DATA: lv_retcode TYPE sy-subrc.
  DATA: lv_file TYPE localfile.
  DATA: lv_path TYPE string.

  DATA: lv_ie_cont_dec           TYPE char9,
        lv_anoapres              TYPE string,
        lv_mesapres              TYPE string,
        lv_nomeresp              TYPE string,
        lv_foneresp              TYPE string,
        lv_emailresp             TYPE string,
        lv_num_nf_arq            TYPE string,
        lv_versao                TYPE string,
        lv_notas                 TYPE string,
        lv_numordemnota          TYPE string,
        lv_chave_nfe             TYPE string,
        lv_reconhece_nfe         TYPE string,
        lv_notafiscal            TYPE string,
        lv_xml_final             TYPE string,
        lv_num_itens             TYPE string,
        lv_zdia_b05              TYPE string,
        lv_produto               TYPE string,
        lv_numitemnfe            TYPE string,
        lv_codinternoproduto     TYPE string,
        lv_indicecodgeralproduto TYPE string,
        lv_codgeralproduto       TYPE string,
        lv_codtipotributacao     TYPE string,
        lv_valorbasecalculoitem  TYPE string,
        lv_valormultiplicador    TYPE string,
        lv_vlr_imp_dec           TYPE string,
        lv_zdia_conf             TYPE string.

  DATA: ld_filename TYPE string,
        ld_path     TYPE string,
        ld_fullpath TYPE string,
        ld_result   TYPE i.

  CONSTANTS: lc_barra TYPE c VALUE '\'.
  CONSTANTS: lc_true(4) TYPE c VALUE 'true'.
  CONSTANTS: lc_null(4) TYPE c VALUE 'NULL'.

  DATA: ls_final_aux TYPE ty_final.
  DATA: ls_final_1 TYPE ty_final.
  DATA: lt_final_aux TYPE STANDARD TABLE OF ty_final.
  DATA: lv_indice TYPE sy-tabix,
        lv_flag   TYPE c.

  lt_final_aux[] = gt_final[].

  DELETE ADJACENT DUPLICATES FROM lt_final_aux
                        COMPARING zdia_b03
                                  zdia_b04.

  LOOP AT lt_final_aux INTO ls_final_1.

    ls_final_aux = ls_final_1.

    AT FIRST.

      MOVE: ls_final_aux-zdia_a04+6(9) TO lv_ie_cont_dec,
            ls_final_aux-zdia_a05 TO lv_anoapres,
            ls_final_aux-zdia_a06 TO lv_mesapres,
            ls_final_aux-zdia_a07 TO lv_nomeresp,
            ls_final_aux-zdia_a08 TO lv_foneresp,
            ls_final_aux-zdia_a09 TO lv_emailresp,
            ls_final_aux-zdia_conf TO lv_zdia_conf,
            '01' TO lv_versao.

      PACK: ls_final_aux-zdia_d01 TO lv_num_nf_arq.

    ENDAT.

    READ TABLE gt_final INTO gs_final
                        WITH KEY zdia_b03 = ls_final_1-zdia_b03
                                 zdia_b04 = ls_final_1-zdia_b04
                                 BINARY SEARCH.
    IF sy-subrc EQ 0.

      lv_indice = sy-tabix.
      CLEAR lv_flag.

      CLEAR: lv_numordemnota,
             lv_notafiscal,
             lv_num_itens,
             lv_chave_nfe,
             lv_reconhece_nfe.

      PACK: ls_final_aux-zdia_b05 TO lv_zdia_b05.

      MOVE: ls_final_aux-zdia_b03 TO lv_numordemnota.
      SHIFT lv_numordemnota LEFT DELETING LEADING '0'.

*      lv_notafiscal = |<listaNotasFiscais><notaFiscal numOrdemNota="{ lv_numordemnota }">|.
      lv_notafiscal = |<notaFiscal numOrdemNota="{ lv_numordemnota }">|.
      lv_num_itens = |<numItens>{ lv_zdia_b05 }</numItens>|.
      lv_chave_nfe = |{ lv_notafiscal }<chaveNFe>{ ls_final_aux-zdia_b04 }</chaveNFe>|.
      lv_reconhece_nfe = |{ lv_chave_nfe }| &&
                         |<reconheceNFe>{ lv_zdia_conf }</reconheceNFe>| &&
                         |<nfeInformadaPeloContribuinte>{ 'N' }</nfeInformadaPeloContribuinte>| &&
                         |{ lv_num_itens }|.

      LOOP AT gt_final INTO ls_final_aux FROM lv_indice.

        IF ls_final_aux-zdia_b03 NE ls_final_1-zdia_b03 OR
           ls_final_aux-zdia_b04 NE ls_final_1-zdia_b04.

          EXIT.

        ENDIF.

        "Detalhes do item:
        MOVE: ls_final_aux-zdia_c06 TO lv_codgeralproduto,
              ls_final_aux-zdia_ind TO lv_indicecodgeralproduto,
              ls_final_aux-zdia_c03 TO lv_codinternoproduto,
              ls_final_aux-zcod_trib TO lv_codtipotributacao,
              ls_final_aux-zdia_c07 TO lv_valorbasecalculoitem,
              ls_final_aux-zmultipl TO lv_valormultiplicador,
              ls_final_aux-zdia_c09 TO lv_vlr_imp_dec.

        PACK: ls_final_aux-zdia_c02 TO lv_numitemnfe.

        IF lv_flag IS INITIAL.

          IF lv_produto IS INITIAL.

            lv_produto = |{ lv_reconhece_nfe }| &&
                         |<produto><numItemNfe>{ lv_numitemnfe }</numItemNfe>| &&
                         |<codInternoProduto>{ lv_codinternoproduto }</codInternoProduto>| &&
                         |<indiceCodGeralProduto>{ lv_indicecodgeralproduto }</indiceCodGeralProduto>| &&
                         |<codGeralProduto>{ lv_codgeralproduto }</codGeralProduto>| &&
                         |<codTipoTributacao>{ lv_codtipotributacao }</codTipoTributacao>| &&
                         |<valorBaseCalculoItem>{ lv_valorbasecalculoitem }</valorBaseCalculoItem>| &&
                         |<valorMultiplicador>{ lv_valormultiplicador }</valorMultiplicador>| &&
                         |<valorImpostoDeclarado>{ lv_vlr_imp_dec }</valorImpostoDeclarado>| &&
                         |<prodAcabado>{ '00' }</prodAcabado>| &&
                         |</produto>|.

          ELSE.

            lv_produto = "|{ lv_reconhece_nfe }| &&
                         |{ lv_produto }{ lv_reconhece_nfe }| &&
                         |<produto><numItemNfe>{ lv_numitemnfe }</numItemNfe>| &&
                         |<codInternoProduto>{ lv_codinternoproduto }</codInternoProduto>| &&
                         |<indiceCodGeralProduto>{ lv_indicecodgeralproduto }</indiceCodGeralProduto>| &&
                         |<codGeralProduto>{ lv_codgeralproduto }</codGeralProduto>| &&
                         |<codTipoTributacao>{ lv_codtipotributacao }</codTipoTributacao>| &&
                         |<valorBaseCalculoItem>{ lv_valorbasecalculoitem }</valorBaseCalculoItem>| &&
                         |<valorMultiplicador>{ lv_valormultiplicador }</valorMultiplicador>| &&
                         |<valorImpostoDeclarado>{ lv_vlr_imp_dec }</valorImpostoDeclarado>| &&
                         |<prodAcabado>{ '00' }</prodAcabado>| &&
                         |</produto>|.

          ENDIF.

          MOVE: 'X' TO lv_flag.

        ELSE.

          lv_produto = |{ lv_produto }<produto><numItemNfe>{ lv_numitemnfe }</numItemNfe>| &&
                       |<codInternoProduto>{ lv_codinternoproduto }</codInternoProduto>| &&
                       |<indiceCodGeralProduto>{ lv_indicecodgeralproduto }</indiceCodGeralProduto>| &&
                       |<codGeralProduto>{ lv_codgeralproduto }</codGeralProduto>| &&
                       |<codTipoTributacao>{ lv_codtipotributacao }</codTipoTributacao>| &&
                       |<valorBaseCalculoItem>{ lv_valorbasecalculoitem }</valorBaseCalculoItem>| &&
                       |<valorMultiplicador>{ lv_valormultiplicador }</valorMultiplicador>| &&
                       |<valorImpostoDeclarado>{ lv_vlr_imp_dec }</valorImpostoDeclarado>| &&
                       |<prodAcabado>{ '00' }</prodAcabado>| &&
                       |</produto>|.

        ENDIF.

        CLEAR: ls_final_aux.
      ENDLOOP.

      CLEAR: gs_final.
    ENDIF.

    lv_produto = |{ lv_produto }</notaFiscal>|.

    AT LAST.
*      lv_notas = |{ lv_produto }</notaFiscal>|.
      lv_notas = |{ lv_produto }|.
      lv_xml_final  = |<infDeclaracaoMensal versao="{ lv_versao }">| &&
                      |<ieContribuinteDeclarante>{ lv_ie_cont_dec }</ieContribuinteDeclarante>| &&
                      |<anoApresentacao>{ lv_anoapres }</anoApresentacao>| &&
                      |<mesApresentacao>{ lv_mesapres }</mesApresentacao>| &&
                      |<nomeResponsavel>{ lv_nomeresp }</nomeResponsavel>| &&
                      |<foneResponsavel>{ lv_foneresp }</foneResponsavel>| &&
                      |<emailResponsavel>{ lv_emailresp }</emailResponsavel>| &&
                      |<listaNotasFiscais>{ lv_notas }</listaNotasFiscais>|.

    ENDAT.

    CLEAR: ls_final_1.
  ENDLOOP.

  lv_xml = cl_abap_codepage=>convert_to(
*  |<?xml version="1.0" encoding="UTF-8" ?>| &&
  |<enviDeclaracaoAutodesembaraco | &&
  |xsi:schemaLocation="http://www.sefaz.am.gov.br/autodesembaraco enviDeclaracaoMensalAuto_v1.02.xsd" | &&
  |xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" | &&
  |xmlns="http://www.sefaz.am.gov.br/autodesembaraco">| &&
  |{ lv_xml_final }| &&
  |<numNotasArquivo>{ lv_num_nf_arq }</numNotasArquivo>| &&
  |</infDeclaracaoMensal>| &&
  |</enviDeclaracaoAutodesembaraco>| ).

  ixml = cl_ixml=>create( ).

  stream_factory = ixml->create_stream_factory( ).

  document = ixml->create_document( ).

  IF ixml->create_parser( document       = document
                          stream_factory = stream_factory
                          istream = stream_factory->create_istream_xstring( string = lv_xml ) )->parse( ) <> 0.

  ENDIF.

  iterator = document->create_iterator( ).

  DO.
    node = iterator->get_next( ).
    IF node IS INITIAL.
      EXIT.
    ENDIF.
    IF node->get_type( ) = if_ixml_node=>co_node_text.
      node->set_value( to_upper( node->get_value( ) ) ).
    ENDIF.
  ENDDO.

  CLEAR lv_xml.

  document->render( ostream = ixml->create_stream_factory( )->create_ostream_xstring( string = lv_xml ) ).

  CREATE OBJECT lo_xml.

  CALL METHOD lo_xml->parse_xstring
    EXPORTING
      stream  = lv_xml
    RECEIVING
      retcode = lv_retcode.

  CALL METHOD lo_xml->set_encoding
    EXPORTING
      charset = 'UTF-8'.


  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
*     window_title      = ' '
      default_extension = 'XML'
      default_file_name = 'Declaração_DIA'
      initial_directory = 'c:\temp\'
    CHANGING
      filename          = ld_filename
      path              = ld_path
      fullpath          = ld_fullpath
      user_action       = ld_result.

  IF ld_fullpath IS INITIAL.
    RETURN.
  ENDIF.

  lv_file = ld_fullpath.
  CALL METHOD lo_xml->export_to_file
    EXPORTING
      filename = lv_file
    RECEIVING
      retcode  = lv_retcode.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_SELECIONA_HISTORICO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_seleciona_historico .

  DATA: lt_ztsd_br147 TYPE STANDARD TABLE OF ty_ztsd_br147,
        lt_dados      TYPE STANDARD TABLE OF ty_dados.

  "Selecionando dados......Por favor Aguarde!
  PERFORM zf_status_message USING TEXT-006.

  SELECT zdia_a03
         zdia_a04
         zdia_a05
         zdia_a06
         zdia_b04
         zdia_c02
         zdia_b03
         zdia_b05
         zdia_cst
         zdia_c03
         zdia_c06
         zdia_c07
         zdia_d01
    FROM ztsd_br147
    INTO TABLE gt_ztsd_br147
   WHERE zdia_a05 EQ p_ano
     AND zdia_a06 EQ p_mes.
  IF sy-subrc EQ 0.

    SORT gt_ztsd_br147 BY zdia_a04
                          zdia_a05
                          zdia_a06.

    SELECT *
      FROM ztsd_br148
      INTO TABLE gt_ztsd_br148
       FOR ALL ENTRIES IN gt_ztsd_br147
     WHERE zdia_a03 EQ gt_ztsd_br147-zdia_a03
       AND zdia_a04 EQ gt_ztsd_br147-zdia_a04
       AND zdia_a05 EQ gt_ztsd_br147-zdia_a05
       AND zdia_a06 EQ gt_ztsd_br147-zdia_a06
       AND zdia_b04 EQ gt_ztsd_br147-zdia_b04
       AND zdia_c02 EQ gt_ztsd_br147-zdia_c02.
    IF sy-subrc EQ 0.

      LOOP AT gt_ztsd_br147 ASSIGNING FIELD-SYMBOL(<fs_ztsd_br147>).

        MOVE: <fs_ztsd_br147>-zdia_b04(2)    TO <fs_ztsd_br147>-regio,
              <fs_ztsd_br147>-zdia_b04+2(2)  TO <fs_ztsd_br147>-nfyear,
              <fs_ztsd_br147>-zdia_b04+4(2)  TO <fs_ztsd_br147>-nfmonth,
              <fs_ztsd_br147>-zdia_b04+6(14) TO <fs_ztsd_br147>-stcd1,
              <fs_ztsd_br147>-zdia_b04+20(2) TO <fs_ztsd_br147>-model,
              <fs_ztsd_br147>-zdia_b04+22(3) TO <fs_ztsd_br147>-serie,
              <fs_ztsd_br147>-zdia_b04+25(9) TO <fs_ztsd_br147>-nfnum9,
              <fs_ztsd_br147>-zdia_b04+34(9) TO <fs_ztsd_br147>-docnum9,
              <fs_ztsd_br147>-zdia_b04+43(1) TO <fs_ztsd_br147>-cdv.

      ENDLOOP.

      lt_ztsd_br147[] = gt_ztsd_br147[].

      DELETE ADJACENT DUPLICATES FROM lt_ztsd_br147
                            COMPARING zdia_b04.
      SELECT ref~docnum
             ref~regio
             ref~nfyear
             ref~nfmonth
             ref~stcd1
             ref~model
             ref~serie
             ref~nfnum9
             ref~docnum9
             ref~cdv
             doc~nftype
             doc~direct
             doc~bukrs
             doc~branch
             doc~docdat
             doc~pstdat
             doc~parid
             doc~nftot
             doc~nfenum
             doc~name1
             doc~cgc
             lin~itmnum
             lin~matnr
             lin~maktx
             lin~cfop
             lin~matorg
             lin~matuse
             lin~netwr
        INTO TABLE gt_dados
        FROM j_1bnfe_active AS ref
        INNER JOIN j_1bnfdoc AS doc
           ON ref~docnum EQ doc~docnum
        INNER JOIN j_1bnflin AS lin
           ON lin~docnum EQ doc~docnum
        FOR ALL ENTRIES IN lt_ztsd_br147
        WHERE ref~regio   EQ lt_ztsd_br147-regio
          AND ref~nfyear  EQ lt_ztsd_br147-nfyear
          AND ref~nfmonth EQ lt_ztsd_br147-nfmonth
          AND ref~stcd1   EQ lt_ztsd_br147-stcd1
          AND ref~model   EQ lt_ztsd_br147-model
          AND ref~serie   EQ lt_ztsd_br147-serie
          AND ref~nfnum9  EQ lt_ztsd_br147-nfnum9
          AND ref~docnum9 EQ lt_ztsd_br147-docnum9
          AND ref~cdv     EQ lt_ztsd_br147-cdv
          AND doc~doctyp  NE '5'
          AND doc~direct  EQ '1'
          AND doc~bukrs   EQ p_bukrs
          AND doc~branch  EQ p_branch
          AND doc~cancel  NE 'X'
          AND doc~ie_bupla EQ p_in_es.
      IF sy-subrc EQ 0.

        SORT gt_dados BY regio
                         nfyear
                         nfmonth
                         stcd1
                         model
                         serie
                         nfnum9
                         docnum9
                         cdv
                         itmnum.

        lt_dados[] = gt_dados[].
        DELETE ADJACENT DUPLICATES FROM lt_dados
                              COMPARING bukrs
                                        branch
                                        cfop
                                        matuse
                                        matorg.

        DELETE lt_dados[] WHERE bukrs IS INITIAL
                             OR branch IS INITIAL
                             OR cfop IS INITIAL
                             OR matuse IS INITIAL
                             OR matorg IS INITIAL.

        IF lt_dados IS NOT INITIAL.

          SELECT bukrs
                 branch
                 cfop
                 matuse
                 matorg
                 multipl
                 zdia_c06
            FROM ztsd_dia_codtrib
            INTO TABLE gt_ztsd_dia_codtrib
             FOR ALL ENTRIES IN lt_dados
           WHERE bukrs  EQ lt_dados-bukrs
             AND branch EQ lt_dados-branch
             AND cfop   EQ lt_dados-cfop
             AND matuse EQ lt_dados-matuse
             AND matorg EQ lt_dados-matorg.
          IF sy-subrc EQ 0.

            SORT gt_ztsd_dia_codtrib BY bukrs
                                        branch
                                        cfop
                                        matuse
                                        matorg.

          ENDIF.

        ENDIF.

        CLEAR: lt_dados[].
        lt_dados[] = gt_dados[].
        DELETE ADJACENT DUPLICATES FROM lt_dados
                              COMPARING bukrs.

        SELECT bukrs
               nome_resp
               tel_resp
               email_resp
          FROM ztsd_dia_resp
          INTO TABLE gt_ztsd_dia_resp
           FOR ALL ENTRIES IN lt_dados
         WHERE bukrs EQ lt_dados-bukrs.
        IF sy-subrc EQ 0.
          SORT gt_ztsd_dia_resp BY bukrs.
        ENDIF.

      ENDIF.

    ELSE.
      "Não há histórico para a seleção informada!
      MESSAGE TEXT-e02 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.

    ENDIF.

  ELSE.

    "Dados não encontrados para a seleção informada!
    MESSAGE TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_STATUS_MESSAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_TEXT_001  text
*&---------------------------------------------------------------------*
FORM zf_status_message USING u_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 99
      text       = u_text.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_MONTA_REL_HISTÓRICO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_monta_rel_historico.

  DATA: lv_indice   TYPE sy-tabix,
        lt_j1bnflin TYPE STANDARD TABLE OF j_1bnflin,
        ls_j1bnflin TYPE j_1bnflin,
        lv_itmnum   TYPE j_1bnflin-itmnum.

  "Montando Relatório......Por favor Aguarde!
  PERFORM zf_status_message USING TEXT-007.

  LOOP AT gt_ztsd_br148 INTO DATA(ls_ztsd_br148).

    MOVE: ls_ztsd_br148-zdia_a03 TO gs_final-zdia_a03,
          ls_ztsd_br148-zdia_a04 TO gs_final-zdia_a04,
          ls_ztsd_br148-zdia_a05 TO gs_final-zdia_a05,
          ls_ztsd_br148-zdia_a06 TO gs_final-zdia_a06,
          ls_ztsd_br148-zdia_b04 TO gs_final-zdia_b04,
          ls_ztsd_br148-zdia_c02 TO gs_final-zdia_c02,
          ls_ztsd_br148-zdia_b03 TO gs_final-zdia_b03,
          ls_ztsd_br148-zdia_b05 TO gs_final-zdia_b05,
          " ls_ztsd_br148-zdia_c03 TO gs_final-zdia_c03,
          ls_ztsd_br148-zdia_c07 TO gs_final-zdia_c07,
          ls_ztsd_br148-zdia_d01 TO gs_final-zdia_d01,
          ls_ztsd_br148-zdia_cst TO gs_final-zdia_cst,
          ls_ztsd_br148-erdat TO gs_final-erdat,
          ls_ztsd_br148-erzet TO gs_final-erzet,
          ls_ztsd_br148-ernam TO gs_final-ernam.

    MOVE: ls_ztsd_br148-zdia_conf      TO gs_final-zdia_conf,
          ls_ztsd_br148-zdia_ind       TO gs_final-zdia_ind,
          ls_ztsd_br148-zdia_prod_acab TO gs_final-zdia_prod_acab.

    MOVE: ls_ztsd_br148-zdia_c06 TO gs_final-zdia_c06.

    MOVE: ls_ztsd_br148-bukrs  TO gs_final-bukrs,
          ls_ztsd_br148-branch TO gs_final-branch,
          ls_ztsd_br148-docnum TO gs_final-docnum,
          ls_ztsd_br148-nftype TO gs_final-nftype,
          ls_ztsd_br148-direct TO gs_final-direct,
          ls_ztsd_br148-nfenum TO gs_final-nfenum,
          ls_ztsd_br148-cfop   TO gs_final-cfop,
          ls_ztsd_br148-itmnum TO gs_final-itmnum,
*          ls_ztsd_br148-maktx  TO gs_final-maktx,
          ls_ztsd_br148-matuse TO gs_final-matuse,
          ls_ztsd_br148-matorg TO gs_final-matorg,
          ls_ztsd_br148-parid  TO gs_final-parid,
          ls_ztsd_br148-name1  TO gs_final-name1,
          ls_ztsd_br148-regio  TO gs_final-regio,
          ls_ztsd_br148-netwr  TO gs_final-netwr,
          ls_ztsd_br148-nftot  TO gs_final-nftot.


    WRITE ls_ztsd_br148-cgc USING EDIT MASK
    '__.___.___/____-__'  TO gs_final-cgc.

    MOVE: ls_ztsd_br148-zdia_c06 TO gs_final-zcod_trib,
          ls_ztsd_br148-zmultipl  TO gs_final-zmultipl,
          ls_ztsd_br148-zdia_c09  TO gs_final-zdia_c09,
          ls_ztsd_br148-zdia_a07  TO gs_final-zdia_a07,
          ls_ztsd_br148-zdia_a08  TO gs_final-zdia_a08,
*          ls_ztsd_br148-zdia_a09 TO gs_final-zdia_a09,
          ls_ztsd_br148-maktx TO gs_final-maktx.

    "Para igualar o formato do item(ex: 10, 20, 30 etc)
    lv_itmnum = ls_ztsd_br148-zdia_c02 * 10.

    READ TABLE gt_dados INTO DATA(ls_data_aux)
                        WITH KEY regio   = ls_ztsd_br148-zdia_b04(2)
                                 nfyear  = ls_ztsd_br148-zdia_b04+2(2)
                                 nfmonth = ls_ztsd_br148-zdia_b04+4(2)
                                 stcd1   = ls_ztsd_br148-zdia_b04+6(14)
                                 model   = ls_ztsd_br148-zdia_b04+20(2)
                                 serie   = ls_ztsd_br148-zdia_b04+22(3)
                                 nfnum9  = ls_ztsd_br148-zdia_b04+25(9)
                                 docnum9 = ls_ztsd_br148-zdia_b04+34(9)
                                 cdv     = ls_ztsd_br148-zdia_b04+43(1)
                                 itmnum  = lv_itmnum
                                 BINARY SEARCH.
    IF sy-subrc EQ 0.

      lv_indice = sy-tabix.

      LOOP AT gt_dados INTO DATA(ls_dados) FROM lv_indice.

        IF ls_dados-regio   NE ls_data_aux-regio OR
           ls_dados-nfyear  NE ls_data_aux-nfyear OR
           ls_dados-nfmonth NE ls_data_aux-nfmonth OR
           ls_dados-stcd1   NE ls_data_aux-stcd1 OR
           ls_dados-model   NE ls_data_aux-model OR
           ls_dados-serie   NE ls_data_aux-serie OR
           ls_dados-nfnum9  NE ls_data_aux-nfnum9 OR
           ls_dados-docnum9 NE ls_data_aux-docnum9 OR
           ls_dados-cdv     NE ls_data_aux-cdv OR
           ls_dados-itmnum  NE lv_itmnum.

          EXIT.

        ENDIF.

        IF ls_ztsd_br148-docnum IS NOT INITIAL.

          MOVE: ls_dados-matnr TO gs_final-matnr,
                ls_dados-maktx TO gs_final-maktx.

        ENDIF.

        MOVE: icon_green_light TO gs_final-status.

        MOVE: ls_dados-docdat TO gs_final-docdat,
              ls_dados-pstdat TO gs_final-pstdat.

        CLEAR: gs_final-zdia_c09.
        READ TABLE gt_ztsd_dia_codtrib INTO DATA(ls_ztsd_dia_codtrib)
                                       WITH KEY bukrs  = ls_dados-bukrs
                                                branch = ls_dados-branch
                                                cfop   = ls_dados-cfop
                                                matuse = ls_dados-matuse
                                                matorg = ls_dados-matorg
                                                BINARY SEARCH.
        IF sy-subrc EQ 0.

          MOVE: ls_ztsd_dia_codtrib-zdia_c06 TO gs_final-zcod_trib,
                ls_ztsd_dia_codtrib-multipl  TO gs_final-zmultipl.

          gs_final-zdia_c09 = ( gs_final-zdia_c07 / 100 ) * ls_ztsd_dia_codtrib-multipl.

        ENDIF.

        CLEAR: ls_ztsd_dia_codtrib.

        APPEND gs_final TO gt_final.
        CLEAR: gs_final-zdia_c09,
               gs_final-matnr,
               gs_final-maktx,
               gs_final-status,
               gs_final-docdat,
               gs_final-pstdat.

      ENDLOOP.
      CLEAR: ls_dados.

    ELSE.

      MOVE: icon_red_light TO gs_final-status.

      APPEND gs_final TO gt_final.
      CLEAR: gs_final.

    ENDIF.

    CLEAR: ls_data_aux,
           ls_ztsd_br148.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_FIELDCAT_CHANGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_fieldcat_change .

  PERFORM zf_fill_catalog_column USING:

    1 '' 'STATUS' 'GT_FINAL' '' 'Status'  'Status'     'Status',
    2 '' 'ZDIA_A03' 'GT_FINAL' 'ZTSD_BR148' 'V. Lay Arq'  'Versão Layout Arq.'     'Versão Layout do Arquivo',
    3 '' 'ZDIA_A04' 'GT_FINAL' 'ZTSD_BR148' 'Ins. Est.'  'Ins Est Contr Decl'  'Ins Est Contr Declarante',
    4 '' 'ZDIA_A05' 'GT_FINAL' 'ZTSD_BR148' 'AnoApDANFE'  'Ano Apres.DANFE'     'Ano de Apresentação DANFE',
    5 '' 'ZDIA_A06' 'GT_FINAL' 'ZTSD_BR148' 'MêsApDANFE'  'Mês Apres.DANFE'     'Mês de Apresentação DANFE',
    6 '' 'ZDIA_B04' 'GT_FINAL' 'ZTSD_BR148' 'ChavAc NFe'  'Chave de Acesso NFe'     'Chave de Acesso NFe',
    7 '' 'ZDIA_C02' 'GT_FINAL' 'ZTSD_BR148' 'Item NFe'  'Item da NFe'     'Item da NFe',
    8 'X' 'BUKRS' 'GT_FINAL' 'ZTSD_BR148' 'Empresa'  'Empresa'     'Empresa',
    9 'X' 'BRANCH' 'GT_FINAL' 'ZTSD_BR148' 'Loc. Neg.'  'Loc Negócios'     'Local de negócios',
    10 'X' 'DOCNUM' 'GT_FINAL' 'ZTSD_BR148' 'Nº Doc.'  'Nº Documento'     'Nº documento',
    11 'X' 'ZDIA_B03' 'GT_FINAL' 'ZTSD_BR148' 'NºOrd Nota'  'Nº de Ord da Nota'     'Nº de Ordem da Nota',
    12 'X' 'ZDIA_CONF' 'GT_FINAL' 'ZTSD_BR148' 'ConfRecNFe'  'Conf. Rec. da NFe'     'Confirmação Receb. da NFe',
    13 'X' 'ZDIA_B05' 'GT_FINAL' 'ZTSD_BR148' 'NºItensNFe'  'Nº Itens da NFe'     'Número de Itens da NFe',
    14 'X' 'ZDIA_C02' 'GT_FINAL' 'ZTSD_BR148' 'CódIntProd'  'Cód Interno Prod.'     'Código Interno do Produto',
    15 'X' 'ZDIA_IND' 'GT_FINAL' 'ZTSD_BR148' 'Índ CGProd'  'Índ. Cód Geral Prod.'     'Índice Cód. Geral Prod.',
    16 'X' 'ZDIA_C06' 'GT_FINAL' 'ZTSD_BR148' 'Cód G.Prod'  'Cód Geral Prod.'     'Código Geral do Produto',
    17 'X' 'ZCOD_TRIB' 'GT_FINAL' 'ZTSD_BR148' 'Cód T.Trib'  'Cód. Tipo Tributação'     'Código Tipo de Tributação',
    18 'X' 'ZDIA_C07' 'GT_FINAL' 'ZTSD_BR148' 'Vl ItemNFe'  'Valor Item NFe'     'Valor Item da NFe',
    19 'X' 'ZMULTIPL' 'GT_FINAL' 'ZTSD_BR148' 'Vlr Mult.'  'Vlr Mult.'     'Valor Multiplicador',
    20 'X' 'ZDIA_C09' 'GT_FINAL' 'ZTSD_BR148' 'Vlr Decl.'  'Vlr Imp. Declarado'     'Valor Imposto Declarado',
    21 'X' 'ZDIA_PROD_ACAB' 'GT_FINAL' 'ZTSD_BR148' 'Prod.Acab.'  'Prod. Acabado'     'Produto Acabado',
    22 'X' 'ZDIA_D01' 'GT_FINAL' 'ZTSD_BR148' 'Nº NT. Arq'  'Núm. Notas Arq'     'Número de Notas no Arquivo',
    23 'X' 'NFTYPE' 'GT_FINAL' 'ZTSD_BR148' 'Ctg. de NF'  'Ctg.de nota fiscal'     'Ctg.de nota fiscal',
    24 'X' 'DIRECT' 'GT_FINAL' 'ZTSD_BR148' 'Dir.M.Merc'  'Direção Mov. Merc.'     'Direção do Mov. de Mercadorias',
    25 'X' 'NFENUM' 'GT_FINAL' 'ZTSD_BR148' 'Número NFe'  'Número NFe'     'Nº NF Eletrônica',
    26 'X' 'CFOP' 'GT_FINAL' 'ZTSD_BR148' 'C.CFOP.Ext'  'Cód. CFOP e Extensão'     'Código CFOP e Extensão',
    27 'X' 'ITMNUM' 'GT_FINAL' 'ZTSD_BR148' 'Nº ItemDoc'  'Nº Item do Doc'     'Nº Item do Documento',
    28 'X' 'MAKTX' 'GT_FINAL' 'ZTSD_BR148' 'Txt Mat.'  'Texto Breve Mat.'     'Texto breve de material',
    29 'X' 'MATUSE' 'GT_FINAL' 'ZTSD_BR148' 'Util. Mat.'  'Util. de Mat.'     'Utilização de Material',
    30 'X' 'MATORG' 'GT_FINAL' 'ZTSD_BR148' 'Orig. Mat.'  'Origem de Mat.'     'Origem de Mat.',
    31 'X' 'ZDIA_CST' 'GT_FINAL' 'ZTSD_BR148' 'Nº de CST'  'Número de CST'     'Número de CST',
    32 'X' 'PARID' 'GT_FINAL' 'ZTSD_BR148' 'ID Parc.'  'Identificação Parc.'     'Identificação do Parceiro ',
    33 'X' 'CGC' 'GT_FINAL' 'ZTSD_BR148' 'Code CGC'  'Code CGC'     'Code CGC',
    34 'X' 'NAME1' 'GT_FINAL' 'ZTSD_BR148' 'Nome 1'  'Nome 1'     'Nome 1',
    35 'X' 'REGIO' 'GT_FINAL' 'ZTSD_BR148' 'Região'  'Região'     'Região',
    36 'X' 'NETWR' 'GT_FINAL' 'ZTSD_BR148' 'Vlr Líq.'  'Valor Líquido'     'Valor Líquido',
    37 'X' 'NFTOT' 'GT_FINAL' 'ZTSD_BR148' 'Vl.T.c/Imp'  'Valor Total c/ Imp.'     'Valor Total Incluindo os Impostos',
    38 'X' 'ZDIA_A07' 'GT_FINAL' 'ZTSD_BR148' 'Nome Resp.'  'Nom.Resp. p/ Env.DIA'     'Nome Resp. p/ Envio da DIA',
    39 'X' 'ZDIA_A08' 'GT_FINAL' 'ZTSD_BR148' 'Tel.Resp.'  'Tel.Resp. p/ Env.DIA'     'Tel.Resp. p/ Envio DIA',
    40 'X' 'ZDIA_A09' 'GT_FINAL' 'ZTSD_BR148' 'Email.Resp'  'Email Resp Env.DIA'     'E-mail do Resp. p/ Env.DIA',
    41 'X' 'ERDAT'    'GT_FINAL' 'ZTSD_BR148' 'Dt Cri Reg'  'Data Criação Reg.'      'Data de Criação do Registro',
    42 'X' 'ERZET'    'GT_FINAL' 'ZTSD_BR148' 'Hora Reg.'   'Hora do Registro'    'Hora do Registro',
    43 'X' 'ERNAM'    'GT_FINAL' 'ZTSD_BR148' 'Nome Resp.'  'Nome Resp p/ Reg'  'Nome Resp pelo Reg.',
    44 'X' 'MANDT'    'GT_FINAL' 'ZTSD_BR148'      ''      ''      ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_FOR_VARIANT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_for_variant .

  DATA: fl_exit(1) TYPE c.

  gs_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = gs_variant
      i_save     = 'A'
    IMPORTING
      e_exit     = fl_exit
      es_variant = gs_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.

    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ELSE.

    IF fl_exit = space.

      p_layout = gs_variant-variant.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_AJUSTE_ITEM_NAO_LOCALIZ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_ajuste_item_nao_localiz .

  TYPES: BEGIN OF ty_intermediaria,
           regio   TYPE j_1bnfe_active-regio,
           nfyear  TYPE j_1bnfe_active-nfyear,
           nfmonth TYPE j_1bnfe_active-nfmonth,
           stcd1   TYPE j_1bnfe_active-stcd1,
           model   TYPE j_1bnfe_active-model,
           serie   TYPE j_1bnfe_active-serie,
           nfnum9  TYPE j_1bnfe_active-nfnum9,
           docnum9 TYPE j_1bnfe_active-docnum9,
           cdv     TYPE j_1bnfe_active-cdv,
         END OF ty_intermediaria.

  DATA: lt_final          TYPE STANDARD TABLE OF ty_final,
        lt_intermediaria  TYPE STANDARD TABLE OF ty_intermediaria,
        lt_j_1bnfe_active TYPE STANDARD TABLE OF ty_intermediaria.

*  lt_final[] = gt_final[].

  LOOP AT gt_final INTO DATA(ls_final).

    APPEND VALUE #( regio   = ls_final-zdia_b04(2)
                    nfyear  = ls_final-zdia_b04+2(2)
                    nfmonth = ls_final-zdia_b04+4(2)
                    stcd1   = ls_final-zdia_b04+6(14)
                    model   = ls_final-zdia_b04+20(2)
                    serie   = ls_final-zdia_b04+22(3)
                    nfnum9  = ls_final-zdia_b04+25(9)
                    docnum9 = ls_final-zdia_b04+34(9)
                    cdv     = ls_final-zdia_b04+43(1) ) TO lt_intermediaria.

  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM lt_intermediaria
             COMPARING ALL FIELDS.

  SELECT regio
         nfyear
         nfmonth
         stcd1
         model
         serie
         nfnum9
         docnum9
         cdv
    FROM j_1bnfe_active
    INTO TABLE lt_j_1bnfe_active
     FOR ALL ENTRIES IN lt_intermediaria
   WHERE regio   EQ lt_intermediaria-regio
     AND nfyear  EQ lt_intermediaria-nfyear
     AND nfmonth EQ lt_intermediaria-nfmonth
     AND stcd1   EQ lt_intermediaria-stcd1
     AND model   EQ lt_intermediaria-model
     AND serie   EQ lt_intermediaria-serie
     AND nfnum9  EQ lt_intermediaria-nfnum9
     AND docnum9 EQ lt_intermediaria-docnum9
     AND cdv     EQ lt_intermediaria-cdv.
  IF sy-subrc EQ 0.

    SORT lt_j_1bnfe_active BY regio
                              nfyear
                              nfmonth
                              stcd1
                              model
                              serie
                              nfnum9
                              docnum9
                              cdv.

    LOOP AT gt_final ASSIGNING FIELD-SYMBOL(<fs_final>).

      READ TABLE lt_j_1bnfe_active INTO DATA(ls_j_1bnfe_active)
                                    WITH KEY regio   = <fs_final>-zdia_b04(2)
                                             nfyear  = <fs_final>-zdia_b04+2(2)
                                             nfmonth = <fs_final>-zdia_b04+4(2)
                                             stcd1   = <fs_final>-zdia_b04+6(14)
                                             model   = <fs_final>-zdia_b04+20(2)
                                             serie   = <fs_final>-zdia_b04+22(3)
                                             nfnum9  = <fs_final>-zdia_b04+25(9)
                                             docnum9 = <fs_final>-zdia_b04+34(9)
                                             cdv     = <fs_final>-zdia_b04+43(1)
                                    BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE: icon_green_light TO <fs_final>-status.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
