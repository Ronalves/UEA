*&---------------------------------------------------------------------*
*& Include          ZSDI_DIA_ANALISE_FISCAL_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_bukrs  TYPE j_1bbranch-bukrs OBLIGATORY,            "Empresa
            p_branch TYPE j_1bbranch-branch OBLIGATORY,           "Local Negócio
            p_in_es  TYPE j_1bbranch-state_insc OBLIGATORY,       "Inscrição Estadual
            p_ano    TYPE ztsd_br147-zdia_a05 OBLIGATORY,         "Ano
            p_mes    TYPE ztsd_br147-zdia_a06 OBLIGATORY.         "Mês


*SELECT-OPTIONS: s_in_es FOR j_1bbranch-state_insc ,               "Inscrição Estadual
*                s_ano   FOR ztsd_br147-zdia_a05 OBLIGATORY,       "Ano
*                s_mes   FOR ztsd_br147-zdia_a06 OBLIGATORY.       "Mês

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-005.

PARAMETER: p_show RADIOBUTTON GROUP rb1,
           p_edit RADIOBUTTON GROUP rb1.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-008.
PARAMETERS:  p_layout  LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b3.
