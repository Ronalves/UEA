*----------------------------------------------------------------------*
*                               SEMP TCL                               *
*----------------------------------------------------------------------*
* Consultoria: MGS Consulting                                          *
* Autor......: Márcio Teixeira                                         *
* Data.......: 31/10/2018                                              *
* Descrição  : DIA - Análise Fiscal                                    *
* Transação..:                                                         *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Consultoria:                                       Data: XX/XX/XXXX  *
* Autor......:                                                         *
* Observações:                                                         *
*----------------------------------------------------------------------*
REPORT zsdr_dia_analise_fiscal_copy MESSAGE-ID zsd.

TYPE-POOLS: icon.

INCLUDE ZSDI_DIA_ANALISE_FISCAL_TOP_C.
*INCLUDE zsdi_dia_analise_fiscal_top.
INCLUDE ZSDI_DIA_ANALISE_FISCAL_SCR_C.
*INCLUDE zsdi_dia_analise_fiscal_scr.
INCLUDE ZSDI_DIA_ANALISE_FISCAL_F01_C.
*INCLUDE zsdi_dia_analise_fiscal_f01.
INCLUDE ZSDI_DIA_ANALISE_FISCAL_PBO_C.
*INCLUDE zsdi_dia_analise_fiscal_pbo.
INCLUDE ZSDI_DIA_ANALISE_FISCAL_PAI_C.
*INCLUDE zsdi_dia_analise_fiscal_pai.

INITIALIZATION.
*n/a

START-OF-SELECTION.

  CASE 'X'.
    WHEN p_edit.
      PERFORM zf_seleciona_dados.
      PERFORM zf_monta_relatorio.
    WHEN p_show.
      PERFORM zf_seleciona_historico.
      PERFORM zf_monta_rel_historico.
    WHEN OTHERS.
  ENDCASE.

  IF gt_final IS NOT INITIAL.

    "Ajusta o farol do itens não localizados
    PERFORM zf_ajuste_item_nao_localiz.

    PERFORM zf_exibi_alv.

  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM zf_for_variant.
