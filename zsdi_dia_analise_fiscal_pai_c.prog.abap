*&---------------------------------------------------------------------*
*& Include          ZSDI_DIA_ANALISE_FISCAL_PAI
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANCEL' OR 'CLOSE'.

      IF o_alv IS BOUND.
        o_alv->free( ).
      ENDIF.

      IF o_container IS BOUND.
        o_container->free( ).
      ENDIF.

      CLEAR: gv_modify.

      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'SAVE'.

      PERFORM zf_valida_tabela.

      PERFORM zf_salva_dados.

    WHEN 'SAVE_XML'.

*      PERFORM zf_gerar_xml.
      PERFORM zf_download_xml.

    WHEN 'VIEW'.

      PERFORM zf_exibi_xml.

    WHEN 'CHANGE'.

      IF o_alv IS BOUND.
        o_alv->free( ).
      ENDIF.

      IF o_container IS BOUND.
        o_container->free( ).
      ENDIF.

      MOVE: abap_true TO gv_modify.

      CLEAR: gt_layout,
             gt_fieldcat,
             gt_exclude.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
