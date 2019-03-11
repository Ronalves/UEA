*&---------------------------------------------------------------------*
*& Include          ZSDI_DIA_ANALISE_FISCAL_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET TITLEBAR 'ZTITLE'.
  SET PF-STATUS 'GUI_0100'.

  gt_layout-cwidth_opt    = 'X'.
  gt_layout-zebra         = 'X'.
  gt_layout-sel_mode      = 'X'.

  gs_variant-report   = sy-repid.
  gs_variant-username = sy-uname.

*  IF o_container IS NOT BOUND.
  IF gt_fieldcat IS INITIAL.

    CREATE OBJECT o_container
      EXPORTING
        container_name = 'CC_ALV'.

    CREATE OBJECT o_alv
      EXPORTING
        i_parent = o_container.

    IF gv_modify EQ abap_true.

      PERFORM zf_fieldcat_change.

    ELSE.

      PERFORM zf_fieldcat.

    ENDIF.

    PERFORM zf_exclui_botoes.

    CREATE OBJECT g_event_receiver.

    SET HANDLER g_event_receiver->handle_data_changed FOR o_alv.

***************
    CALL METHOD o_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD o_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
***************

    CALL METHOD o_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gt_layout
        is_variant           = gs_variant
        i_save               = 'A'
        it_toolbar_excluding = gt_exclude[]
      CHANGING
        it_outtab            = gt_final[]
        it_fieldcatalog      = gt_fieldcat[].

  ELSE.

    CALL METHOD o_alv->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.

  ENDIF.

ENDMODULE.
