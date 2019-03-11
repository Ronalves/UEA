*&---------------------------------------------------------------------*
*& Include          ZSDI_DIA_ANALISE_FISCAL_TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& INCLUDE
*&---------------------------------------------------------------------*
INCLUDE <icon>.
INCLUDE <symbol>.

*&---------------------------------------------------------------------*
*& Tables
*&---------------------------------------------------------------------*
TABLES: t001,
        j_1bnfdoc,
        j_1bbranch,
        ztsd_br147.

*&---------------------------------------------------------------------*
*& Types
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_final,
         status            TYPE icon_d,
         zdia_a03          TYPE ztsd_br147-zdia_a03,
         zdia_a04          TYPE ztsd_br147-zdia_a04,
         zdia_a05          TYPE ztsd_br147-zdia_a05,
         zdia_a06          TYPE ztsd_br147-zdia_a06,
         zdia_b04          TYPE ztsd_br147-zdia_b04,
         zdia_c02          TYPE ztsd_br147-zdia_c02,
         bukrs             TYPE j_1bnfdoc-bukrs,
         branch            TYPE j_1bnfdoc-branch,
         docdat            TYPE j_1bnfdoc-docdat,
         pstdat            TYPE j_1bnfdoc-pstdat,
         docnum            TYPE j_1bnfdoc-docnum,
         zdia_b03          TYPE ztsd_br147-zdia_b03,
         zdia_conf         TYPE c,
         zdia_b05          TYPE ztsd_br147-zdia_b05,
         zdia_c03          TYPE ztsd_br147-zdia_c03,
         matnr             TYPE mara-matnr,
         zdia_ind          TYPE c,
         zdia_c06          TYPE ztsd_br147-zdia_c06,
         zcod_trib         TYPE ztsd_dia_codtrib-zdia_c06,
         zdia_c07          TYPE ztsd_br147-zdia_c07,
         zmultipl          TYPE ztsd_dia_codtrib-multipl,
         zdia_c09          TYPE ztsd_br147-zdia_c07,
         zdia_prod_acab(2) TYPE c,
         zdia_d01          TYPE ztsd_br147-zdia_d01,
         nftype            TYPE j_1bnfdoc-nftype,
         direct            TYPE j_1bnfdoc-direct,
         nfenum            TYPE j_1bnfdoc-nfenum,
         cfop              TYPE j_1bnflin-cfop,
         itmnum            TYPE j_1bnflin-itmnum,
         matnr_c03         TYPE j_1bnflin-matnr,
         maktx             TYPE j_1bnflin-maktx,
         matuse            TYPE j_1bnflin-matuse,
         matorg            TYPE j_1bnflin-matorg,
         zdia_cst          TYPE ztsd_br147-zdia_cst,
         parid             TYPE j_1bnfdoc-parid,
*         cgc               TYPE j_1bnfdoc-cgc,
         cgc               TYPE char18,
         name1             TYPE j_1bnfdoc-name1,
         regio             TYPE j_1bnfdoc-regio,
         netwr             TYPE j_1bnflin-netwr,
         nftot             TYPE j_1bnfdoc-nftot,
         zdia_a07          TYPE ztsd_dia_resp-nome_resp,
         zdia_a08          TYPE ztsd_dia_resp-tel_resp,
         zdia_a09          TYPE ztsd_dia_resp-email_resp,
         erdat             TYPE ztsd_br148-erdat,
         erzet             TYPE ztsd_br148-erzet,
         ernam             TYPE ztsd_br148-ernam,
       END OF ty_final,

       BEGIN OF ty_ztsd_br147,
         zdia_a03 TYPE ztsd_br147-zdia_a03,
         zdia_a04 TYPE ztsd_br147-zdia_a04,
         zdia_a05 TYPE ztsd_br147-zdia_a05,
         zdia_a06 TYPE ztsd_br147-zdia_a06,
         zdia_b04 TYPE ztsd_br147-zdia_b04,
         zdia_c02 TYPE ztsd_br147-zdia_c02,
         zdia_b03 TYPE ztsd_br147-zdia_b03,
         zdia_b05 TYPE ztsd_br147-zdia_b05,
         zdia_cst TYPE ztsd_br147-zdia_cst,
         zdia_c03 TYPE ztsd_br147-zdia_c03,
         zdia_c06 TYPE ztsd_br147-zdia_c06,
         zdia_c07 TYPE ztsd_br147-zdia_c07,
         zdia_d01 TYPE ztsd_br147-zdia_d01,
         regio    TYPE j_1bregio,
         nfyear   TYPE j_1byear,
         nfmonth  TYPE j_1bmonth,
         stcd1    TYPE j_1bstcd1,
         model    TYPE j_1bmodel,
         serie    TYPE j_1bseries,
         nfnum9   TYPE j_1bnfnum9,
         docnum9  TYPE j_1bdocnum9,
         cdv      TYPE j_1bcheckdigit,
       END OF ty_ztsd_br147,



       BEGIN OF ty_dados,
         docnum  TYPE j_1bnfe_active-docnum,
         regio   TYPE j_1bnfe_active-regio,
         nfyear  TYPE j_1bnfe_active-nfyear,
         nfmonth TYPE j_1bnfe_active-nfmonth,
         stcd1   TYPE j_1bnfe_active-stcd1,
         model   TYPE j_1bnfe_active-model,
         serie   TYPE j_1bnfe_active-serie,
         nfnum9  TYPE j_1bnfe_active-nfnum9,
         docnum9 TYPE j_1bnfe_active-docnum9,
         cdv     TYPE j_1bnfe_active-cdv,
         nftype  TYPE j_1bnfdoc-nftype,
         direct  TYPE j_1bnfdoc-direct,
         bukrs   TYPE j_1bnfdoc-bukrs,
         branch  TYPE j_1bnfdoc-branch,
         docdat  TYPE j_1bnfdoc-docdat,
         pstdat  TYPE j_1bnfdoc-pstdat,
         parid   TYPE j_1bnfdoc-parid,
         nftot   TYPE j_1bnfdoc-nftot,
         nfenum  TYPE j_1bnfdoc-nfenum,
         name1   TYPE j_1bnfdoc-name1,
         cgc     TYPE j_1bnfdoc-cgc,
         itmnum  TYPE j_1bnflin-itmnum,
         matnr   TYPE j_1bnflin-matnr,
         maktx   TYPE j_1bnflin-maktx,
         cfop    TYPE j_1bnflin-cfop,
         matorg  TYPE j_1bnflin-matorg,
         matuse  TYPE j_1bnflin-matuse,
         netwr   TYPE j_1bnflin-netwr,
       END OF ty_dados,

       BEGIN OF ty_ztsd_dia_codtrib,
         bukrs    TYPE ztsd_dia_codtrib-bukrs,
         branch   TYPE ztsd_dia_codtrib-branch,
         cfop     TYPE ztsd_dia_codtrib-cfop,
         matuse   TYPE ztsd_dia_codtrib-matuse,
         matorg   TYPE ztsd_dia_codtrib-matorg,
         multipl  TYPE ztsd_dia_codtrib-multipl,
         zdia_c06 TYPE ztsd_dia_codtrib-zdia_c06,
       END OF ty_ztsd_dia_codtrib,

       BEGIN OF ty_ztsd_dia_resp,
         bukrs      TYPE ztsd_dia_resp-bukrs,
         nome_resp  TYPE ztsd_dia_resp-nome_resp,
         tel_resp   TYPE ztsd_dia_resp-tel_resp,
         email_resp TYPE ztsd_dia_resp-email_resp,
       END OF ty_ztsd_dia_resp.

*&---------------------------------------------------------------------*
*& Internal Tables
*&---------------------------------------------------------------------*
DATA: gt_final            TYPE STANDARD TABLE OF ty_final,
      gt_ztsd_br147       TYPE STANDARD TABLE OF ty_ztsd_br147,
      gt_dados            TYPE STANDARD TABLE OF ty_dados,
      gt_ztsd_dia_codtrib TYPE STANDARD TABLE OF ty_ztsd_dia_codtrib,
      gt_ztsd_dia_resp    TYPE STANDARD TABLE OF ty_ztsd_dia_resp,
      gt_ztsd_br148       TYPE STANDARD TABLE OF ztsd_br148,
      gt_layout           TYPE lvc_s_layo,
      gt_fieldcat         TYPE lvc_t_fcat,
      gt_exclude          TYPE ui_functions.

*&---------------------------------------------------------------------*
*& Workarea
*&---------------------------------------------------------------------*
DATA: gs_final      TYPE ty_final,
      gs_ztsd_br148 TYPE ztsd_br148,
      gs_variant    TYPE disvariant.

*---------------------------------------------------------------------*
* GLOBAL ALV VARIABLES
*---------------------------------------------------------------------*
DATA:
  o_alv       TYPE REF TO cl_gui_alv_grid,
  o_container TYPE REF TO cl_gui_custom_container,
  gv_modify.

CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA: g_event_receiver TYPE REF TO lcl_event_receiver.

*&---------------------------------------------------------------------*
*& CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS:
      handle_data_changed
                    FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.

ENDCLASS.                    "lcl_event_receiver DEFINITION

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_data_changed.
*
    DATA: ls_alv    TYPE lvc_s_modi,
          lv_datbg  TYPE datbg,
          lv_flgupd TYPE c,
          lv_erro.

    LOOP AT er_data_changed->mt_mod_cells INTO ls_alv.

      CASE ls_alv-fieldname.
        WHEN 'BUKRS'.

          IF ls_alv-value IS NOT INITIAL.

            PERFORM zf_valida_empresa USING ls_alv-value
                                            ls_alv-row_id
                                   CHANGING lv_erro.

            IF lv_erro IS NOT INITIAL.
              EXIT.
            ENDIF.

          ENDIF.

        WHEN 'BRANCH'.

          IF ls_alv-value IS NOT INITIAL.

            PERFORM zf_valida_loc_neg USING ls_alv-value
                                            ls_alv-row_id
                                   CHANGING lv_erro.

            IF lv_erro IS NOT INITIAL.
              EXIT.
            ENDIF.

          ENDIF.

        WHEN OTHERS.
      ENDCASE.

      PERFORM zf_atualiza_campos_alv USING ls_alv-row_id
                                           ls_alv-fieldname
                                           ls_alv-value.

    ENDLOOP.

  ENDMETHOD.                    "handle_data_changed

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
