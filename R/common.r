reshape.imported <- function(object, to.NA, study, cycle, type) {
  setDT(object)
  setnames(x = object, toupper(names(object)))
  object[ , colnames(object) := lapply(.SD, function(i) {
    spss.format.string <- attr(x = i, which = "format.spss")
    start.char <- substr(x = spss.format.string, start = 1, stop = 1)
    var.labels <- attr(x = i, which = "label")
    var.labels <- gsub(pattern = "\\\\", replacement = "/", x = var.labels)
    var.labels <- gsub(pattern = "\u00e2\u20ac\u2122", replacement = "\\\\'", x = var.labels)
    var.labels <- gsub(pattern = "\u00e2\u0080\u009cA |\u00e2\u0080\u009d|\u00e2\u0080\u009c|\u00e2\u0080\u0099 ", replacement = '"', x = var.labels)
    var.labels <- gsub(pattern = "\u00e2\u0080\u0094", replacement = "-", x = var.labels)
    var.labels <- gsub(pattern = "\u00e2\u0080\u00a6", replacement = "...", x = var.labels)
    miss.labels <- lapply(X = all.missing.values.combinations, FUN = function(j) {
      identical(x = j, y = tail(names(sort(attr(x = i, which = "labels"))), n = length(j)))
    })
    miss.labels <- all.missing.values.combinations[which(unlist(miss.labels))]
    if(length(miss.labels) > 0) {
      miss.labels <- miss.labels[[which.max(lengths(miss.labels))]]
      miss.labels <- attr(x = i, which = "labels")[names(attr(x = i, which = "labels")) %in% miss.labels]
    } else {
      miss.labels <- names(attr(x = i, which = "labels"))[unlist(all.missing.values.combinations)]
      if(is.null(miss.labels)) {
        miss.labels <- attr(x = i, which = "na_values")
      }
      if(all(is.na(miss.labels))) {
        miss.labels <- NULL
      } else {
        miss.labels <- miss.labels
      }
    }
    if(start.char %in% c("A", "a") && is.null(attr(x = i, which = "labels"))) {
      i <- as.character(i)
    } else if(start.char %in% c("A", "a") && !is.null(attr(x = i, which = "labels"))) {
      if(isTRUE(to.NA)) {
        if(!is.null(miss.labels)) {
          i[i %in% as.numeric(miss.labels)] <- NA
        }
        i <- as.character(as_factor(x = i, levels = "both"))
      } else if(isFALSE(to.NA)) {
        if(!is.null(miss.labels)) {
          tmp.miss <- paste0("[", miss.labels, "] ", names(miss.labels))
        }
        i <- as.character(as_factor(i, levels = "both"))
        if(!is.null(miss.labels)) {
          attr(i, "missings") <- tmp.miss
        }
      }
    } else if(start.char %in% c("F", "f") && !is.null(attr(x = i, which = "labels"))) {
      names(attr(x = i, which = "labels")) <- make.unique(names(attr(x = i, which = "labels")))
      if(isTRUE(to.NA)) {
        if(all(names(attr(i, "labels")) %in% names(miss.labels))) {
          i <- as.numeric(i)
          i[i %in% miss.labels] <- NA
        } else if(!all(names(attr(i, "labels")) %in% names(miss.labels))) {
          i <- as_factor(i)
          i[i %in% names(miss.labels)] <- NA
          i <- factor(x = i, levels = levels(i)[!levels(i) %in% names(miss.labels)])
        }
      } else if(isFALSE(to.NA)) {
        if(all(names(attr(i, "labels")) %in% names(miss.labels))) {
          i <- as.numeric(i)
          attr(x = i, which = "missings") <- miss.labels
        } else if(all(!attr(i, "labels") %in% names(miss.labels))) {
          i <- as_factor(i)
          attr(i, "missings") <- names(miss.labels)
        }
      }
    } else if(start.char %in% c("F", "f") && is.null(attr(x = i, which = "labels"))) {
      i <- as.numeric(i)
      if(isTRUE(to.NA)) {
        if(is.null(miss.labels)) {
          i
        } else {
          i[i %in% as.numeric(miss.labels)] <- NA
        }
      } else {
        attr(i, "missings") <- miss.labels
      }
    } else if("Date" %in% class(i)) {
      i <- as.numeric(gsub(pattern = "[[:punct:]]+", replacement = "", x = as.character(i)))
    }
    if(length(var.labels)) {
      setattr(x = object[ , (i)], name = "variable.label", value = var.labels)
    }
    setattr(x = object[ , (i)], name = "label", value = NULL)
  })]
  rep.indicator <- names(object)[which(colnames(object) %in% c("JKINDIC", "JKCREP", "JKREP", "jkrep"))]
  if(length(rep.indicator) > 0) {
    object[ , eval(rep.indicator) := lapply(.SD, function(i) {
      tmp.var.label <- attr(x = i, which = "variable.label")
      tmp.missings <- attr(x = i, which = "missings")
      if(is.factor(i)) {
        i <- as.numeric(i) - 1
      } else {
        i <- i
      }
      if(length(tmp.var.label) > 0) {
        attr(x = i, which = "variable.label") <- tmp.var.label
      } else {
        i <- i
      }
      if(length(tmp.missings) > 0) {
        attr(x = i, which = "missings") <- tmp.missings
      } else {
        i <- i
      }
      return(i)
    }), .SDcols = rep.indicator]
  }
  if(study %in% c("TIMSS", "PIRLS", "TIMSS Advanced", "RLII", "TiPi", "prePIRLS", "preTIMSS", "ePIRLS", "eTIMSS PSI", "CivED", "ICCS") || study %in% c("ICILS") & cycle %in% c("2023")) {
    idcntry.var.lab <- attr(x = object[ , IDCNTRY], which = "variable.label")
    if(isFALSE(to.NA)) {
      idcntry.missings <- attr(x = object[ , IDCNTRY], which = "missings")
    }
    idx.cnt.num.code <- match(unique(object[ , IDCNTRY]), cnt.ID.list[["Numeric"]])
    object[ , IDCNTRY := factor(x = IDCNTRY, labels = cnt.ID.list[["ISO"]][idx.cnt.num.code])]
    object[ , setattr(x = IDCNTRY, name = "variable.label", value = idcntry.var.lab)]
  } else if(study %in% c("SITES", "TEDS-M", "TALIS", "REDS") || study %in% c("ICILS") & cycle %in% c("2013", "2018")) {
    idcntry.var.lab <- attr(x = object[ , IDCNTRY], which = "variable.label")
    if(isFALSE(to.NA)) {
      idcntry.missings <- attr(x = object[ , IDCNTRY], which = "missings")
    }
    object[ , IDCNTRY := droplevels(x = IDCNTRY)]
    if(isFALSE(to.NA) && length(idcntry.missings) > 0) {
      object[ , setattr(x = IDCNTRY, name = "missings", value = idcntry.missings)]
    }
    object[ , setattr(x = IDCNTRY, name = "variable.label", value = idcntry.var.lab)]
  } else if(study %in% c("PISA", "PISA for Development")) {
    if(is.character(object[ , CNT])) {
      cnt.var.lab <- attr(x = object[ , CNT], which = "variable.label")
      object[ , CNT := as.factor(gsub(pattern = "^\\[[[:alpha:]]+\\][[:space:]]+", replacement = "", x = CNT))]
      object[ , setattr(x = CNT, name = "variable.label", value = cnt.var.lab)]
    }
  }
  setattr(x = object, name = "study", value = get(x = "study.attribute", envir = parent.frame()))
  setattr(x = object, name = "cycle", value = get(x = "cycle.attribute", envir = parent.frame()))
  setattr(x = object, name = "file.type", value = get(x = "file.type.attribute", envir = parent.frame()))
  setattr(x = object, name = "class", value = c("lsa.data", attr(x = object, which = "class")))
  if(isFALSE(to.NA)) {
    object[ , colnames(object) := lapply(.SD, function(i) {
      if(is.factor(i) & is.numeric(attr(x = i, which = "missings"))) {
        tmp.label <- attr(x = i, which = "variable.label")
        names.miss <- names(attr(x = i, which = "missings"))
        i <- factor(x = i, levels = c(levels(x = i)[!levels(i) %in% as.character(attr(x = i, which = "missings"))], names.miss))
        attr(x = i, which = "missings") <- names.miss
        attr(x = i, which = "variable.label") <- tmp.label
        return(i)
      } else {
        i
      }
    })]
  } else {
    object
  }
  if(study %in% c("PISA", "PISA for Development")) {
    setkeyv(x = object, cols = "CNT")
  } else {
    setkeyv(x = object, cols = "IDCNTRY")
  }
}
cnt.ID.list <- list(
  Numeric = c(32, 51, 36, 40, 48, 3724, 956, 957, 56, 84, 72, 100, 124, 9132, 9133, 9134, 9135, 9136, 152, 158, 170, 196, 203, 200, 208, 818, 926, 826, 233, 246, 250, 268, 276, 288, 300, 344, 348, 352, 9352, 11800, 360, 364, 372, 376, 380, 392, 400, 410, 414, 428, 422, 440, 442, 807, 458, 498, 504, 528, 554, 578, 9578, 275, 608, 616, 620, 634, 642, 643, 6431, 682, 927, 891, 702, 703, 222, 705, 710, 4710, 724, 7241, 752, 3752, 756, 760, 764, 780, 788, 792, 840, 887, 470, 12700, 12500, 512, 804, 12, 398, 496, 70, 7841, 76, 484, 48401, 48402, 48499, 214, 320, 438, 600, 6162, 57891, 57892, 57893, 57894, 7842, 784, 31, 72401, 72404, 7246, 340, 191, 6504, 9470, 928, 9528, 7554, 7702, 688, 10400, 11100, 10800, 10900, 11200, 13700, 6887, 32001, 8261, 9642, 48411, 48420, 48412, 48415, 48416, 48417, 48418, 48421, 48422, 48425, 48426, 48427, 48428, 188, 558, 604, 9130, 156001, 643002, 5784, 5788, 276001, 724005, 446, 643001, 7105, 208001, 724004, 710003, 704, 858, 218, 8, 499, 586, 411, 710004, 854, 231, 356, 404, 646, 800, 860, 7106, 276005, 7843, 384, 368, 566, 686, 124008, 310001, 760005, 156003),
  ISO = c("Argentina", "Armenia", "Australia", "Austria", "Bahrain", "Spain (Basque Country)", "Belgium (Flemish)", "Belgium (French)", "Belgium", "Belize", "Botswana", "Bulgaria", "Canada", "Canada (Ontario)", "Canada (Quebec)", "Canada (Alberta)", "Canada (British Columbia)", "Canada (Nova Scotia)", "Chile", "Chinese Taipei", "Colombia", "Cyprus", "Czech Republic", "Czech Republic", "Denmark", "Egypt", "England", "United Kingdom", "Estonia", "Finland", "France", "Georgia", "Germany", "Ghana", "Greece", "Hong Kong, SAR", "Hungary", "Iceland", "Iceland (Grade 5)", "United States (Indiana)", "Indonesia", "Iran, Islamic Republic of", "Ireland", "Israel", "Italy", "Japan", "Jordan", "Korea, Republic of", "Kuwait", "Latvia", "Lebanon", "Lithuania", "Luxembourg", "North Macedonia", "Malaysia", "Moldova", "Morocco", "Netherlands", "New Zealand", "Norway", "Norway (Grade 5)", "Palestinian National Authority", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russian Federation", "Russian Federation (Moscow)", "Saudi Arabia", "Scotland", "Serbia", "Singapore", "Slovak Republic", "El Salvador", "Slovenia", "South Africa", "South Africa (Grade 4)", "Spain", "Spain (Catalonia)", "Sweden", "Sweden (Grade 3)", "Switzerland", "Syria, Arab Republic of", "Thailand", "Trinidad And Tobago", "Tunisia", "Turkey", "United States", "Yemen", "Malta", "United States (Minnesota)", "United States (Massachusetts)", "Oman", "Ukraine", "Algeria", "Kazakhstan", "Mongolia", "Bosnia and Herzegovina", "United Arab Emirates (Dubai)", "Brazil", "Mexico", "Mexico (Generales/Tecnicas/Privadas)", "Mexico (Telesecundarias)", "Mexico (Talis-Nacional)", "Dominican Republic", "Guatemala", "Liechtenstein", "Paraguay", "Poland (Second-Cycle Programs)", "Norway (ALU)", "Norway (ALU +)", "Norway (PPU)", "Norway (MASTERS)", "United Arab Emirates (Abu Dhabi)", "United Arab Emirates", "Azerbaijan, Republic of", "Spain (Andalucia)", "Spain (Canary Islands)", "Finland (Grade 7)", "Honduras, Republic of", "Croatia", "Morocco (Grade 6)", "Malta (Maltese)", "Northern Ireland", "The Netherlands (50 additional schools)", "New Zealand (TIMSS data processing)", "Singapore (Chinese Grade 7)", "Serbia", "United States (Alabama)", "United States (California)", "United States (Colorado)", "United States (Connecticut)", "United States (Florida)", "United States (North Carolina)", "Yemen (Grade 6)", "Argentina, Buenos Aires", "England and Northern Ireland (UK)", "Romania", "Mexico (Distrito Federal)", "Mexico (International Telesecundaria)", "Mexico (Jalisco)", "Mexico (Nuevo Leon)", "Mexico (Quintana Roo)", "Mexico (San Luis Potosi)", "Mexico (Tamaulipas)", "Mexico (Telesecundaria-Distrito Federal)", "Mexico (Telesecundaria-Jalisco)", "Mexico (Telesecundaria-Nuevo Leon)", "Mexico (Telesecundaria-Quintana Roo)", "Mexico (Telesecundaria-San Luis Potosi)", "Mexico (Telesecundaria-Tamaulipas)", "Costa Rica", "Nicaragua", "Peru", "Canada (Newfoundland and Labrador)", "China (Shanghai)", "Russia (8+ sample)", "Norway (4)", "Norway (8)", "Germany, North-Rhine Westphalia", "Spain, Madrid", "Macao SAR", "Russian Federation, Moscow", "South Africa (Eng/Afr)", "Denmark (Grade 3)", "Spain, Madrid, Bilingual", "South Africa (Gauteng)", "Vietnam", "Uruguay", "Ecuador", "Albania", "Montenegro", "Pakistan", "Kosovo", "South Africa (Western Cape Province)", "Burkina Faso", "Ethiopia", "India", "Kenya", "Rwanda", "Uganda", "Uzbekistan", "South Africa (Grade 6)", "Germany, Schleswig-Holstein", "United Arab Emirates (Sharjah)", "Ivory Coast", "Iraq", "Nigeria", "Senegal", "Canada (New Brunswick)", "Baku, Sumgait (Azerbaijan)", "Cear\u00e1, Par\u00e1, S\u00e3o Paulo (Brazil)", "Hangzhou (China)")
)
import.data <- function(path) {
  tmp <- load(path)
  return(get(tmp))
}
study.dataset.files <- list(
  ZIP.data.folders.and.files = list(
    CivED_1999 = list(
      CivED_1999_G8 = list("CivED1999_IDB_SPSS_G8/Data",
                           c("bc_ausf2.sav", "bc_bfrf2.sav", "bc_bgrf2.sav", "bc_chef2.sav", "bc_chlf2.sav", "bc_colf2.sav", "bc_cypf2.sav", "bc_czef2.sav", "bc_deuf2.sav", "bc_dnkf2.sav", "bc_engf2.sav", "bc_estf2.sav", "bc_finf2.sav", "bc_grcf2.sav", "bc_hkgf2.sav", "bc_hunf2.sav", "bc_itaf2.sav", "bc_ltuf2.sav", "bc_lvaf2.sav", "bc_norf2.sav", "bc_polf2.sav", "bc_prtf2.sav", "bc_romf2.sav", "bc_rusf2.sav", "bc_svkf2.sav", "bc_svnf2.sav", "bc_swef2.sav", "bc_usaf2.sav", "bl_ausf2.sav", "bl_bfrf2.sav", "bl_bgrf2.sav", "bl_chef2.sav", "bl_chlf2.sav", "bl_cypf2.sav", "bl_czef2.sav", "bl_deuf2.sav", "bl_dnkf2.sav", "bl_engf2.sav", "bl_estf2.sav", "bl_finf2.sav", "bl_grcf2.sav", "bl_hkgf2.sav", "bl_hunf2.sav", "bl_itaf2.sav", "bl_ltuf2.sav", "bl_lvaf2.sav", "bl_norf2.sav", "bl_polf2.sav", "bl_prtf2.sav", "bl_romf2.sav", "bl_rusf2.sav", "bl_svkf2.sav", "bl_svnf2.sav", "bl_swef2.sav", "bl_usaf2.sav", "bs_ausf2.sav", "bs_bfrf2.sav", "bs_bgrf2.sav", "bs_chef2.sav", "bs_chlf2.sav", "bs_colf2.sav", "bs_cypf2.sav", "bs_czef2.sav", "bs_deuf2.sav", "bs_dnkf2.sav", "bs_engf2.sav", "bs_estf2.sav", "bs_finf2.sav", "bs_grcf2.sav", "bs_hkgf2.sav", "bs_hunf2.sav", "bs_itaf2.sav", "bs_ltuf2.sav", "bs_lvaf2.sav", "bs_norf2.sav", "bs_polf2.sav", "bs_prtf2.sav", "bs_romf2.sav", "bs_rusf2.sav", "bs_svkf2.sav", "bs_svnf2.sav", "bs_swef2.sav", "bs_usaf2.sav", "bt_ausf2.sav", "bt_bfrf2.sav", "bt_bgrf2.sav", "bt_chef2.sav", "bt_chlf2.sav", "bt_cypf2.sav", "bt_czef2.sav", "bt_deuf2.sav", "bt_dnkf2.sav", "bt_engf2.sav", "bt_estf2.sav", "bt_finf2.sav", "bt_grcf2.sav", "bt_hkgf2.sav", "bt_hunf2.sav", "bt_itaf2.sav", "bt_ltuf2.sav", "bt_lvaf2.sav", "bt_norf2.sav", "bt_polf2.sav", "bt_prtf2.sav", "bt_romf2.sav", "bt_rusf2.sav", "bt_svkf2.sav", "bt_svnf2.sav", "bt_swef2.sav", "bt_usaf2.sav")
      ),
      CivED_1999_G12 = list(
        "CivED1999_IDB_SPSS_G12/Data", c("cs_chef2.sav", "cs_chlf2.sav", "cs_colf2.sav", "cs_cypf2.sav", "cs_czef2.sav", "cs_dnkf2.sav", "cs_estf2.sav", "cs_hkgf2.sav", "cs_isrf2.sav", "cs_lvaf2.sav", "cs_norf2.sav", "cs_polf2.sav", "cs_prtf2.sav", "cs_rusf2.sav", "cs_svnf2.sav", "cs_swef2.sav")
      )),
    ePIRLS_2016 = list(
      ePIRLS_2016_G4 = list("ePIRLS2016_IDB_SPSS/Data", c("ACGAADE1.sav", "ACGADUE1.sav", "ACGAREE1.sav", "ACGCANE1.sav", "ACGDNKE1.sav", "ACGGEOE1.sav", "ACGIRLE1.sav", "ACGISRE1.sav", "ACGITAE1.sav", "ACGNORE1.sav", "ACGPRTE1.sav", "ACGSGPE1.sav", "ACGSVNE1.sav", "ACGSWEE1.sav", "ACGTWNE1.sav", "ACGUSAE1.sav", "ASAAADE1.sav", "ASAADUE1.sav", "ASAAREE1.sav", "ASACANE1.sav", "ASADNKE1.sav", "ASAGEOE1.sav", "ASAIRLE1.sav", "ASAISRE1.sav", "ASAITAE1.sav", "ASANORE1.sav", "ASAPRTE1.sav", "ASASGPE1.sav", "ASASVNE1.sav", "ASASWEE1.sav", "ASATWNE1.sav", "ASAUSAE1.sav", "ASGAADE1.sav", "ASGADUE1.sav", "ASGAREE1.sav", "ASGCANE1.sav", "ASGDNKE1.sav", "ASGGEOE1.sav", "ASGIRLE1.sav", "ASGISRE1.sav", "ASGITAE1.sav", "ASGNORE1.sav", "ASGPRTE1.sav", "ASGSGPE1.sav", "ASGSVNE1.sav", "ASGSWEE1.sav", "ASGTWNE1.sav", "ASGUSAE1.sav", "ASHAADE1.sav", "ASHADUE1.sav", "ASHAREE1.sav", "ASHCANE1.sav", "ASHDNKE1.sav", "ASHGEOE1.sav", "ASHIRLE1.sav", "ASHISRE1.sav", "ASHITAE1.sav", "ASHNORE1.sav", "ASHPRTE1.sav", "ASHSGPE1.sav", "ASHSVNE1.sav", "ASHSWEE1.sav", "ASHTWNE1.sav", "ASHUSAE1.sav", "ASRAADE1.sav", "ASRADUE1.sav", "ASRAREE1.sav", "ASRCANE1.sav", "ASRDNKE1.sav", "ASRGEOE1.sav", "ASRIRLE1.sav", "ASRISRE1.sav", "ASRITAE1.sav", "ASRNORE1.sav", "ASRPRTE1.sav", "ASRSGPE1.sav", "ASRSVNE1.sav", "ASRSWEE1.sav", "ASRTWNE1.sav", "ASRUSAE1.sav", "ASTAADE1.sav", "ASTADUE1.sav", "ASTAREE1.sav", "ASTCANE1.sav", "ASTDNKE1.sav", "ASTGEOE1.sav", "ASTIRLE1.sav", "ASTISRE1.sav", "ASTITAE1.sav", "ASTNORE1.sav", "ASTPRTE1.sav", "ASTSGPE1.sav", "ASTSVNE1.sav", "ASTSWEE1.sav", "ASTTWNE1.sav", "ASTUSAE1.sav", "ATGAADE1.sav", "ATGADUE1.sav", "ATGAREE1.sav", "ATGCANE1.sav", "ATGDNKE1.sav", "ATGGEOE1.sav", "ATGIRLE1.sav", "ATGISRE1.sav", "ATGITAE1.sav", "ATGNORE1.sav", "ATGPRTE1.sav", "ATGSGPE1.sav", "ATGSVNE1.sav", "ATGSWEE1.sav", "ATGTWNE1.sav", "ATGUSAE1.sav")
      )),
    ICCS_2009 = list(
      ICCS_2009_G8 = list("ICCS2009_IDB_SPSS/Data_G8", c("ICGAUTC2.sav", "ICGBFLC2.sav", "ICGBGRC2.sav", "ICGCHEC2.sav", "ICGCHLC2.sav", "ICGCOLC2.sav", "ICGCYPC2.sav", "ICGCZEC2.sav", "ICGDNKC2.sav", "ICGDOMC2.sav", "ICGENGC2.sav", "ICGESPC2.sav", "ICGESTC2.sav", "ICGFINC2.sav", "ICGGRCC2.sav", "ICGGTMC2.sav", "ICGHKGC2.sav", "ICGIDNC2.sav", "ICGIRLC2.sav", "ICGITAC2.sav", "ICGKORC2.sav", "ICGLIEC2.sav", "ICGLTUC2.sav", "ICGLUXC2.sav", "ICGLVAC2.sav", "ICGMEXC2.sav", "ICGMLTC2.sav", "ICGNLDC2.sav", "ICGNORC2.sav", "ICGNZLC2.sav", "ICGPOLC2.sav", "ICGPRYC2.sav", "ICGRUSC2.sav", "ICGSVKC2.sav", "ICGSVNC2.sav", "ICGSWEC2.sav", "ICGTHAC2.sav", "ICGTWNC2.sav", "ISAAUTC2.sav", "ISABFLC2.sav", "ISABGRC2.sav", "ISACHEC2.sav", "ISACHLC2.sav", "ISACOLC2.sav", "ISACYPC2.sav", "ISACZEC2.sav", "ISADNKC2.sav", "ISADOMC2.sav", "ISAENGC2.sav", "ISAESPC2.sav", "ISAESTC2.sav", "ISAFINC2.sav", "ISAGRCC2.sav", "ISAGTMC2.sav", "ISAHKGC2.sav", "ISAIDNC2.sav", "ISAIRLC2.sav", "ISAITAC2.sav", "ISAKORC2.sav", "ISALIEC2.sav", "ISALTUC2.sav", "ISALUXC2.sav", "ISALVAC2.sav", "ISAMEXC2.sav", "ISAMLTC2.sav", "ISANLDC2.sav", "ISANORC2.sav", "ISANZLC2.sav", "ISAPOLC2.sav", "ISAPRYC2.sav", "ISARUSC2.sav", "ISASVKC2.sav", "ISASVNC2.sav", "ISASWEC2.sav", "ISATHAC2.sav", "ISATWNC2.sav", "ISEAUTC2.sav", "ISEBFLC2.sav", "ISEBGRC2.sav", "ISECHEC2.sav", "ISECYPC2.sav", "ISECZEC2.sav", "ISEDNKC2.sav", "ISEENGC2.sav", "ISEESPC2.sav", "ISEESTC2.sav", "ISEFINC2.sav", "ISEGRCC2.sav", "ISEIRLC2.sav", "ISEITAC2.sav", "ISELIEC2.sav", "ISELTUC2.sav", "ISELUXC2.sav", "ISELVAC2.sav", "ISEMLTC2.sav", "ISENLDC2.sav", "ISEPOLC2.sav", "ISESVKC2.sav", "ISESVNC2.sav", "ISESWEC2.sav", "ISGAUTC2.sav", "ISGBFLC2.sav", "ISGBGRC2.sav", "ISGCHEC2.sav", "ISGCHLC2.sav", "ISGCOLC2.sav", "ISGCYPC2.sav", "ISGCZEC2.sav", "ISGDNKC2.sav", "ISGDOMC2.sav", "ISGENGC2.sav", "ISGESPC2.sav", "ISGESTC2.sav", "ISGFINC2.sav", "ISGGRCC2.sav", "ISGGTMC2.sav", "ISGHKGC2.sav", "ISGIDNC2.sav", "ISGIRLC2.sav", "ISGITAC2.sav", "ISGKORC2.sav", "ISGLIEC2.sav", "ISGLTUC2.sav", "ISGLUXC2.sav", "ISGLVAC2.sav", "ISGMEXC2.sav", "ISGMLTC2.sav", "ISGNLDC2.sav", "ISGNORC2.sav", "ISGNZLC2.sav", "ISGPOLC2.sav", "ISGPRYC2.sav", "ISGRUSC2.sav", "ISGSVKC2.sav", "ISGSVNC2.sav", "ISGSWEC2.sav", "ISGTHAC2.sav", "ISGTWNC2.sav", "ISLCHLC2.sav", "ISLCOLC2.sav", "ISLDOMC2.sav", "ISLGTMC2.sav", "ISLMEXC2.sav", "ISLPRYC2.sav", "ISRAUTC2.sav", "ISRBFLC2.sav", "ISRBGRC2.sav", "ISRCHEC2.sav", "ISRCHLC2.sav", "ISRCOLC2.sav", "ISRCYPC2.sav", "ISRCZEC2.sav", "ISRDNKC2.sav", "ISRDOMC2.sav", "ISRENGC2.sav", "ISRESPC2.sav", "ISRESTC2.sav", "ISRFINC2.sav", "ISRGRCC2.sav", "ISRGTMC2.sav", "ISRHKGC2.sav", "ISRIDNC2.sav", "ISRIRLC2.sav", "ISRITAC2.sav", "ISRKORC2.sav", "ISRLIEC2.sav", "ISRLTUC2.sav", "ISRLUXC2.sav", "ISRLVAC2.sav", "ISRMEXC2.sav", "ISRMLTC2.sav", "ISRNLDC2.sav", "ISRNORC2.sav", "ISRNZLC2.sav", "ISRPOLC2.sav", "ISRPRYC2.sav", "ISRRUSC2.sav", "ISRSVKC2.sav", "ISRSVNC2.sav", "ISRSWEC2.sav", "ISRTHAC2.sav", "ISRTWNC2.sav", "ISSHKGC2.sav", "ISSIDNC2.sav", "ISSKORC2.sav", "ISSTHAC2.sav", "ISSTWNC2.sav", "ITGAUTC2.sav", "ITGBFLC2.sav", "ITGBGRC2.sav", "ITGCHEC2.sav", "ITGCHLC2.sav", "ITGCOLC2.sav", "ITGCYPC2.sav", "ITGCZEC2.sav", "ITGDNKC2.sav", "ITGDOMC2.sav", "ITGENGC2.sav", "ITGESPC2.sav", "ITGESTC2.sav", "ITGFINC2.sav", "ITGGTMC2.sav", "ITGHKGC2.sav", "ITGIDNC2.sav", "ITGIRLC2.sav", "ITGITAC2.sav", "ITGKORC2.sav", "ITGLIEC2.sav", "ITGLTUC2.sav", "ITGLUXC2.sav", "ITGLVAC2.sav", "ITGMEXC2.sav", "ITGMLTC2.sav", "ITGNORC2.sav", "ITGNZLC2.sav", "ITGPOLC2.sav", "ITGPRYC2.sav", "ITGRUSC2.sav", "ITGSVKC2.sav", "ITGSVNC2.sav", "ITGSWEC2.sav", "ITGTHAC2.sav", "ITGTWNC2.sav")
      ),
      ICCS_2009_G9 = list(
        "ICCS2009_IDB_SPSS/Data_G9",
        c("JSAGRCC2.sav", "JSANORC2.sav", "JSASVNC2.sav", "JSASWEC2.sav", "JSEGRCC2.sav", "JSESVNC2.sav", "JSESWEC2.sav", "JSGGRCC2.sav", "JSGNORC2.sav", "JSGSVNC2.sav", "JSGSWEC2.sav", "JSRGRCC2.sav", "JSRNORC2.sav", "JSRSVNC2.sav", "JSRSWEC2.sav")
      )),
    ICCS_2016 = list(
      ICCS_2016_G8 = list("ICCS2016_IDB_SPSS/Data",
                          c("ICGBFLC3.sav", "ICGBGRC3.sav", "ICGCHLC3.sav", "ICGCOLC3.sav", "ICGDNKC3.sav", "ICGDNWC3.sav", "ICGDOMC3.sav", "ICGESTC3.sav", "ICGFINC3.sav", "ICGHKGC3.sav", "ICGHRVC3.sav", "ICGITAC3.sav", "ICGKORC3.sav", "ICGLTUC3.sav", "ICGLVAC3.sav", "ICGMEXC3.sav", "ICGMLTC3.sav", "ICGNLDC3.sav", "ICGNORC3.sav", "ICGPERC3.sav", "ICGRUSC3.sav", "ICGSVNC3.sav", "ICGSWEC3.sav", "ICGTWNC3.sav", "ISABFLC3.sav", "ISABGRC3.sav", "ISACHLC3.sav", "ISACOLC3.sav", "ISADNKC3.sav", "ISADNWC3.sav", "ISADOMC3.sav", "ISAESTC3.sav", "ISAFINC3.sav", "ISAHKGC3.sav", "ISAHRVC3.sav", "ISAITAC3.sav", "ISAKORC3.sav", "ISALTUC3.sav", "ISALVAC3.sav", "ISAMEXC3.sav", "ISAMLTC3.sav", "ISANLDC3.sav", "ISANORC3.sav", "ISAPERC3.sav", "ISARUSC3.sav", "ISASVNC3.sav", "ISASWEC3.sav", "ISATWNC3.sav", "ISEBFLC3.sav", "ISEBGRC3.sav", "ISEDNKC3.sav", "ISEDNWC3.sav", "ISEESTC3.sav", "ISEFINC3.sav", "ISEHRVC3.sav", "ISEITAC3.sav", "ISELTUC3.sav", "ISELVAC3.sav", "ISEMLTC3.sav", "ISENLDC3.sav", "ISENORC3.sav", "ISESVNC3.sav", "ISESWEC3.sav", "ISGBFLC3.sav", "ISGBGRC3.sav", "ISGCHLC3.sav", "ISGCOLC3.sav", "ISGDNKC3.sav", "ISGDNWC3.sav", "ISGDOMC3.sav", "ISGESTC3.sav", "ISGFINC3.sav", "ISGHKGC3.sav", "ISGHRVC3.sav", "ISGITAC3.sav", "ISGKORC3.sav", "ISGLTUC3.sav", "ISGLVAC3.sav", "ISGMEXC3.sav", "ISGMLTC3.sav", "ISGNLDC3.sav", "ISGNORC3.sav", "ISGPERC3.sav", "ISGRUSC3.sav", "ISGSVNC3.sav", "ISGSWEC3.sav", "ISGTWNC3.sav", "ISLCHLC3.sav", "ISLCOLC3.sav", "ISLDOMC3.sav", "ISLMEXC3.sav", "ISLPERC3.sav", "ISRBFLC3.sav", "ISRBGRC3.sav", "ISRCHLC3.sav", "ISRCOLC3.sav", "ISRDNKC3.sav", "ISRDNWC3.sav", "ISRDOMC3.sav", "ISRESTC3.sav", "ISRFINC3.sav", "ISRHKGC3.sav", "ISRHRVC3.sav", "ISRITAC3.sav", "ISRKORC3.sav", "ISRLTUC3.sav", "ISRLVAC3.sav", "ISRMEXC3.sav", "ISRMLTC3.sav", "ISRNLDC3.sav", "ISRNORC3.sav", "ISRPERC3.sav", "ISRRUSC3.sav", "ISRSVNC3.sav", "ISRSWEC3.sav", "ISRTWNC3.sav", "ITGBFLC3.sav", "ITGBGRC3.sav", "ITGCHLC3.sav", "ITGCOLC3.sav", "ITGDNKC3.sav", "ITGDOMC3.sav", "ITGESTC3.sav", "ITGFINC3.sav", "ITGHRVC3.sav", "ITGITAC3.sav", "ITGKORC3.sav", "ITGLTUC3.sav", "ITGLVAC3.sav", "ITGMEXC3.sav", "ITGMLTC3.sav", "ITGNLDC3.sav", "ITGNORC3.sav", "ITGPERC3.sav", "ITGRUSC3.sav", "ITGSVNC3.sav", "ITGSWEC3.sav", "ITGTWNC3.sav")
      )),
    ICCS_2022 = list(
      ICCS_2022_G8 = list("ICCS2022_IDB_SPSS/Data",
                          c("ICGBGRC4.sav", "ICGBRAC4.sav", "ICGCOLC4.sav", "ICGCYPC4.sav", "ICGDNKC4.sav", "ICGDNWC4.sav", "ICGDSHC4.sav", "ICGESPC4.sav", "ICGESTC4.sav", "ICGFRAC4.sav", "ICGHRVC4.sav", "ICGITAC4.sav", "ICGLTUC4.sav", "ICGLVAC4.sav", "ICGMLTC4.sav", "ICGNLDC4.sav", "ICGNORC4.sav", "ICGPOLC4.sav", "ICGROUC4.sav", "ICGSRBC4.sav", "ICGSVKC4.sav", "ICGSVNC4.sav", "ICGSWEC4.sav", "ICGTWNC4.sav", "ISABGRC4.sav", "ISABRAC4.sav", "ISACOLC4.sav", "ISACYPC4.sav", "ISADNKC4.sav", "ISADNWC4.sav", "ISADSHC4.sav", "ISAESPC4.sav", "ISAESTC4.sav", "ISAFRAC4.sav", "ISAHRVC4.sav", "ISAITAC4.sav", "ISALTUC4.sav", "ISALVAC4.sav", "ISAMLTC4.sav", "ISANLDC4.sav", "ISANORC4.sav", "ISAPOLC4.sav", "ISAROUC4.sav", "ISASRBC4.sav", "ISASVKC4.sav", "ISASVNC4.sav", "ISASWEC4.sav", "ISATWNC4.sav", "ISEBGRC4.sav", "ISECYPC4.sav", "ISEDNKC4.sav", "ISEDNWC4.sav", "ISEDSHC4.sav", "ISEESPC4.sav", "ISEESTC4.sav", "ISEFRAC4.sav", "ISEHRVC4.sav", "ISEITAC4.sav", "ISELTUC4.sav", "ISELVAC4.sav", "ISEMLTC4.sav", "ISENLDC4.sav", "ISENORC4.sav", "ISEPOLC4.sav", "ISEROUC4.sav", "ISESVKC4.sav", "ISESVNC4.sav", "ISESWEC4.sav", "ISGBGRC4.sav", "ISGBRAC4.sav", "ISGCOLC4.sav", "ISGCYPC4.sav", "ISGDNKC4.sav", "ISGDNWC4.sav", "ISGDSHC4.sav", "ISGESPC4.sav", "ISGESTC4.sav", "ISGFRAC4.sav", "ISGHRVC4.sav", "ISGITAC4.sav", "ISGLTUC4.sav", "ISGLVAC4.sav", "ISGMLTC4.sav", "ISGNLDC4.sav", "ISGNORC4.sav", "ISGPOLC4.sav", "ISGROUC4.sav", "ISGSRBC4.sav", "ISGSVKC4.sav", "ISGSVNC4.sav", "ISGSWEC4.sav", "ISGTWNC4.sav", "ISLBRAC4.sav", "ISLCOLC4.sav", "ISPBGRC4.sav", "ISPBRAC4.sav", "ISPCOLC4.sav", "ISPCYPC4.sav", "ISPDNWC4.sav", "ISPDSHC4.sav", "ISPESPC4.sav", "ISPESTC4.sav", "ISPFRAC4.sav", "ISPITAC4.sav", "ISPLTUC4.sav", "ISPLVAC4.sav", "ISPMLTC4.sav", "ISPNORC4.sav", "ISPSVKC4.sav", "ISPSVNC4.sav", "ISPSWEC4.sav", "ISPTWNC4.sav", "ISRBGRC4.sav", "ISRBRAC4.sav", "ISRCOLC4.sav", "ISRCYPC4.sav", "ISRDNKC4.sav", "ISRDNWC4.sav", "ISRDSHC4.sav", "ISRESPC4.sav", "ISRESTC4.sav", "ISRFRAC4.sav", "ISRHRVC4.sav", "ISRITAC4.sav", "ISRLTUC4.sav", "ISRLVAC4.sav", "ISRMLTC4.sav", "ISRNLDC4.sav", "ISRNORC4.sav", "ISRPOLC4.sav", "ISRROUC4.sav", "ISRSRBC4.sav", "ISRSVKC4.sav", "ISRSVNC4.sav", "ISRSWEC4.sav", "ISRTWNC4.sav", "ITGBGRC4.sav", "ITGBRAC4.sav", "ITGCOLC4.sav", "ITGCYPC4.sav", "ITGDNKC4.sav", "ITGDNWC4.sav", "ITGDSHC4.sav", "ITGESPC4.sav", "ITGESTC4.sav", "ITGFRAC4.sav", "ITGHRVC4.sav", "ITGITAC4.sav", "ITGLTUC4.sav", "ITGLVAC4.sav", "ITGMLTC4.sav", "ITGNLDC4.sav", "ITGNORC4.sav", "ITGPOLC4.sav", "ITGROUC4.sav", "ITGSRBC4.sav", "ITGSVKC4.sav", "ITGSVNC4.sav", "ITGSWEC4.sav", "ITGTWNC4.sav")
      ),
      ICCS_2022_G8_Bridge = list("ICCS2022_IDB_SPSS/Data",
                                 c("ICGCOLB4.sav", "ICGSVNB4.sav", "ICGTWNB4.sav", "ISABGRB4.sav", "ISACOLB4.sav", "ISAESTB4.sav", "ISAITAB4.sav", "ISALTUB4.sav", "ISALVAB4.sav", "ISAMLTB4.sav", "ISANORB4.sav", "ISASVNB4.sav", "ISASWEB4.sav", "ISATWNB4.sav", "ISEBGRB4.sav", "ISEESTB4.sav", "ISEITAB4.sav", "ISELTUB4.sav", "ISELVAB4.sav", "ISEMLTB4.sav", "ISENORB4.sav", "ISESVNB4.sav", "ISESWEB4.sav", "ISGBGRB4.sav", "ISGCOLB4.sav", "ISGESTB4.sav", "ISGITAB4.sav", "ISGLTUB4.sav", "ISGLVAB4.sav", "ISGMLTB4.sav", "ISGNORB4.sav", "ISGSVNB4.sav", "ISGSWEB4.sav", "ISGTWNB4.sav", "ISLCOLB4.sav", "ISRBGRB4.sav", "ISRCOLB4.sav", "ISRESTB4.sav", "ISRITAB4.sav", "ISRLTUB4.sav", "ISRLVAB4.sav", "ISRMLTB4.sav", "ISRNORB4.sav", "ISRSVNB4.sav", "ISRSWEB4.sav", "ISRTWNB4.sav")
      )),
    ICILS_2013 = list(
      ICILS_2013_G8 = list("ICILS2013_IDB_SPSS/Data",
                           c("BCGABAI1.sav", "BCGAUSI1.sav", "BCGCHEI1.sav", "BCGCHLI1.sav", "BCGCNLI1.sav", "BCGCOTI1.sav", "BCGCZEI1.sav", "BCGDEUI1.sav", "BCGDNKI1.sav", "BCGHKGI1.sav", "BCGHRVI1.sav", "BCGKORI1.sav", "BCGLTUI1.sav", "BCGNLDI1.sav", "BCGNORI1.sav", "BCGPOLI1.sav", "BCGRUSI1.sav", "BCGSVKI1.sav", "BCGSVNI1.sav", "BCGTHAI1.sav", "BCGTURI1.sav", "BSGABAI1.sav", "BSGAUSI1.sav", "BSGCHEI1.sav", "BSGCHLI1.sav", "BSGCNLI1.sav", "BSGCOTI1.sav", "BSGCZEI1.sav", "BSGDEUI1.sav", "BSGDNKI1.sav", "BSGHKGI1.sav", "BSGHRVI1.sav", "BSGKORI1.sav", "BSGLTUI1.sav", "BSGNLDI1.sav", "BSGNORI1.sav", "BSGPOLI1.sav", "BSGRUSI1.sav", "BSGSVKI1.sav", "BSGSVNI1.sav", "BSGTHAI1.sav", "BSGTURI1.sav", "BTGAUSI1.sav", "BTGCHLI1.sav", "BTGCNLI1.sav", "BTGCOTI1.sav", "BTGCZEI1.sav", "BTGDEUI1.sav", "BTGDNKI1.sav", "BTGHKGI1.sav", "BTGHRVI1.sav", "BTGKORI1.sav", "BTGLTUI1.sav", "BTGNLDI1.sav", "BTGNORI1.sav", "BTGPOLI1.sav", "BTGRUSI1.sav", "BTGSVKI1.sav", "BTGSVNI1.sav", "BTGTHAI1.sav", "BTGTURI1.sav")
      )),
    ICILS_2018 = list(
      ICILS_2018_G8 = list("ICILS2018_IDB_SPSS/Data",
                           c("BCGCHLI2.sav", "BCGDEUI2.sav", "BCGDNKI2.sav", "BCGDNWI2.sav", "BCGFINI2.sav", "BCGFRAI2.sav", "BCGITAI2.sav", "BCGKAZI2.sav", "BCGKORI2.sav", "BCGLUXI2.sav", "BCGPRTI2.sav", "BCGRMOI2.sav", "BCGURYI2.sav", "BCGUSAI2.sav", "BSGCHLI2.sav", "BSGDEUI2.sav", "BSGDNKI2.sav", "BSGDNWI2.sav", "BSGFINI2.sav", "BSGFRAI2.sav", "BSGITAI2.sav", "BSGKAZI2.sav", "BSGKORI2.sav", "BSGLUXI2.sav", "BSGPRTI2.sav", "BSGRMOI2.sav", "BSGURYI2.sav", "BSGUSAI2.sav", "BTGCHLI2.sav", "BTGDEUI2.sav", "BTGDNKI2.sav", "BTGDNWI2.sav", "BTGFINI2.sav", "BTGFRAI2.sav", "BTGITAI2.sav", "BTGKAZI2.sav", "BTGKORI2.sav", "BTGLUXI2.sav", "BTGPRTI2.sav", "BTGRMOI2.sav", "BTGURYI2.sav", "BTGUSAI2.sav")
      )),
    ICILS_2023 = list(
      ICILS_2023_G8 = list("ICILS2023_IDB_SPSS/Data",
                           c("BCGAUTI3.sav", "BCGAZEI3.sav", "BCGBFLI3.sav", "BCGBIHI3.sav", "BCGCHLI3.sav", "BCGCYPI3.sav", "BCGCZEI3.sav", "BCGDEUI3.sav", "BCGDNKI3.sav", "BCGDNWI3.sav", "BCGESPI3.sav", "BCGFINI3.sav", "BCGFRAI3.sav", "BCGGRCI3.sav", "BCGHRVI3.sav", "BCGHUNI3.sav", "BCGITAI3.sav", "BCGKAZI3.sav", "BCGKORI3.sav", "BCGLUXI3.sav", "BCGLVAI3.sav", "BCGMLTI3.sav", "BCGNLDI3.sav", "BCGNORI3.sav", "BCGOMNI3.sav", "BCGPRTI3.sav", "BCGROUI3.sav", "BCGSRBI3.sav", "BCGSVKI3.sav", "BCGSVNI3.sav", "BCGSWEI3.sav", "BCGTWNI3.sav", "BCGURYI3.sav", "BCGUSAI3.sav", "BCGXKXI3.sav", "BSGAUTI3.sav", "BSGAZEI3.sav", "BSGBFLI3.sav", "BSGBIHI3.sav", "BSGCHLI3.sav", "BSGCYPI3.sav", "BSGCZEI3.sav", "BSGDEUI3.sav", "BSGDNKI3.sav", "BSGDNWI3.sav", "BSGESPI3.sav", "BSGFINI3.sav", "BSGFRAI3.sav", "BSGGRCI3.sav", "BSGHRVI3.sav", "BSGHUNI3.sav", "BSGITAI3.sav", "BSGKAZI3.sav", "BSGKORI3.sav", "BSGLUXI3.sav", "BSGLVAI3.sav", "BSGMLTI3.sav", "BSGNLDI3.sav", "BSGNORI3.sav", "BSGOMNI3.sav", "BSGPRTI3.sav", "BSGROUI3.sav", "BSGSRBI3.sav", "BSGSVKI3.sav", "BSGSVNI3.sav", "BSGSWEI3.sav", "BSGTWNI3.sav", "BSGURYI3.sav", "BSGUSAI3.sav", "BSGXKXI3.sav", "BTGAUTI3.sav", "BTGAZEI3.sav", "BTGBFLI3.sav", "BTGBIHI3.sav", "BTGCHLI3.sav", "BTGCYPI3.sav", "BTGCZEI3.sav", "BTGDEUI3.sav", "BTGDNKI3.sav", "BTGDNWI3.sav", "BTGESPI3.sav", "BTGFINI3.sav", "BTGFRAI3.sav", "BTGGRCI3.sav", "BTGHRVI3.sav", "BTGHUNI3.sav", "BTGITAI3.sav", "BTGKAZI3.sav", "BTGKORI3.sav", "BTGLUXI3.sav", "BTGLVAI3.sav", "BTGMLTI3.sav", "BTGNLDI3.sav", "BTGNORI3.sav", "BTGOMNI3.sav", "BTGPRTI3.sav", "BTGROUI3.sav", "BTGSRBI3.sav", "BTGSVKI3.sav", "BTGSVNI3.sav", "BTGSWEI3.sav", "BTGTWNI3.sav", "BTGURYI3.sav", "BTGUSAI3.sav", "BTGXKXI3.sav")
      )),
    PIRLS_2001 = list(
      PIRLS_2001_G4 = list("PIRLS2001_IDB_SPSS/Data",
                           c("ACGARGr1.sav", "ACGBGRr1.sav", "ACGBLZr1.sav", "ACGCANr1.sav", "ACGCOLr1.sav", "ACGCOTR1.sav", "ACGCQUR1.sav", "ACGCYPr1.sav", "ACGCZEr1.sav", "ACGDEUr1.sav", "ACGENGr1.sav", "ACGFRAr1.sav", "ACGGRCr1.sav", "ACGHKGr1.sav", "ACGHUNr1.sav", "ACGIRNr1.sav", "ACGISLr1.sav", "ACGISRr1.sav", "ACGITAr1.sav", "ACGKWTr1.sav", "ACGLTUr1.sav", "ACGLVAr1.sav", "ACGMARr1.sav", "ACGMDAr1.sav", "ACGMKDr1.sav", "ACGNLDr1.sav", "ACGNORr1.sav", "ACGNZLr1.sav", "ACGROMr1.sav", "ACGRUSr1.sav", "ACGSCOr1.sav", "ACGSE3r1.sav", "ACGSGPr1.sav", "ACGSVKr1.sav", "ACGSVNr1.sav", "ACGSWEr1.sav", "ACGTURr1.sav", "ACGUSAr1.sav", "ASAARGR1.sav", "ASABGRR1.sav", "ASABLZR1.sav", "ASACANr1.sav", "ASACOLR1.sav", "ASACOTR1.sav", "ASACQUR1.sav", "ASACYPR1.sav", "ASACZER1.sav", "ASADEUR1.sav", "ASAENGR1.sav", "ASAFRAR1.sav", "ASAGRCR1.sav", "ASAHKGR1.sav", "ASAHUNR1.sav", "ASAIRNR1.sav", "ASAISLR1.sav", "ASAISRR1.sav", "ASAITAR1.sav", "ASAKWTR1.sav", "ASALTUR1.sav", "ASALVAR1.sav", "ASAMARR1.sav", "ASAMDAR1.sav", "ASAMKDR1.sav", "ASANLDR1.sav", "ASANORR1.sav", "ASANZLR1.sav", "ASAROMR1.sav", "ASARUSR1.sav", "ASASCOR1.sav", "ASASE3R1.sav", "ASASGPR1.sav", "ASASVKR1.sav", "ASASVNR1.sav", "ASASWER1.sav", "ASATURR1.sav", "ASAUSAR1.sav", "ASGARGR1.sav", "ASGBGRR1.sav", "ASGBLZR1.sav", "ASGCANr1.sav", "ASGCOLR1.sav", "ASGCOTR1.sav", "ASGCQUR1.sav", "ASGCYPR1.sav", "ASGCZER1.sav", "ASGDEUR1.sav", "ASGENGR1.sav", "ASGFRAR1.sav", "ASGGRCR1.sav", "ASGHKGR1.sav", "ASGHUNR1.sav", "ASGIRNR1.sav", "ASGISLR1.sav", "ASGISRR1.sav", "ASGITAR1.sav", "ASGKWTR1.sav", "ASGLTUR1.sav", "ASGLVAR1.sav", "ASGMARR1.sav", "ASGMDAR1.sav", "ASGMKDR1.sav", "ASGNLDR1.sav", "ASGNORR1.sav", "ASGNZLR1.sav", "ASGROMR1.sav", "ASGRUSR1.sav", "ASGSCOR1.sav", "ASGSE3R1.sav", "ASGSGPR1.sav", "ASGSVKR1.sav", "ASGSVNR1.sav", "ASGSWER1.sav", "ASGTURR1.sav", "ASGUSAR1.sav", "ASHARGr1.sav", "ASHBGRr1.sav", "ASHBLZr1.sav", "ASHCANr1.sav", "ASHCOLr1.sav", "ASHCOTR1.sav", "ASHCQUR1.sav", "ASHCYPr1.sav", "ASHCZEr1.sav", "ASHDEUr1.sav", "ASHENGr1.sav", "ASHFRAr1.sav", "ASHGRCr1.sav", "ASHHKGr1.sav", "ASHHUNr1.sav", "ASHIRNr1.sav", "ASHISLr1.sav", "ASHISRr1.sav", "ASHITAr1.sav", "ASHKWTr1.sav", "ASHLTUr1.sav", "ASHLVAr1.sav", "ASHMARr1.sav", "ASHMDAr1.sav", "ASHMKDr1.sav", "ASHNLDr1.sav", "ASHNORr1.sav", "ASHNZLr1.sav", "ASHROMr1.sav", "ASHRUSr1.sav", "ASHSCOr1.sav", "ASHSE3r1.sav", "ASHSGPr1.sav", "ASHSVKr1.sav", "ASHSVNr1.sav", "ASHSWEr1.sav", "ASHTURr1.sav", "ASHUSAr1.sav", "ASRARGr1.sav", "ASRBGRr1.sav", "ASRBLZr1.sav", "ASRCANr1.sav", "ASRCOLr1.sav", "ASRCOTR1.sav", "ASRCQUR1.sav", "ASRCYPr1.sav", "ASRCZEr1.sav", "ASRDEUr1.sav", "ASRENGr1.sav", "ASRFRAr1.sav", "ASRGRCr1.sav", "ASRHKGr1.sav", "ASRHUNr1.sav", "ASRIRNr1.sav", "ASRISLr1.sav", "ASRISRr1.sav", "ASRITAr1.sav", "ASRKWTr1.sav", "ASRLTUr1.sav", "ASRLVAr1.sav", "ASRMARr1.sav", "ASRMDAr1.sav", "ASRMKDr1.sav", "ASRNLDr1.sav", "ASRNORr1.sav", "ASRNZLr1.sav", "ASRROMr1.sav", "ASRRUSr1.sav", "ASRSCOr1.sav", "ASRSE3r1.sav", "ASRSGPr1.sav", "ASRSVKr1.sav", "ASRSVNr1.sav", "ASRSWEr1.sav", "ASRTURr1.sav", "ASRUSAr1.sav", "ASTARGR1.sav", "ASTBGRR1.sav", "ASTBLZR1.sav", "ASTCANr1.sav", "ASTCOLR1.sav", "ASTCOTR1.sav", "ASTCQUR1.sav", "ASTCYPR1.sav", "ASTCZER1.sav", "ASTDEUR1.sav", "ASTENGR1.sav", "ASTFRAR1.sav", "ASTGRCR1.sav", "ASTHKGR1.sav", "ASTHUNR1.sav", "ASTIRNR1.sav", "ASTISLR1.sav", "ASTISRR1.sav", "ASTITAR1.sav", "ASTKWTR1.sav", "ASTLTUR1.sav", "ASTLVAR1.sav", "ASTMARR1.sav", "ASTMDAR1.sav", "ASTMKDR1.sav", "ASTNLDR1.sav", "ASTNORR1.sav", "ASTNZLR1.sav", "ASTROMR1.sav", "ASTRUSR1.sav", "ASTSCOR1.sav", "ASTSE3R1.sav", "ASTSGPR1.sav", "ASTSVKR1.sav", "ASTSVNR1.sav", "ASTSWER1.sav", "ASTTURR1.sav", "ASTUSAR1.sav", "ATGARGr1.sav", "ATGBGRr1.sav", "ATGBLZr1.sav", "ATGCANr1.sav", "ATGCOLr1.sav", "ATGCOTR1.sav", "ATGCQUR1.sav", "ATGCYPr1.sav", "ATGCZEr1.sav", "ATGDEUr1.sav", "ATGENGr1.sav", "ATGFRAr1.sav", "ATGGRCr1.sav", "ATGHKGr1.sav", "ATGHUNr1.sav", "ATGIRNr1.sav", "ATGISLr1.sav", "ATGISRr1.sav", "ATGITAr1.sav", "ATGKWTr1.sav", "ATGLTUr1.sav", "ATGLVAr1.sav", "ATGMARr1.sav", "ATGMDAr1.sav", "ATGMKDr1.sav", "ATGNLDr1.sav", "ATGNORr1.sav", "ATGNZLr1.sav", "ATGROMr1.sav", "ATGRUSr1.sav", "ATGSCOr1.sav", "ATGSE3r1.sav", "ATGSGPr1.sav", "ATGSVKr1.sav", "ATGSVNr1.sav", "ATGSWEr1.sav", "ATGTURr1.sav", "ATGUSAr1.sav")
      )),
    PIRLS_2006 = list(
      PIRLS_2006_G4 = list("PIRLS2006_IDB_SPSS/Data",
                           c("acgautr2.sav", "acgbflr2.sav", "acgbfrr2.sav", "acgbgrr2.sav", "acgcabr2.sav", "acgcbcr2.sav", "acgcnsr2.sav", "acgcotr2.sav", "acgcqur2.sav", "acgdeur2.sav", "acgdnkr2.sav", "acgengr2.sav", "acgespr2.sav", "acgfrar2.sav", "acggeor2.sav", "acghkgr2.sav", "acghunr2.sav", "acgidnr2.sav", "acgirnr2.sav", "acgis5r2.sav", "acgislr2.sav", "acgisrr2.sav", "acgitar2.sav", "acgkwtr2.sav", "acgltur2.sav", "acgluxr2.sav", "acglvar2.sav", "acgmarr2.sav", "acgmdar2.sav", "acgmkdr2.sav", "acgnldr2.sav", "acgno5r2.sav", "acgnorr2.sav", "acgnzlr2.sav", "acgpolr2.sav", "acgqatr2.sav", "acgromr2.sav", "acgrusr2.sav", "acgscor2.sav", "acgsgpr2.sav", "acgsvkr2.sav", "acgsvnr2.sav", "acgswer2.sav", "acgttor2.sav", "acgtwnr2.sav", "acgusar2.sav", "acgzafr2.sav", "asaautr2.sav", "asabflr2.sav", "asabfrr2.sav", "asabgrr2.sav", "asacabr2.sav", "asacbcr2.sav", "asacnsr2.sav", "asacotr2.sav", "asacqur2.sav", "asadeur2.sav", "asadnkr2.sav", "asaengr2.sav", "asaespr2.sav", "asafrar2.sav", "asageor2.sav", "asahkgr2.sav", "asahunr2.sav", "asaidnr2.sav", "asairnr2.sav", "asais5r2.sav", "asaislr2.sav", "asaisrr2.sav", "asaitar2.sav", "asakwtr2.sav", "asaltur2.sav", "asaluxr2.sav", "asalvar2.sav", "asamarr2.sav", "asamdar2.sav", "asamkdr2.sav", "asanldr2.sav", "asano5r2.sav", "asanorr2.sav", "asanzlr2.sav", "asapolr2.sav", "asaqatr2.sav", "asaromr2.sav", "asarusr2.sav", "asascor2.sav", "asasgpr2.sav", "asasvkr2.sav", "asasvnr2.sav", "asaswer2.sav", "asattor2.sav", "asatwnr2.sav", "asausar2.sav", "asazafr2.sav", "asgautr2.sav", "asgbflr2.sav", "asgbfrr2.sav", "asgbgrr2.sav", "asgcabr2.sav", "asgcbcr2.sav", "asgcnsr2.sav", "asgcotr2.sav", "asgcqur2.sav", "asgdeur2.sav", "asgdnkr2.sav", "asgengr2.sav", "asgespr2.sav", "asgfrar2.sav", "asggeor2.sav", "asghkgr2.sav", "asghunr2.sav", "asgidnr2.sav", "asgirnr2.sav", "asgis5r2.sav", "asgislr2.sav", "asgisrr2.sav", "asgitar2.sav", "asgkwtr2.sav", "asgltur2.sav", "asgluxr2.sav", "asglvar2.sav", "asgmarr2.sav", "asgmdar2.sav", "asgmkdr2.sav", "asgnldr2.sav", "asgno5r2.sav", "asgnorr2.sav", "asgnzlr2.sav", "asgpolr2.sav", "asgqatr2.sav", "asgromr2.sav", "asgrusr2.sav", "asgscor2.sav", "asgsgpr2.sav", "asgsvkr2.sav", "asgsvnr2.sav", "asgswer2.sav", "asgttor2.sav", "asgtwnr2.sav", "asgusar2.sav", "asgzafr2.sav", "ashautr2.sav", "ashbflr2.sav", "ashbfrr2.sav", "ashbgrr2.sav", "ashcabr2.sav", "ashcbcr2.sav", "ashcnsr2.sav", "ashcotr2.sav", "ashcqur2.sav", "ashdeur2.sav", "ashdnkr2.sav", "ashengr2.sav", "ashespr2.sav", "ashfrar2.sav", "ashgeor2.sav", "ashhkgr2.sav", "ashhunr2.sav", "ashidnr2.sav", "ashirnr2.sav", "ashis5r2.sav", "ashislr2.sav", "ashisrr2.sav", "ashitar2.sav", "ashkwtr2.sav", "ashltur2.sav", "ashluxr2.sav", "ashlvar2.sav", "ashmarr2.sav", "ashmdar2.sav", "ashmkdr2.sav", "ashnldr2.sav", "ashno5r2.sav", "ashnorr2.sav", "ashnzlr2.sav", "ashpolr2.sav", "ashqatr2.sav", "ashromr2.sav", "ashrusr2.sav", "ashscor2.sav", "ashsgpr2.sav", "ashsvkr2.sav", "ashsvnr2.sav", "ashswer2.sav", "ashttor2.sav", "ashtwnr2.sav", "ashusar2.sav", "ashzafr2.sav", "asrautr2.sav", "asrbflr2.sav", "asrbfrr2.sav", "asrbgrr2.sav", "asrcabr2.sav", "asrcbcr2.sav", "asrcnsr2.sav", "asrcotr2.sav", "asrcqur2.sav", "asrdeur2.sav", "asrdnkr2.sav", "asrengr2.sav", "asrespr2.sav", "asrfrar2.sav", "asrgeor2.sav", "asrhkgr2.sav", "asrhunr2.sav", "asridnr2.sav", "asrirnr2.sav", "asris5r2.sav", "asrislr2.sav", "asrisrr2.sav", "asritar2.sav", "asrkwtr2.sav", "asrltur2.sav", "asrluxr2.sav", "asrlvar2.sav", "asrmarr2.sav", "asrmdar2.sav", "asrmkdr2.sav", "asrnldr2.sav", "asrno5r2.sav", "asrnorr2.sav", "asrnzlr2.sav", "asrpolr2.sav", "asrqatr2.sav", "asrromr2.sav", "asrrusr2.sav", "asrscor2.sav", "asrsgpr2.sav", "asrsvkr2.sav", "asrsvnr2.sav", "asrswer2.sav", "asrttor2.sav", "asrtwnr2.sav", "asrusar2.sav", "asrzafr2.sav", "astautr2.sav", "astbflr2.sav", "astbfrr2.sav", "astbgrr2.sav", "astcabr2.sav", "astcbcr2.sav", "astcnsr2.sav", "astcotr2.sav", "astcqur2.sav", "astdeur2.sav", "astdnkr2.sav", "astengr2.sav", "astespr2.sav", "astfrar2.sav", "astgeor2.sav", "asthkgr2.sav", "asthunr2.sav", "astidnr2.sav", "astirnr2.sav", "astis5r2.sav", "astislr2.sav", "astisrr2.sav", "astitar2.sav", "astkwtr2.sav", "astltur2.sav", "astluxr2.sav", "astlvar2.sav", "astmarr2.sav", "astmdar2.sav", "astmkdr2.sav", "astnldr2.sav", "astno5r2.sav", "astnorr2.sav", "astnzlr2.sav", "astpolr2.sav", "astqatr2.sav", "astromr2.sav", "astrusr2.sav", "astscor2.sav", "astsgpr2.sav", "astsvkr2.sav", "astsvnr2.sav", "astswer2.sav", "astttor2.sav", "asttwnr2.sav", "astusar2.sav", "astzafr2.sav", "atgautr2.sav", "atgbflr2.sav", "atgbfrr2.sav", "atgbgrr2.sav", "atgcabr2.sav", "atgcbcr2.sav", "atgcnsr2.sav", "atgcotr2.sav", "atgcqur2.sav", "atgdeur2.sav", "atgdnkr2.sav", "atgengr2.sav", "atgespr2.sav", "atgfrar2.sav", "atggeor2.sav", "atghkgr2.sav", "atghunr2.sav", "atgidnr2.sav", "atgirnr2.sav", "atgis5r2.sav", "atgislr2.sav", "atgisrr2.sav", "atgitar2.sav", "atgkwtr2.sav", "atgltur2.sav", "atgluxr2.sav", "atglvar2.sav", "atgmarr2.sav", "atgmdar2.sav", "atgmkdr2.sav", "atgnldr2.sav", "atgno5r2.sav", "atgnorr2.sav", "atgnzlr2.sav", "atgpolr2.sav", "atgqatr2.sav", "atgromr2.sav", "atgrusr2.sav", "atgscor2.sav", "atgsgpr2.sav", "atgsvkr2.sav", "atgsvnr2.sav", "atgswer2.sav", "atgttor2.sav", "atgtwnr2.sav", "atgusar2.sav", "atgzafr2.sav")
      )),
    PIRLS_2011 = list(
      PIRLS_2011_G4 = list("PIRLS2011_IDB_SPSS/Data",
                           c("acgaadr3.sav", "acgadur3.sav", "acgarer3.sav", "acgausr3.sav", "acgautr3.sav", "acgazer3.sav", "acgbfrr3.sav", "acgbgrr3.sav", "acgbwar3.sav", "acgcabr3.sav", "acgcanr3.sav", "acgcolr3.sav", "acgcotr3.sav", "acgcqur3.sav", "acgczer3.sav", "acgdeur3.sav", "acgdnkr3.sav", "acgeanr3.sav", "acgengr3.sav", "acgespr3.sav", "acgfinr3.sav", "acgfrar3.sav", "acggeor3.sav", "acghkgr3.sav", "acghndr3.sav", "acghrvr3.sav", "acghunr3.sav", "acgidnr3.sav", "acgirlr3.sav", "acgirnr3.sav", "acgisrr3.sav", "acgitar3.sav", "acgkwtr3.sav", "acgltur3.sav", "acgma6r3.sav", "acgmarr3.sav", "acgmlnr3.sav", "acgmltr3.sav", "acgnirr3.sav", "acgnldr3.sav", "acgnorr3.sav", "acgnzlr3.sav", "acgomnr3.sav", "acgpolr3.sav", "acgprtr3.sav", "acgqatr3.sav", "acgromr3.sav", "acgrusr3.sav", "acgsaur3.sav", "acgsgpr3.sav", "acgsvkr3.sav", "acgsvnr3.sav", "acgswer3.sav", "acgttor3.sav", "acgtwnr3.sav", "acgusar3.sav", "acgzafr3.sav", "asaaadr3.sav", "asaadur3.sav", "asaarer3.sav", "asaausr3.sav", "asaautr3.sav", "asaazer3.sav", "asabfrr3.sav", "asabgrr3.sav", "asabwar3.sav", "asacabr3.sav", "asacanr3.sav", "asacolr3.sav", "asacotr3.sav", "asacqur3.sav", "asaczer3.sav", "asadeur3.sav", "asadnkr3.sav", "asaeanr3.sav", "asaengr3.sav", "asaespr3.sav", "asafinr3.sav", "asafrar3.sav", "asageor3.sav", "asahkgr3.sav", "asahndr3.sav", "asahrvr3.sav", "asahunr3.sav", "asaidnr3.sav", "asairlr3.sav", "asairnr3.sav", "asaisrr3.sav", "asaitar3.sav", "asakwtr3.sav", "asaltur3.sav", "asama6r3.sav", "asamarr3.sav", "asamlnr3.sav", "asamltr3.sav", "asanirr3.sav", "asanldr3.sav", "asanorr3.sav", "asanzlr3.sav", "asaomnr3.sav", "asapolr3.sav", "asaprtr3.sav", "asaqatr3.sav", "asaromr3.sav", "asarusr3.sav", "asasaur3.sav", "asasgpr3.sav", "asasvkr3.sav", "asasvnr3.sav", "asaswer3.sav", "asattor3.sav", "asatwnr3.sav", "asausar3.sav", "asazafr3.sav", "asgaadr3.sav", "asgadur3.sav", "asgarer3.sav", "asgausr3.sav", "asgautr3.sav", "asgazer3.sav", "asgbfrr3.sav", "asgbgrr3.sav", "asgbwar3.sav", "asgcabr3.sav", "asgcanr3.sav", "asgcolr3.sav", "asgcotr3.sav", "asgcqur3.sav", "asgczer3.sav", "asgdeur3.sav", "asgdnkr3.sav", "asgeanr3.sav", "asgengr3.sav", "asgespr3.sav", "asgfinr3.sav", "asgfrar3.sav", "asggeor3.sav", "asghkgr3.sav", "asghndr3.sav", "asghrvr3.sav", "asghunr3.sav", "asgidnr3.sav", "asgirlr3.sav", "asgirnr3.sav", "asgisrr3.sav", "asgitar3.sav", "asgkwtr3.sav", "asgltur3.sav", "asgma6r3.sav", "asgmarr3.sav", "asgmlnr3.sav", "asgmltr3.sav", "asgnirr3.sav", "asgnldr3.sav", "asgnorr3.sav", "asgnzlr3.sav", "asgomnr3.sav", "asgpolr3.sav", "asgprtr3.sav", "asgqatr3.sav", "asgromr3.sav", "asgrusr3.sav", "asgsaur3.sav", "asgsgpr3.sav", "asgsvkr3.sav", "asgsvnr3.sav", "asgswer3.sav", "asgttor3.sav", "asgtwnr3.sav", "asgusar3.sav", "asgzafr3.sav", "ashaadr3.sav", "ashadur3.sav", "asharer3.sav", "ashausr3.sav", "ashautr3.sav", "ashazer3.sav", "ashbfrr3.sav", "ashbgrr3.sav", "ashbwar3.sav", "ashcabr3.sav", "ashcanr3.sav", "ashcolr3.sav", "ashcotr3.sav", "ashcqur3.sav", "ashczer3.sav", "ashdeur3.sav", "ashdnkr3.sav", "asheanr3.sav", "ashengr3.sav", "ashespr3.sav", "ashfinr3.sav", "ashfrar3.sav", "ashgeor3.sav", "ashhkgr3.sav", "ashhndr3.sav", "ashhrvr3.sav", "ashhunr3.sav", "ashidnr3.sav", "ashirlr3.sav", "ashirnr3.sav", "ashisrr3.sav", "ashitar3.sav", "ashkwtr3.sav", "ashltur3.sav", "ashma6r3.sav", "ashmarr3.sav", "ashmlnr3.sav", "ashmltr3.sav", "ashnirr3.sav", "ashnldr3.sav", "ashnorr3.sav", "ashnzlr3.sav", "ashomnr3.sav", "ashpolr3.sav", "ashprtr3.sav", "ashqatr3.sav", "ashromr3.sav", "ashrusr3.sav", "ashsaur3.sav", "ashsgpr3.sav", "ashsvkr3.sav", "ashsvnr3.sav", "ashswer3.sav", "ashttor3.sav", "ashtwnr3.sav", "ashusar3.sav", "ashzafr3.sav", "asraadr3.sav", "asradur3.sav", "asrarer3.sav", "asrausr3.sav", "asrautr3.sav", "asrazer3.sav", "asrbfrr3.sav", "asrbgrr3.sav", "asrbwar3.sav", "asrcabr3.sav", "asrcanr3.sav", "asrcolr3.sav", "asrcotr3.sav", "asrcqur3.sav", "asrczer3.sav", "asrdeur3.sav", "asrdnkr3.sav", "asreanr3.sav", "asrengr3.sav", "asrespr3.sav", "asrfinr3.sav", "asrfrar3.sav", "asrgeor3.sav", "asrhkgr3.sav", "asrhndr3.sav", "asrhrvr3.sav", "asrhunr3.sav", "asridnr3.sav", "asrirlr3.sav", "asrirnr3.sav", "asrisrr3.sav", "asritar3.sav", "asrkwtr3.sav", "asrltur3.sav", "asrma6r3.sav", "asrmarr3.sav", "asrmlnr3.sav", "asrmltr3.sav", "asrnirr3.sav", "asrnldr3.sav", "asrnorr3.sav", "asrnzlr3.sav", "asromnr3.sav", "asrpolr3.sav", "asrprtr3.sav", "asrqatr3.sav", "asrromr3.sav", "asrrusr3.sav", "asrsaur3.sav", "asrsgpr3.sav", "asrsvkr3.sav", "asrsvnr3.sav", "asrswer3.sav", "asrttor3.sav", "asrtwnr3.sav", "asrusar3.sav", "asrzafr3.sav", "astaadr3.sav", "astadur3.sav", "astarer3.sav", "astausr3.sav", "astautr3.sav", "astazer3.sav", "astbfrr3.sav", "astbgrr3.sav", "astbwar3.sav", "astcabr3.sav", "astcanr3.sav", "astcolr3.sav", "astcotr3.sav", "astcqur3.sav", "astczer3.sav", "astdeur3.sav", "astdnkr3.sav", "asteanr3.sav", "astengr3.sav", "astespr3.sav", "astfinr3.sav", "astfrar3.sav", "astgeor3.sav", "asthkgr3.sav", "asthndr3.sav", "asthrvr3.sav", "asthunr3.sav", "astidnr3.sav", "astirlr3.sav", "astirnr3.sav", "astisrr3.sav", "astitar3.sav", "astkwtr3.sav", "astltur3.sav", "astma6r3.sav", "astmarr3.sav", "astmlnr3.sav", "astmltr3.sav", "astnirr3.sav", "astnldr3.sav", "astnorr3.sav", "astnzlr3.sav", "astomnr3.sav", "astpolr3.sav", "astprtr3.sav", "astqatr3.sav", "astromr3.sav", "astrusr3.sav", "astsaur3.sav", "astsgpr3.sav", "astsvkr3.sav", "astsvnr3.sav", "astswer3.sav", "astttor3.sav", "asttwnr3.sav", "astusar3.sav", "astzafr3.sav", "atgaadr3.sav", "atgadur3.sav", "atgarer3.sav", "atgausr3.sav", "atgautr3.sav", "atgazer3.sav", "atgbfrr3.sav", "atgbgrr3.sav", "atgbwar3.sav", "atgcabr3.sav", "atgcanr3.sav", "atgcolr3.sav", "atgcotr3.sav", "atgcqur3.sav", "atgczer3.sav", "atgdeur3.sav", "atgdnkr3.sav", "atgeanr3.sav", "atgengr3.sav", "atgespr3.sav", "atgfinr3.sav", "atgfrar3.sav", "atggeor3.sav", "atghkgr3.sav", "atghndr3.sav", "atghrvr3.sav", "atghunr3.sav", "atgidnr3.sav", "atgirlr3.sav", "atgirnr3.sav", "atgisrr3.sav", "atgitar3.sav", "atgkwtr3.sav", "atgltur3.sav", "atgma6r3.sav", "atgmarr3.sav", "atgmlnr3.sav", "atgmltr3.sav", "atgnirr3.sav", "atgnldr3.sav", "atgnorr3.sav", "atgnzlr3.sav", "atgomnr3.sav", "atgpolr3.sav", "atgprtr3.sav", "atgqatr3.sav", "atgromr3.sav", "atgrusr3.sav", "atgsaur3.sav", "atgsgpr3.sav", "atgsvkr3.sav", "atgsvnr3.sav", "atgswer3.sav", "atgttor3.sav", "atgtwnr3.sav", "atgusar3.sav", "atgzafr3.sav")
      )),
    PIRLS_2016 = list(
      PIRLS_2016_G4 = list("PIRLS2016_IDB_SPSS/Data",
                           c("ACGAADR4.sav", "ACGABAR4.sav", "ACGADUR4.sav", "ACGARER4.sav", "ACGAUSR4.sav", "ACGAUTR4.sav", "ACGAZER4.sav", "ACGBFLR4.sav", "ACGBFRR4.sav", "ACGBGRR4.sav", "ACGBHRR4.sav", "ACGCANR4.sav", "ACGCHLR4.sav", "ACGCOTR4.sav", "ACGCQUR4.sav", "ACGCZER4.sav", "ACGDEUR4.sav", "ACGDNKR4.sav", "ACGEANR4.sav", "ACGEMAR4.sav", "ACGENGR4.sav", "ACGESPR4.sav", "ACGFINR4.sav", "ACGFRAR4.sav", "ACGGEOR4.sav", "ACGHKGR4.sav", "ACGHUNR4.sav", "ACGIRLR4.sav", "ACGIRNR4.sav", "ACGISRR4.sav", "ACGITAR4.sav", "ACGKAZR4.sav", "ACGLTUR4.sav", "ACGLVAR4.sav", "ACGMACR4.sav", "ACGMARR4.sav", "ACGMLTR4.sav", "ACGNIRR4.sav", "ACGNLDR4.sav", "ACGNO4R4.sav", "ACGNORR4.sav", "ACGNZLR4.sav", "ACGOMNR4.sav", "ACGPOLR4.sav", "ACGPRTR4.sav", "ACGQATR4.sav", "ACGRMOR4.sav", "ACGRUSR4.sav", "ACGSAUR4.sav", "ACGSGPR4.sav", "ACGSVKR4.sav", "ACGSVNR4.sav", "ACGSWER4.sav", "ACGTTOR4.sav", "ACGTWNR4.sav", "ACGUSAR4.sav", "ACGZA5R4.sav", "ASAAADR4.sav", "ASAABAR4.sav", "ASAADUR4.sav", "ASAARER4.sav", "ASAAUSR4.sav", "ASAAUTR4.sav", "ASAAZER4.sav", "ASABFLR4.sav", "ASABFRR4.sav", "ASABGRR4.sav", "ASABHRR4.sav", "ASACANR4.sav", "ASACHLR4.sav", "ASACOTR4.sav", "ASACQUR4.sav", "ASACZER4.sav", "ASADEUR4.sav", "ASADNKR4.sav", "ASAEANR4.sav", "ASAEMAR4.sav", "ASAENGR4.sav", "ASAESPR4.sav", "ASAFINR4.sav", "ASAFRAR4.sav", "ASAGEOR4.sav", "ASAHKGR4.sav", "ASAHUNR4.sav", "ASAIRLR4.sav", "ASAIRNR4.sav", "ASAISRR4.sav", "ASAITAR4.sav", "ASAKAZR4.sav", "ASALTUR4.sav", "ASALVAR4.sav", "ASAMACR4.sav", "ASAMARR4.sav", "ASAMLTR4.sav", "ASANIRR4.sav", "ASANLDR4.sav", "ASANO4R4.sav", "ASANORR4.sav", "ASANZLR4.sav", "ASAOMNR4.sav", "ASAPOLR4.sav", "ASAPRTR4.sav", "ASAQATR4.sav", "ASARMOR4.sav", "ASARUSR4.sav", "ASASAUR4.sav", "ASASGPR4.sav", "ASASVKR4.sav", "ASASVNR4.sav", "ASASWER4.sav", "ASATTOR4.sav", "ASATWNR4.sav", "ASAUSAR4.sav", "ASAZA5R4.sav", "ASGAADR4.sav", "ASGABAR4.sav", "ASGADUR4.sav", "ASGARER4.sav", "ASGAUSR4.sav", "ASGAUTR4.sav", "ASGAZER4.sav", "ASGBFLR4.sav", "ASGBFRR4.sav", "ASGBGRR4.sav", "ASGBHRR4.sav", "ASGCANR4.sav", "ASGCHLR4.sav", "ASGCOTR4.sav", "ASGCQUR4.sav", "ASGCZER4.sav", "ASGDEUR4.sav", "ASGDNKR4.sav", "ASGEANR4.sav", "ASGEMAR4.sav", "ASGENGR4.sav", "ASGESPR4.sav", "ASGFINR4.sav", "ASGFRAR4.sav", "ASGGEOR4.sav", "ASGHKGR4.sav", "ASGHUNR4.sav", "ASGIRLR4.sav", "ASGIRNR4.sav", "ASGISRR4.sav", "ASGITAR4.sav", "ASGKAZR4.sav", "ASGLTUR4.sav", "ASGLVAR4.sav", "ASGMACR4.sav", "ASGMARR4.sav", "ASGMLTR4.sav", "ASGNIRR4.sav", "ASGNLDR4.sav", "ASGNO4R4.sav", "ASGNORR4.sav", "ASGNZLR4.sav", "ASGOMNR4.sav", "ASGPOLR4.sav", "ASGPRTR4.sav", "ASGQATR4.sav", "ASGRMOR4.sav", "ASGRUSR4.sav", "ASGSAUR4.sav", "ASGSGPR4.sav", "ASGSVKR4.sav", "ASGSVNR4.sav", "ASGSWER4.sav", "ASGTTOR4.sav", "ASGTWNR4.sav", "ASGUSAR4.sav", "ASGZA5R4.sav", "ASHAADR4.sav", "ASHABAR4.sav", "ASHADUR4.sav", "ASHARER4.sav", "ASHAUSR4.sav", "ASHAUTR4.sav", "ASHAZER4.sav", "ASHBFLR4.sav", "ASHBFRR4.sav", "ASHBGRR4.sav", "ASHBHRR4.sav", "ASHCANR4.sav", "ASHCHLR4.sav", "ASHCOTR4.sav", "ASHCQUR4.sav", "ASHCZER4.sav", "ASHDEUR4.sav", "ASHDNKR4.sav", "ASHEANR4.sav", "ASHEMAR4.sav", "ASHENGR4.sav", "ASHESPR4.sav", "ASHFINR4.sav", "ASHFRAR4.sav", "ASHGEOR4.sav", "ASHHKGR4.sav", "ASHHUNR4.sav", "ASHIRLR4.sav", "ASHIRNR4.sav", "ASHISRR4.sav", "ASHITAR4.sav", "ASHKAZR4.sav", "ASHLTUR4.sav", "ASHLVAR4.sav", "ASHMACR4.sav", "ASHMARR4.sav", "ASHMLTR4.sav", "ASHNIRR4.sav", "ASHNLDR4.sav", "ASHNO4R4.sav", "ASHNORR4.sav", "ASHNZLR4.sav", "ASHOMNR4.sav", "ASHPOLR4.sav", "ASHPRTR4.sav", "ASHQATR4.sav", "ASHRMOR4.sav", "ASHRUSR4.sav", "ASHSAUR4.sav", "ASHSGPR4.sav", "ASHSVKR4.sav", "ASHSVNR4.sav", "ASHSWER4.sav", "ASHTTOR4.sav", "ASHTWNR4.sav", "ASHUSAR4.sav", "ASHZA5R4.sav", "ASRAADR4.sav", "ASRABAR4.sav", "ASRADUR4.sav", "ASRARER4.sav", "ASRAUSR4.sav", "ASRAUTR4.sav", "ASRAZER4.sav", "ASRBFLR4.sav", "ASRBFRR4.sav", "ASRBGRR4.sav", "ASRBHRR4.sav", "ASRCANR4.sav", "ASRCHLR4.sav", "ASRCOTR4.sav", "ASRCQUR4.sav", "ASRCZER4.sav", "ASRDEUR4.sav", "ASRDNKR4.sav", "ASREANR4.sav", "ASREMAR4.sav", "ASRENGR4.sav", "ASRESPR4.sav", "ASRFINR4.sav", "ASRFRAR4.sav", "ASRGEOR4.sav", "ASRHKGR4.sav", "ASRHUNR4.sav", "ASRIRLR4.sav", "ASRIRNR4.sav", "ASRISRR4.sav", "ASRITAR4.sav", "ASRKAZR4.sav", "ASRLTUR4.sav", "ASRLVAR4.sav", "ASRMACR4.sav", "ASRMARR4.sav", "ASRMLTR4.sav", "ASRNIRR4.sav", "ASRNLDR4.sav", "ASRNO4R4.sav", "ASRNORR4.sav", "ASRNZLR4.sav", "ASROMNR4.sav", "ASRPOLR4.sav", "ASRPRTR4.sav", "ASRQATR4.sav", "ASRRMOR4.sav", "ASRRUSR4.sav", "ASRSAUR4.sav", "ASRSGPR4.sav", "ASRSVKR4.sav", "ASRSVNR4.sav", "ASRSWER4.sav", "ASRTTOR4.sav", "ASRTWNR4.sav", "ASRUSAR4.sav", "ASRZA5R4.sav", "ASTAADR4.sav", "ASTABAR4.sav", "ASTADUR4.sav", "ASTARER4.sav", "ASTAUSR4.sav", "ASTAUTR4.sav", "ASTAZER4.sav", "ASTBFLR4.sav", "ASTBFRR4.sav", "ASTBGRR4.sav", "ASTBHRR4.sav", "ASTCANR4.sav", "ASTCHLR4.sav", "ASTCOTR4.sav", "ASTCQUR4.sav", "ASTCZER4.sav", "ASTDEUR4.sav", "ASTDNKR4.sav", "ASTEANR4.sav", "ASTEMAR4.sav", "ASTENGR4.sav", "ASTESPR4.sav", "ASTFINR4.sav", "ASTFRAR4.sav", "ASTGEOR4.sav", "ASTHKGR4.sav", "ASTHUNR4.sav", "ASTIRLR4.sav", "ASTIRNR4.sav", "ASTISRR4.sav", "ASTITAR4.sav", "ASTKAZR4.sav", "ASTLTUR4.sav", "ASTLVAR4.sav", "ASTMACR4.sav", "ASTMARR4.sav", "ASTMLTR4.sav", "ASTNIRR4.sav", "ASTNLDR4.sav", "ASTNO4R4.sav", "ASTNORR4.sav", "ASTNZLR4.sav", "ASTOMNR4.sav", "ASTPOLR4.sav", "ASTPRTR4.sav", "ASTQATR4.sav", "ASTRMOR4.sav", "ASTRUSR4.sav", "ASTSAUR4.sav", "ASTSGPR4.sav", "ASTSVKR4.sav", "ASTSVNR4.sav", "ASTSWER4.sav", "ASTTTOR4.sav", "ASTTWNR4.sav", "ASTUSAR4.sav", "ASTZA5R4.sav", "ATGAADR4.sav", "ATGABAR4.sav", "ATGADUR4.sav", "ATGARER4.sav", "ATGAUSR4.sav", "ATGAUTR4.sav", "ATGAZER4.sav", "ATGBFLR4.sav", "ATGBFRR4.sav", "ATGBGRR4.sav", "ATGBHRR4.sav", "ATGCANR4.sav", "ATGCHLR4.sav", "ATGCOTR4.sav", "ATGCQUR4.sav", "ATGCZER4.sav", "ATGDEUR4.sav", "ATGDNKR4.sav", "ATGEANR4.sav", "ATGEMAR4.sav", "ATGENGR4.sav", "ATGESPR4.sav", "ATGFINR4.sav", "ATGFRAR4.sav", "ATGGEOR4.sav", "ATGHKGR4.sav", "ATGHUNR4.sav", "ATGIRLR4.sav", "ATGIRNR4.sav", "ATGISRR4.sav", "ATGITAR4.sav", "ATGKAZR4.sav", "ATGLTUR4.sav", "ATGLVAR4.sav", "ATGMACR4.sav", "ATGMARR4.sav", "ATGMLTR4.sav", "ATGNIRR4.sav", "ATGNLDR4.sav", "ATGNO4R4.sav", "ATGNORR4.sav", "ATGNZLR4.sav", "ATGOMNR4.sav", "ATGPOLR4.sav", "ATGPRTR4.sav", "ATGQATR4.sav", "ATGRMOR4.sav", "ATGRUSR4.sav", "ATGSAUR4.sav", "ATGSGPR4.sav", "ATGSVKR4.sav", "ATGSVNR4.sav", "ATGSWER4.sav", "ATGTTOR4.sav", "ATGTWNR4.sav", "ATGUSAR4.sav", "ATGZA5R4.sav")
      )),
    PIRLS_2021 = list(
      PIRLS_2021_G4 = list("PIRLS2021_IDB_SPSS/3_International Database/1_SPSS Data",
                           c("ACGAADR5.sav", "ACGADUR5.sav", "ACGALBR5.sav", "ACGARER5.sav", "ACGAUSR5.sav", "ACGAUTR5.sav", "ACGAZER5.sav", "ACGBFLR5.sav", "ACGBFRR5.sav", "ACGBGRR5.sav", "ACGBHRR5.sav", "ACGBRAR5.sav", "ACGCABR5.sav", "ACGCBCR5.sav", "ACGCNLR5.sav", "ACGCQUR5.sav", "ACGCYPR5.sav", "ACGCZER5.sav", "ACGDEUR5.sav", "ACGDNKR5.sav", "ACGEGYR5.sav", "ACGENGR5.sav", "ACGESPR5.sav", "ACGFINR5.sav", "ACGFRAR5.sav", "ACGGEOR5.sav", "ACGHKGR5.sav", "ACGHRVR5.sav", "ACGHUNR5.sav", "ACGIRLR5.sav", "ACGIRNR5.sav", "ACGISRR5.sav", "ACGITAR5.sav", "ACGJORR5.sav", "ACGKAZR5.sav", "ACGLTUR5.sav", "ACGLVAR5.sav", "ACGMACR5.sav", "ACGMARR5.sav", "ACGMKDR5.sav", "ACGMLTR5.sav", "ACGMNER5.sav", "ACGNIRR5.sav", "ACGNLDR5.sav", "ACGNORR5.sav", "ACGNZLR5.sav", "ACGOMNR5.sav", "ACGPOLR5.sav", "ACGPRTR5.sav", "ACGQATR5.sav", "ACGRMOR5.sav", "ACGRUSR5.sav", "ACGSAUR5.sav", "ACGSGPR5.sav", "ACGSRBR5.sav", "ACGSVKR5.sav", "ACGSVNR5.sav", "ACGSWER5.sav", "ACGTURR5.sav", "ACGTWNR5.sav", "ACGUSAR5.sav", "ACGUZBR5.sav", "ACGXKXR5.sav", "ACGZA6R5.sav", "ACGZAFR5.sav", "ASAAADR5.sav", "ASAADUR5.sav", "ASAALBR5.sav", "ASAARER5.sav", "ASAAUSR5.sav", "ASAAUTR5.sav", "ASAAZER5.sav", "ASABFLR5.sav", "ASABFRR5.sav", "ASABGRR5.sav", "ASABHRR5.sav", "ASABRAR5.sav", "ASACABR5.sav", "ASACBCR5.sav", "ASACNLR5.sav", "ASACQUR5.sav", "ASACYPR5.sav", "ASACZER5.sav", "ASADEUR5.sav", "ASADNKR5.sav", "ASAEGYR5.sav", "ASAENGR5.sav", "ASAESPR5.sav", "ASAFINR5.sav", "ASAFRAR5.sav", "ASAGEOR5.sav", "ASAHKGR5.sav", "ASAHRVR5.sav", "ASAHUNR5.sav", "ASAIRLR5.sav", "ASAIRNR5.sav", "ASAISRR5.sav", "ASAITAR5.sav", "ASAJORR5.sav", "ASAKAZR5.sav", "ASALTUR5.sav", "ASALVAR5.sav", "ASAMACR5.sav", "ASAMARR5.sav", "ASAMKDR5.sav", "ASAMLTR5.sav", "ASAMNER5.sav", "ASANIRR5.sav", "ASANLDR5.sav", "ASANORR5.sav", "ASANZLR5.sav", "ASAOMNR5.sav", "ASAPOLR5.sav", "ASAPRTR5.sav", "ASAQATR5.sav", "ASARMOR5.sav", "ASARUSR5.sav", "ASASAUR5.sav", "ASASGPR5.sav", "ASASRBR5.sav", "ASASVKR5.sav", "ASASVNR5.sav", "ASASWER5.sav", "ASATURR5.sav", "ASATWNR5.sav", "ASAUSAR5.sav", "ASAUZBR5.sav", "ASAXKXR5.sav", "ASAZA6R5.sav", "ASAZAFR5.sav", "ASGAADR5.sav", "ASGADUR5.sav", "ASGALBR5.sav", "ASGARER5.sav", "ASGAUSR5.sav", "ASGAUTR5.sav", "ASGAZER5.sav", "ASGBFLR5.sav", "ASGBFRR5.sav", "ASGBGRR5.sav", "ASGBHRR5.sav", "ASGBRAR5.sav", "ASGCABR5.sav", "ASGCBCR5.sav", "ASGCNLR5.sav", "ASGCQUR5.sav", "ASGCYPR5.sav", "ASGCZER5.sav", "ASGDEUR5.sav", "ASGDNKR5.sav", "ASGEGYR5.sav", "ASGENGR5.sav", "ASGESPR5.sav", "ASGFINR5.sav", "ASGFRAR5.sav", "ASGGEOR5.sav", "ASGHKGR5.sav", "ASGHRVR5.sav", "ASGHUNR5.sav", "ASGIRLR5.sav", "ASGIRNR5.sav", "ASGISRR5.sav", "ASGITAR5.sav", "ASGJORR5.sav", "ASGKAZR5.sav", "ASGLTUR5.sav", "ASGLVAR5.sav", "ASGMACR5.sav", "ASGMARR5.sav", "ASGMKDR5.sav", "ASGMLTR5.sav", "ASGMNER5.sav", "ASGNIRR5.sav", "ASGNLDR5.sav", "ASGNORR5.sav", "ASGNZLR5.sav", "ASGOMNR5.sav", "ASGPOLR5.sav", "ASGPRTR5.sav", "ASGQATR5.sav", "ASGRMOR5.sav", "ASGRUSR5.sav", "ASGSAUR5.sav", "ASGSGPR5.sav", "ASGSRBR5.sav", "ASGSVKR5.sav", "ASGSVNR5.sav", "ASGSWER5.sav", "ASGTURR5.sav", "ASGTWNR5.sav", "ASGUSAR5.sav", "ASGUZBR5.sav", "ASGXKXR5.sav", "ASGZA6R5.sav", "ASGZAFR5.sav", "ASHAADR5.sav", "ASHADUR5.sav", "ASHALBR5.sav", "ASHARER5.sav", "ASHAUSR5.sav", "ASHAUTR5.sav", "ASHAZER5.sav", "ASHBFLR5.sav", "ASHBFRR5.sav", "ASHBGRR5.sav", "ASHBHRR5.sav", "ASHBRAR5.sav", "ASHCABR5.sav", "ASHCBCR5.sav", "ASHCNLR5.sav", "ASHCQUR5.sav", "ASHCYPR5.sav", "ASHCZER5.sav", "ASHDEUR5.sav", "ASHDNKR5.sav", "ASHEGYR5.sav", "ASHENGR5.sav", "ASHESPR5.sav", "ASHFINR5.sav", "ASHFRAR5.sav", "ASHGEOR5.sav", "ASHHKGR5.sav", "ASHHRVR5.sav", "ASHHUNR5.sav", "ASHIRLR5.sav", "ASHIRNR5.sav", "ASHISRR5.sav", "ASHITAR5.sav", "ASHJORR5.sav", "ASHKAZR5.sav", "ASHLTUR5.sav", "ASHLVAR5.sav", "ASHMACR5.sav", "ASHMARR5.sav", "ASHMKDR5.sav", "ASHMLTR5.sav", "ASHMNER5.sav", "ASHNIRR5.sav", "ASHNLDR5.sav", "ASHNORR5.sav", "ASHNZLR5.sav", "ASHOMNR5.sav", "ASHPOLR5.sav", "ASHPRTR5.sav", "ASHQATR5.sav", "ASHRMOR5.sav", "ASHRUSR5.sav", "ASHSAUR5.sav", "ASHSGPR5.sav", "ASHSRBR5.sav", "ASHSVKR5.sav", "ASHSVNR5.sav", "ASHSWER5.sav", "ASHTURR5.sav", "ASHTWNR5.sav", "ASHUSAR5.sav", "ASHUZBR5.sav", "ASHXKXR5.sav", "ASHZA6R5.sav", "ASHZAFR5.sav", "ASPAADR5.sav", "ASPADUR5.sav", "ASPALBR5.sav", "ASPARER5.sav", "ASPAUSR5.sav", "ASPAUTR5.sav", "ASPAZER5.sav", "ASPBFLR5.sav", "ASPBFRR5.sav", "ASPBGRR5.sav", "ASPBHRR5.sav", "ASPBRAR5.sav", "ASPCABR5.sav", "ASPCBCR5.sav", "ASPCNLR5.sav", "ASPCQUR5.sav", "ASPCYPR5.sav", "ASPCZER5.sav", "ASPDEUR5.sav", "ASPDNKR5.sav", "ASPEGYR5.sav", "ASPENGR5.sav", "ASPESPR5.sav", "ASPFINR5.sav", "ASPFRAR5.sav", "ASPGEOR5.sav", "ASPHKGR5.sav", "ASPHRVR5.sav", "ASPHUNR5.sav", "ASPIRLR5.sav", "ASPIRNR5.sav", "ASPISRR5.sav", "ASPITAR5.sav", "ASPJORR5.sav", "ASPKAZR5.sav", "ASPLTUR5.sav", "ASPLVAR5.sav", "ASPMACR5.sav", "ASPMARR5.sav", "ASPMKDR5.sav", "ASPMLTR5.sav", "ASPMNER5.sav", "ASPNIRR5.sav", "ASPNLDR5.sav", "ASPNORR5.sav", "ASPNZLR5.sav", "ASPOMNR5.sav", "ASPPOLR5.sav", "ASPPRTR5.sav", "ASPQATR5.sav", "ASPRMOR5.sav", "ASPRUSR5.sav", "ASPSAUR5.sav", "ASPSGPR5.sav", "ASPSRBR5.sav", "ASPSVKR5.sav", "ASPSVNR5.sav", "ASPSWER5.sav", "ASPTURR5.sav", "ASPTWNR5.sav", "ASPUSAR5.sav", "ASPUZBR5.sav", "ASPXKXR5.sav", "ASPZA6R5.sav", "ASPZAFR5.sav", "ASRAADR5.sav", "ASRADUR5.sav", "ASRALBR5.sav", "ASRARER5.sav", "ASRAUSR5.sav", "ASRAUTR5.sav", "ASRAZER5.sav", "ASRBFLR5.sav", "ASRBFRR5.sav", "ASRBGRR5.sav", "ASRBHRR5.sav", "ASRBRAR5.sav", "ASRCABR5.sav", "ASRCBCR5.sav", "ASRCNLR5.sav", "ASRCQUR5.sav", "ASRCYPR5.sav", "ASRCZER5.sav", "ASRDEUR5.sav", "ASRDNKR5.sav", "ASREGYR5.sav", "ASRENGR5.sav", "ASRESPR5.sav", "ASRFINR5.sav", "ASRFRAR5.sav", "ASRGEOR5.sav", "ASRHKGR5.sav", "ASRHRVR5.sav", "ASRHUNR5.sav", "ASRIRLR5.sav", "ASRIRNR5.sav", "ASRISRR5.sav", "ASRITAR5.sav", "ASRJORR5.sav", "ASRKAZR5.sav", "ASRLTUR5.sav", "ASRLVAR5.sav", "ASRMACR5.sav", "ASRMARR5.sav", "ASRMKDR5.sav", "ASRMLTR5.sav", "ASRMNER5.sav", "ASRNIRR5.sav", "ASRNLDR5.sav", "ASRNORR5.sav", "ASRNZLR5.sav", "ASROMNR5.sav", "ASRPOLR5.sav", "ASRPRTR5.sav", "ASRQATR5.sav", "ASRRMOR5.sav", "ASRRUSR5.sav", "ASRSAUR5.sav", "ASRSGPR5.sav", "ASRSRBR5.sav", "ASRSVKR5.sav", "ASRSVNR5.sav", "ASRSWER5.sav", "ASRTURR5.sav", "ASRTWNR5.sav", "ASRUSAR5.sav", "ASRUZBR5.sav", "ASRXKXR5.sav", "ASRZA6R5.sav", "ASRZAFR5.sav", "ASTAADR5.sav", "ASTADUR5.sav", "ASTALBR5.sav", "ASTARER5.sav", "ASTAUSR5.sav", "ASTAUTR5.sav", "ASTAZER5.sav", "ASTBFLR5.sav", "ASTBFRR5.sav", "ASTBGRR5.sav", "ASTBHRR5.sav", "ASTBRAR5.sav", "ASTCABR5.sav", "ASTCBCR5.sav", "ASTCNLR5.sav", "ASTCQUR5.sav", "ASTCYPR5.sav", "ASTCZER5.sav", "ASTDEUR5.sav", "ASTDNKR5.sav", "ASTEGYR5.sav", "ASTENGR5.sav", "ASTESPR5.sav", "ASTFINR5.sav", "ASTFRAR5.sav", "ASTGEOR5.sav", "ASTHKGR5.sav", "ASTHRVR5.sav", "ASTHUNR5.sav", "ASTIRLR5.sav", "ASTIRNR5.sav", "ASTISRR5.sav", "ASTITAR5.sav", "ASTJORR5.sav", "ASTKAZR5.sav", "ASTLTUR5.sav", "ASTLVAR5.sav", "ASTMACR5.sav", "ASTMARR5.sav", "ASTMKDR5.sav", "ASTMLTR5.sav", "ASTMNER5.sav", "ASTNIRR5.sav", "ASTNLDR5.sav", "ASTNORR5.sav", "ASTNZLR5.sav", "ASTOMNR5.sav", "ASTPOLR5.sav", "ASTPRTR5.sav", "ASTQATR5.sav", "ASTRMOR5.sav", "ASTRUSR5.sav", "ASTSAUR5.sav", "ASTSGPR5.sav", "ASTSRBR5.sav", "ASTSVKR5.sav", "ASTSVNR5.sav", "ASTSWER5.sav", "ASTTURR5.sav", "ASTTWNR5.sav", "ASTUSAR5.sav", "ASTUZBR5.sav", "ASTXKXR5.sav", "ASTZA6R5.sav", "ASTZAFR5.sav", "ATGAADR5.sav", "ATGADUR5.sav", "ATGALBR5.sav", "ATGARER5.sav", "ATGAUSR5.sav", "ATGAUTR5.sav", "ATGAZER5.sav", "ATGBFLR5.sav", "ATGBFRR5.sav", "ATGBGRR5.sav", "ATGBHRR5.sav", "ATGBRAR5.sav", "ATGCABR5.sav", "ATGCBCR5.sav", "ATGCNLR5.sav", "ATGCQUR5.sav", "ATGCYPR5.sav", "ATGCZER5.sav", "ATGDEUR5.sav", "ATGDNKR5.sav", "ATGEGYR5.sav", "ATGENGR5.sav", "ATGESPR5.sav", "ATGFINR5.sav", "ATGFRAR5.sav", "ATGGEOR5.sav", "ATGHKGR5.sav", "ATGHRVR5.sav", "ATGHUNR5.sav", "ATGIRLR5.sav", "ATGIRNR5.sav", "ATGISRR5.sav", "ATGITAR5.sav", "ATGJORR5.sav", "ATGKAZR5.sav", "ATGLTUR5.sav", "ATGLVAR5.sav", "ATGMACR5.sav", "ATGMARR5.sav", "ATGMKDR5.sav", "ATGMLTR5.sav", "ATGMNER5.sav", "ATGNIRR5.sav", "ATGNLDR5.sav", "ATGNORR5.sav", "ATGNZLR5.sav", "ATGOMNR5.sav", "ATGPOLR5.sav", "ATGPRTR5.sav", "ATGQATR5.sav", "ATGRMOR5.sav", "ATGRUSR5.sav", "ATGSAUR5.sav", "ATGSGPR5.sav", "ATGSRBR5.sav", "ATGSVKR5.sav", "ATGSVNR5.sav", "ATGSWER5.sav", "ATGTURR5.sav", "ATGTWNR5.sav", "ATGUSAR5.sav", "ATGUZBR5.sav", "ATGXKXR5.sav", "ATGZA6R5.sav", "ATGZAFR5.sav")
      ),
      PIRLS_2021_G4_Bridge = list("PIRLS2021_IDB_SPSS/3_International Database/1_SPSS Data",
                                  c("ACGAREA5.sav", "ACGBFLA5.sav", "ACGCZEA5.sav", "ACGDEUA5.sav", "ACGDNKA5.sav", "ACGESPA5.sav", "ACGFINA5.sav", "ACGHRVA5.sav", "ACGHUNA5.sav", "ACGISRA5.sav", "ACGITAA5.sav", "ACGKAZA5.sav", "ACGLTUA5.sav", "ACGMLTA5.sav", "ACGNORA5.sav", "ACGNZLA5.sav", "ACGPRTA5.sav", "ACGQATA5.sav", "ACGRMOA5.sav", "ACGRUSA5.sav", "ACGSAUA5.sav", "ACGSGPA5.sav", "ACGSVKA5.sav", "ACGSVNA5.sav", "ACGSWEA5.sav", "ACGTWNA5.sav", "ASAAREA5.sav", "ASABFLA5.sav", "ASACZEA5.sav", "ASADEUA5.sav", "ASADNKA5.sav", "ASAESPA5.sav", "ASAFINA5.sav", "ASAHRVA5.sav", "ASAHUNA5.sav", "ASAISRA5.sav", "ASAITAA5.sav", "ASAKAZA5.sav", "ASALTUA5.sav", "ASAMLTA5.sav", "ASANORA5.sav", "ASANZLA5.sav", "ASAPRTA5.sav", "ASAQATA5.sav", "ASARMOA5.sav", "ASARUSA5.sav", "ASASAUA5.sav", "ASASGPA5.sav", "ASASVKA5.sav", "ASASVNA5.sav", "ASASWEA5.sav", "ASATWNA5.sav", "ASGAREA5.sav", "ASGBFLA5.sav", "ASGCZEA5.sav", "ASGDEUA5.sav", "ASGDNKA5.sav", "ASGESPA5.sav", "ASGFINA5.sav", "ASGHRVA5.sav", "ASGHUNA5.sav", "ASGISRA5.sav", "ASGITAA5.sav", "ASGKAZA5.sav", "ASGLTUA5.sav", "ASGMLTA5.sav", "ASGNORA5.sav", "ASGNZLA5.sav", "ASGPRTA5.sav", "ASGQATA5.sav", "ASGRMOA5.sav", "ASGRUSA5.sav", "ASGSAUA5.sav", "ASGSGPA5.sav", "ASGSVKA5.sav", "ASGSVNA5.sav", "ASGSWEA5.sav", "ASGTWNA5.sav", "ASHAREA5.sav", "ASHBFLA5.sav", "ASHCZEA5.sav", "ASHDEUA5.sav", "ASHDNKA5.sav", "ASHESPA5.sav", "ASHFINA5.sav", "ASHHRVA5.sav", "ASHHUNA5.sav", "ASHISRA5.sav", "ASHITAA5.sav", "ASHKAZA5.sav", "ASHLTUA5.sav", "ASHMLTA5.sav", "ASHNORA5.sav", "ASHNZLA5.sav", "ASHPRTA5.sav", "ASHQATA5.sav", "ASHRMOA5.sav", "ASHRUSA5.sav", "ASHSAUA5.sav", "ASHSGPA5.sav", "ASHSVKA5.sav", "ASHSVNA5.sav", "ASHSWEA5.sav", "ASHTWNA5.sav", "ASRAREA5.sav", "ASRBFLA5.sav", "ASRCZEA5.sav", "ASRDEUA5.sav", "ASRDNKA5.sav", "ASRESPA5.sav", "ASRFINA5.sav", "ASRHRVA5.sav", "ASRHUNA5.sav", "ASRISRA5.sav", "ASRITAA5.sav", "ASRKAZA5.sav", "ASRLTUA5.sav", "ASRMLTA5.sav", "ASRNORA5.sav", "ASRNZLA5.sav", "ASRPRTA5.sav", "ASRQATA5.sav", "ASRRMOA5.sav", "ASRRUSA5.sav", "ASRSAUA5.sav", "ASRSGPA5.sav", "ASRSVKA5.sav", "ASRSVNA5.sav", "ASRSWEA5.sav", "ASRTWNA5.sav", "ASTAREA5.sav", "ASTBFLA5.sav", "ASTCZEA5.sav", "ASTDEUA5.sav", "ASTDNKA5.sav", "ASTESPA5.sav", "ASTFINA5.sav", "ASTHRVA5.sav", "ASTHUNA5.sav", "ASTISRA5.sav", "ASTITAA5.sav", "ASTKAZA5.sav", "ASTLTUA5.sav", "ASTMLTA5.sav", "ASTNORA5.sav", "ASTNZLA5.sav", "ASTPRTA5.sav", "ASTQATA5.sav", "ASTRMOA5.sav", "ASTRUSA5.sav", "ASTSAUA5.sav", "ASTSGPA5.sav", "ASTSVKA5.sav", "ASTSVNA5.sav", "ASTSWEA5.sav", "ASTTWNA5.sav", "ATGAREA5.sav", "ATGBFLA5.sav", "ATGCZEA5.sav", "ATGDEUA5.sav", "ATGDNKA5.sav", "ATGESPA5.sav", "ATGFINA5.sav", "ATGHRVA5.sav", "ATGHUNA5.sav", "ATGISRA5.sav", "ATGITAA5.sav", "ATGKAZA5.sav", "ATGLTUA5.sav", "ATGMLTA5.sav", "ATGNORA5.sav", "ATGNZLA5.sav", "ATGPRTA5.sav", "ATGQATA5.sav", "ATGRMOA5.sav", "ATGRUSA5.sav", "ATGSAUA5.sav", "ATGSGPA5.sav", "ATGSVKA5.sav", "ATGSVNA5.sav", "ATGSWEA5.sav", "ATGTWNA5.sav")
      )),
    prePIRLS_2011 = list(
      prePIRLS_2011_G4 = list("PIRLS2011_IDB_SPSS/Data",
                              c("acgbwal1.sav", "acgcoll1.sav", "acgzafl1.sav", "asabwal1.sav", "asacoll1.sav", "asazafl1.sav", "asgbwal1.sav", "asgcoll1.sav", "asgzafl1.sav", "ashbwal1.sav", "ashcoll1.sav", "ashzafl1.sav", "asrbwal1.sav", "asrcoll1.sav", "asrzafl1.sav", "astbwal1.sav", "astcoll1.sav", "astzafl1.sav", "atgbwal1.sav", "atgcoll1.sav", "atgzafl1.sav")
      )),
    prePIRLS_2016 = list(
      prePIRLS_2016_G4 = list("PIRLS_Literacy2016_IDB_SPSS/Data",
                              c("ACGDN3L2.sav", "ACGEGYL2.sav", "ACGIRNL2.sav", "ACGKWTL2.sav", "ACGMARL2.sav", "ACGZAFL2.sav", "ASADN3L2.sav", "ASAEGYL2.sav", "ASAIRNL2.sav", "ASAKWTL2.sav", "ASAMARL2.sav", "ASAZAFL2.sav", "ASGDN3L2.sav", "ASGEGYL2.sav", "ASGIRNL2.sav", "ASGKWTL2.sav", "ASGMARL2.sav", "ASGZAFL2.sav", "ASHDN3L2.sav", "ASHEGYL2.sav", "ASHIRNL2.sav", "ASHKWTL2.sav", "ASHMARL2.sav", "ASHZAFL2.sav", "ASRDN3L2.sav", "ASREGYL2.sav", "ASRIRNL2.sav", "ASRKWTL2.sav", "ASRMARL2.sav", "ASRZAFL2.sav", "ASTDN3L2.sav", "ASTEGYL2.sav", "ASTIRNL2.sav", "ASTKWTL2.sav", "ASTMARL2.sav", "ASTZAFL2.sav", "ATGDN3L2.sav", "ATGEGYL2.sav", "ATGIRNL2.sav", "ATGKWTL2.sav", "ATGMARL2.sav", "ATGZAFL2.sav")
      )),
    preTIMSS_2015 = list(
      preTIMSS_2015_G4 = list("TIMSS2015_IDB_SPSS_G4/Data",
                              c("ACGABAN1.sav", "ACGBHRN1.sav", "ACGIDNN1.sav", "ACGIRNN1.sav", "ACGJORN1.sav", "ACGKWTN1.sav", "ACGMARN1.sav", "ACGZAFN1.sav", "ASAABAN1.sav", "ASABHRN1.sav", "ASAIDNN1.sav", "ASAIRNN1.sav", "ASAJORN1.sav", "ASAKWTN1.sav", "ASAMARN1.sav", "ASAZAFN1.sav", "ASGABAN1.sav", "ASGBHRN1.sav", "ASGIDNN1.sav", "ASGIRNN1.sav", "ASGJORN1.sav", "ASGKWTN1.sav", "ASGMARN1.sav", "ASGZAFN1.sav", "ASHABAN1.sav", "ASHBHRN1.sav", "ASHIDNN1.sav", "ASHIRNN1.sav", "ASHJORN1.sav", "ASHKWTN1.sav", "ASHMARN1.sav", "ASHZAFN1.sav", "ASRABAN1.sav", "ASRBHRN1.sav", "ASRIDNN1.sav", "ASRIRNN1.sav", "ASRJORN1.sav", "ASRKWTN1.sav", "ASRMARN1.sav", "ASRZAFN1.sav", "ASTABAN1.sav", "ASTBHRN1.sav", "ASTIDNN1.sav", "ASTIRNN1.sav", "ASTJORN1.sav", "ASTKWTN1.sav", "ASTMARN1.sav", "ASTZAFN1.sav", "ATGABAN1.sav", "ATGBHRN1.sav", "ATGIDNN1.sav", "ATGIRNN1.sav", "ATGJORN1.sav", "ATGKWTN1.sav", "ATGMARN1.sav", "ATGZAFN1.sav")
      )),
    REDS_2021 = list(
      REDS_2021_G8 = list("REDS2021_IDB_SPSS/Data",
                          c("bcgarev1.sav", "bcgbfav1.sav", "bcgdnkv1.sav", "bcgethv1.sav", "bcgindv1.sav", "bcgkenv1.sav", "bcgrusv1.sav", "bcgrwav1.sav", "bcgsvnv1.sav", "bcguryv1.sav", "bcguzbv1.sav", "bsgarev1.sav", "bsgbfav1.sav", "bsgdnkv1.sav", "bsgethv1.sav", "bsgkenv1.sav", "bsgrusv1.sav", "bsgsvnv1.sav", "bsguzbv1.sav", "btgarev1.sav", "btgbfav1.sav", "btgdnkv1.sav", "btgethv1.sav", "btgindv1.sav", "btgkenv1.sav", "btgrusv1.sav", "btgsvnv1.sav", "btguryv1.sav", "btguzbv1.sav")
      )),
    RLII_1991 = list(
      RLII_1991_G4 = list("RLII1991_IDB_SPSS/Data",
                          c("ASCGRCt1.sav", "ASCHUNt1.sav", "ASCISLt1.sav", "ASCITAt1.sav", "ASCNZLt1.sav", "ASCSGPt1.sav", "ASCSVNt1.sav", "ASCSWEt1.sav", "ASCUSAt1.sav")
      )),
    RLII_2001 = list(
      RLII_2001_G4 = list("RLII2001_IDB_SPSS/Data",
                          c("ASCGRCt2.sav", "ASCHUNt2.sav", "ASCISLt2.sav", "ASCITAt2.sav", "ASCNZLt2.sav", "ASCSGPt2.sav", "ASCSVNt2.sav", "ASCSWEt2.sav", "ASCUSAt2.sav")
      )),
    SITES_1998 = list(
      SITES_1998_M1_POP_A = list("SITES1998_IDB_SPSS",
                                 c("AxgALLS0.sav")
      ),
      SITES_1998_M1_POP_B = list("SITES1998_IDB_SPSS",
                                 c("BxgALLS0.sav")
      ),
      SITES_1998_M1_POP_C = list("SITES1998_IDB_SPSS",
                                 c("CxgALLS0.sav")
      )),
    SITES_2006 = list(
      SITES_2006_M2 = list("SITES2006_IDB_SPSS/Data",
                           c("BCGAUSS1.sav", "BCGCABS1.sav", "BCGCHLS1.sav", "BCGCOTS1.sav", "BCGDNKS1.sav", "BCGECTS1.sav", "BCGESTS1.sav", "BCGFINS1.sav", "BCGFRAS1.sav", "BCGHKGS1.sav", "BCGISRS1.sav", "BCGITAS1.sav", "BCGJPNS1.sav", "BCGLTUS1.sav", "BCGNORS1.sav", "BCGRUMS1.sav", "BCGRUSS1.sav", "BCGSGPS1.sav", "BCGSVKS1.sav", "BCGSVNS1.sav", "BCGTHAS1.sav", "BCGTWNS1.sav", "BCGZAFS1.sav", "BTMAUSS1.sav", "BTMCABS1.sav", "BTMCHLS1.sav", "BTMCOTS1.sav", "BTMDNKS1.sav", "BTMECTS1.sav", "BTMESTS1.sav", "BTMFINS1.sav", "BTMFRAS1.sav", "BTMHKGS1.sav", "BTMISRS1.sav", "BTMITAS1.sav", "BTMJPNS1.sav", "BTMLTUS1.sav", "BTMNORS1.sav", "BTMRUMS1.sav", "BTMRUSS1.sav", "BTMSGPS1.sav", "BTMSVKS1.sav", "BTMSVNS1.sav", "BTMTHAS1.sav", "BTMTWNS1.sav", "BTMZAFS1.sav", "BTSAUSS1.sav", "BTSCABS1.sav", "BTSCHLS1.sav", "BTSCOTS1.sav", "BTSDNKS1.sav", "BTSECTS1.sav", "BTSESTS1.sav", "BTSFINS1.sav", "BTSFRAS1.sav", "BTSHKGS1.sav", "BTSISRS1.sav", "BTSITAS1.sav", "BTSJPNS1.sav", "BTSLTUS1.sav", "BTSNORS1.sav", "BTSRUMS1.sav", "BTSRUSS1.sav", "BTSSGPS1.sav", "BTSSVKS1.sav", "BTSSVNS1.sav", "BTSTHAS1.sav", "BTSTWNS1.sav", "BTSZAFS1.sav", "BNGSITS1.sav")
      )),
    TALIS_2008 = list(
      TALIS_2008_I2 = list(".",
                           c("BCGAUST1.sav", "BCGAUTT1.sav", "BCGBFLT1.sav", "BCGBGRT1.sav", "BCGBRAT1.sav", "BCGDNKT1.sav", "BCGESPT1.sav", "BCGESTT1.sav", "BCGHUNT1.sav", "BCGIRLT1.sav", "BCGITAT1.sav", "BCGKORT1.sav", "BCGLTUT1.sav", "BCGMEXT1.sav", "BCGMLTT1.sav", "BCGMYST1.sav", "BCGNLDT1.sav", "BCGNORT1.sav", "BCGPOLT1.sav", "BCGPRTT1.sav", "BCGSVKT1.sav", "BCGSVNT1.sav", "BCGTURT1.sav", "BTGAUST1.sav", "BTGAUTT1.sav", "BTGBFLT1.sav", "BTGBGRT1.sav", "BTGBRAT1.sav", "BTGDNKT1.sav", "BTGESPT1.sav", "BTGESTT1.sav", "BTGHUNT1.sav", "BTGIRLT1.sav", "BTGITAT1.sav", "BTGKORT1.sav", "BTGLTUT1.sav", "BTGMEXT1.sav", "BTGMLTT1.sav", "BTGMYST1.sav", "BTGNLDT1.sav", "BTGNORT1.sav", "BTGPOLT1.sav", "BTGPRTT1.sav", "BTGSVKT1.sav", "BTGSVNT1.sav", "BTGTURT1.sav")
      )),
    TALIS_2013 = list(
      TALIS_2013_I1 = list(".",
                           c("ACGBFLT2.sav", "ACGDNKT2.sav", "ACGFINT2.sav", "ACGMEXT2.sav", "ACGNORT2.sav", "ACGPOLT2.sav", "ATGBFLT2.sav", "ATGDNKT2.sav", "ATGFINT2.sav", "ATGMEXT2.sav", "ATGNORT2.sav", "ATGPOLT2.sav")
      ),
      TALIS_2013_I2 = list(
        ".",
        c("BCGAADT2.sav", "BCGAUST2.sav", "BCGBFLT2.sav", "BCGBGRT2.sav", "BCGBRAT2.sav", "BCGCABT2.sav", "BCGCHLT2.sav", "BCGCSHT2.sav", "BCGCZET2.sav", "BCGDNKT2.sav", "BCGENGT2.sav", "BCGESPT2.sav", "BCGESTT2.sav", "BCGFINT2.sav", "BCGFRAT2.sav", "BCGGEOT2.sav", "BCGHRVT2.sav", "BCGISRT2.sav", "BCGITAT2.sav", "BCGJPNT2.sav", "BCGKORT2.sav", "BCGLVAT2.sav", "BCGMEXT2.sav", "BCGMYST2.sav", "BCGNLDT2.sav", "BCGNORT2.sav", "BCGNZLT2.sav", "BCGPOLT2.sav", "BCGPRTT2.sav", "BCGROUT2.sav", "BCGRUST2.sav", "BCGSGPT2.sav", "BCGSRBT2.sav", "BCGSVKT2.sav", "BCGSWET2.sav", "BCGUSAT2.sav", "BTGAADT2.sav", "BTGAUST2.sav", "BTGBFLT2.sav", "BTGBGRT2.sav", "BTGBRAT2.sav", "BTGCABT2.sav", "BTGCHLT2.sav", "BTGCSHT2.sav", "BTGCZET2.sav", "BTGDNKT2.sav", "BTGENGT2.sav", "BTGESPT2.sav", "BTGESTT2.sav", "BTGFINT2.sav", "BTGFRAT2.sav", "BTGGEOT2.sav", "BTGHRVT2.sav", "BTGISRT2.sav", "BTGITAT2.sav", "BTGJPNT2.sav", "BTGKORT2.sav", "BTGLVAT2.sav", "BTGMEXT2.sav", "BTGMYST2.sav", "BTGNLDT2.sav", "BTGNORT2.sav", "BTGNZLT2.sav", "BTGPOLT2.sav", "BTGPRTT2.sav", "BTGROUT2.sav", "BTGRUST2.sav", "BTGSGPT2.sav", "BTGSRBT2.sav", "BTGSVKT2.sav", "BTGSWET2.sav", "BTGUSAT2.sav")
      ),
      TALIS_2013_I3 = list(
        ".",
        c("CCGAADT2.sav", "CCGAUST2.sav", "CCGDNKT2.sav", "CCGFINT2.sav", "CCGGEOT2.sav", "CCGITAT2.sav", "CCGMEXT2.sav", "CCGNORT2.sav", "CCGPOLT2.sav", "CCGSGPT2.sav", "CTGAADT2.sav", "CTGAUST2.sav", "CTGDNKT2.sav", "CTGFINT2.sav", "CTGGEOT2.sav", "CTGITAT2.sav", "CTGMEXT2.sav", "CTGNORT2.sav", "CTGPOLT2.sav", "CTGSGPT2.sav")
      ),
      TALIS_2013_P = list(
        ".",
        c("PCGAUST2.sav", "PCGESPT2.sav", "PCGFINT2.sav", "PCGLVAT2.sav", "PCGMEXT2.sav", "PCGPRTT2.sav", "PCGROUT2.sav", "PCGSGPT2.sav", "PTGAUST2.sav", "PTGESPT2.sav", "PTGFINT2.sav", "PTGLVAT2.sav", "PTGMEXT2.sav", "PTGPRTT2.sav", "PTGROUT2.sav", "PTGSGPT2.sav")
      )),
    TALIS_2018 = list(
      TALIS_2018_I1 = list(".",
                           c("ACGABAT3.sav", "ACGARET3.sav", "ACGAUST3.sav", "ACGBFLT3.sav", "ACGDNKT3.sav", "ACGENGT3.sav", "ACGESPT3.sav", "ACGFRAT3.sav", "ACGJPNT3.sav", "ACGKORT3.sav", "ACGNLDT3.sav", "ACGSWET3.sav", "ACGTURT3.sav", "ACGTWNT3.sav", "ACGVNMT3.sav", "ATGABAT3.sav", "ATGARET3.sav", "ATGAUST3.sav", "ATGBFLT3.sav", "ATGDNKT3.sav", "ATGENGT3.sav", "ATGESPT3.sav", "ATGFRAT3.sav", "ATGJPNT3.sav", "ATGKORT3.sav", "ATGNLDT3.sav", "ATGSWET3.sav", "ATGTURT3.sav", "ATGTWNT3.sav", "ATGVNMT3.sav")
      ),
      TALIS_2018_I2 = list(
        ".",
        c("BCGABAT3.sav", "BCGARET3.sav", "BCGAUST3.sav", "BCGAUTT3.sav", "BCGBELT3.sav", "BCGBGRT3.sav", "BCGBRAT3.sav", "BCGCABT3.sav", "BCGCHLT3.sav", "BCGCOLT3.sav", "BCGCSHT3.sav", "BCGCYPT3.sav", "BCGCZET3.sav", "BCGDNKT3.sav", "BCGENGT3.sav", "BCGESPT3.sav", "BCGESTT3.sav", "BCGFINT3.sav", "BCGFRAT3.sav", "BCGGEOT3.sav", "BCGHRVT3.sav", "BCGHUNT3.sav", "BCGISRT3.sav", "BCGITAT3.sav", "BCGJPNT3.sav", "BCGKAZT3.sav", "BCGKORT3.sav", "BCGLTUT3.sav", "BCGLVAT3.sav", "BCGMEXT3.sav", "BCGMLTT3.sav", "BCGNLDT3.sav", "BCGNORT3.sav", "BCGNZLT3.sav", "BCGPRTT3.sav", "BCGROUT3.sav", "BCGRUST3.sav", "BCGSAUT3.sav", "BCGSGPT3.sav", "BCGSVKT3.sav", "BCGSVNT3.sav", "BCGSWET3.sav", "BCGTURT3.sav", "BCGTWNT3.sav", "BCGUSAT3.sav", "BCGVNMT3.sav", "BCGZAFT3.sav", "BTGABAT3.sav", "BTGARET3.sav", "BTGAUST3.sav", "BTGAUTT3.sav", "BTGBELT3.sav", "BTGBGRT3.sav", "BTGBRAT3.sav", "BTGCABT3.sav", "BTGCHLT3.sav", "BTGCOLT3.sav", "BTGCSHT3.sav", "BTGCYPT3.sav", "BTGCZET3.sav", "BTGDNKT3.sav", "BTGENGT3.sav", "BTGESPT3.sav", "BTGESTT3.sav", "BTGFINT3.sav", "BTGFRAT3.sav", "BTGGEOT3.sav", "BTGHRVT3.sav", "BTGHUNT3.sav", "BTGISRT3.sav", "BTGITAT3.sav", "BTGJPNT3.sav", "BTGKAZT3.sav", "BTGKORT3.sav", "BTGLTUT3.sav", "BTGLVAT3.sav", "BTGMEXT3.sav", "BTGMLTT3.sav", "BTGNLDT3.sav", "BTGNORT3.sav", "BTGNZLT3.sav", "BTGPRTT3.sav", "BTGROUT3.sav", "BTGRUST3.sav", "BTGSAUT3.sav", "BTGSGPT3.sav", "BTGSVKT3.sav", "BTGSVNT3.sav", "BTGSWET3.sav", "BTGTURT3.sav", "BTGTWNT3.sav", "BTGUSAT3.sav", "BTGVNMT3.sav", "BTGZAFT3.sav")
      ),
      TALIS_2018_I3 = list(
        ".",
        c("CCGARET3.sav", "CCGBRAT3.sav", "CCGCABT3.sav", "CCGDNKT3.sav", "CCGHRVT3.sav", "CCGPRTT3.sav", "CCGSVNT3.sav", "CCGSWET3.sav", "CCGTURT3.sav", "CCGTWNT3.sav", "CCGVNMT3.sav", "CTGARET3.sav", "CTGBRAT3.sav", "CTGCABT3.sav", "CTGDNKT3.sav", "CTGHRVT3.sav", "CTGPRTT3.sav", "CTGSVNT3.sav", "CTGSWET3.sav", "CTGTURT3.sav", "CTGTWNT3.sav", "CTGVNMT3.sav")
      ),
      TALIS_2018_P = list(
        ".",
        c("PCGABAT3.sav", "PCGAUST3.sav", "PCGCOLT3.sav", "PCGCZET3.sav", "PCGDNKT3.sav", "PCGGEOT3.sav", "PCGMLTT3.sav", "PCGTURT3.sav", "PCGVNMT3.sav", "PTGABAT3.sav", "PTGAUST3.sav", "PTGCOLT3.sav", "PTGCZET3.sav", "PTGDNKT3.sav", "PTGGEOT3.sav", "PTGMLTT3.sav", "PTGTURT3.sav", "PTGVNMT3.sav")
      )),
    TALIS_2024 = list(
      TALIS_2024_I1 = list(".",
                           c("acgintt4.sav", "atgintt4.sav")
      ),
      TALIS_2024_I2 = list(".",
                           c("bcgintt4.sav", "btgintt4.sav")
      ),
      TALIS_2024_I3 = list(".",
                           c("ccgintt4.sav", "ctgintt4.sav")
      )
    ),
    TALIS_3S_2018 = list(
      TALIS_3S_2018_I0.2 = list("TALIS-Starting-Strong-By-country-SPSS/SPSS",
                                c("BLGCHLS1.sav", "BLGDEUS1.sav", "BLGDNKS1.sav", "BLGISRS1.sav", "BLGJPNS1.sav", "BLGKORS1.sav", "BLGNORS1.sav", "BLGTURS1.sav", "BSGCHLS1.sav", "BSGDEUS1.sav", "BSGDNKS1.sav", "BSGISRS1.sav", "BSGJPNS1.sav", "BSGKORS1.sav", "BSGNORS1.sav", "BSGTURS1.sav")
      ),
      TALIS_3S_2018_IU3 = list(
        "TALIS-Starting-Strong-By-country-SPSS/SPSS",
        c("ALGDEUS1.sav", "ALGDNKS1.sav", "ALGISRS1.sav", "ALGNORS1.sav", "ASGDEUS1.sav", "ASGDNKS1.sav", "ASGISRS1.sav", "ASGNORS1.sav")
      )),
    TIMSS_1995 = list(
      TIMSS_1995_G4 = list("TIMSS1995_IDB_SPSS_G4/Data",
                           c("ACGAUSm1.sav", "ACGAUTm1.sav", "ACGCANm1.sav", "ACGCYPm1.sav", "ACGCZEm1.sav", "ACGENGm1.sav", "ACGGRCm1.sav", "ACGHKGm1.sav", "ACGHUNm1.sav", "ACGIRLm1.sav", "ACGIRNm1.sav", "ACGISLm1.sav", "ACGISRm1.sav", "ACGJPNm1.sav", "ACGKORm1.sav", "ACGKWTm1.sav", "ACGLVAm1.sav", "ACGNLDm1.sav", "ACGNORm1.sav", "ACGNZLm1.sav", "ACGPRTm1.sav", "ACGSCOm1.sav", "ACGSGPm1.sav", "ACGSVNm1.sav", "ACGTHAm1.sav", "ACGUSAm1.sav", "ASAAUSm1.sav", "ASAAUTm1.sav", "ASACANm1.sav", "ASACYPm1.sav", "ASACZEm1.sav", "ASAENGm1.sav", "ASAGRCm1.sav", "ASAHKGm1.sav", "ASAHUNm1.sav", "ASAIRLm1.sav", "ASAIRNm1.sav", "ASAISLm1.sav", "ASAISRm1.sav", "ASAJPNm1.sav", "ASAKORm1.sav", "ASAKWTm1.sav", "ASALVAm1.sav", "ASANLDm1.sav", "ASANORm1.sav", "ASANZLm1.sav", "ASAPRTm1.sav", "ASASCOm1.sav", "ASASGPm1.sav", "ASASVNm1.sav", "ASATHAm1.sav", "ASAUSAm1.sav", "ASGAUSm1.sav", "ASGAUTm1.sav", "ASGCANm1.sav", "ASGCYPm1.sav", "ASGCZEm1.sav", "ASGENGm1.sav", "ASGGRCm1.sav", "ASGHKGm1.sav", "ASGHUNm1.sav", "ASGIRLm1.sav", "ASGIRNm1.sav", "ASGISLm1.sav", "ASGISRm1.sav", "ASGJPNm1.sav", "ASGKORm1.sav", "ASGKWTm1.sav", "ASGLVAm1.sav", "ASGNLDm1.sav", "ASGNORm1.sav", "ASGNZLm1.sav", "ASGPRTm1.sav", "ASGSCOm1.sav", "ASGSGPm1.sav", "ASGSVNm1.sav", "ASGTHAm1.sav", "ASGUSAm1.sav", "ASTAUSm1.sav", "ASTAUTm1.sav", "ASTCANm1.sav", "ASTCYPm1.sav", "ASTCZEm1.sav", "ASTENGm1.sav", "ASTGRCm1.sav", "ASTHKGm1.sav", "ASTHUNm1.sav", "ASTIRLm1.sav", "ASTIRNm1.sav", "ASTISLm1.sav", "ASTISRm1.sav", "ASTJPNm1.sav", "ASTKORm1.sav", "ASTKWTm1.sav", "ASTLVAm1.sav", "ASTNLDm1.sav", "ASTNORm1.sav", "ASTNZLm1.sav", "ASTPRTm1.sav", "ASTSCOm1.sav", "ASTSGPm1.sav", "ASTSVNm1.sav", "ASTTHAm1.sav", "ASTUSAm1.sav", "ATGAUSm1.sav", "ATGAUTm1.sav", "ATGCANm1.sav", "ATGCYPm1.sav", "ATGCZEm1.sav", "ATGENGm1.sav", "ATGGRCm1.sav", "ATGHKGm1.sav", "ATGHUNm1.sav", "ATGIRLm1.sav", "ATGIRNm1.sav", "ATGISLm1.sav", "ATGISRm1.sav", "ATGJPNm1.sav", "ATGKORm1.sav", "ATGKWTm1.sav", "ATGLVAm1.sav", "ATGNLDm1.sav", "ATGNORm1.sav", "ATGNZLm1.sav", "ATGPRTm1.sav", "ATGSCOm1.sav", "ATGSGPm1.sav", "ATGSVNm1.sav", "ATGTHAm1.sav", "ATGUSAm1.sav")
      ),
      TIMSS_1995_G8 = list(
        "TIMSS1995_IDB_SPSS_G8/Data",
        c("bcgAUSm1.sav", "bcgAUTm1.sav", "bcgBFLm1.sav", "bcgBFRm1.sav", "bcgCANm1.sav", "bcgCHEm1.sav", "bcgCOLm1.sav", "bcgCYPm1.sav", "bcgCZEm1.sav", "bcgDEUm1.sav", "bcgDNKm1.sav", "bcgENGm1.sav", "bcgESPm1.sav", "bcgFRAm1.sav", "bcgGRCm1.sav", "bcgHKGm1.sav", "bcgHUNm1.sav", "bcgIRLm1.sav", "bcgIRNm1.sav", "bcgISLm1.sav", "bcgISRm1.sav", "bcgITAm1.sav", "bcgJPNm1.sav", "bcgKORm1.sav", "bcgKWTm1.sav", "bcgLTUm1.sav", "bcgLVAm1.sav", "bcgNLDm1.sav", "bcgNORm1.sav", "bcgNZLm1.sav", "bcgPRTm1.sav", "bcgROMm1.sav", "bcgRUSm1.sav", "bcgSCOm1.sav", "bcgSGPm1.sav", "bcgSVKm1.sav", "bcgSVNm1.sav", "bcgSWEm1.sav", "bcgTHAm1.sav", "bcgUSAm1.sav", "bsaAUSm1.sav", "bsaAUTm1.sav", "bsaBFLm1.sav", "bsaBFRm1.sav", "bsaBGRm1.sav", "bsaCANm1.sav", "bsaCHEm1.sav", "bsaCOLm1.sav", "bsaCYPm1.sav", "bsaCZEm1.sav", "bsaDEUm1.sav", "bsaDNKm1.sav", "bsaENGm1.sav", "bsaESPm1.sav", "bsaFRAm1.sav", "bsaGRCm1.sav", "bsaHKGm1.sav", "bsaHUNm1.sav", "bsaIRLm1.sav", "bsaIRNm1.sav", "bsaISLm1.sav", "bsaISRm1.sav", "bsaITAm1.sav", "bsaJPNm1.sav", "bsaKORm1.sav", "bsaKWTm1.sav", "bsaLTUm1.sav", "bsaLVAm1.sav", "bsaNLDm1.sav", "bsaNORm1.sav", "bsaNZLm1.sav", "bsaPRTm1.sav", "bsaROMm1.sav", "bsaRUSm1.sav", "bsaSCOm1.sav", "bsaSGPm1.sav", "bsaSVKm1.sav", "bsaSVNm1.sav", "bsaSWEm1.sav", "bsaTHAm1.sav", "bsaUSAm1.sav", "bsaZAFm1.sav", "bsgAUSm1.sav", "bsgAUTm1.sav", "bsgBFLm1.sav", "bsgBFRm1.sav", "bsgCANm1.sav", "bsgCHEm1.sav", "bsgCOLm1.sav", "bsgCYPm1.sav", "bsgCZEm1.sav", "bsgDEUm1.sav", "bsgDNKm1.sav", "bsgENGm1.sav", "bsgESPm1.sav", "bsgFRAm1.sav", "bsgGRCm1.sav", "bsgHKGm1.sav", "bsgHUNm1.sav", "bsgIRLm1.sav", "bsgIRNm1.sav", "bsgISLm1.sav", "bsgISRm1.sav", "bsgITAm1.sav", "bsgJPNm1.sav", "bsgKORm1.sav", "bsgKWTm1.sav", "bsgLTUm1.sav", "bsgLVAm1.sav", "bsgNLDm1.sav", "bsgNORm1.sav", "bsgNZLm1.sav", "bsgPRTm1.sav", "bsgROMm1.sav", "bsgRUSm1.sav", "bsgSCOm1.sav", "bsgSGPm1.sav", "bsgSVKm1.sav", "bsgSVNm1.sav", "bsgSWEm1.sav", "bsgTHAm1.sav", "bsgUSAm1.sav", "bstAUSm1.sav", "bstAUTm1.sav", "bstBFLm1.sav", "bstBFRm1.sav", "bstCANm1.sav", "bstCHEm1.sav", "bstCOLm1.sav", "bstCYPm1.sav", "bstCZEm1.sav", "bstDEUm1.sav", "bstDNKm1.sav", "bstENGm1.sav", "bstESPm1.sav", "bstFRAm1.sav", "bstGRCm1.sav", "bstHKGm1.sav", "bstHUNm1.sav", "bstIRLm1.sav", "bstIRNm1.sav", "bstISLm1.sav", "bstISRm1.sav", "bstITAm1.sav", "bstJPNm1.sav", "bstKORm1.sav", "bstKWTm1.sav", "bstLTUm1.sav", "bstLVAm1.sav", "bstNLDm1.sav", "bstNORm1.sav", "bstNZLm1.sav", "bstPRTm1.sav", "bstROMm1.sav", "bstRUSm1.sav", "bstSCOm1.sav", "bstSGPm1.sav", "bstSVKm1.sav", "bstSVNm1.sav", "bstSWEm1.sav", "bstTHAm1.sav", "bstUSAm1.sav", "btmAUSm1.sav", "btmAUTm1.sav", "btmBFLm1.sav", "btmBFRm1.sav", "btmBGRm1.sav", "btmCANm1.sav", "btmCHEm1.sav", "btmCOLm1.sav", "btmCYPm1.sav", "btmCZEm1.sav", "btmDEUm1.sav", "btmDNKm1.sav", "btmENGm1.sav", "btmESPm1.sav", "btmFRAm1.sav", "btmGRCm1.sav", "btmHKGm1.sav", "btmHUNm1.sav", "btmIRLm1.sav", "btmIRNm1.sav", "btmISLm1.sav", "btmISRm1.sav", "btmITAm1.sav", "btmJPNm1.sav", "btmKORm1.sav", "btmKWTm1.sav", "btmLTUm1.sav", "btmLVAm1.sav", "btmNLDm1.sav", "btmNORm1.sav", "btmNZLm1.sav", "btmPRTm1.sav", "btmROMm1.sav", "btmRUSm1.sav", "btmSCOm1.sav", "btmSGPm1.sav", "btmSVKm1.sav", "btmSVNm1.sav", "btmSWEm1.sav", "btmTHAm1.sav", "btmUSAm1.sav", "btmZAFm1.sav", "btsAUSm1.sav", "btsAUTm1.sav", "btsBFLm1.sav", "btsBFRm1.sav", "btsBGRm1.sav", "btsCANm1.sav", "btsCHEm1.sav", "btsCOLm1.sav", "btsCYPm1.sav", "btsCZEm1.sav", "btsDEUm1.sav", "btsDNKm1.sav", "btsENGm1.sav", "btsESPm1.sav", "btsFRAm1.sav", "btsGRCm1.sav", "btsHKGm1.sav", "btsHUNm1.sav", "btsIRLm1.sav", "btsIRNm1.sav", "btsISLm1.sav", "btsISRm1.sav", "btsITAm1.sav", "btsJPNm1.sav", "btsKORm1.sav", "btsKWTm1.sav", "btsLTUm1.sav", "btsLVAm1.sav", "btsNLDm1.sav", "btsNORm1.sav", "btsNZLm1.sav", "btsPRTm1.sav", "btsROMm1.sav", "btsRUSm1.sav", "btsSCOm1.sav", "btsSGPm1.sav", "btsSVKm1.sav", "btsSVNm1.sav", "btsSWEm1.sav", "btsTHAm1.sav", "btsUSAm1.sav", "btsZAFm1.sav")
      )),
    TIMSS_1999 = list(
      TIMSS_1999_G8 = list("TIMSS1999_IDB_SPSS_G8/Data",
                           c("BCGAUSm2.sav", "BCGBFLm2.sav", "BCGBGRm2.sav", "BCGCANm2.sav", "BCGCHLm2.sav", "BCGCYPm2.sav", "BCGCZEm2.sav", "BCGENGm2.sav", "BCGFINm2.sav", "BCGHKGm2.sav", "BCGHUNm2.sav", "BCGIDNm2.sav", "BCGIRNm2.sav", "BCGISRm2.sav", "BCGITAm2.sav", "BCGJORm2.sav", "BCGJPNm2.sav", "BCGKORm2.sav", "BCGLTUm2.sav", "BCGLVAm2.sav", "BCGMARm2.sav", "BCGMDAm2.sav", "BCGMKDm2.sav", "BCGMYSm2.sav", "BCGNLDm2.sav", "BCGNZLm2.sav", "BCGPHLm2.sav", "BCGROMm2.sav", "BCGRUSm2.sav", "BCGSGPm2.sav", "BCGSVKm2.sav", "BCGSVNm2.sav", "BCGTHAm2.sav", "BCGTUNm2.sav", "BCGTURm2.sav", "BCGTWNm2.sav", "BCGUSAm2.sav", "BCGZAFm2.sav", "BSAAUSm2.sav", "BSABFLm2.sav", "BSABGRm2.sav", "BSACANm2.sav", "BSACHLm2.sav", "BSACYPm2.sav", "BSACZEm2.sav", "BSAENGm2.sav", "BSAFINm2.sav", "BSAHKGm2.sav", "BSAHUNm2.sav", "BSAIDNm2.sav", "BSAIRNm2.sav", "BSAISRm2.sav", "BSAITAm2.sav", "BSAJORm2.sav", "BSAJPNm2.sav", "BSAKORm2.sav", "BSALTUm2.sav", "BSALVAm2.sav", "BSAMARm2.sav", "BSAMDAm2.sav", "BSAMKDm2.sav", "BSAMYSm2.sav", "BSANLDm2.sav", "BSANZLm2.sav", "BSAPHLm2.sav", "BSAROMm2.sav", "BSARUSm2.sav", "BSASGPm2.sav", "BSASVKm2.sav", "BSASVNm2.sav", "BSATHAm2.sav", "BSATUNm2.sav", "BSATURm2.sav", "BSATWNm2.sav", "BSAUSAm2.sav", "BSAZAFm2.sav", "BSGAUSm2.sav", "BSGBFLm2.sav", "BSGBGRm2.sav", "BSGCANm2.sav", "BSGCHLm2.sav", "BSGCYPm2.sav", "BSGCZEm2.sav", "BSGENGm2.sav", "BSGFINm2.sav", "BSGHKGm2.sav", "BSGHUNm2.sav", "BSGIDNm2.sav", "BSGIRNm2.sav", "BSGISRm2.sav", "BSGITAm2.sav", "BSGJORm2.sav", "BSGJPNm2.sav", "BSGKORm2.sav", "BSGLTUm2.sav", "BSGLVAm2.sav", "BSGMARm2.sav", "BSGMDAm2.sav", "BSGMKDm2.sav", "BSGMYSm2.sav", "BSGNLDm2.sav", "BSGNZLm2.sav", "BSGPHLm2.sav", "BSGROMm2.sav", "BSGRUSm2.sav", "BSGSGPm2.sav", "BSGSVKm2.sav", "BSGSVNm2.sav", "BSGTHAm2.sav", "BSGTUNm2.sav", "BSGTURm2.sav", "BSGTWNm2.sav", "BSGUSAm2.sav", "BSGZAFm2.sav", "BSRAUSm2.sav", "BSRBFLm2.sav", "BSRBGRm2.sav", "BSRCANm2.sav", "BSRCHLm2.sav", "BSRCZEm2.sav", "BSRENGm2.sav", "BSRFINm2.sav", "BSRHKGm2.sav", "BSRHUNm2.sav", "BSRIDNm2.sav", "BSRIRNm2.sav", "BSRISRm2.sav", "BSRITAm2.sav", "BSRJORm2.sav", "BSRJPNm2.sav", "BSRKORm2.sav", "BSRLTUm2.sav", "BSRLVAm2.sav", "BSRMARm2.sav", "BSRMDAm2.sav", "BSRMKDm2.sav", "BSRMYSm2.sav", "BSRNLDm2.sav", "BSRNZLm2.sav", "BSRPHLm2.sav", "BSRROMm2.sav", "BSRRUSm2.sav", "BSRSGPm2.sav", "BSRSVKm2.sav", "BSRSVNm2.sav", "BSRTHAm2.sav", "BSRTUNm2.sav", "BSRTURm2.sav", "BSRTWNm2.sav", "BSRUSAm2.sav", "BSRZAFm2.sav", "BSTAUSm2.sav", "BSTBFLm2.sav", "BSTBGRm2.sav", "BSTCANm2.sav", "BSTCHLm2.sav", "BSTCYPm2.sav", "BSTCZEm2.sav", "BSTENGm2.sav", "BSTFINm2.sav", "BSTHKGm2.sav", "BSTHUNm2.sav", "BSTIDNm2.sav", "BSTIRNm2.sav", "BSTISRm2.sav", "BSTITAm2.sav", "BSTJORm2.sav", "BSTJPNm2.sav", "BSTKORm2.sav", "BSTLTUm2.sav", "BSTLVAm2.sav", "BSTMARm2.sav", "BSTMDAm2.sav", "BSTMKDm2.sav", "BSTMYSm2.sav", "BSTNLDm2.sav", "BSTNZLm2.sav", "BSTPHLm2.sav", "BSTROMm2.sav", "BSTRUSm2.sav", "BSTSGPm2.sav", "BSTSVKm2.sav", "BSTSVNm2.sav", "BSTTHAm2.sav", "BSTTUNm2.sav", "BSTTURm2.sav", "BSTTWNm2.sav", "BSTUSAm2.sav", "BSTZAFm2.sav", "BTMAUSm2.sav", "BTMBFLm2.sav", "BTMBGRm2.sav", "BTMCANm2.sav", "BTMCHLm2.sav", "BTMCYPm2.sav", "BTMCZEm2.sav", "BTMENGm2.sav", "BTMFINm2.sav", "BTMHKGm2.sav", "BTMHUNm2.sav", "BTMIDNm2.sav", "BTMIRNm2.sav", "BTMISRm2.sav", "BTMITAm2.sav", "BTMJORm2.sav", "BTMJPNm2.sav", "BTMKORm2.sav", "BTMLTUm2.sav", "BTMLVAm2.sav", "BTMMARm2.sav", "BTMMDAm2.sav", "BTMMKDm2.sav", "BTMMYSm2.sav", "BTMNLDm2.sav", "BTMNZLm2.sav", "BTMPHLm2.sav", "BTMROMm2.sav", "BTMRUSm2.sav", "BTMSGPm2.sav", "BTMSVKm2.sav", "BTMSVNm2.sav", "BTMTHAm2.sav", "BTMTUNm2.sav", "BTMTURm2.sav", "BTMTWNm2.sav", "BTMUSAm2.sav", "BTMZAFm2.sav", "BTSAUSm2.sav", "BTSBFLm2.sav", "BTSBGRm2.sav", "BTSCANm2.sav", "BTSCHLm2.sav", "BTSCYPm2.sav", "BTSCZEm2.sav", "BTSENGm2.sav", "BTSFINm2.sav", "BTSHKGm2.sav", "BTSHUNm2.sav", "BTSIDNm2.sav", "BTSIRNm2.sav", "BTSISRm2.sav", "BTSITAm2.sav", "BTSJORm2.sav", "BTSJPNm2.sav", "BTSKORm2.sav", "BTSLTUm2.sav", "BTSLVAm2.sav", "BTSMARm2.sav", "BTSMDAm2.sav", "BTSMKDm2.sav", "BTSMYSm2.sav", "BTSNLDm2.sav", "BTSNZLm2.sav", "BTSPHLm2.sav", "BTSROMm2.sav", "BTSRUSm2.sav", "BTSSGPm2.sav", "BTSSVKm2.sav", "BTSSVNm2.sav", "BTSTHAm2.sav", "BTSTUNm2.sav", "BTSTURm2.sav", "BTSTWNm2.sav", "BTSUSAm2.sav", "BTSZAFm2.sav")
      )),
    TIMSS_2003 = list(
      TIMSS_2003_G4 = list("TIMSS2003_IDB_SPSS_G4/Data",
                           c("ACGARMm3.sav", "ACGAUSm3.sav", "ACGBFLm3.sav", "ACGCOTm3.sav", "ACGCQUm3.sav", "ACGCYPm3.sav", "ACGENGm3.sav", "ACGHKGm3.sav", "ACGHUNm3.sav", "ACGIRNm3.sav", "ACGITAm3.sav", "ACGJPNm3.sav", "ACGLTUm3.sav", "ACGLVAm3.sav", "ACGMARm3.sav", "ACGMDAm3.sav", "ACGNLDm3.sav", "ACGNORm3.sav", "ACGNZLm3.sav", "ACGPHLm3.sav", "ACGRUSm3.sav", "ACGSCOm3.sav", "ACGSGPm3.sav", "ACGSVNm3.sav", "ACGTUNm3.sav", "ACGTWNm3.sav", "ACGUINm3.sav", "ACGUSAm3.sav", "ACGYEMm3.sav", "ASAARMm3.sav", "ASAAUSm3.sav", "ASABFLm3.sav", "ASACOTm3.sav", "ASACQUm3.sav", "ASACYPm3.sav", "ASAENGm3.sav", "ASAHKGm3.sav", "ASAHUNm3.sav", "ASAIRNm3.sav", "ASAITAm3.sav", "ASAJPNm3.sav", "ASALTUm3.sav", "ASALVAm3.sav", "ASAMARm3.sav", "ASAMDAm3.sav", "ASANLDm3.sav", "ASANORm3.sav", "ASANZLm3.sav", "ASAPHLm3.sav", "ASARUSm3.sav", "ASASCOm3.sav", "ASASGPm3.sav", "ASASVNm3.sav", "ASATUNm3.sav", "ASATWNm3.sav", "ASAUINm3.sav", "ASAUSAm3.sav", "ASAYEMm3.sav", "ASGARMm3.sav", "ASGAUSm3.sav", "ASGBFLm3.sav", "ASGCOTm3.sav", "ASGCQUm3.sav", "ASGCYPm3.sav", "ASGENGm3.sav", "ASGHKGm3.sav", "ASGHUNm3.sav", "ASGIRNm3.sav", "ASGITAm3.sav", "ASGJPNm3.sav", "ASGLTUm3.sav", "ASGLVAm3.sav", "ASGMARm3.sav", "ASGMDAm3.sav", "ASGNLDm3.sav", "ASGNORm3.sav", "ASGNZLm3.sav", "ASGPHLm3.sav", "ASGRUSm3.sav", "ASGSCOm3.sav", "ASGSGPm3.sav", "ASGSVNm3.sav", "ASGTUNm3.sav", "ASGTWNm3.sav", "ASGUINm3.sav", "ASGUSAm3.sav", "ASGYEMm3.sav", "ASRARMm3.sav", "ASRAUSm3.sav", "ASRBFLm3.sav", "ASRCOTm3.sav", "ASRCQUm3.sav", "ASRCYPm3.sav", "ASRENGm3.sav", "ASRHKGm3.sav", "ASRHUNm3.sav", "ASRIRNm3.sav", "ASRITAm3.sav", "ASRJPNm3.sav", "ASRLTUm3.sav", "ASRLVAm3.sav", "ASRMARm3.sav", "ASRMDAm3.sav", "ASRNLDm3.sav", "ASRNORm3.sav", "ASRNZLm3.sav", "ASRPHLm3.sav", "ASRRUSm3.sav", "ASRSCOm3.sav", "ASRSGPm3.sav", "ASRSVNm3.sav", "ASRTUNm3.sav", "ASRTWNm3.sav", "ASRUINm3.sav", "ASRUSAm3.sav", "ASRYEMm3.sav", "ASTARMm3.sav", "ASTAUSm3.sav", "ASTBFLm3.sav", "ASTCOTm3.sav", "ASTCQUm3.sav", "ASTCYPm3.sav", "ASTENGm3.sav", "ASTHKGm3.sav", "ASTHUNm3.sav", "ASTIRNm3.sav", "ASTITAm3.sav", "ASTJPNm3.sav", "ASTLTUm3.sav", "ASTLVAm3.sav", "ASTMARm3.sav", "ASTMDAm3.sav", "ASTNLDm3.sav", "ASTNORm3.sav", "ASTNZLm3.sav", "ASTPHLm3.sav", "ASTRUSm3.sav", "ASTSCOm3.sav", "ASTSGPm3.sav", "ASTSVNm3.sav", "ASTTUNm3.sav", "ASTTWNm3.sav", "ASTUINm3.sav", "ASTUSAm3.sav", "ASTYEMm3.sav", "ATGARMm3.sav", "ATGAUSm3.sav", "ATGBFLm3.sav", "ATGCOTm3.sav", "ATGCQUm3.sav", "ATGCYPm3.sav", "ATGENGm3.sav", "ATGHKGm3.sav", "ATGHUNm3.sav", "ATGIRNm3.sav", "ATGITAm3.sav", "ATGJPNm3.sav", "ATGLTUm3.sav", "ATGLVAm3.sav", "ATGMARm3.sav", "ATGMDAm3.sav", "ATGNLDm3.sav", "ATGNORm3.sav", "ATGNZLm3.sav", "ATGPHLm3.sav", "ATGRUSm3.sav", "ATGSCOm3.sav", "ATGSGPm3.sav", "ATGSVNm3.sav", "ATGTUNm3.sav", "ATGTWNm3.sav", "ATGUINm3.sav", "ATGUSAm3.sav", "ATGYEMm3.sav")
      ),
      TIMSS_2003_G8 = list("TIMSS2003_IDB_SPSS_G8/Data",
                           c("BCGARMm3.sav", "BCGAUSm3.sav", "BCGBFLm3.sav", "BCGBGRm3.sav", "BCGBHRm3.sav", "BCGBSQm3.sav", "BCGBWAm3.sav", "BCGCHLm3.sav", "BCGCOTm3.sav", "BCGCQUm3.sav", "BCGCYPm3.sav", "BCGEGYm3.sav", "BCGENGm3.sav", "BCGESTm3.sav", "BCGGHAm3.sav", "BCGHKGm3.sav", "BCGHUNm3.sav", "BCGIDNm3.sav", "BCGIRNm3.sav", "BCGISRm3.sav", "BCGITAm3.sav", "BCGJORm3.sav", "BCGJPNm3.sav", "BCGKORm3.sav", "BCGLBNm3.sav", "BCGLTUm3.sav", "BCGLVAm3.sav", "BCGMARm3.sav", "BCGMDAm3.sav", "BCGMKDm3.sav", "BCGMYSm3.sav", "BCGNLDm3.sav", "BCGNORm3.sav", "BCGNZLm3.sav", "BCGPHLm3.sav", "BCGPSEm3.sav", "BCGROMm3.sav", "BCGRUSm3.sav", "BCGSAUm3.sav", "BCGSCGm3.sav", "BCGSCOm3.sav", "BCGSGPm3.sav", "BCGSVKm3.sav", "BCGSVNm3.sav", "BCGSWEm3.sav", "BCGSYRm3.sav", "BCGTUNm3.sav", "BCGTWNm3.sav", "BCGUINm3.sav", "BCGUSAm3.sav", "BCGZAFm3.sav", "BSAARMm3.sav", "BSAAUSm3.sav", "BSABFLm3.sav", "BSABGRm3.sav", "BSABHRm3.sav", "BSABSQm3.sav", "BSABWAm3.sav", "BSACHLm3.sav", "BSACOTm3.sav", "BSACQUm3.sav", "BSACYPm3.sav", "BSAEGYm3.sav", "BSAENGm3.sav", "BSAESTm3.sav", "BSAGHAm3.sav", "BSAHKGm3.sav", "BSAHUNm3.sav", "BSAIDNm3.sav", "BSAIRNm3.sav", "BSAISRm3.sav", "BSAITAm3.sav", "BSAJORm3.sav", "BSAJPNm3.sav", "BSAKORm3.sav", "BSALBNm3.sav", "BSALTUm3.sav", "BSALVAm3.sav", "BSAMARm3.sav", "BSAMDAm3.sav", "BSAMKDm3.sav", "BSAMYSm3.sav", "BSANLDm3.sav", "BSANORm3.sav", "BSANZLm3.sav", "BSAPHLm3.sav", "BSAPSEm3.sav", "BSAROMm3.sav", "BSARUSm3.sav", "BSASAUm3.sav", "BSASCGm3.sav", "BSASCOm3.sav", "BSASGPm3.sav", "BSASVKm3.sav", "BSASVNm3.sav", "BSASWEm3.sav", "BSASYRm3.sav", "BSATUNm3.sav", "BSATWNm3.sav", "BSAUINm3.sav", "BSAUSAm3.sav", "BSAZAFm3.sav", "BSGARMm3.sav", "BSGAUSm3.sav", "BSGBFLm3.sav", "BSGBGRm3.sav", "BSGBHRm3.sav", "BSGBSQm3.sav", "BSGBWAm3.sav", "BSGCHLm3.sav", "BSGCOTm3.sav", "BSGCQUm3.sav", "BSGCYPm3.sav", "BSGEGYm3.sav", "BSGENGm3.sav", "BSGESTm3.sav", "BSGGHAm3.sav", "BSGHKGm3.sav", "BSGHUNm3.sav", "BSGIDNm3.sav", "BSGIRNm3.sav", "BSGISRm3.sav", "BSGITAm3.sav", "BSGJORm3.sav", "BSGJPNm3.sav", "BSGKORm3.sav", "BSGLBNm3.sav", "BSGLTUm3.sav", "BSGLVAm3.sav", "BSGMARm3.sav", "BSGMDAm3.sav", "BSGMKDm3.sav", "BSGMYSm3.sav", "BSGNLDm3.sav", "BSGNORm3.sav", "BSGNZLm3.sav", "BSGPHLm3.sav", "BSGPSEm3.sav", "BSGROMm3.sav", "BSGRUSm3.sav", "BSGSAUm3.sav", "BSGSCGm3.sav", "BSGSCOm3.sav", "BSGSGPm3.sav", "BSGSVKm3.sav", "BSGSVNm3.sav", "BSGSWEm3.sav", "BSGSYRm3.sav", "BSGTUNm3.sav", "BSGTWNm3.sav", "BSGUINm3.sav", "BSGUSAm3.sav", "BSGZAFm3.sav", "BSRARMm3.sav", "BSRAUSm3.sav", "BSRBFLm3.sav", "BSRBGRm3.sav", "BSRBHRm3.sav", "BSRBSQm3.sav", "BSRBWAm3.sav", "BSRCHLm3.sav", "BSRCOTm3.sav", "BSRCQUm3.sav", "BSRCYPm3.sav", "BSREGYm3.sav", "BSRENGm3.sav", "BSRESTm3.sav", "BSRGHAm3.sav", "BSRHKGm3.sav", "BSRHUNm3.sav", "BSRIDNm3.sav", "BSRIRNm3.sav", "BSRISRm3.sav", "BSRITAm3.sav", "BSRJORm3.sav", "BSRJPNm3.sav", "BSRKORm3.sav", "BSRLBNm3.sav", "BSRLTUm3.sav", "BSRLVAm3.sav", "BSRMARm3.sav", "BSRMDAm3.sav", "BSRMKDm3.sav", "BSRMYSm3.sav", "BSRNLDm3.sav", "BSRNORm3.sav", "BSRNZLm3.sav", "BSRPHLm3.sav", "BSRPSEm3.sav", "BSRROMm3.sav", "BSRRUSm3.sav", "BSRSAUm3.sav", "BSRSCGm3.sav", "BSRSCOm3.sav", "BSRSGPm3.sav", "BSRSVKm3.sav", "BSRSVNm3.sav", "BSRSWEm3.sav", "BSRSYRm3.sav", "BSRTUNm3.sav", "BSRTWNm3.sav", "BSRUINm3.sav", "BSRUSAm3.sav", "BSRZAFm3.sav", "BSTARMm3.sav", "BSTAUSm3.sav", "BSTBFLm3.sav", "BSTBGRm3.sav", "BSTBHRm3.sav", "BSTBSQm3.sav", "BSTBWAm3.sav", "BSTCHLm3.sav", "BSTCOTm3.sav", "BSTCQUm3.sav", "BSTCYPm3.sav", "BSTEGYm3.sav", "BSTENGm3.sav", "BSTESTm3.sav", "BSTGHAm3.sav", "BSTHKGm3.sav", "BSTHUNm3.sav", "BSTIDNm3.sav", "BSTIRNm3.sav", "BSTISRm3.sav", "BSTITAm3.sav", "BSTJORm3.sav", "BSTJPNm3.sav", "BSTKORm3.sav", "BSTLBNm3.sav", "BSTLTUm3.sav", "BSTLVAm3.sav", "BSTMARm3.sav", "BSTMDAm3.sav", "BSTMKDm3.sav", "BSTMYSm3.sav", "BSTNLDm3.sav", "BSTNORm3.sav", "BSTNZLm3.sav", "BSTPHLm3.sav", "BSTPSEm3.sav", "BSTROMm3.sav", "BSTRUSm3.sav", "BSTSAUm3.sav", "BSTSCGm3.sav", "BSTSCOm3.sav", "BSTSGPm3.sav", "BSTSVKm3.sav", "BSTSVNm3.sav", "BSTSWEm3.sav", "BSTSYRm3.sav", "BSTTUNm3.sav", "BSTTWNm3.sav", "BSTUINm3.sav", "BSTUSAm3.sav", "BSTZAFm3.sav", "BTMARMm3.sav", "BTMAUSm3.sav", "BTMBFLm3.sav", "BTMBGRm3.sav", "BTMBHRm3.sav", "BTMBSQm3.sav", "BTMBWAm3.sav", "BTMCHLm3.sav", "BTMCOTm3.sav", "BTMCQUm3.sav", "BTMCYPm3.sav", "BTMEGYm3.sav", "BTMENGm3.sav", "BTMESTm3.sav", "BTMGHAm3.sav", "BTMHKGm3.sav", "BTMHUNm3.sav", "BTMIDNm3.sav", "BTMIRNm3.sav", "BTMISRm3.sav", "BTMITAm3.sav", "BTMJORm3.sav", "BTMJPNm3.sav", "BTMKORm3.sav", "BTMLBNm3.sav", "BTMLTUm3.sav", "BTMLVAm3.sav", "BTMMARm3.sav", "BTMMDAm3.sav", "BTMMKDm3.sav", "BTMMYSm3.sav", "BTMNLDm3.sav", "BTMNORm3.sav", "BTMNZLm3.sav", "BTMPHLm3.sav", "BTMPSEm3.sav", "BTMROMm3.sav", "BTMRUSm3.sav", "BTMSAUm3.sav", "BTMSCGm3.sav", "BTMSCOm3.sav", "BTMSGPm3.sav", "BTMSVKm3.sav", "BTMSVNm3.sav", "BTMSWEm3.sav", "BTMSYRm3.sav", "BTMTUNm3.sav", "BTMTWNm3.sav", "BTMUINm3.sav", "BTMUSAm3.sav", "BTMZAFm3.sav", "BTSARMm3.sav", "BTSAUSm3.sav", "BTSBFLm3.sav", "BTSBGRm3.sav", "BTSBHRm3.sav", "BTSBSQm3.sav", "BTSBWAm3.sav", "BTSCHLm3.sav", "BTSCOTm3.sav", "BTSCQUm3.sav", "BTSCYPm3.sav", "BTSEGYm3.sav", "BTSENGm3.sav", "BTSESTm3.sav", "BTSGHAm3.sav", "BTSHKGm3.sav", "BTSHUNm3.sav", "BTSIDNm3.sav", "BTSIRNm3.sav", "BTSISRm3.sav", "BTSITAm3.sav", "BTSJORm3.sav", "BTSJPNm3.sav", "BTSKORm3.sav", "BTSLBNm3.sav", "BTSLTUm3.sav", "BTSLVAm3.sav", "BTSMARm3.sav", "BTSMDAm3.sav", "BTSMKDm3.sav", "BTSMYSm3.sav", "BTSNLDm3.sav", "BTSNORm3.sav", "BTSNZLm3.sav", "BTSPHLm3.sav", "BTSPSEm3.sav", "BTSROMm3.sav", "BTSRUSm3.sav", "BTSSAUm3.sav", "BTSSCGm3.sav", "BTSSCOm3.sav", "BTSSGPm3.sav", "BTSSVKm3.sav", "BTSSVNm3.sav", "BTSSWEm3.sav", "BTSSYRm3.sav", "BTSTUNm3.sav", "BTSTWNm3.sav", "BTSUINm3.sav", "BTSUSAm3.sav", "BTSZAFm3.sav")
      )),
    TIMSS_2007 = list(
      TIMSS_2007_G4 = list("TIMSS2007_IDB_SPSS_G4/Data",
                           c("acgadum4.sav", "acgarmm4.sav", "acgausm4.sav", "acgautm4.sav", "acgcabm4.sav", "acgcbcm4.sav", "acgcolm4.sav", "acgcotm4.sav", "acgcqum4.sav", "acgczem4.sav", "acgdeum4.sav", "acgdnkm4.sav", "acgdzam4.sav", "acgengm4.sav", "acggeom4.sav", "acghkgm4.sav", "acghunm4.sav", "acgirnm4.sav", "acgitam4.sav", "acgjpnm4.sav", "acgkazm4.sav", "acgkwtm4.sav", "acgltum4.sav", "acglvam4.sav", "acgmarm4.sav", "acgmngm4.sav", "acgnldm4.sav", "acgnorm4.sav", "acgnzlm4.sav", "acgqatm4.sav", "acgrusm4.sav", "acgscom4.sav", "acgsgpm4.sav", "acgslvm4.sav", "acgsvkm4.sav", "acgsvnm4.sav", "acgswem4.sav", "acgtunm4.sav", "acgtwnm4.sav", "acgukrm4.sav", "acgumam4.sav", "acgumnm4.sav", "acgusam4.sav", "acgyemm4.sav", "asaadum4.sav", "asaarmm4.sav", "asaausm4.sav", "asaautm4.sav", "asacabm4.sav", "asacbcm4.sav", "asacolm4.sav", "asacotm4.sav", "asacqum4.sav", "asaczem4.sav", "asadeum4.sav", "asadnkm4.sav", "asadzam4.sav", "asaengm4.sav", "asageom4.sav", "asahkgm4.sav", "asahunm4.sav", "asairnm4.sav", "asaitam4.sav", "asajpnm4.sav", "asakazm4.sav", "asakwtm4.sav", "asaltum4.sav", "asalvam4.sav", "asamarm4.sav", "asamngm4.sav", "asanldm4.sav", "asanorm4.sav", "asanzlm4.sav", "asaqatm4.sav", "asarusm4.sav", "asascom4.sav", "asasgpm4.sav", "asaslvm4.sav", "asasvkm4.sav", "asasvnm4.sav", "asaswem4.sav", "asatunm4.sav", "asatwnm4.sav", "asaukrm4.sav", "asaumam4.sav", "asaumnm4.sav", "asausam4.sav", "asayemm4.sav", "asgadum4.sav", "asgarmm4.sav", "asgausm4.sav", "asgautm4.sav", "asgcabm4.sav", "asgcbcm4.sav", "asgcolm4.sav", "asgcotm4.sav", "asgcqum4.sav", "asgczem4.sav", "asgdeum4.sav", "asgdnkm4.sav", "asgdzam4.sav", "asgengm4.sav", "asggeom4.sav", "asghkgm4.sav", "asghunm4.sav", "asgirnm4.sav", "asgitam4.sav", "asgjpnm4.sav", "asgkazm4.sav", "asgkwtm4.sav", "asgltum4.sav", "asglvam4.sav", "asgmarm4.sav", "asgmngm4.sav", "asgnldm4.sav", "asgnorm4.sav", "asgnzlm4.sav", "asgqatm4.sav", "asgrusm4.sav", "asgscom4.sav", "asgsgpm4.sav", "asgslvm4.sav", "asgsvkm4.sav", "asgsvnm4.sav", "asgswem4.sav", "asgtunm4.sav", "asgtwnm4.sav", "asgukrm4.sav", "asgumam4.sav", "asgumnm4.sav", "asgusam4.sav", "asgyemm4.sav", "asradum4.sav", "asrarmm4.sav", "asrausm4.sav", "asrautm4.sav", "asrcabm4.sav", "asrcbcm4.sav", "asrcolm4.sav", "asrcotm4.sav", "asrcqum4.sav", "asrczem4.sav", "asrdeum4.sav", "asrdnkm4.sav", "asrdzam4.sav", "asrengm4.sav", "asrgeom4.sav", "asrhkgm4.sav", "asrhunm4.sav", "asrirnm4.sav", "asritam4.sav", "asrjpnm4.sav", "asrkazm4.sav", "asrkwtm4.sav", "asrltum4.sav", "asrlvam4.sav", "asrmarm4.sav", "asrmngm4.sav", "asrnldm4.sav", "asrnorm4.sav", "asrnzlm4.sav", "asrqatm4.sav", "asrrusm4.sav", "asrscom4.sav", "asrsgpm4.sav", "asrslvm4.sav", "asrsvkm4.sav", "asrsvnm4.sav", "asrswem4.sav", "asrtunm4.sav", "asrtwnm4.sav", "asrukrm4.sav", "asrumam4.sav", "asrumnm4.sav", "asrusam4.sav", "asryemm4.sav", "astadum4.sav", "astarmm4.sav", "astausm4.sav", "astautm4.sav", "astcabm4.sav", "astcbcm4.sav", "astcolm4.sav", "astcotm4.sav", "astcqum4.sav", "astczem4.sav", "astdeum4.sav", "astdnkm4.sav", "astdzam4.sav", "astengm4.sav", "astgeom4.sav", "asthkgm4.sav", "asthunm4.sav", "astirnm4.sav", "astitam4.sav", "astjpnm4.sav", "astkazm4.sav", "astkwtm4.sav", "astltum4.sav", "astlvam4.sav", "astmarm4.sav", "astmngm4.sav", "astnldm4.sav", "astnorm4.sav", "astnzlm4.sav", "astqatm4.sav", "astrusm4.sav", "astscom4.sav", "astsgpm4.sav", "astslvm4.sav", "astsvkm4.sav", "astsvnm4.sav", "astswem4.sav", "asttunm4.sav", "asttwnm4.sav", "astukrm4.sav", "astumam4.sav", "astumnm4.sav", "astusam4.sav", "astyemm4.sav", "atgadum4.sav", "atgarmm4.sav", "atgausm4.sav", "atgautm4.sav", "atgcabm4.sav", "atgcbcm4.sav", "atgcolm4.sav", "atgcotm4.sav", "atgcqum4.sav", "atgczem4.sav", "atgdeum4.sav", "atgdnkm4.sav", "atgdzam4.sav", "atgengm4.sav", "atggeom4.sav", "atghkgm4.sav", "atghunm4.sav", "atgirnm4.sav", "atgitam4.sav", "atgjpnm4.sav", "atgkazm4.sav", "atgkwtm4.sav", "atgltum4.sav", "atglvam4.sav", "atgmarm4.sav", "atgmngm4.sav", "atgnldm4.sav", "atgnorm4.sav", "atgnzlm4.sav", "atgqatm4.sav", "atgrusm4.sav", "atgscom4.sav", "atgsgpm4.sav", "atgslvm4.sav", "atgsvkm4.sav", "atgsvnm4.sav", "atgswem4.sav", "atgtunm4.sav", "atgtwnm4.sav", "atgukrm4.sav", "atgumam4.sav", "atgumnm4.sav", "atgusam4.sav", "atgyemm4.sav")
      ),
      TIMSS_2007_G8 = list(
        "TIMSS2007_IDB_SPSS_G8/Data",
        c("bcgadum4.sav", "bcgarmm4.sav", "bcgausm4.sav", "bcgbgrm4.sav", "bcgbhrm4.sav", "bcgbihm4.sav", "bcgbsqm4.sav", "bcgbwam4.sav", "bcgcbcm4.sav", "bcgcolm4.sav", "bcgcotm4.sav", "bcgcqum4.sav", "bcgcypm4.sav", "bcgczem4.sav", "bcgdzam4.sav", "bcgegym4.sav", "bcgengm4.sav", "bcggeom4.sav", "bcggham4.sav", "bcghkgm4.sav", "bcghunm4.sav", "bcgidnm4.sav", "bcgirnm4.sav", "bcgisrm4.sav", "bcgitam4.sav", "bcgjorm4.sav", "bcgjpnm4.sav", "bcgkorm4.sav", "bcgkwtm4.sav", "bcglbnm4.sav", "bcgltum4.sav", "bcgmarm4.sav", "bcgmltm4.sav", "bcgmngm4.sav", "bcgmysm4.sav", "bcgnorm4.sav", "bcgomnm4.sav", "bcgpsem4.sav", "bcgqatm4.sav", "bcgromm4.sav", "bcgrusm4.sav", "bcgsaum4.sav", "bcgscgm4.sav", "bcgscom4.sav", "bcgsgpm4.sav", "bcgslvm4.sav", "bcgsvnm4.sav", "bcgswem4.sav", "bcgsyrm4.sav", "bcgtham4.sav", "bcgtunm4.sav", "bcgturm4.sav", "bcgtwnm4.sav", "bcgukrm4.sav", "bcgumam4.sav", "bcgumnm4.sav", "bcgusam4.sav", "bsaadum4.sav", "bsaarmm4.sav", "bsaausm4.sav", "bsabgrm4.sav", "bsabhrm4.sav", "bsabihm4.sav", "bsabsqm4.sav", "bsabwam4.sav", "bsacbcm4.sav", "bsacolm4.sav", "bsacotm4.sav", "bsacqum4.sav", "bsacypm4.sav", "bsaczem4.sav", "bsadzam4.sav", "bsaegym4.sav", "bsaengm4.sav", "bsageom4.sav", "bsagham4.sav", "bsahkgm4.sav", "bsahunm4.sav", "bsaidnm4.sav", "bsairnm4.sav", "bsaisrm4.sav", "bsaitam4.sav", "bsajorm4.sav", "bsajpnm4.sav", "bsakorm4.sav", "bsakwtm4.sav", "bsalbnm4.sav", "bsaltum4.sav", "bsamarm4.sav", "bsamltm4.sav", "bsamngm4.sav", "bsamysm4.sav", "bsanorm4.sav", "bsaomnm4.sav", "bsapsem4.sav", "bsaqatm4.sav", "bsaromm4.sav", "bsarusm4.sav", "bsasaum4.sav", "bsascgm4.sav", "bsascom4.sav", "bsasgpm4.sav", "bsaslvm4.sav", "bsasvnm4.sav", "bsaswem4.sav", "bsasyrm4.sav", "bsatham4.sav", "bsatunm4.sav", "bsaturm4.sav", "bsatwnm4.sav", "bsaukrm4.sav", "bsaumam4.sav", "bsaumnm4.sav", "bsausam4.sav", "bsgadum4.sav", "bsgarmm4.sav", "bsgausm4.sav", "bsgbgrm4.sav", "bsgbhrm4.sav", "bsgbihm4.sav", "bsgbsqm4.sav", "bsgbwam4.sav", "bsgcbcm4.sav", "bsgcolm4.sav", "bsgcotm4.sav", "bsgcqum4.sav", "bsgcypm4.sav", "bsgczem4.sav", "bsgdzam4.sav", "bsgegym4.sav", "bsgengm4.sav", "bsggeom4.sav", "bsggham4.sav", "bsghkgm4.sav", "bsghunm4.sav", "bsgidnm4.sav", "bsgirnm4.sav", "bsgisrm4.sav", "bsgitam4.sav", "bsgjorm4.sav", "bsgjpnm4.sav", "bsgkorm4.sav", "bsgkwtm4.sav", "bsglbnm4.sav", "bsgltum4.sav", "bsgmarm4.sav", "bsgmltm4.sav", "bsgmngm4.sav", "bsgmysm4.sav", "bsgnorm4.sav", "bsgomnm4.sav", "bsgpsem4.sav", "bsgqatm4.sav", "bsgromm4.sav", "bsgrusm4.sav", "bsgsaum4.sav", "bsgscgm4.sav", "bsgscom4.sav", "bsgsgpm4.sav", "bsgslvm4.sav", "bsgsvnm4.sav", "bsgswem4.sav", "bsgsyrm4.sav", "bsgtham4.sav", "bsgtunm4.sav", "bsgturm4.sav", "bsgtwnm4.sav", "bsgukrm4.sav", "bsgumam4.sav", "bsgumnm4.sav", "bsgusam4.sav", "bsradum4.sav", "bsrarmm4.sav", "bsrausm4.sav", "bsrbgrm4.sav", "bsrbhrm4.sav", "bsrbihm4.sav", "bsrbsqm4.sav", "bsrbwam4.sav", "bsrcbcm4.sav", "bsrcolm4.sav", "bsrcotm4.sav", "bsrcqum4.sav", "bsrczem4.sav", "bsrdzam4.sav", "bsregym4.sav", "bsrengm4.sav", "bsrgeom4.sav", "bsrgham4.sav", "bsrhkgm4.sav", "bsrhunm4.sav", "bsridnm4.sav", "bsrirnm4.sav", "bsrisrm4.sav", "bsritam4.sav", "bsrjorm4.sav", "bsrjpnm4.sav", "bsrkorm4.sav", "bsrkwtm4.sav", "bsrlbnm4.sav", "bsrltum4.sav", "bsrmarm4.sav", "bsrmltm4.sav", "bsrmngm4.sav", "bsrmysm4.sav", "bsrnorm4.sav", "bsromnm4.sav", "bsrpsem4.sav", "bsrqatm4.sav", "bsrromm4.sav", "bsrrusm4.sav", "bsrsaum4.sav", "bsrscgm4.sav", "bsrscom4.sav", "bsrsgpm4.sav", "bsrslvm4.sav", "bsrsvnm4.sav", "bsrswem4.sav", "bsrsyrm4.sav", "bsrtham4.sav", "bsrtunm4.sav", "bsrturm4.sav", "bsrtwnm4.sav", "bsrukrm4.sav", "bsrumam4.sav", "bsrumnm4.sav", "bsrusam4.sav", "bstadum4.sav", "bstarmm4.sav", "bstausm4.sav", "bstbgrm4.sav", "bstbhrm4.sav", "bstbihm4.sav", "bstbsqm4.sav", "bstbwam4.sav", "bstcbcm4.sav", "bstcolm4.sav", "bstcotm4.sav", "bstcqum4.sav", "bstcypm4.sav", "bstczem4.sav", "bstdzam4.sav", "bstegym4.sav", "bstengm4.sav", "bstgeom4.sav", "bstgham4.sav", "bsthkgm4.sav", "bsthunm4.sav", "bstidnm4.sav", "bstirnm4.sav", "bstisrm4.sav", "bstitam4.sav", "bstjorm4.sav", "bstjpnm4.sav", "bstkorm4.sav", "bstkwtm4.sav", "bstlbnm4.sav", "bstltum4.sav", "bstmarm4.sav", "bstmltm4.sav", "bstmngm4.sav", "bstmysm4.sav", "bstnorm4.sav", "bstomnm4.sav", "bstpsem4.sav", "bstqatm4.sav", "bstromm4.sav", "bstrusm4.sav", "bstsaum4.sav", "bstscgm4.sav", "bstscom4.sav", "bstsgpm4.sav", "bstslvm4.sav", "bstsvnm4.sav", "bstswem4.sav", "bstsyrm4.sav", "bsttham4.sav", "bsttunm4.sav", "bstturm4.sav", "bsttwnm4.sav", "bstukrm4.sav", "bstumam4.sav", "bstumnm4.sav", "bstusam4.sav", "btmadum4.sav", "btmarmm4.sav", "btmausm4.sav", "btmbgrm4.sav", "btmbhrm4.sav", "btmbihm4.sav", "btmbsqm4.sav", "btmbwam4.sav", "btmcbcm4.sav", "btmcolm4.sav", "btmcotm4.sav", "btmcqum4.sav", "btmcypm4.sav", "btmczem4.sav", "btmdzam4.sav", "btmegym4.sav", "btmengm4.sav", "btmgeom4.sav", "btmgham4.sav", "btmhkgm4.sav", "btmhunm4.sav", "btmidnm4.sav", "btmirnm4.sav", "btmisrm4.sav", "btmitam4.sav", "btmjorm4.sav", "btmjpnm4.sav", "btmkorm4.sav", "btmkwtm4.sav", "btmlbnm4.sav", "btmltum4.sav", "btmmarm4.sav", "btmmltm4.sav", "btmmngm4.sav", "btmmysm4.sav", "btmnorm4.sav", "btmomnm4.sav", "btmpsem4.sav", "btmqatm4.sav", "btmromm4.sav", "btmrusm4.sav", "btmsaum4.sav", "btmscgm4.sav", "btmscom4.sav", "btmsgpm4.sav", "btmslvm4.sav", "btmsvnm4.sav", "btmswem4.sav", "btmsyrm4.sav", "btmtham4.sav", "btmtunm4.sav", "btmturm4.sav", "btmtwnm4.sav", "btmukrm4.sav", "btmumam4.sav", "btmumnm4.sav", "btmusam4.sav", "btsadum4.sav", "btsarmm4.sav", "btsausm4.sav", "btsbgrm4.sav", "btsbhrm4.sav", "btsbihm4.sav", "btsbsqm4.sav", "btsbwam4.sav", "btscbcm4.sav", "btscolm4.sav", "btscotm4.sav", "btscqum4.sav", "btscypm4.sav", "btsczem4.sav", "btsdzam4.sav", "btsegym4.sav", "btsengm4.sav", "btsgeom4.sav", "btsgham4.sav", "btshkgm4.sav", "btshunm4.sav", "btsidnm4.sav", "btsirnm4.sav", "btsisrm4.sav", "btsitam4.sav", "btsjorm4.sav", "btsjpnm4.sav", "btskorm4.sav", "btskwtm4.sav", "btslbnm4.sav", "btsltum4.sav", "btsmarm4.sav", "btsmltm4.sav", "btsmngm4.sav", "btsmysm4.sav", "btsnorm4.sav", "btsomnm4.sav", "btspsem4.sav", "btsqatm4.sav", "btsromm4.sav", "btsrusm4.sav", "btssaum4.sav", "btsscgm4.sav", "btsscom4.sav", "btssgpm4.sav", "btsslvm4.sav", "btssvnm4.sav", "btsswem4.sav", "btssyrm4.sav", "btstham4.sav", "btstunm4.sav", "btsturm4.sav", "btstwnm4.sav", "btsukrm4.sav", "btsumam4.sav", "btsumnm4.sav", "btsusam4.sav")
      )),
    TIMSS_2011 = list(
      TIMSS_2011_G4 = list("TIMSS2011_IDB_SPSS_G4/Data",
                           c("acgaadm5.sav", "acgadum5.sav", "acgarem5.sav", "acgarmm5.sav", "acgausm5.sav", "acgautm5.sav", "acgazem5.sav", "acgbflm5.sav", "acgbhrm5.sav", "acgbwam5.sav", "acgcabm5.sav", "acgchlm5.sav", "acgcotm5.sav", "acgcqum5.sav", "acgczem5.sav", "acgdeum5.sav", "acgdnkm5.sav", "acgengm5.sav", "acgespm5.sav", "acgfinm5.sav", "acggeom5.sav", "acghkgm5.sav", "acghndm5.sav", "acghrvm5.sav", "acghunm5.sav", "acgirlm5.sav", "acgirnm5.sav", "acgitam5.sav", "acgjpnm5.sav", "acgkazm5.sav", "acgkorm5.sav", "acgkwtm5.sav", "acgltum5.sav", "acgmarm5.sav", "acgmltm5.sav", "acgnirm5.sav", "acgnldm5.sav", "acgnorm5.sav", "acgnzlm5.sav", "acgomnm5.sav", "acgpolm5.sav", "acgprtm5.sav", "acgqatm5.sav", "acgromm5.sav", "acgrusm5.sav", "acgsaum5.sav", "acgsgpm5.sav", "acgsrbm5.sav", "acgsvkm5.sav", "acgsvnm5.sav", "acgswem5.sav", "acgtham5.sav", "acgtunm5.sav", "acgturm5.sav", "acgtwnm5.sav", "acgusam5.sav", "acgye6m5.sav", "acgyemm5.sav", "asaaadm5.sav", "asaadum5.sav", "asaarem5.sav", "asaarmm5.sav", "asaausm5.sav", "asaautm5.sav", "asaazem5.sav", "asabflm5.sav", "asabhrm5.sav", "asabwam5.sav", "asacabm5.sav", "asachlm5.sav", "asacotm5.sav", "asacqum5.sav", "asaczem5.sav", "asadeum5.sav", "asadnkm5.sav", "asaengm5.sav", "asaespm5.sav", "asafinm5.sav", "asageom5.sav", "asahkgm5.sav", "asahndm5.sav", "asahrvm5.sav", "asahunm5.sav", "asairlm5.sav", "asairnm5.sav", "asaitam5.sav", "asajpnm5.sav", "asakazm5.sav", "asakorm5.sav", "asakwtm5.sav", "asaltum5.sav", "asamarm5.sav", "asamltm5.sav", "asanirm5.sav", "asanldm5.sav", "asanorm5.sav", "asanzlm5.sav", "asaomnm5.sav", "asapolm5.sav", "asaprtm5.sav", "asaqatm5.sav", "asaromm5.sav", "asarusm5.sav", "asasaum5.sav", "asasgpm5.sav", "asasrbm5.sav", "asasvkm5.sav", "asasvnm5.sav", "asaswem5.sav", "asatham5.sav", "asatunm5.sav", "asaturm5.sav", "asatwnm5.sav", "asausam5.sav", "asaye6m5.sav", "asayemm5.sav", "asgaadm5.sav", "asgadum5.sav", "asgarem5.sav", "asgarmm5.sav", "asgausm5.sav", "asgautm5.sav", "asgazem5.sav", "asgbflm5.sav", "asgbhrm5.sav", "asgbwam5.sav", "asgcabm5.sav", "asgchlm5.sav", "asgcotm5.sav", "asgcqum5.sav", "asgczem5.sav", "asgdeum5.sav", "asgdnkm5.sav", "asgengm5.sav", "asgespm5.sav", "asgfinm5.sav", "asggeom5.sav", "asghkgm5.sav", "asghndm5.sav", "asghrvm5.sav", "asghunm5.sav", "asgirlm5.sav", "asgirnm5.sav", "asgitam5.sav", "asgjpnm5.sav", "asgkazm5.sav", "asgkorm5.sav", "asgkwtm5.sav", "asgltum5.sav", "asgmarm5.sav", "asgmltm5.sav", "asgnirm5.sav", "asgnldm5.sav", "asgnorm5.sav", "asgnzlm5.sav", "asgomnm5.sav", "asgpolm5.sav", "asgprtm5.sav", "asgqatm5.sav", "asgromm5.sav", "asgrusm5.sav", "asgsaum5.sav", "asgsgpm5.sav", "asgsrbm5.sav", "asgsvkm5.sav", "asgsvnm5.sav", "asgswem5.sav", "asgtham5.sav", "asgtunm5.sav", "asgturm5.sav", "asgtwnm5.sav", "asgusam5.sav", "asgye6m5.sav", "asgyemm5.sav", "ashaadm5.sav", "ashadum5.sav", "asharem5.sav", "ashausm5.sav", "ashautm5.sav", "ashazem5.sav", "ashbwam5.sav", "ashcqum5.sav", "ashczem5.sav", "ashdeum5.sav", "ashespm5.sav", "ashfinm5.sav", "ashgeom5.sav", "ashhkgm5.sav", "ashhndm5.sav", "ashhrvm5.sav", "ashhunm5.sav", "ashirlm5.sav", "ashirnm5.sav", "ashitam5.sav", "ashltum5.sav", "ashmarm5.sav", "ashmltm5.sav", "ashnirm5.sav", "ashnorm5.sav", "ashomnm5.sav", "ashpolm5.sav", "ashprtm5.sav", "ashqatm5.sav", "ashromm5.sav", "ashrusm5.sav", "ashsaum5.sav", "ashsgpm5.sav", "ashsvkm5.sav", "ashsvnm5.sav", "ashswem5.sav", "ashtwnm5.sav", "asraadm5.sav", "asradum5.sav", "asrarem5.sav", "asrarmm5.sav", "asrausm5.sav", "asrautm5.sav", "asrazem5.sav", "asrbflm5.sav", "asrbhrm5.sav", "asrbwam5.sav", "asrcabm5.sav", "asrchlm5.sav", "asrcotm5.sav", "asrcqum5.sav", "asrczem5.sav", "asrdeum5.sav", "asrdnkm5.sav", "asrengm5.sav", "asrespm5.sav", "asrfinm5.sav", "asrgeom5.sav", "asrhkgm5.sav", "asrhndm5.sav", "asrhrvm5.sav", "asrhunm5.sav", "asrirlm5.sav", "asrirnm5.sav", "asritam5.sav", "asrjpnm5.sav", "asrkazm5.sav", "asrkorm5.sav", "asrkwtm5.sav", "asrltum5.sav", "asrmarm5.sav", "asrmltm5.sav", "asrnirm5.sav", "asrnldm5.sav", "asrnorm5.sav", "asrnzlm5.sav", "asromnm5.sav", "asrpolm5.sav", "asrprtm5.sav", "asrqatm5.sav", "asrromm5.sav", "asrrusm5.sav", "asrsaum5.sav", "asrsgpm5.sav", "asrsrbm5.sav", "asrsvkm5.sav", "asrsvnm5.sav", "asrswem5.sav", "asrtham5.sav", "asrtunm5.sav", "asrturm5.sav", "asrtwnm5.sav", "asrusam5.sav", "asrye6m5.sav", "asryemm5.sav", "astaadm5.sav", "astadum5.sav", "astarem5.sav", "astarmm5.sav", "astausm5.sav", "astautm5.sav", "astazem5.sav", "astbflm5.sav", "astbhrm5.sav", "astbwam5.sav", "astcabm5.sav", "astchlm5.sav", "astcotm5.sav", "astcqum5.sav", "astczem5.sav", "astdeum5.sav", "astdnkm5.sav", "astengm5.sav", "astespm5.sav", "astfinm5.sav", "astgeom5.sav", "asthkgm5.sav", "asthndm5.sav", "asthrvm5.sav", "asthunm5.sav", "astirlm5.sav", "astirnm5.sav", "astitam5.sav", "astjpnm5.sav", "astkazm5.sav", "astkorm5.sav", "astkwtm5.sav", "astltum5.sav", "astmarm5.sav", "astmltm5.sav", "astnirm5.sav", "astnldm5.sav", "astnorm5.sav", "astnzlm5.sav", "astomnm5.sav", "astpolm5.sav", "astprtm5.sav", "astqatm5.sav", "astromm5.sav", "astrusm5.sav", "astsaum5.sav", "astsgpm5.sav", "astsrbm5.sav", "astsvkm5.sav", "astsvnm5.sav", "astswem5.sav", "asttham5.sav", "asttunm5.sav", "astturm5.sav", "asttwnm5.sav", "astusam5.sav", "astye6m5.sav", "astyemm5.sav", "atgaadm5.sav", "atgadum5.sav", "atgarem5.sav", "atgarmm5.sav", "atgausm5.sav", "atgautm5.sav", "atgazem5.sav", "atgbflm5.sav", "atgbhrm5.sav", "atgbwam5.sav", "atgcabm5.sav", "atgchlm5.sav", "atgcotm5.sav", "atgcqum5.sav", "atgczem5.sav", "atgdeum5.sav", "atgdnkm5.sav", "atgengm5.sav", "atgespm5.sav", "atgfinm5.sav", "atggeom5.sav", "atghkgm5.sav", "atghndm5.sav", "atghrvm5.sav", "atghunm5.sav", "atgirlm5.sav", "atgirnm5.sav", "atgitam5.sav", "atgjpnm5.sav", "atgkazm5.sav", "atgkorm5.sav", "atgkwtm5.sav", "atgltum5.sav", "atgmarm5.sav", "atgmltm5.sav", "atgnirm5.sav", "atgnldm5.sav", "atgnorm5.sav", "atgnzlm5.sav", "atgomnm5.sav", "atgpolm5.sav", "atgprtm5.sav", "atgqatm5.sav", "atgromm5.sav", "atgrusm5.sav", "atgsaum5.sav", "atgsgpm5.sav", "atgsrbm5.sav", "atgsvkm5.sav", "atgsvnm5.sav", "atgswem5.sav", "atgtham5.sav", "atgtunm5.sav", "atgturm5.sav", "atgtwnm5.sav", "atgusam5.sav", "atgye6m5.sav", "atgyemm5.sav")
      ),
      TIMSS_2011_G8 = list(
        "TIMSS2011_IDB_SPSS_G8/Data",
        c("bcgaadm5.sav", "bcgadum5.sav", "bcgarem5.sav", "bcgarmm5.sav", "bcgausm5.sav", "bcgbhrm5.sav", "bcgbwam5.sav", "bcgcabm5.sav", "bcgchlm5.sav", "bcgcotm5.sav", "bcgcqum5.sav", "bcgengm5.sav", "bcgfinm5.sav", "bcggeom5.sav", "bcggham5.sav", "bcghkgm5.sav", "bcghndm5.sav", "bcghunm5.sav", "bcgidnm5.sav", "bcgirnm5.sav", "bcgisrm5.sav", "bcgitam5.sav", "bcgjorm5.sav", "bcgjpnm5.sav", "bcgkazm5.sav", "bcgkorm5.sav", "bcglbnm5.sav", "bcgltum5.sav", "bcgmarm5.sav", "bcgmkdm5.sav", "bcgmysm5.sav", "bcgnorm5.sav", "bcgnzlm5.sav", "bcgomnm5.sav", "bcgpsem5.sav", "bcgqatm5.sav", "bcgromm5.sav", "bcgrusm5.sav", "bcgsaum5.sav", "bcgsgpm5.sav", "bcgsvnm5.sav", "bcgswem5.sav", "bcgsyrm5.sav", "bcgtham5.sav", "bcgtunm5.sav", "bcgturm5.sav", "bcgtwnm5.sav", "bcgukrm5.sav", "bcgusam5.sav", "bcgzafm5.sav", "bsaaadm5.sav", "bsaadum5.sav", "bsaarem5.sav", "bsaarmm5.sav", "bsaausm5.sav", "bsabhrm5.sav", "bsabwam5.sav", "bsacabm5.sav", "bsachlm5.sav", "bsacotm5.sav", "bsacqum5.sav", "bsaengm5.sav", "bsafinm5.sav", "bsageom5.sav", "bsagham5.sav", "bsahkgm5.sav", "bsahndm5.sav", "bsahunm5.sav", "bsaidnm5.sav", "bsairnm5.sav", "bsaisrm5.sav", "bsaitam5.sav", "bsajorm5.sav", "bsajpnm5.sav", "bsakazm5.sav", "bsakorm5.sav", "bsalbnm5.sav", "bsaltum5.sav", "bsamarm5.sav", "bsamkdm5.sav", "bsamysm5.sav", "bsanorm5.sav", "bsanzlm5.sav", "bsaomnm5.sav", "bsapsem5.sav", "bsaqatm5.sav", "bsaromm5.sav", "bsarusm5.sav", "bsasaum5.sav", "bsasgpm5.sav", "bsasvnm5.sav", "bsaswem5.sav", "bsasyrm5.sav", "bsatham5.sav", "bsatunm5.sav", "bsaturm5.sav", "bsatwnm5.sav", "bsaukrm5.sav", "bsausam5.sav", "bsazafm5.sav", "bsgaadm5.sav", "bsgadum5.sav", "bsgarem5.sav", "bsgarmm5.sav", "bsgausm5.sav", "bsgbhrm5.sav", "bsgbwam5.sav", "bsgcabm5.sav", "bsgchlm5.sav", "bsgcotm5.sav", "bsgcqum5.sav", "bsgengm5.sav", "bsgfinm5.sav", "bsggeom5.sav", "bsggham5.sav", "bsghkgm5.sav", "bsghndm5.sav", "bsghunm5.sav", "bsgidnm5.sav", "bsgirnm5.sav", "bsgisrm5.sav", "bsgitam5.sav", "bsgjorm5.sav", "bsgjpnm5.sav", "bsgkazm5.sav", "bsgkorm5.sav", "bsglbnm5.sav", "bsgltum5.sav", "bsgmarm5.sav", "bsgmkdm5.sav", "bsgmysm5.sav", "bsgnorm5.sav", "bsgnzlm5.sav", "bsgomnm5.sav", "bsgpsem5.sav", "bsgqatm5.sav", "bsgromm5.sav", "bsgrusm5.sav", "bsgsaum5.sav", "bsgsgpm5.sav", "bsgsvnm5.sav", "bsgswem5.sav", "bsgsyrm5.sav", "bsgtham5.sav", "bsgtunm5.sav", "bsgturm5.sav", "bsgtwnm5.sav", "bsgukrm5.sav", "bsgusam5.sav", "bsgzafm5.sav", "bsraadm5.sav", "bsradum5.sav", "bsrarem5.sav", "bsrarmm5.sav", "bsrausm5.sav", "bsrbhrm5.sav", "bsrbwam5.sav", "bsrcabm5.sav", "bsrchlm5.sav", "bsrcotm5.sav", "bsrcqum5.sav", "bsrengm5.sav", "bsrfinm5.sav", "bsrgeom5.sav", "bsrgham5.sav", "bsrhkgm5.sav", "bsrhndm5.sav", "bsrhunm5.sav", "bsridnm5.sav", "bsrirnm5.sav", "bsrisrm5.sav", "bsritam5.sav", "bsrjorm5.sav", "bsrjpnm5.sav", "bsrkazm5.sav", "bsrkorm5.sav", "bsrlbnm5.sav", "bsrltum5.sav", "bsrmarm5.sav", "bsrmkdm5.sav", "bsrmysm5.sav", "bsrnorm5.sav", "bsrnzlm5.sav", "bsromnm5.sav", "bsrpsem5.sav", "bsrqatm5.sav", "bsrromm5.sav", "bsrrusm5.sav", "bsrsaum5.sav", "bsrsgpm5.sav", "bsrsvnm5.sav", "bsrswem5.sav", "bsrsyrm5.sav", "bsrtham5.sav", "bsrtunm5.sav", "bsrturm5.sav", "bsrtwnm5.sav", "bsrukrm5.sav", "bsrusam5.sav", "bsrzafm5.sav", "bstaadm5.sav", "bstadum5.sav", "bstarem5.sav", "bstarmm5.sav", "bstausm5.sav", "bstbhrm5.sav", "bstbwam5.sav", "bstcabm5.sav", "bstchlm5.sav", "bstcotm5.sav", "bstcqum5.sav", "bstengm5.sav", "bstfinm5.sav", "bstgeom5.sav", "bstgham5.sav", "bsthkgm5.sav", "bsthndm5.sav", "bsthunm5.sav", "bstidnm5.sav", "bstirnm5.sav", "bstisrm5.sav", "bstitam5.sav", "bstjorm5.sav", "bstjpnm5.sav", "bstkazm5.sav", "bstkorm5.sav", "bstlbnm5.sav", "bstltum5.sav", "bstmarm5.sav", "bstmkdm5.sav", "bstmysm5.sav", "bstnorm5.sav", "bstnzlm5.sav", "bstomnm5.sav", "bstpsem5.sav", "bstqatm5.sav", "bstromm5.sav", "bstrusm5.sav", "bstsaum5.sav", "bstsgpm5.sav", "bstsvnm5.sav", "bstswem5.sav", "bstsyrm5.sav", "bsttham5.sav", "bsttunm5.sav", "bstturm5.sav", "bsttwnm5.sav", "bstukrm5.sav", "bstusam5.sav", "bstzafm5.sav", "btmaadm5.sav", "btmadum5.sav", "btmarem5.sav", "btmarmm5.sav", "btmausm5.sav", "btmbhrm5.sav", "btmbwam5.sav", "btmcabm5.sav", "btmchlm5.sav", "btmcotm5.sav", "btmcqum5.sav", "btmengm5.sav", "btmfinm5.sav", "btmgeom5.sav", "btmgham5.sav", "btmhkgm5.sav", "btmhndm5.sav", "btmhunm5.sav", "btmidnm5.sav", "btmirnm5.sav", "btmisrm5.sav", "btmitam5.sav", "btmjorm5.sav", "btmjpnm5.sav", "btmkazm5.sav", "btmkorm5.sav", "btmlbnm5.sav", "btmltum5.sav", "btmmarm5.sav", "btmmkdm5.sav", "btmmysm5.sav", "btmnorm5.sav", "btmnzlm5.sav", "btmomnm5.sav", "btmpsem5.sav", "btmqatm5.sav", "btmromm5.sav", "btmrusm5.sav", "btmsaum5.sav", "btmsgpm5.sav", "btmsvnm5.sav", "btmswem5.sav", "btmsyrm5.sav", "btmtham5.sav", "btmtunm5.sav", "btmturm5.sav", "btmtwnm5.sav", "btmukrm5.sav", "btmusam5.sav", "btmzafm5.sav", "btsaadm5.sav", "btsadum5.sav", "btsarem5.sav", "btsarmm5.sav", "btsausm5.sav", "btsbhrm5.sav", "btsbwam5.sav", "btscabm5.sav", "btschlm5.sav", "btscotm5.sav", "btscqum5.sav", "btsengm5.sav", "btsfinm5.sav", "btsgeom5.sav", "btsgham5.sav", "btshkgm5.sav", "btshndm5.sav", "btshunm5.sav", "btsidnm5.sav", "btsirnm5.sav", "btsisrm5.sav", "btsitam5.sav", "btsjorm5.sav", "btsjpnm5.sav", "btskazm5.sav", "btskorm5.sav", "btslbnm5.sav", "btsltum5.sav", "btsmarm5.sav", "btsmkdm5.sav", "btsmysm5.sav", "btsnorm5.sav", "btsnzlm5.sav", "btsomnm5.sav", "btspsem5.sav", "btsqatm5.sav", "btsromm5.sav", "btsrusm5.sav", "btssaum5.sav", "btssgpm5.sav", "btssvnm5.sav", "btsswem5.sav", "btssyrm5.sav", "btstham5.sav", "btstunm5.sav", "btsturm5.sav", "btstwnm5.sav", "btsukrm5.sav", "btsusam5.sav", "btszafm5.sav")
      )),
    TIMSS_2015 = list(
      TIMSS_2015_G4 = list("TIMSS2015_IDB_SPSS_G4/Data",
                           c("ACGAADM6.sav", "ACGABAM6.sav", "ACGADUM6.sav", "ACGAREM6.sav", "ACGARMM6.sav", "ACGAUSM6.sav", "ACGBFLM6.sav", "ACGBGRM6.sav", "ACGBHRM6.sav", "ACGCANM6.sav", "ACGCHLM6.sav", "ACGCOTM6.sav", "ACGCQUM6.sav", "ACGCYPM6.sav", "ACGCZEM6.sav", "ACGDEUM6.sav", "ACGDNKM6.sav", "ACGENGM6.sav", "ACGESPM6.sav", "ACGFINM6.sav", "ACGFRAM6.sav", "ACGGEOM6.sav", "ACGHKGM6.sav", "ACGHRVM6.sav", "ACGHUNM6.sav", "ACGIDNM6.sav", "ACGIRLM6.sav", "ACGIRNM6.sav", "ACGITAM6.sav", "ACGJPNM6.sav", "ACGKAZM6.sav", "ACGKORM6.sav", "ACGKWTM6.sav", "ACGLTUM6.sav", "ACGMARM6.sav", "ACGNIRM6.sav", "ACGNLDM6.sav", "ACGNO4M6.sav", "ACGNORM6.sav", "ACGNZLM6.sav", "ACGOMNM6.sav", "ACGPOLM6.sav", "ACGPRTM6.sav", "ACGQATM6.sav", "ACGRUSM6.sav", "ACGSAUM6.sav", "ACGSGPM6.sav", "ACGSRBM6.sav", "ACGSVKM6.sav", "ACGSVNM6.sav", "ACGSWEM6.sav", "ACGTURM6.sav", "ACGTWNM6.sav", "ACGUSAM6.sav", "ASAAADM6.sav", "ASAABAM6.sav", "ASAADUM6.sav", "ASAAREM6.sav", "ASAARMM6.sav", "ASAAUSM6.sav", "ASABFLM6.sav", "ASABGRM6.sav", "ASABHRM6.sav", "ASACANM6.sav", "ASACHLM6.sav", "ASACOTM6.sav", "ASACQUM6.sav", "ASACYPM6.sav", "ASACZEM6.sav", "ASADEUM6.sav", "ASADNKM6.sav", "ASAENGM6.sav", "ASAESPM6.sav", "ASAFINM6.sav", "ASAFRAM6.sav", "ASAGEOM6.sav", "ASAHKGM6.sav", "ASAHRVM6.sav", "ASAHUNM6.sav", "ASAIDNM6.sav", "ASAIRLM6.sav", "ASAIRNM6.sav", "ASAITAM6.sav", "ASAJPNM6.sav", "ASAKAZM6.sav", "ASAKORM6.sav", "ASAKWTM6.sav", "ASALTUM6.sav", "ASAMARM6.sav", "ASANIRM6.sav", "ASANLDM6.sav", "ASANO4M6.sav", "ASANORM6.sav", "ASANZLM6.sav", "ASAOMNM6.sav", "ASAPOLM6.sav", "ASAPRTM6.sav", "ASAQATM6.sav", "ASARUSM6.sav", "ASASAUM6.sav", "ASASGPM6.sav", "ASASRBM6.sav", "ASASVKM6.sav", "ASASVNM6.sav", "ASASWEM6.sav", "ASATURM6.sav", "ASATWNM6.sav", "ASAUSAM6.sav", "ASGAADM6.sav", "ASGABAM6.sav", "ASGADUM6.sav", "ASGAREM6.sav", "ASGARMM6.sav", "ASGAUSM6.sav", "ASGBFLM6.sav", "ASGBGRM6.sav", "ASGBHRM6.sav", "ASGCANM6.sav", "ASGCHLM6.sav", "ASGCOTM6.sav", "ASGCQUM6.sav", "ASGCYPM6.sav", "ASGCZEM6.sav", "ASGDEUM6.sav", "ASGDNKM6.sav", "ASGENGM6.sav", "ASGESPM6.sav", "ASGFINM6.sav", "ASGFRAM6.sav", "ASGGEOM6.sav", "ASGHKGM6.sav", "ASGHRVM6.sav", "ASGHUNM6.sav", "ASGIDNM6.sav", "ASGIRLM6.sav", "ASGIRNM6.sav", "ASGITAM6.sav", "ASGJPNM6.sav", "ASGKAZM6.sav", "ASGKORM6.sav", "ASGKWTM6.sav", "ASGLTUM6.sav", "ASGMARM6.sav", "ASGNIRM6.sav", "ASGNLDM6.sav", "ASGNO4M6.sav", "ASGNORM6.sav", "ASGNZLM6.sav", "ASGOMNM6.sav", "ASGPOLM6.sav", "ASGPRTM6.sav", "ASGQATM6.sav", "ASGRUSM6.sav", "ASGSAUM6.sav", "ASGSGPM6.sav", "ASGSRBM6.sav", "ASGSVKM6.sav", "ASGSVNM6.sav", "ASGSWEM6.sav", "ASGTURM6.sav", "ASGTWNM6.sav", "ASGUSAM6.sav", "ASHAADM6.sav", "ASHABAM6.sav", "ASHADUM6.sav", "ASHAREM6.sav", "ASHARMM6.sav", "ASHAUSM6.sav", "ASHBFLM6.sav", "ASHBGRM6.sav", "ASHBHRM6.sav", "ASHCANM6.sav", "ASHCHLM6.sav", "ASHCOTM6.sav", "ASHCQUM6.sav", "ASHCYPM6.sav", "ASHCZEM6.sav", "ASHDEUM6.sav", "ASHDNKM6.sav", "ASHENGM6.sav", "ASHESPM6.sav", "ASHFINM6.sav", "ASHFRAM6.sav", "ASHGEOM6.sav", "ASHHKGM6.sav", "ASHHRVM6.sav", "ASHHUNM6.sav", "ASHIDNM6.sav", "ASHIRLM6.sav", "ASHIRNM6.sav", "ASHITAM6.sav", "ASHJPNM6.sav", "ASHKAZM6.sav", "ASHKORM6.sav", "ASHKWTM6.sav", "ASHLTUM6.sav", "ASHMARM6.sav", "ASHNIRM6.sav", "ASHNLDM6.sav", "ASHNO4M6.sav", "ASHNORM6.sav", "ASHNZLM6.sav", "ASHOMNM6.sav", "ASHPOLM6.sav", "ASHPRTM6.sav", "ASHQATM6.sav", "ASHRUSM6.sav", "ASHSAUM6.sav", "ASHSGPM6.sav", "ASHSRBM6.sav", "ASHSVKM6.sav", "ASHSVNM6.sav", "ASHSWEM6.sav", "ASHTURM6.sav", "ASHTWNM6.sav", "ASHUSAM6.sav", "ASRAADM6.sav", "ASRABAM6.sav", "ASRADUM6.sav", "ASRAREM6.sav", "ASRARMM6.sav", "ASRAUSM6.sav", "ASRBFLM6.sav", "ASRBGRM6.sav", "ASRBHRM6.sav", "ASRCANM6.sav", "ASRCHLM6.sav", "ASRCOTM6.sav", "ASRCQUM6.sav", "ASRCYPM6.sav", "ASRCZEM6.sav", "ASRDEUM6.sav", "ASRDNKM6.sav", "ASRENGM6.sav", "ASRESPM6.sav", "ASRFINM6.sav", "ASRFRAM6.sav", "ASRGEOM6.sav", "ASRHKGM6.sav", "ASRHRVM6.sav", "ASRHUNM6.sav", "ASRIDNM6.sav", "ASRIRLM6.sav", "ASRIRNM6.sav", "ASRITAM6.sav", "ASRJPNM6.sav", "ASRKAZM6.sav", "ASRKORM6.sav", "ASRKWTM6.sav", "ASRLTUM6.sav", "ASRMARM6.sav", "ASRNIRM6.sav", "ASRNLDM6.sav", "ASRNO4M6.sav", "ASRNORM6.sav", "ASRNZLM6.sav", "ASROMNM6.sav", "ASRPOLM6.sav", "ASRPRTM6.sav", "ASRQATM6.sav", "ASRRUSM6.sav", "ASRSAUM6.sav", "ASRSGPM6.sav", "ASRSRBM6.sav", "ASRSVKM6.sav", "ASRSVNM6.sav", "ASRSWEM6.sav", "ASRTURM6.sav", "ASRTWNM6.sav", "ASRUSAM6.sav", "ASTAADM6.sav", "ASTABAM6.sav", "ASTADUM6.sav", "ASTAREM6.sav", "ASTARMM6.sav", "ASTAUSM6.sav", "ASTBFLM6.sav", "ASTBGRM6.sav", "ASTBHRM6.sav", "ASTCANM6.sav", "ASTCHLM6.sav", "ASTCOTM6.sav", "ASTCQUM6.sav", "ASTCYPM6.sav", "ASTCZEM6.sav", "ASTDEUM6.sav", "ASTDNKM6.sav", "ASTENGM6.sav", "ASTESPM6.sav", "ASTFINM6.sav", "ASTFRAM6.sav", "ASTGEOM6.sav", "ASTHKGM6.sav", "ASTHRVM6.sav", "ASTHUNM6.sav", "ASTIDNM6.sav", "ASTIRLM6.sav", "ASTIRNM6.sav", "ASTITAM6.sav", "ASTJPNM6.sav", "ASTKAZM6.sav", "ASTKORM6.sav", "ASTKWTM6.sav", "ASTLTUM6.sav", "ASTMARM6.sav", "ASTNIRM6.sav", "ASTNLDM6.sav", "ASTNO4M6.sav", "ASTNORM6.sav", "ASTNZLM6.sav", "ASTOMNM6.sav", "ASTPOLM6.sav", "ASTPRTM6.sav", "ASTQATM6.sav", "ASTRUSM6.sav", "ASTSAUM6.sav", "ASTSGPM6.sav", "ASTSRBM6.sav", "ASTSVKM6.sav", "ASTSVNM6.sav", "ASTSWEM6.sav", "ASTTURM6.sav", "ASTTWNM6.sav", "ASTUSAM6.sav", "ATGAADM6.sav", "ATGABAM6.sav", "ATGADUM6.sav", "ATGAREM6.sav", "ATGARMM6.sav", "ATGAUSM6.sav", "ATGBFLM6.sav", "ATGBGRM6.sav", "ATGBHRM6.sav", "ATGCANM6.sav", "ATGCHLM6.sav", "ATGCOTM6.sav", "ATGCQUM6.sav", "ATGCYPM6.sav", "ATGCZEM6.sav", "ATGDEUM6.sav", "ATGDNKM6.sav", "ATGENGM6.sav", "ATGESPM6.sav", "ATGFINM6.sav", "ATGFRAM6.sav", "ATGGEOM6.sav", "ATGHKGM6.sav", "ATGHRVM6.sav", "ATGHUNM6.sav", "ATGIDNM6.sav", "ATGIRLM6.sav", "ATGIRNM6.sav", "ATGITAM6.sav", "ATGJPNM6.sav", "ATGKAZM6.sav", "ATGKORM6.sav", "ATGKWTM6.sav", "ATGLTUM6.sav", "ATGMARM6.sav", "ATGNIRM6.sav", "ATGNLDM6.sav", "ATGNO4M6.sav", "ATGNORM6.sav", "ATGNZLM6.sav", "ATGOMNM6.sav", "ATGPOLM6.sav", "ATGPRTM6.sav", "ATGQATM6.sav", "ATGRUSM6.sav", "ATGSAUM6.sav", "ATGSGPM6.sav", "ATGSRBM6.sav", "ATGSVKM6.sav", "ATGSVNM6.sav", "ATGSWEM6.sav", "ATGTURM6.sav", "ATGTWNM6.sav", "ATGUSAM6.sav")
      ),
      TIMSS_2015_G8 = list(
        "TIMSS2015_IDB_SPSS_G8/Data",
        c("BCGAADM6.sav", "BCGABAM6.sav", "BCGADUM6.sav", "BCGAREM6.sav", "BCGARMM6.sav", "BCGAUSM6.sav", "BCGBHRM6.sav", "BCGBWAM6.sav", "BCGCANM6.sav", "BCGCHLM6.sav", "BCGCOTM6.sav", "BCGCQUM6.sav", "BCGEGYM6.sav", "BCGENGM6.sav", "BCGGEOM6.sav", "BCGHKGM6.sav", "BCGHUNM6.sav", "BCGIRLM6.sav", "BCGIRNM6.sav", "BCGISRM6.sav", "BCGITAM6.sav", "BCGJORM6.sav", "BCGJPNM6.sav", "BCGKAZM6.sav", "BCGKORM6.sav", "BCGKWTM6.sav", "BCGLBNM6.sav", "BCGLTUM6.sav", "BCGMARM6.sav", "BCGMLTM6.sav", "BCGMYSM6.sav", "BCGNO8M6.sav", "BCGNORM6.sav", "BCGNZLM6.sav", "BCGOMNM6.sav", "BCGQATM6.sav", "BCGRUSM6.sav", "BCGSAUM6.sav", "BCGSGPM6.sav", "BCGSVNM6.sav", "BCGSWEM6.sav", "BCGTHAM6.sav", "BCGTURM6.sav", "BCGTWNM6.sav", "BCGUSAM6.sav", "BCGZAFM6.sav", "BSAAADM6.sav", "BSAABAM6.sav", "BSAADUM6.sav", "BSAAREM6.sav", "BSAARMM6.sav", "BSAAUSM6.sav", "BSABHRM6.sav", "BSABWAM6.sav", "BSACANM6.sav", "BSACHLM6.sav", "BSACOTM6.sav", "BSACQUM6.sav", "BSAEGYM6.sav", "BSAENGM6.sav", "BSAGEOM6.sav", "BSAHKGM6.sav", "BSAHUNM6.sav", "BSAIRLM6.sav", "BSAIRNM6.sav", "BSAISRM6.sav", "BSAITAM6.sav", "BSAJORM6.sav", "BSAJPNM6.sav", "BSAKAZM6.sav", "BSAKORM6.sav", "BSAKWTM6.sav", "BSALBNM6.sav", "BSALTUM6.sav", "BSAMARM6.sav", "BSAMLTM6.sav", "BSAMYSM6.sav", "BSANO8M6.sav", "BSANORM6.sav", "BSANZLM6.sav", "BSAOMNM6.sav", "BSAQATM6.sav", "BSARUSM6.sav", "BSASAUM6.sav", "BSASGPM6.sav", "BSASVNM6.sav", "BSASWEM6.sav", "BSATHAM6.sav", "BSATURM6.sav", "BSATWNM6.sav", "BSAUSAM6.sav", "BSAZAFM6.sav", "BSGAADM6.sav", "BSGABAM6.sav", "BSGADUM6.sav", "BSGAREM6.sav", "BSGARMM6.sav", "BSGAUSM6.sav", "BSGBHRM6.sav", "BSGBWAM6.sav", "BSGCANM6.sav", "BSGCHLM6.sav", "BSGCOTM6.sav", "BSGCQUM6.sav", "BSGEGYM6.sav", "BSGENGM6.sav", "BSGGEOM6.sav", "BSGHKGM6.sav", "BSGHUNM6.sav", "BSGIRLM6.sav", "BSGIRNM6.sav", "BSGISRM6.sav", "BSGITAM6.sav", "BSGJORM6.sav", "BSGJPNM6.sav", "BSGKAZM6.sav", "BSGKORM6.sav", "BSGKWTM6.sav", "BSGLBNM6.sav", "BSGLTUM6.sav", "BSGMARM6.sav", "BSGMLTM6.sav", "BSGMYSM6.sav", "BSGNO8M6.sav", "BSGNORM6.sav", "BSGNZLM6.sav", "BSGOMNM6.sav", "BSGQATM6.sav", "BSGRUSM6.sav", "BSGSAUM6.sav", "BSGSGPM6.sav", "BSGSVNM6.sav", "BSGSWEM6.sav", "BSGTHAM6.sav", "BSGTURM6.sav", "BSGTWNM6.sav", "BSGUSAM6.sav", "BSGZAFM6.sav", "BSRAADM6.sav", "BSRABAM6.sav", "BSRADUM6.sav", "BSRAREM6.sav", "BSRARMM6.sav", "BSRAUSM6.sav", "BSRBHRM6.sav", "BSRBWAM6.sav", "BSRCANM6.sav", "BSRCHLM6.sav", "BSRCOTM6.sav", "BSRCQUM6.sav", "BSREGYM6.sav", "BSRENGM6.sav", "BSRGEOM6.sav", "BSRHKGM6.sav", "BSRHUNM6.sav", "BSRIRLM6.sav", "BSRIRNM6.sav", "BSRISRM6.sav", "BSRITAM6.sav", "BSRJORM6.sav", "BSRJPNM6.sav", "BSRKAZM6.sav", "BSRKORM6.sav", "BSRKWTM6.sav", "BSRLBNM6.sav", "BSRLTUM6.sav", "BSRMARM6.sav", "BSRMLTM6.sav", "BSRMYSM6.sav", "BSRNO8M6.sav", "BSRNORM6.sav", "BSRNZLM6.sav", "BSROMNM6.sav", "BSRQATM6.sav", "BSRRUSM6.sav", "BSRSAUM6.sav", "BSRSGPM6.sav", "BSRSVNM6.sav", "BSRSWEM6.sav", "BSRTHAM6.sav", "BSRTURM6.sav", "BSRTWNM6.sav", "BSRUSAM6.sav", "BSRZAFM6.sav", "BSTAADM6.sav", "BSTABAM6.sav", "BSTADUM6.sav", "BSTAREM6.sav", "BSTARMM6.sav", "BSTAUSM6.sav", "BSTBHRM6.sav", "BSTBWAM6.sav", "BSTCANM6.sav", "BSTCHLM6.sav", "BSTCOTM6.sav", "BSTCQUM6.sav", "BSTEGYM6.sav", "BSTENGM6.sav", "BSTGEOM6.sav", "BSTHKGM6.sav", "BSTHUNM6.sav", "BSTIRLM6.sav", "BSTIRNM6.sav", "BSTISRM6.sav", "BSTITAM6.sav", "BSTJORM6.sav", "BSTJPNM6.sav", "BSTKAZM6.sav", "BSTKORM6.sav", "BSTKWTM6.sav", "BSTLBNM6.sav", "BSTLTUM6.sav", "BSTMARM6.sav", "BSTMLTM6.sav", "BSTMYSM6.sav", "BSTNO8M6.sav", "BSTNORM6.sav", "BSTNZLM6.sav", "BSTOMNM6.sav", "BSTQATM6.sav", "BSTRUSM6.sav", "BSTSAUM6.sav", "BSTSGPM6.sav", "BSTSVNM6.sav", "BSTSWEM6.sav", "BSTTHAM6.sav", "BSTTURM6.sav", "BSTTWNM6.sav", "BSTUSAM6.sav", "BSTZAFM6.sav", "BTMAADM6.sav", "BTMABAM6.sav", "BTMADUM6.sav", "BTMAREM6.sav", "BTMARMM6.sav", "BTMAUSM6.sav", "BTMBHRM6.sav", "BTMBWAM6.sav", "BTMCANM6.sav", "BTMCHLM6.sav", "BTMCOTM6.sav", "BTMCQUM6.sav", "BTMEGYM6.sav", "BTMENGM6.sav", "BTMGEOM6.sav", "BTMHKGM6.sav", "BTMHUNM6.sav", "BTMIRLM6.sav", "BTMIRNM6.sav", "BTMISRM6.sav", "BTMITAM6.sav", "BTMJORM6.sav", "BTMJPNM6.sav", "BTMKAZM6.sav", "BTMKORM6.sav", "BTMKWTM6.sav", "BTMLBNM6.sav", "BTMLTUM6.sav", "BTMMARM6.sav", "BTMMLTM6.sav", "BTMMYSM6.sav", "BTMNO8M6.sav", "BTMNORM6.sav", "BTMNZLM6.sav", "BTMOMNM6.sav", "BTMQATM6.sav", "BTMRUSM6.sav", "BTMSAUM6.sav", "BTMSGPM6.sav", "BTMSVNM6.sav", "BTMSWEM6.sav", "BTMTHAM6.sav", "BTMTURM6.sav", "BTMTWNM6.sav", "BTMUSAM6.sav", "BTMZAFM6.sav", "BTSAADM6.sav", "BTSABAM6.sav", "BTSADUM6.sav", "BTSAREM6.sav", "BTSARMM6.sav", "BTSAUSM6.sav", "BTSBHRM6.sav", "BTSBWAM6.sav", "BTSCANM6.sav", "BTSCHLM6.sav", "BTSCOTM6.sav", "BTSCQUM6.sav", "BTSEGYM6.sav", "BTSENGM6.sav", "BTSGEOM6.sav", "BTSHKGM6.sav", "BTSHUNM6.sav", "BTSIRLM6.sav", "BTSIRNM6.sav", "BTSISRM6.sav", "BTSITAM6.sav", "BTSJORM6.sav", "BTSJPNM6.sav", "BTSKAZM6.sav", "BTSKORM6.sav", "BTSKWTM6.sav", "BTSLBNM6.sav", "BTSLTUM6.sav", "BTSMARM6.sav", "BTSMLTM6.sav", "BTSMYSM6.sav", "BTSNO8M6.sav", "BTSNORM6.sav", "BTSNZLM6.sav", "BTSOMNM6.sav", "BTSQATM6.sav", "BTSRUSM6.sav", "BTSSAUM6.sav", "BTSSGPM6.sav", "BTSSVNM6.sav", "BTSSWEM6.sav", "BTSTHAM6.sav", "BTSTURM6.sav", "BTSTWNM6.sav", "BTSUSAM6.sav", "BTSZAFM6.sav")
      )),
    TIMSS_2019 = list(
      TIMSS_2019_G4 = list("TIMSS2019_IDB_SPSS_G4/Data",
                           c("acgaadm7.sav", "acgadum7.sav", "acgalbm7.sav", "acgarem7.sav", "acgarmm7.sav", "acgausm7.sav", "acgautm7.sav", "acgazem7.sav", "acgbflm7.sav", "acgbgrm7.sav", "acgbhrm7.sav", "acgbihm7.sav", "acgcanm7.sav", "acgchlm7.sav", "acgcotm7.sav", "acgcqum7.sav", "acgcypm7.sav", "acgczem7.sav", "acgdeum7.sav", "acgdnkm7.sav", "acgemam7.sav", "acgengm7.sav", "acgespm7.sav", "acgfinm7.sav", "acgfram7.sav", "acggeom7.sav", "acghkgm7.sav", "acghrvm7.sav", "acghunm7.sav", "acgirlm7.sav", "acgirnm7.sav", "acgitam7.sav", "acgjpnm7.sav", "acgkazm7.sav", "acgkorm7.sav", "acgkwtm7.sav", "acgltum7.sav", "acglvam7.sav", "acgmarm7.sav", "acgmkdm7.sav", "acgmltm7.sav", "acgmnem7.sav", "acgnirm7.sav", "acgnldm7.sav", "acgnorm7.sav", "acgnzlm7.sav", "acgomnm7.sav", "acgpakm7.sav", "acgphlm7.sav", "acgpolm7.sav", "acgprtm7.sav", "acgqatm7.sav", "acgrmom7.sav", "acgrusm7.sav", "acgsaum7.sav", "acgsgpm7.sav", "acgsrbm7.sav", "acgsvkm7.sav", "acgswem7.sav", "acgturm7.sav", "acgtwnm7.sav", "acgusam7.sav", "acgxkxm7.sav", "acgzafm7.sav", "asaaadm7.sav", "asaadum7.sav", "asaalbm7.sav", "asaarem7.sav", "asaarmm7.sav", "asaausm7.sav", "asaautm7.sav", "asaazem7.sav", "asabflm7.sav", "asabgrm7.sav", "asabhrm7.sav", "asabihm7.sav", "asacanm7.sav", "asachlm7.sav", "asacotm7.sav", "asacqum7.sav", "asacypm7.sav", "asaczem7.sav", "asadeum7.sav", "asadnkm7.sav", "asaemam7.sav", "asaengm7.sav", "asaespm7.sav", "asafinm7.sav", "asafram7.sav", "asageom7.sav", "asahkgm7.sav", "asahrvm7.sav", "asahunm7.sav", "asairlm7.sav", "asairnm7.sav", "asaitam7.sav", "asajpnm7.sav", "asakazm7.sav", "asakorm7.sav", "asakwtm7.sav", "asaltum7.sav", "asalvam7.sav", "asamarm7.sav", "asamkdm7.sav", "asamltm7.sav", "asamnem7.sav", "asanirm7.sav", "asanldm7.sav", "asanorm7.sav", "asanzlm7.sav", "asaomnm7.sav", "asapakm7.sav", "asaphlm7.sav", "asapolm7.sav", "asaprtm7.sav", "asaqatm7.sav", "asarmom7.sav", "asarusm7.sav", "asasaum7.sav", "asasgpm7.sav", "asasrbm7.sav", "asasvkm7.sav", "asaswem7.sav", "asaturm7.sav", "asatwnm7.sav", "asausam7.sav", "asaxkxm7.sav", "asazafm7.sav", "asgaadm7.sav", "asgadum7.sav", "asgalbm7.sav", "asgarem7.sav", "asgarmm7.sav", "asgausm7.sav", "asgautm7.sav", "asgazem7.sav", "asgbflm7.sav", "asgbgrm7.sav", "asgbhrm7.sav", "asgbihm7.sav", "asgcanm7.sav", "asgchlm7.sav", "asgcotm7.sav", "asgcqum7.sav", "asgcypm7.sav", "asgczem7.sav", "asgdeum7.sav", "asgdnkm7.sav", "asgemam7.sav", "asgengm7.sav", "asgespm7.sav", "asgfinm7.sav", "asgfram7.sav", "asggeom7.sav", "asghkgm7.sav", "asghrvm7.sav", "asghunm7.sav", "asgirlm7.sav", "asgirnm7.sav", "asgitam7.sav", "asgjpnm7.sav", "asgkazm7.sav", "asgkorm7.sav", "asgkwtm7.sav", "asgltum7.sav", "asglvam7.sav", "asgmarm7.sav", "asgmkdm7.sav", "asgmltm7.sav", "asgmnem7.sav", "asgnirm7.sav", "asgnldm7.sav", "asgnorm7.sav", "asgnzlm7.sav", "asgomnm7.sav", "asgpakm7.sav", "asgphlm7.sav", "asgpolm7.sav", "asgprtm7.sav", "asgqatm7.sav", "asgrmom7.sav", "asgrusm7.sav", "asgsaum7.sav", "asgsgpm7.sav", "asgsrbm7.sav", "asgsvkm7.sav", "asgswem7.sav", "asgturm7.sav", "asgtwnm7.sav", "asgusam7.sav", "asgxkxm7.sav", "asgzafm7.sav", "ashaadm7.sav", "ashadum7.sav", "ashalbm7.sav", "asharem7.sav", "asharmm7.sav", "ashausm7.sav", "ashautm7.sav", "ashazem7.sav", "ashbflm7.sav", "ashbgrm7.sav", "ashbhrm7.sav", "ashbihm7.sav", "ashcanm7.sav", "ashchlm7.sav", "ashcotm7.sav", "ashcqum7.sav", "ashcypm7.sav", "ashczem7.sav", "ashdeum7.sav", "ashdnkm7.sav", "ashemam7.sav", "ashengm7.sav", "ashespm7.sav", "ashfinm7.sav", "ashfram7.sav", "ashgeom7.sav", "ashhkgm7.sav", "ashhrvm7.sav", "ashhunm7.sav", "ashirlm7.sav", "ashirnm7.sav", "ashitam7.sav", "ashjpnm7.sav", "ashkazm7.sav", "ashkorm7.sav", "ashkwtm7.sav", "ashltum7.sav", "ashlvam7.sav", "ashmarm7.sav", "ashmkdm7.sav", "ashmltm7.sav", "ashmnem7.sav", "ashnirm7.sav", "ashnldm7.sav", "ashnorm7.sav", "ashnzlm7.sav", "ashomnm7.sav", "ashpakm7.sav", "ashphlm7.sav", "ashpolm7.sav", "ashprtm7.sav", "ashqatm7.sav", "ashrmom7.sav", "ashrusm7.sav", "ashsaum7.sav", "ashsgpm7.sav", "ashsrbm7.sav", "ashsvkm7.sav", "ashswem7.sav", "ashturm7.sav", "ashtwnm7.sav", "ashusam7.sav", "ashxkxm7.sav", "ashzafm7.sav", "asraadm7.sav", "asradum7.sav", "asralbm7.sav", "asrarem7.sav", "asrarmm7.sav", "asrausm7.sav", "asrautm7.sav", "asrazem7.sav", "asrbflm7.sav", "asrbgrm7.sav", "asrbhrm7.sav", "asrbihm7.sav", "asrcanm7.sav", "asrchlm7.sav", "asrcotm7.sav", "asrcqum7.sav", "asrcypm7.sav", "asrczem7.sav", "asrdeum7.sav", "asrdnkm7.sav", "asremam7.sav", "asrengm7.sav", "asrespm7.sav", "asrfinm7.sav", "asrfram7.sav", "asrgeom7.sav", "asrhkgm7.sav", "asrhrvm7.sav", "asrhunm7.sav", "asrirlm7.sav", "asrirnm7.sav", "asritam7.sav", "asrjpnm7.sav", "asrkazm7.sav", "asrkorm7.sav", "asrkwtm7.sav", "asrltum7.sav", "asrlvam7.sav", "asrmarm7.sav", "asrmkdm7.sav", "asrmltm7.sav", "asrmnem7.sav", "asrnirm7.sav", "asrnldm7.sav", "asrnorm7.sav", "asrnzlm7.sav", "asromnm7.sav", "asrpakm7.sav", "asrphlm7.sav", "asrpolm7.sav", "asrprtm7.sav", "asrqatm7.sav", "asrrmom7.sav", "asrrusm7.sav", "asrsaum7.sav", "asrsgpm7.sav", "asrsrbm7.sav", "asrsvkm7.sav", "asrswem7.sav", "asrturm7.sav", "asrtwnm7.sav", "asrusam7.sav", "asrxkxm7.sav", "asrzafm7.sav", "astaadm7.sav", "astadum7.sav", "astalbm7.sav", "astarem7.sav", "astarmm7.sav", "astausm7.sav", "astautm7.sav", "astazem7.sav", "astbflm7.sav", "astbgrm7.sav", "astbhrm7.sav", "astbihm7.sav", "astcanm7.sav", "astchlm7.sav", "astcotm7.sav", "astcqum7.sav", "astcypm7.sav", "astczem7.sav", "astdeum7.sav", "astdnkm7.sav", "astemam7.sav", "astengm7.sav", "astespm7.sav", "astfinm7.sav", "astfram7.sav", "astgeom7.sav", "asthkgm7.sav", "asthrvm7.sav", "asthunm7.sav", "astirlm7.sav", "astirnm7.sav", "astitam7.sav", "astjpnm7.sav", "astkazm7.sav", "astkorm7.sav", "astkwtm7.sav", "astltum7.sav", "astlvam7.sav", "astmarm7.sav", "astmkdm7.sav", "astmltm7.sav", "astmnem7.sav", "astnirm7.sav", "astnldm7.sav", "astnorm7.sav", "astnzlm7.sav", "astomnm7.sav", "astpakm7.sav", "astphlm7.sav", "astpolm7.sav", "astprtm7.sav", "astqatm7.sav", "astrmom7.sav", "astrusm7.sav", "astsaum7.sav", "astsgpm7.sav", "astsrbm7.sav", "astsvkm7.sav", "astswem7.sav", "astturm7.sav", "asttwnm7.sav", "astusam7.sav", "astxkxm7.sav", "astzafm7.sav", "atgaadm7.sav", "atgadum7.sav", "atgalbm7.sav", "atgarem7.sav", "atgarmm7.sav", "atgausm7.sav", "atgautm7.sav", "atgazem7.sav", "atgbflm7.sav", "atgbgrm7.sav", "atgbhrm7.sav", "atgbihm7.sav", "atgcanm7.sav", "atgchlm7.sav", "atgcotm7.sav", "atgcqum7.sav", "atgcypm7.sav", "atgczem7.sav", "atgdeum7.sav", "atgdnkm7.sav", "atgemam7.sav", "atgengm7.sav", "atgespm7.sav", "atgfinm7.sav", "atgfram7.sav", "atggeom7.sav", "atghkgm7.sav", "atghrvm7.sav", "atghunm7.sav", "atgirlm7.sav", "atgirnm7.sav", "atgitam7.sav", "atgjpnm7.sav", "atgkazm7.sav", "atgkorm7.sav", "atgkwtm7.sav", "atgltum7.sav", "atglvam7.sav", "atgmarm7.sav", "atgmkdm7.sav", "atgmltm7.sav", "atgmnem7.sav", "atgnirm7.sav", "atgnldm7.sav", "atgnorm7.sav", "atgnzlm7.sav", "atgomnm7.sav", "atgpakm7.sav", "atgphlm7.sav", "atgpolm7.sav", "atgprtm7.sav", "atgqatm7.sav", "atgrmom7.sav", "atgrusm7.sav", "atgsaum7.sav", "atgsgpm7.sav", "atgsrbm7.sav", "atgsvkm7.sav", "atgswem7.sav", "atgturm7.sav", "atgtwnm7.sav", "atgusam7.sav", "atgxkxm7.sav", "atgzafm7.sav")
      ),
      TIMSS_2019_G4_Bridge = list("TIMSS2019_IDB_SPSS_G4/Data",
                                  c("acgareb7.sav", "acgautb7.sav", "acgcanb7.sav", "acgchlb7.sav", "acgczeb7.sav", "acgdeub7.sav", "acgdnkb7.sav", "acgengb7.sav", "acgespb7.sav", "acgfinb7.sav", "acgfrab7.sav", "acggeob7.sav", "acghkgb7.sav", "acghrvb7.sav", "acghunb7.sav", "acgitab7.sav", "acgkorb7.sav", "acgltub7.sav", "acgnldb7.sav", "acgnorb7.sav", "acgprtb7.sav", "acgqatb7.sav", "acgrusb7.sav", "acgsgpb7.sav", "acgsvkb7.sav", "acgsweb7.sav", "acgtwnb7.sav", "acgusab7.sav", "asaareb7.sav", "asaautb7.sav", "asacanb7.sav", "asachlb7.sav", "asaczeb7.sav", "asadeub7.sav", "asadnkb7.sav", "asaengb7.sav", "asaespb7.sav", "asafinb7.sav", "asafrab7.sav", "asageob7.sav", "asahkgb7.sav", "asahrvb7.sav", "asahunb7.sav", "asaitab7.sav", "asakorb7.sav", "asaltub7.sav", "asanldb7.sav", "asanorb7.sav", "asaprtb7.sav", "asaqatb7.sav", "asarusb7.sav", "asasgpb7.sav", "asasvkb7.sav", "asasweb7.sav", "asatwnb7.sav", "asausab7.sav", "asgareb7.sav", "asgautb7.sav", "asgcanb7.sav", "asgchlb7.sav", "asgczeb7.sav", "asgdeub7.sav", "asgdnkb7.sav", "asgengb7.sav", "asgespb7.sav", "asgfinb7.sav", "asgfrab7.sav", "asggeob7.sav", "asghkgb7.sav", "asghrvb7.sav", "asghunb7.sav", "asgitab7.sav", "asgkorb7.sav", "asgltub7.sav", "asgnldb7.sav", "asgnorb7.sav", "asgprtb7.sav", "asgqatb7.sav", "asgrusb7.sav", "asgsgpb7.sav", "asgsvkb7.sav", "asgsweb7.sav", "asgtwnb7.sav", "asgusab7.sav", "ashareb7.sav", "ashautb7.sav", "ashcanb7.sav", "ashchlb7.sav", "ashczeb7.sav", "ashdeub7.sav", "ashdnkb7.sav", "ashengb7.sav", "ashespb7.sav", "ashfinb7.sav", "ashfrab7.sav", "ashgeob7.sav", "ashhkgb7.sav", "ashhrvb7.sav", "ashhunb7.sav", "ashitab7.sav", "ashkorb7.sav", "ashltub7.sav", "ashnldb7.sav", "ashnorb7.sav", "ashprtb7.sav", "ashqatb7.sav", "ashrusb7.sav", "ashsgpb7.sav", "ashsvkb7.sav", "ashsweb7.sav", "ashtwnb7.sav", "ashusab7.sav", "asrareb7.sav", "asrautb7.sav", "asrcanb7.sav", "asrchlb7.sav", "asrczeb7.sav", "asrdeub7.sav", "asrdnkb7.sav", "asrengb7.sav", "asrespb7.sav", "asrfinb7.sav", "asrfrab7.sav", "asrgeob7.sav", "asrhkgb7.sav", "asrhrvb7.sav", "asrhunb7.sav", "asritab7.sav", "asrkorb7.sav", "asrltub7.sav", "asrnldb7.sav", "asrnorb7.sav", "asrprtb7.sav", "asrqatb7.sav", "asrrusb7.sav", "asrsgpb7.sav", "asrsvkb7.sav", "asrsweb7.sav", "asrtwnb7.sav", "asrusab7.sav", "astareb7.sav", "astautb7.sav", "astcanb7.sav", "astchlb7.sav", "astczeb7.sav", "astdeub7.sav", "astdnkb7.sav", "astengb7.sav", "astespb7.sav", "astfinb7.sav", "astfrab7.sav", "astgeob7.sav", "asthkgb7.sav", "asthrvb7.sav", "asthunb7.sav", "astitab7.sav", "astkorb7.sav", "astltub7.sav", "astnldb7.sav", "astnorb7.sav", "astprtb7.sav", "astqatb7.sav", "astrusb7.sav", "astsgpb7.sav", "astsvkb7.sav", "astsweb7.sav", "asttwnb7.sav", "astusab7.sav", "atgareb7.sav", "atgautb7.sav", "atgcanb7.sav", "atgchlb7.sav", "atgczeb7.sav", "atgdeub7.sav", "atgdnkb7.sav", "atgengb7.sav", "atgespb7.sav", "atgfinb7.sav", "atgfrab7.sav", "atggeob7.sav", "atghkgb7.sav", "atghrvb7.sav", "atghunb7.sav", "atgitab7.sav", "atgkorb7.sav", "atgltub7.sav", "atgnldb7.sav", "atgnorb7.sav", "atgprtb7.sav", "atgqatb7.sav", "atgrusb7.sav", "atgsgpb7.sav", "atgsvkb7.sav", "atgsweb7.sav", "atgtwnb7.sav", "atgusab7.sav")
      ),
      TIMSS_2019_G8 = list(
        "TIMSS2019_IDB_SPSS_G8/Data",
        c("bcgaadm7.sav", "bcgadum7.sav", "bcgarem7.sav", "bcgausm7.sav", "bcgbhrm7.sav", "bcgchlm7.sav", "bcgcotm7.sav", "bcgcqum7.sav", "bcgcypm7.sav", "bcgegym7.sav", "bcgengm7.sav", "bcgfinm7.sav", "bcgfram7.sav", "bcggeom7.sav", "bcghkgm7.sav", "bcghunm7.sav", "bcgirlm7.sav", "bcgirnm7.sav", "bcgisrm7.sav", "bcgitam7.sav", "bcgjorm7.sav", "bcgjpnm7.sav", "bcgkazm7.sav", "bcgkorm7.sav", "bcgkwtm7.sav", "bcglbnm7.sav", "bcgltum7.sav", "bcgmarm7.sav", "bcgmysm7.sav", "bcgnorm7.sav", "bcgnzlm7.sav", "bcgomnm7.sav", "bcgprtm7.sav", "bcgqatm7.sav", "bcgrmom7.sav", "bcgromm7.sav", "bcgrusm7.sav", "bcgsaum7.sav", "bcgsgpm7.sav", "bcgswem7.sav", "bcgturm7.sav", "bcgtwnm7.sav", "bcgusam7.sav", "bcgzafm7.sav", "bcgzgtm7.sav", "bcgzwcm7.sav", "bsaaadm7.sav", "bsaadum7.sav", "bsaarem7.sav", "bsaausm7.sav", "bsabhrm7.sav", "bsachlm7.sav", "bsacotm7.sav", "bsacqum7.sav", "bsacypm7.sav", "bsaegym7.sav", "bsaengm7.sav", "bsafinm7.sav", "bsafram7.sav", "bsageom7.sav", "bsahkgm7.sav", "bsahunm7.sav", "bsairlm7.sav", "bsairnm7.sav", "bsaisrm7.sav", "bsaitam7.sav", "bsajorm7.sav", "bsajpnm7.sav", "bsakazm7.sav", "bsakorm7.sav", "bsakwtm7.sav", "bsalbnm7.sav", "bsaltum7.sav", "bsamarm7.sav", "bsamysm7.sav", "bsanorm7.sav", "bsanzlm7.sav", "bsaomnm7.sav", "bsaprtm7.sav", "bsaqatm7.sav", "bsarmom7.sav", "bsaromm7.sav", "bsarusm7.sav", "bsasaum7.sav", "bsasgpm7.sav", "bsaswem7.sav", "bsaturm7.sav", "bsatwnm7.sav", "bsausam7.sav", "bsazafm7.sav", "bsazgtm7.sav", "bsazwcm7.sav", "bsgaadm7.sav", "bsgadum7.sav", "bsgarem7.sav", "bsgausm7.sav", "bsgbhrm7.sav", "bsgchlm7.sav", "bsgcotm7.sav", "bsgcqum7.sav", "bsgcypm7.sav", "bsgegym7.sav", "bsgengm7.sav", "bsgfinm7.sav", "bsgfram7.sav", "bsggeom7.sav", "bsghkgm7.sav", "bsghunm7.sav", "bsgirlm7.sav", "bsgirnm7.sav", "bsgisrm7.sav", "bsgitam7.sav", "bsgjorm7.sav", "bsgjpnm7.sav", "bsgkazm7.sav", "bsgkorm7.sav", "bsgkwtm7.sav", "bsglbnm7.sav", "bsgltum7.sav", "bsgmarm7.sav", "bsgmysm7.sav", "bsgnorm7.sav", "bsgnzlm7.sav", "bsgomnm7.sav", "bsgprtm7.sav", "bsgqatm7.sav", "bsgrmom7.sav", "bsgromm7.sav", "bsgrusm7.sav", "bsgsaum7.sav", "bsgsgpm7.sav", "bsgswem7.sav", "bsgturm7.sav", "bsgtwnm7.sav", "bsgusam7.sav", "bsgzafm7.sav", "bsgzgtm7.sav", "bsgzwcm7.sav", "bsraadm7.sav", "bsradum7.sav", "bsrarem7.sav", "bsrausm7.sav", "bsrbhrm7.sav", "bsrchlm7.sav", "bsrcotm7.sav", "bsrcqum7.sav", "bsrcypm7.sav", "bsregym7.sav", "bsrengm7.sav", "bsrfinm7.sav", "bsrfram7.sav", "bsrgeom7.sav", "bsrhkgm7.sav", "bsrhunm7.sav", "bsrirlm7.sav", "bsrirnm7.sav", "bsrisrm7.sav", "bsritam7.sav", "bsrjorm7.sav", "bsrjpnm7.sav", "bsrkazm7.sav", "bsrkorm7.sav", "bsrkwtm7.sav", "bsrlbnm7.sav", "bsrltum7.sav", "bsrmarm7.sav", "bsrmysm7.sav", "bsrnorm7.sav", "bsrnzlm7.sav", "bsromnm7.sav", "bsrprtm7.sav", "bsrqatm7.sav", "bsrrmom7.sav", "bsrromm7.sav", "bsrrusm7.sav", "bsrsaum7.sav", "bsrsgpm7.sav", "bsrswem7.sav", "bsrturm7.sav", "bsrtwnm7.sav", "bsrusam7.sav", "bsrzafm7.sav", "bsrzgtm7.sav", "bsrzwcm7.sav", "bstaadm7.sav", "bstadum7.sav", "bstarem7.sav", "bstausm7.sav", "bstbhrm7.sav", "bstchlm7.sav", "bstcotm7.sav", "bstcqum7.sav", "bstcypm7.sav", "bstegym7.sav", "bstengm7.sav", "bstfinm7.sav", "bstfram7.sav", "bstgeom7.sav", "bsthkgm7.sav", "bsthunm7.sav", "bstirlm7.sav", "bstirnm7.sav", "bstisrm7.sav", "bstitam7.sav", "bstjorm7.sav", "bstjpnm7.sav", "bstkazm7.sav", "bstkorm7.sav", "bstkwtm7.sav", "bstlbnm7.sav", "bstltum7.sav", "bstmarm7.sav", "bstmysm7.sav", "bstnorm7.sav", "bstnzlm7.sav", "bstomnm7.sav", "bstprtm7.sav", "bstqatm7.sav", "bstrmom7.sav", "bstromm7.sav", "bstrusm7.sav", "bstsaum7.sav", "bstsgpm7.sav", "bstswem7.sav", "bstturm7.sav", "bsttwnm7.sav", "bstusam7.sav", "bstzafm7.sav", "bstzgtm7.sav", "bstzwcm7.sav", "btmaadm7.sav", "btmadum7.sav", "btmarem7.sav", "btmausm7.sav", "btmbhrm7.sav", "btmchlm7.sav", "btmcotm7.sav", "btmcqum7.sav", "btmcypm7.sav", "btmegym7.sav", "btmengm7.sav", "btmfinm7.sav", "btmfram7.sav", "btmgeom7.sav", "btmhkgm7.sav", "btmhunm7.sav", "btmirlm7.sav", "btmirnm7.sav", "btmisrm7.sav", "btmitam7.sav", "btmjorm7.sav", "btmjpnm7.sav", "btmkazm7.sav", "btmkorm7.sav", "btmkwtm7.sav", "btmlbnm7.sav", "btmltum7.sav", "btmmarm7.sav", "btmmysm7.sav", "btmnorm7.sav", "btmnzlm7.sav", "btmomnm7.sav", "btmprtm7.sav", "btmqatm7.sav", "btmrmom7.sav", "btmromm7.sav", "btmrusm7.sav", "btmsaum7.sav", "btmsgpm7.sav", "btmswem7.sav", "btmturm7.sav", "btmtwnm7.sav", "btmusam7.sav", "btmzafm7.sav", "btmzgtm7.sav", "btmzwcm7.sav", "btsaadm7.sav", "btsadum7.sav", "btsarem7.sav", "btsausm7.sav", "btsbhrm7.sav", "btschlm7.sav", "btscotm7.sav", "btscqum7.sav", "btscypm7.sav", "btsegym7.sav", "btsengm7.sav", "btsfinm7.sav", "btsfram7.sav", "btsgeom7.sav", "btshkgm7.sav", "btshunm7.sav", "btsirlm7.sav", "btsirnm7.sav", "btsisrm7.sav", "btsitam7.sav", "btsjorm7.sav", "btsjpnm7.sav", "btskazm7.sav", "btskorm7.sav", "btskwtm7.sav", "btslbnm7.sav", "btsltum7.sav", "btsmarm7.sav", "btsmysm7.sav", "btsnorm7.sav", "btsnzlm7.sav", "btsomnm7.sav", "btsprtm7.sav", "btsqatm7.sav", "btsrmom7.sav", "btsromm7.sav", "btsrusm7.sav", "btssaum7.sav", "btssgpm7.sav", "btsswem7.sav", "btsturm7.sav", "btstwnm7.sav", "btsusam7.sav", "btszafm7.sav", "btszgtm7.sav", "btszwcm7.sav")
      ),
      TIMSS_2019_G8_Bridge = list(
        "TIMSS2019_IDB_SPSS_G8/Data",
        c("bcgareb7.sav", "bcgchlb7.sav", "bcgengb7.sav", "bcggeob7.sav", "bcghkgb7.sav", "bcghunb7.sav", "bcgisrb7.sav", "bcgitab7.sav", "bcgkorb7.sav", "bcgltub7.sav", "bcgmysb7.sav", "bcgnorb7.sav", "bcgqatb7.sav", "bcgrusb7.sav", "bcgsgpb7.sav", "bcgsweb7.sav", "bcgturb7.sav", "bcgtwnb7.sav", "bcgusab7.sav", "bsaareb7.sav", "bsachlb7.sav", "bsaengb7.sav", "bsageob7.sav", "bsahkgb7.sav", "bsahunb7.sav", "bsaisrb7.sav", "bsaitab7.sav", "bsakorb7.sav", "bsaltub7.sav", "bsamysb7.sav", "bsanorb7.sav", "bsaqatb7.sav", "bsarusb7.sav", "bsasgpb7.sav", "bsasweb7.sav", "bsaturb7.sav", "bsatwnb7.sav", "bsausab7.sav", "bsgareb7.sav", "bsgchlb7.sav", "bsgengb7.sav", "bsggeob7.sav", "bsghkgb7.sav", "bsghunb7.sav", "bsgisrb7.sav", "bsgitab7.sav", "bsgkorb7.sav", "bsgltub7.sav", "bsgmysb7.sav", "bsgnorb7.sav", "bsgqatb7.sav", "bsgrusb7.sav", "bsgsgpb7.sav", "bsgsweb7.sav", "bsgturb7.sav", "bsgtwnb7.sav", "bsgusab7.sav", "bsrareb7.sav", "bsrchlb7.sav", "bsrengb7.sav", "bsrgeob7.sav", "bsrhkgb7.sav", "bsrhunb7.sav", "bsrisrb7.sav", "bsritab7.sav", "bsrkorb7.sav", "bsrltub7.sav", "bsrmysb7.sav", "bsrnorb7.sav", "bsrqatb7.sav", "bsrrusb7.sav", "bsrsgpb7.sav", "bsrsweb7.sav", "bsrturb7.sav", "bsrtwnb7.sav", "bsrusab7.sav", "bstareb7.sav", "bstchlb7.sav", "bstengb7.sav", "bstgeob7.sav", "bsthkgb7.sav", "bsthunb7.sav", "bstisrb7.sav", "bstitab7.sav", "bstkorb7.sav", "bstltub7.sav", "bstmysb7.sav", "bstnorb7.sav", "bstqatb7.sav", "bstrusb7.sav", "bstsgpb7.sav", "bstsweb7.sav", "bstturb7.sav", "bsttwnb7.sav", "bstusab7.sav", "btmareb7.sav", "btmchlb7.sav", "btmengb7.sav", "btmgeob7.sav", "btmhkgb7.sav", "btmhunb7.sav", "btmisrb7.sav", "btmitab7.sav", "btmkorb7.sav", "btmltub7.sav", "btmmysb7.sav", "btmnorb7.sav", "btmqatb7.sav", "btmrusb7.sav", "btmsgpb7.sav", "btmsweb7.sav", "btmturb7.sav", "btmtwnb7.sav", "btmusab7.sav", "btsareb7.sav", "btschlb7.sav", "btsengb7.sav", "btsgeob7.sav", "btshkgb7.sav", "btshunb7.sav", "btsisrb7.sav", "btsitab7.sav", "btskorb7.sav", "btsltub7.sav", "btsmysb7.sav", "btsnorb7.sav", "btsqatb7.sav", "btsrusb7.sav", "btssgpb7.sav", "btssweb7.sav", "btsturb7.sav", "btstwnb7.sav", "btsusab7.sav")
      )),
    eTIMSS_PSI_2019 = list(
      eTIMSS_PSI_2019_G4 = list("TIMSS2019_IDB_SPSS_G4/Data",
                                c("acgaadz7.sav", "acgaduz7.sav", "acgarez7.sav", "acgautz7.sav", "acgcanz7.sav", "acgchlz7.sav", "acgcotz7.sav", "acgcquz7.sav", "acgczez7.sav", "acgdeuz7.sav", "acgdnkz7.sav", "acgemaz7.sav", "acgengz7.sav", "acgespz7.sav", "acgfinz7.sav", "acgfraz7.sav", "acggeoz7.sav", "acghkgz7.sav", "acghrvz7.sav", "acghunz7.sav", "acgitaz7.sav", "acgkorz7.sav", "acgltuz7.sav", "acgmltz7.sav", "acgnldz7.sav", "acgnorz7.sav", "acgprtz7.sav", "acgqatz7.sav", "acgrmoz7.sav", "acgrusz7.sav", "acgsgpz7.sav", "acgsvkz7.sav", "acgswez7.sav", "acgturz7.sav", "acgtwnz7.sav", "acgusaz7.sav", "asaaadz7.sav", "asaaduz7.sav", "asaarez7.sav", "asaautz7.sav", "asacanz7.sav", "asachlz7.sav", "asacotz7.sav", "asacquz7.sav", "asaczez7.sav", "asadeuz7.sav", "asadnkz7.sav", "asaemaz7.sav", "asaengz7.sav", "asaespz7.sav", "asafinz7.sav", "asafraz7.sav", "asageoz7.sav", "asahkgz7.sav", "asahrvz7.sav", "asahunz7.sav", "asaitaz7.sav", "asakorz7.sav", "asaltuz7.sav", "asamltz7.sav", "asanldz7.sav", "asanorz7.sav", "asaprtz7.sav", "asaqatz7.sav", "asarmoz7.sav", "asarusz7.sav", "asasgpz7.sav", "asasvkz7.sav", "asaswez7.sav", "asaturz7.sav", "asatwnz7.sav", "asausaz7.sav", "asgaadz7.sav", "asgaduz7.sav", "asgarez7.sav", "asgautz7.sav", "asgcanz7.sav", "asgchlz7.sav", "asgcotz7.sav", "asgcquz7.sav", "asgczez7.sav", "asgdeuz7.sav", "asgdnkz7.sav", "asgemaz7.sav", "asgengz7.sav", "asgespz7.sav", "asgfinz7.sav", "asgfraz7.sav", "asggeoz7.sav", "asghkgz7.sav", "asghrvz7.sav", "asghunz7.sav", "asgitaz7.sav", "asgkorz7.sav", "asgltuz7.sav", "asgmltz7.sav", "asgnldz7.sav", "asgnorz7.sav", "asgprtz7.sav", "asgqatz7.sav", "asgrmoz7.sav", "asgrusz7.sav", "asgsgpz7.sav", "asgsvkz7.sav", "asgswez7.sav", "asgturz7.sav", "asgtwnz7.sav", "asgusaz7.sav", "ashaadz7.sav", "ashaduz7.sav", "asharez7.sav", "ashautz7.sav", "ashcanz7.sav", "ashchlz7.sav", "ashcotz7.sav", "ashcquz7.sav", "ashczez7.sav", "ashdeuz7.sav", "ashdnkz7.sav", "ashemaz7.sav", "ashengz7.sav", "ashespz7.sav", "ashfinz7.sav", "ashfraz7.sav", "ashgeoz7.sav", "ashhkgz7.sav", "ashhrvz7.sav", "ashhunz7.sav", "ashitaz7.sav", "ashkorz7.sav", "ashltuz7.sav", "ashmltz7.sav", "ashnldz7.sav", "ashnorz7.sav", "ashprtz7.sav", "ashqatz7.sav", "ashrmoz7.sav", "ashrusz7.sav", "ashsgpz7.sav", "ashsvkz7.sav", "ashswez7.sav", "ashturz7.sav", "ashtwnz7.sav", "ashusaz7.sav", "asraadz7.sav", "asraduz7.sav", "asrarez7.sav", "asrautz7.sav", "asrcanz7.sav", "asrchlz7.sav", "asrcotz7.sav", "asrcquz7.sav", "asrczez7.sav", "asrdeuz7.sav", "asrdnkz7.sav", "asremaz7.sav", "asrengz7.sav", "asrespz7.sav", "asrfinz7.sav", "asrfraz7.sav", "asrgeoz7.sav", "asrhkgz7.sav", "asrhrvz7.sav", "asrhunz7.sav", "asritaz7.sav", "asrkorz7.sav", "asrltuz7.sav", "asrmltz7.sav", "asrnldz7.sav", "asrnorz7.sav", "asrprtz7.sav", "asrqatz7.sav", "asrrmoz7.sav", "asrrusz7.sav", "asrsgpz7.sav", "asrsvkz7.sav", "asrswez7.sav", "asrturz7.sav", "asrtwnz7.sav", "asrusaz7.sav", "astaadz7.sav", "astaduz7.sav", "astarez7.sav", "astautz7.sav", "astcanz7.sav", "astchlz7.sav", "astcotz7.sav", "astcquz7.sav", "astczez7.sav", "astdeuz7.sav", "astdnkz7.sav", "astemaz7.sav", "astengz7.sav", "astespz7.sav", "astfinz7.sav", "astfraz7.sav", "astgeoz7.sav", "asthkgz7.sav", "asthrvz7.sav", "asthunz7.sav", "astitaz7.sav", "astkorz7.sav", "astltuz7.sav", "astmltz7.sav", "astnldz7.sav", "astnorz7.sav", "astprtz7.sav", "astqatz7.sav", "astrmoz7.sav", "astrusz7.sav", "astsgpz7.sav", "astsvkz7.sav", "astswez7.sav", "astturz7.sav", "asttwnz7.sav", "astusaz7.sav", "atgaadz7.sav", "atgaduz7.sav", "atgarez7.sav", "atgautz7.sav", "atgcanz7.sav", "atgchlz7.sav", "atgcotz7.sav", "atgcquz7.sav", "atgczez7.sav", "atgdeuz7.sav", "atgdnkz7.sav", "atgemaz7.sav", "atgengz7.sav", "atgespz7.sav", "atgfinz7.sav", "atgfraz7.sav", "atggeoz7.sav", "atghkgz7.sav", "atghrvz7.sav", "atghunz7.sav", "atgitaz7.sav", "atgkorz7.sav", "atgltuz7.sav", "atgmltz7.sav", "atgnldz7.sav", "atgnorz7.sav", "atgprtz7.sav", "atgqatz7.sav", "atgrmoz7.sav", "atgrusz7.sav", "atgsgpz7.sav", "atgsvkz7.sav", "atgswez7.sav", "atgturz7.sav", "atgtwnz7.sav", "atgusaz7.sav")
      ),
      eTIMSS_PSI_2019_G8 = list("TIMSS2019_IDB_SPSS_G8/Data",
                                c("bcgaadz7.sav", "bcgaduz7.sav", "bcgarez7.sav", "bcgchlz7.sav", "bcgcotz7.sav", "bcgcquz7.sav", "bcgengz7.sav", "bcgfinz7.sav", "bcgfraz7.sav", "bcggeoz7.sav", "bcghkgz7.sav", "bcghunz7.sav", "bcgisrz7.sav", "bcgitaz7.sav", "bcgkorz7.sav", "bcgltuz7.sav", "bcgmysz7.sav", "bcgnorz7.sav", "bcgprtz7.sav", "bcgqatz7.sav", "bcgrmoz7.sav", "bcgrusz7.sav", "bcgsgpz7.sav", "bcgswez7.sav", "bcgturz7.sav", "bcgtwnz7.sav", "bcgusaz7.sav", "bsaaadz7.sav", "bsaaduz7.sav", "bsaarez7.sav", "bsachlz7.sav", "bsacotz7.sav", "bsacquz7.sav", "bsaengz7.sav", "bsafinz7.sav", "bsafraz7.sav", "bsageoz7.sav", "bsahkgz7.sav", "bsahunz7.sav", "bsaisrz7.sav", "bsaitaz7.sav", "bsakorz7.sav", "bsaltuz7.sav", "bsamysz7.sav", "bsanorz7.sav", "bsaprtz7.sav", "bsaqatz7.sav", "bsarmoz7.sav", "bsarusz7.sav", "bsasgpz7.sav", "bsaswez7.sav", "bsaturz7.sav", "bsatwnz7.sav", "bsausaz7.sav", "bsgaadz7.sav", "bsgaduz7.sav", "bsgarez7.sav", "bsgchlz7.sav", "bsgcotz7.sav", "bsgcquz7.sav", "bsgengz7.sav", "bsgfinz7.sav", "bsgfraz7.sav", "bsggeoz7.sav", "bsghkgz7.sav", "bsghunz7.sav", "bsgisrz7.sav", "bsgitaz7.sav", "bsgkorz7.sav", "bsgltuz7.sav", "bsgmysz7.sav", "bsgnorz7.sav", "bsgprtz7.sav", "bsgqatz7.sav", "bsgrmoz7.sav", "bsgrusz7.sav", "bsgsgpz7.sav", "bsgswez7.sav", "bsgturz7.sav", "bsgtwnz7.sav", "bsgusaz7.sav", "bsraadz7.sav", "bsraduz7.sav", "bsrarez7.sav", "bsrchlz7.sav", "bsrcotz7.sav", "bsrcquz7.sav", "bsrengz7.sav", "bsrfinz7.sav", "bsrfraz7.sav", "bsrgeoz7.sav", "bsrhkgz7.sav", "bsrhunz7.sav", "bsrisrz7.sav", "bsritaz7.sav", "bsrkorz7.sav", "bsrltuz7.sav", "bsrmysz7.sav", "bsrnorz7.sav", "bsrprtz7.sav", "bsrqatz7.sav", "bsrrmoz7.sav", "bsrrusz7.sav", "bsrsgpz7.sav", "bsrswez7.sav", "bsrturz7.sav", "bsrtwnz7.sav", "bsrusaz7.sav", "bstaadz7.sav", "bstaduz7.sav", "bstarez7.sav", "bstchlz7.sav", "bstcotz7.sav", "bstcquz7.sav", "bstengz7.sav", "bstfinz7.sav", "bstfraz7.sav", "bstgeoz7.sav", "bsthkgz7.sav", "bsthunz7.sav", "bstisrz7.sav", "bstitaz7.sav", "bstkorz7.sav", "bstltuz7.sav", "bstmysz7.sav", "bstnorz7.sav", "bstprtz7.sav", "bstqatz7.sav", "bstrmoz7.sav", "bstrusz7.sav", "bstsgpz7.sav", "bstswez7.sav", "bstturz7.sav", "bsttwnz7.sav", "bstusaz7.sav", "btmaadz7.sav", "btmaduz7.sav", "btmarez7.sav", "btmchlz7.sav", "btmcotz7.sav", "btmcquz7.sav", "btmengz7.sav", "btmfinz7.sav", "btmfraz7.sav", "btmgeoz7.sav", "btmhkgz7.sav", "btmhunz7.sav", "btmisrz7.sav", "btmitaz7.sav", "btmkorz7.sav", "btmltuz7.sav", "btmmysz7.sav", "btmnorz7.sav", "btmprtz7.sav", "btmqatz7.sav", "btmrmoz7.sav", "btmrusz7.sav", "btmsgpz7.sav", "btmswez7.sav", "btmturz7.sav", "btmtwnz7.sav", "btmusaz7.sav", "btsaadz7.sav", "btsaduz7.sav", "btsarez7.sav", "btschlz7.sav", "btscotz7.sav", "btscquz7.sav", "btsengz7.sav", "btsfinz7.sav", "btsfraz7.sav", "btsgeoz7.sav", "btshkgz7.sav", "btshunz7.sav", "btsisrz7.sav", "btsitaz7.sav", "btskorz7.sav", "btsltuz7.sav", "btsmysz7.sav", "btsnorz7.sav", "btsprtz7.sav", "btsqatz7.sav", "btsrmoz7.sav", "btsrusz7.sav", "btssgpz7.sav", "btsswez7.sav", "btsturz7.sav", "btstwnz7.sav", "btsusaz7.sav")
      )),
    TIMSS_2023 = list(
      TIMSS_2023_G4 = list("TIMSS2023_IDB_SPSS_G4/2_Data Files/SPSS Data",
                           c("acgaadm8.sav", "acgadum8.sav", "acgalbm8.sav", "acgarem8.sav", "acgarmm8.sav", "acgashm8.sav", "acgausm8.sav", "acgazem8.sav", "acgbflm8.sav", "acgbfrm8.sav", "acgbgrm8.sav", "acgbhrm8.sav", "acgbihm8.sav", "acgbram8.sav", "acgcanm8.sav", "acgchlm8.sav", "acgcotm8.sav", "acgcqum8.sav", "acgcypm8.sav", "acgczem8.sav", "acgdeum8.sav", "acgdnkm8.sav", "acgengm8.sav", "acgespm8.sav", "acgfinm8.sav", "acgfram8.sav", "acggeom8.sav", "acghkgm8.sav", "acghunm8.sav", "acgirlm8.sav", "acgirnm8.sav", "acgitam8.sav", "acgjorm8.sav", "acgjpnm8.sav", "acgkazm8.sav", "acgkorm8.sav", "acgkwtm8.sav", "acgltum8.sav", "acglvam8.sav", "acgmacm8.sav", "acgmarm8.sav", "acgmkdm8.sav", "acgmnem8.sav", "acgnldm8.sav", "acgnorm8.sav", "acgnzlm8.sav", "acgomnm8.sav", "acgpolm8.sav", "acgprtm8.sav", "acgqatm8.sav", "acgromm8.sav", "acgsaum8.sav", "acgsgpm8.sav", "acgsrbm8.sav", "acgsvkm8.sav", "acgsvnm8.sav", "acgswem8.sav", "acgturm8.sav", "acgtwnm8.sav", "acgusam8.sav", "acguzbm8.sav", "acgxkxm8.sav", "acgzafm8.sav", "asaaadm8.sav", "asaadum8.sav", "asaalbm8.sav", "asaarem8.sav", "asaarmm8.sav", "asaashm8.sav", "asaausm8.sav", "asaazem8.sav", "asabflm8.sav", "asabfrm8.sav", "asabgrm8.sav", "asabhrm8.sav", "asabihm8.sav", "asabram8.sav", "asacanm8.sav", "asachlm8.sav", "asacotm8.sav", "asacqum8.sav", "asacypm8.sav", "asaczem8.sav", "asadeum8.sav", "asadnkm8.sav", "asaengm8.sav", "asaespm8.sav", "asafinm8.sav", "asafram8.sav", "asageom8.sav", "asahkgm8.sav", "asahunm8.sav", "asairlm8.sav", "asairnm8.sav", "asaitam8.sav", "asajorm8.sav", "asajpnm8.sav", "asakazm8.sav", "asakorm8.sav", "asakwtm8.sav", "asaltum8.sav", "asalvam8.sav", "asamacm8.sav", "asamarm8.sav", "asamkdm8.sav", "asamnem8.sav", "asanldm8.sav", "asanorm8.sav", "asanzlm8.sav", "asaomnm8.sav", "asapolm8.sav", "asaprtm8.sav", "asaqatm8.sav", "asaromm8.sav", "asasaum8.sav", "asasgpm8.sav", "asasrbm8.sav", "asasvkm8.sav", "asasvnm8.sav", "asaswem8.sav", "asaturm8.sav", "asatwnm8.sav", "asausam8.sav", "asauzbm8.sav", "asaxkxm8.sav", "asazafm8.sav", "asgaadm8.sav", "asgadum8.sav", "asgalbm8.sav", "asgarem8.sav", "asgarmm8.sav", "asgashm8.sav", "asgausm8.sav", "asgazem8.sav", "asgbflm8.sav", "asgbfrm8.sav", "asgbgrm8.sav", "asgbhrm8.sav", "asgbihm8.sav", "asgbram8.sav", "asgcanm8.sav", "asgchlm8.sav", "asgcotm8.sav", "asgcqum8.sav", "asgcypm8.sav", "asgczem8.sav", "asgdeum8.sav", "asgdnkm8.sav", "asgengm8.sav", "asgespm8.sav", "asgfinm8.sav", "asgfram8.sav", "asggeom8.sav", "asghkgm8.sav", "asghunm8.sav", "asgirlm8.sav", "asgirnm8.sav", "asgitam8.sav", "asgjorm8.sav", "asgjpnm8.sav", "asgkazm8.sav", "asgkorm8.sav", "asgkwtm8.sav", "asgltum8.sav", "asglvam8.sav", "asgmacm8.sav", "asgmarm8.sav", "asgmkdm8.sav", "asgmnem8.sav", "asgnldm8.sav", "asgnorm8.sav", "asgnzlm8.sav", "asgomnm8.sav", "asgpolm8.sav", "asgprtm8.sav", "asgqatm8.sav", "asgromm8.sav", "asgsaum8.sav", "asgsgpm8.sav", "asgsrbm8.sav", "asgsvkm8.sav", "asgsvnm8.sav", "asgswem8.sav", "asgturm8.sav", "asgtwnm8.sav", "asgusam8.sav", "asguzbm8.sav", "asgxkxm8.sav", "asgzafm8.sav", "ashaadm8.sav", "ashadum8.sav", "ashalbm8.sav", "asharem8.sav", "asharmm8.sav", "ashashm8.sav", "ashausm8.sav", "ashazem8.sav", "ashbflm8.sav", "ashbfrm8.sav", "ashbgrm8.sav", "ashbhrm8.sav", "ashbihm8.sav", "ashbram8.sav", "ashcanm8.sav", "ashchlm8.sav", "ashcotm8.sav", "ashcqum8.sav", "ashcypm8.sav", "ashczem8.sav", "ashdeum8.sav", "ashdnkm8.sav", "ashengm8.sav", "ashespm8.sav", "ashfinm8.sav", "ashfram8.sav", "ashgeom8.sav", "ashhkgm8.sav", "ashhunm8.sav", "ashirlm8.sav", "ashirnm8.sav", "ashitam8.sav", "ashjorm8.sav", "ashjpnm8.sav", "ashkazm8.sav", "ashkorm8.sav", "ashkwtm8.sav", "ashltum8.sav", "ashlvam8.sav", "ashmacm8.sav", "ashmarm8.sav", "ashmkdm8.sav", "ashmnem8.sav", "ashnldm8.sav", "ashnorm8.sav", "ashnzlm8.sav", "ashomnm8.sav", "ashpolm8.sav", "ashprtm8.sav", "ashqatm8.sav", "ashromm8.sav", "ashsaum8.sav", "ashsgpm8.sav", "ashsrbm8.sav", "ashsvkm8.sav", "ashsvnm8.sav", "ashswem8.sav", "ashturm8.sav", "ashtwnm8.sav", "ashusam8.sav", "ashuzbm8.sav", "ashxkxm8.sav", "ashzafm8.sav", "aspaadm8.sav", "aspadum8.sav", "aspalbm8.sav", "asparem8.sav", "asparmm8.sav", "aspashm8.sav", "aspausm8.sav", "aspazem8.sav", "aspbflm8.sav", "aspbfrm8.sav", "aspbgrm8.sav", "aspbhrm8.sav", "aspbihm8.sav", "aspbram8.sav", "aspcanm8.sav", "aspchlm8.sav", "aspcotm8.sav", "aspcqum8.sav", "aspcypm8.sav", "aspczem8.sav", "aspdeum8.sav", "aspdnkm8.sav", "aspengm8.sav", "aspespm8.sav", "aspfinm8.sav", "aspfram8.sav", "aspgeom8.sav", "asphkgm8.sav", "asphunm8.sav", "aspirlm8.sav", "aspirnm8.sav", "aspitam8.sav", "aspjorm8.sav", "aspjpnm8.sav", "aspkazm8.sav", "aspkorm8.sav", "aspkwtm8.sav", "aspltum8.sav", "asplvam8.sav", "aspmacm8.sav", "aspmarm8.sav", "aspmkdm8.sav", "aspmnem8.sav", "aspnldm8.sav", "aspnorm8.sav", "aspnzlm8.sav", "aspomnm8.sav", "asppolm8.sav", "aspprtm8.sav", "aspqatm8.sav", "aspromm8.sav", "aspsaum8.sav", "aspsgpm8.sav", "aspsrbm8.sav", "aspsvkm8.sav", "aspsvnm8.sav", "aspswem8.sav", "aspturm8.sav", "asptwnm8.sav", "aspusam8.sav", "aspuzbm8.sav", "aspxkxm8.sav", "aspzafm8.sav", "asraadm8.sav", "asradum8.sav", "asralbm8.sav", "asrarem8.sav", "asrarmm8.sav", "asrashm8.sav", "asrausm8.sav", "asrazem8.sav", "asrbflm8.sav", "asrbfrm8.sav", "asrbgrm8.sav", "asrbhrm8.sav", "asrbihm8.sav", "asrbram8.sav", "asrcanm8.sav", "asrchlm8.sav", "asrcotm8.sav", "asrcqum8.sav", "asrcypm8.sav", "asrczem8.sav", "asrdeum8.sav", "asrdnkm8.sav", "asrengm8.sav", "asrespm8.sav", "asrfinm8.sav", "asrfram8.sav", "asrgeom8.sav", "asrhkgm8.sav", "asrhunm8.sav", "asrirlm8.sav", "asrirnm8.sav", "asritam8.sav", "asrjorm8.sav", "asrjpnm8.sav", "asrkazm8.sav", "asrkorm8.sav", "asrkwtm8.sav", "asrltum8.sav", "asrlvam8.sav", "asrmacm8.sav", "asrmarm8.sav", "asrmkdm8.sav", "asrmnem8.sav", "asrnldm8.sav", "asrnorm8.sav", "asrnzlm8.sav", "asromnm8.sav", "asrpolm8.sav", "asrprtm8.sav", "asrqatm8.sav", "asrromm8.sav", "asrsaum8.sav", "asrsgpm8.sav", "asrsrbm8.sav", "asrsvkm8.sav", "asrsvnm8.sav", "asrswem8.sav", "asrturm8.sav", "asrtwnm8.sav", "asrusam8.sav", "asruzbm8.sav", "asrxkxm8.sav", "asrzafm8.sav", "astaadm8.sav", "astadum8.sav", "astalbm8.sav", "astarem8.sav", "astarmm8.sav", "astashm8.sav", "astausm8.sav", "astazem8.sav", "astbflm8.sav", "astbfrm8.sav", "astbgrm8.sav", "astbhrm8.sav", "astbihm8.sav", "astbram8.sav", "astcanm8.sav", "astchlm8.sav", "astcotm8.sav", "astcqum8.sav", "astcypm8.sav", "astczem8.sav", "astdeum8.sav", "astdnkm8.sav", "astengm8.sav", "astespm8.sav", "astfinm8.sav", "astfram8.sav", "astgeom8.sav", "asthkgm8.sav", "asthunm8.sav", "astirlm8.sav", "astirnm8.sav", "astitam8.sav", "astjorm8.sav", "astjpnm8.sav", "astkazm8.sav", "astkorm8.sav", "astkwtm8.sav", "astltum8.sav", "astlvam8.sav", "astmacm8.sav", "astmarm8.sav", "astmkdm8.sav", "astmnem8.sav", "astnldm8.sav", "astnorm8.sav", "astnzlm8.sav", "astomnm8.sav", "astpolm8.sav", "astprtm8.sav", "astqatm8.sav", "astromm8.sav", "astsaum8.sav", "astsgpm8.sav", "astsrbm8.sav", "astsvkm8.sav", "astsvnm8.sav", "astswem8.sav", "astturm8.sav", "asttwnm8.sav", "astusam8.sav", "astuzbm8.sav", "astxkxm8.sav", "astzafm8.sav", "atgaadm8.sav", "atgadum8.sav", "atgalbm8.sav", "atgarem8.sav", "atgarmm8.sav", "atgashm8.sav", "atgausm8.sav", "atgazem8.sav", "atgbflm8.sav", "atgbfrm8.sav", "atgbgrm8.sav", "atgbhrm8.sav", "atgbihm8.sav", "atgbram8.sav", "atgcanm8.sav", "atgchlm8.sav", "atgcotm8.sav", "atgcqum8.sav", "atgcypm8.sav", "atgczem8.sav", "atgdeum8.sav", "atgdnkm8.sav", "atgengm8.sav", "atgespm8.sav", "atgfinm8.sav", "atgfram8.sav", "atggeom8.sav", "atghkgm8.sav", "atghunm8.sav", "atgirlm8.sav", "atgirnm8.sav", "atgitam8.sav", "atgjorm8.sav", "atgjpnm8.sav", "atgkazm8.sav", "atgkorm8.sav", "atgkwtm8.sav", "atgltum8.sav", "atglvam8.sav", "atgmacm8.sav", "atgmarm8.sav", "atgmkdm8.sav", "atgmnem8.sav", "atgnldm8.sav", "atgnorm8.sav", "atgnzlm8.sav", "atgomnm8.sav", "atgpolm8.sav", "atgprtm8.sav", "atgqatm8.sav", "atgromm8.sav", "atgsaum8.sav", "atgsgpm8.sav", "atgsrbm8.sav", "atgsvkm8.sav", "atgsvnm8.sav", "atgswem8.sav", "atgturm8.sav", "atgtwnm8.sav", "atgusam8.sav", "atguzbm8.sav", "atgxkxm8.sav", "atgzafm8.sav")),
      TIMSS_2023_G8 = list("TIMSS2023_IDB_SPSS_G8/2_Data Files/SPSS Data",
                           c("bcgaadm8.sav", "bcgadum8.sav", "bcgarem8.sav", "bcgashm8.sav", "bcgausm8.sav", "bcgautm8.sav", "bcgazem8.sav", "bcgbhrm8.sav", "bcgbram8.sav", "bcgchlm8.sav", "bcgcivm8.sav", "bcgcypm8.sav", "bcgczem8.sav", "bcgengm8.sav", "bcgfinm8.sav", "bcgfram8.sav", "bcggeom8.sav", "bcghkgm8.sav", "bcghunm8.sav", "bcgirlm8.sav", "bcgirnm8.sav", "bcgisrm8.sav", "bcgitam8.sav", "bcgjorm8.sav", "bcgjpnm8.sav", "bcgkazm8.sav", "bcgkorm8.sav", "bcgkwtm8.sav", "bcgltum8.sav", "bcgmarm8.sav", "bcgmltm8.sav", "bcgmysm8.sav", "bcgnorm8.sav", "bcgnzlm8.sav", "bcgomnm8.sav", "bcgprtm8.sav", "bcgpsem8.sav", "bcgqatm8.sav", "bcgromm8.sav", "bcgsaum8.sav", "bcgsgpm8.sav", "bcgswem8.sav", "bcgturm8.sav", "bcgtwnm8.sav", "bcgusam8.sav", "bcguzbm8.sav", "bcgzafm8.sav", "bsaaadm8.sav", "bsaadum8.sav", "bsaarem8.sav", "bsaashm8.sav", "bsaausm8.sav", "bsaautm8.sav", "bsaazem8.sav", "bsabhrm8.sav", "bsabram8.sav", "bsachlm8.sav", "bsacivm8.sav", "bsacypm8.sav", "bsaczem8.sav", "bsaengm8.sav", "bsafinm8.sav", "bsafram8.sav", "bsageom8.sav", "bsahkgm8.sav", "bsahunm8.sav", "bsairlm8.sav", "bsairnm8.sav", "bsaisrm8.sav", "bsaitam8.sav", "bsajorm8.sav", "bsajpnm8.sav", "bsakazm8.sav", "bsakorm8.sav", "bsakwtm8.sav", "bsaltum8.sav", "bsamarm8.sav", "bsamltm8.sav", "bsamysm8.sav", "bsanorm8.sav", "bsanzlm8.sav", "bsaomnm8.sav", "bsaprtm8.sav", "bsapsem8.sav", "bsaqatm8.sav", "bsaromm8.sav", "bsasaum8.sav", "bsasgpm8.sav", "bsaswem8.sav", "bsaturm8.sav", "bsatwnm8.sav", "bsausam8.sav", "bsauzbm8.sav", "bsazafm8.sav", "bsgaadm8.sav", "bsgadum8.sav", "bsgarem8.sav", "bsgashm8.sav", "bsgausm8.sav", "bsgautm8.sav", "bsgazem8.sav", "bsgbhrm8.sav", "bsgbram8.sav", "bsgchlm8.sav", "bsgcivm8.sav", "bsgcypm8.sav", "bsgczem8.sav", "bsgengm8.sav", "bsgfinm8.sav", "bsgfram8.sav", "bsggeom8.sav", "bsghkgm8.sav", "bsghunm8.sav", "bsgirlm8.sav", "bsgirnm8.sav", "bsgisrm8.sav", "bsgitam8.sav", "bsgjorm8.sav", "bsgjpnm8.sav", "bsgkazm8.sav", "bsgkorm8.sav", "bsgkwtm8.sav", "bsgltum8.sav", "bsgmarm8.sav", "bsgmltm8.sav", "bsgmysm8.sav", "bsgnorm8.sav", "bsgnzlm8.sav", "bsgomnm8.sav", "bsgprtm8.sav", "bsgpsem8.sav", "bsgqatm8.sav", "bsgromm8.sav", "bsgsaum8.sav", "bsgsgpm8.sav", "bsgswem8.sav", "bsgturm8.sav", "bsgtwnm8.sav", "bsgusam8.sav", "bsguzbm8.sav", "bsgzafm8.sav", "bspaadm8.sav", "bspadum8.sav", "bsparem8.sav", "bspashm8.sav", "bspausm8.sav", "bspautm8.sav", "bspazem8.sav", "bspbhrm8.sav", "bspbram8.sav", "bspchlm8.sav", "bspcivm8.sav", "bspcypm8.sav", "bspczem8.sav", "bspengm8.sav", "bspfinm8.sav", "bspfram8.sav", "bspgeom8.sav", "bsphkgm8.sav", "bsphunm8.sav", "bspirlm8.sav", "bspirnm8.sav", "bspisrm8.sav", "bspitam8.sav", "bspjorm8.sav", "bspjpnm8.sav", "bspkazm8.sav", "bspkorm8.sav", "bspkwtm8.sav", "bspltum8.sav", "bspmarm8.sav", "bspmltm8.sav", "bspmysm8.sav", "bspnorm8.sav", "bspnzlm8.sav", "bspomnm8.sav", "bspprtm8.sav", "bsppsem8.sav", "bspqatm8.sav", "bspromm8.sav", "bspsaum8.sav", "bspsgpm8.sav", "bspswem8.sav", "bspturm8.sav", "bsptwnm8.sav", "bspusam8.sav", "bspuzbm8.sav", "bspzafm8.sav", "bsraadm8.sav", "bsradum8.sav", "bsrarem8.sav", "bsrashm8.sav", "bsrausm8.sav", "bsrautm8.sav", "bsrazem8.sav", "bsrbhrm8.sav", "bsrbram8.sav", "bsrchlm8.sav", "bsrcivm8.sav", "bsrcypm8.sav", "bsrczem8.sav", "bsrengm8.sav", "bsrfinm8.sav", "bsrfram8.sav", "bsrgeom8.sav", "bsrhkgm8.sav", "bsrhunm8.sav", "bsrirlm8.sav", "bsrirnm8.sav", "bsrisrm8.sav", "bsritam8.sav", "bsrjorm8.sav", "bsrjpnm8.sav", "bsrkazm8.sav", "bsrkorm8.sav", "bsrkwtm8.sav", "bsrltum8.sav", "bsrmarm8.sav", "bsrmltm8.sav", "bsrmysm8.sav", "bsrnorm8.sav", "bsrnzlm8.sav", "bsromnm8.sav", "bsrprtm8.sav", "bsrpsem8.sav", "bsrqatm8.sav", "bsrromm8.sav", "bsrsaum8.sav", "bsrsgpm8.sav", "bsrswem8.sav", "bsrturm8.sav", "bsrtwnm8.sav", "bsrusam8.sav", "bsruzbm8.sav", "bsrzafm8.sav", "bstaadm8.sav", "bstadum8.sav", "bstarem8.sav", "bstashm8.sav", "bstausm8.sav", "bstautm8.sav", "bstazem8.sav", "bstbhrm8.sav", "bstbram8.sav", "bstchlm8.sav", "bstcivm8.sav", "bstcypm8.sav", "bstczem8.sav", "bstengm8.sav", "bstfinm8.sav", "bstfram8.sav", "bstgeom8.sav", "bsthkgm8.sav", "bsthunm8.sav", "bstirlm8.sav", "bstirnm8.sav", "bstisrm8.sav", "bstitam8.sav", "bstjorm8.sav", "bstjpnm8.sav", "bstkazm8.sav", "bstkorm8.sav", "bstkwtm8.sav", "bstltum8.sav", "bstmarm8.sav", "bstmltm8.sav", "bstmysm8.sav", "bstnorm8.sav", "bstnzlm8.sav", "bstomnm8.sav", "bstprtm8.sav", "bstpsem8.sav", "bstqatm8.sav", "bstromm8.sav", "bstsaum8.sav", "bstsgpm8.sav", "bstswem8.sav", "bstturm8.sav", "bsttwnm8.sav", "bstusam8.sav", "bstuzbm8.sav", "bstzafm8.sav", "btmaadm8.sav", "btmadum8.sav", "btmarem8.sav", "btmashm8.sav", "btmausm8.sav", "btmautm8.sav", "btmazem8.sav", "btmbhrm8.sav", "btmbram8.sav", "btmchlm8.sav", "btmcivm8.sav", "btmcypm8.sav", "btmczem8.sav", "btmengm8.sav", "btmfinm8.sav", "btmfram8.sav", "btmgeom8.sav", "btmhkgm8.sav", "btmhunm8.sav", "btmirlm8.sav", "btmirnm8.sav", "btmisrm8.sav", "btmitam8.sav", "btmjorm8.sav", "btmjpnm8.sav", "btmkazm8.sav", "btmkorm8.sav", "btmkwtm8.sav", "btmltum8.sav", "btmmarm8.sav", "btmmltm8.sav", "btmmysm8.sav", "btmnorm8.sav", "btmnzlm8.sav", "btmomnm8.sav", "btmprtm8.sav", "btmpsem8.sav", "btmqatm8.sav", "btmromm8.sav", "btmsaum8.sav", "btmsgpm8.sav", "btmswem8.sav", "btmturm8.sav", "btmtwnm8.sav", "btmusam8.sav", "btmuzbm8.sav", "btmzafm8.sav", "btsaadm8.sav", "btsadum8.sav", "btsarem8.sav", "btsashm8.sav", "btsausm8.sav", "btsautm8.sav", "btsazem8.sav", "btsbhrm8.sav", "btsbram8.sav", "btschlm8.sav", "btscivm8.sav", "btscypm8.sav", "btsczem8.sav", "btsengm8.sav", "btsfinm8.sav", "btsfram8.sav", "btsgeom8.sav", "btshkgm8.sav", "btshunm8.sav", "btsirlm8.sav", "btsirnm8.sav", "btsisrm8.sav", "btsitam8.sav", "btsjorm8.sav", "btsjpnm8.sav", "btskazm8.sav", "btskorm8.sav", "btskwtm8.sav", "btsltum8.sav", "btsmarm8.sav", "btsmltm8.sav", "btsmysm8.sav", "btsnorm8.sav", "btsnzlm8.sav", "btsomnm8.sav", "btsprtm8.sav", "btspsem8.sav", "btsqatm8.sav", "btsromm8.sav", "btssaum8.sav", "btssgpm8.sav", "btsswem8.sav", "btsturm8.sav", "btstwnm8.sav", "btsusam8.sav", "btsuzbm8.sav", "btszafm8.sav"))
    ),
    TIMSS_Advanced_Mathematics_1995 = list(
      TIMSS_Advanced_Mathematics_1995_G12 = list("TA1995_IDB_SPSS_Mathematics/Data",
                                                 c("MCGAUSM1.SAV", "MCGAUTM1.SAV", "MCGCANM1.SAV", "MCGCHEM1.SAV", "MCGCYPM1.SAV", "MCGCZEM1.SAV", "MCGDEUM1.SAV", "MCGDNKM1.SAV", "MCGFRAM1.SAV", "MCGGRCM1.SAV", "MCGISRM1.SAV", "MCGITAM1.SAV", "MCGLTUM1.SAV", "MCGRUSM1.SAV", "MCGSVNM1.SAV", "MCGSWEM1.SAV", "MCGUSAM1.SAV", "MSAAUSM1.SAV", "MSAAUTM1.SAV", "MSACANM1.SAV", "MSACHEM1.SAV", "MSACYPM1.SAV", "MSACZEM1.SAV", "MSADEUM1.SAV", "MSADNKM1.SAV", "MSAFRAM1.SAV", "MSAGRCM1.SAV", "MSAISRM1.SAV", "MSAITAM1.SAV", "MSALTUM1.SAV", "MSARUSM1.SAV", "MSASVNM1.SAV", "MSASWEM1.SAV", "MSAUSAM1.SAV", "MSGAUSM1.SAV", "MSGAUTM1.SAV", "MSGCANM1.SAV", "MSGCHEM1.SAV", "MSGCYPM1.SAV", "MSGCZEM1.SAV", "MSGDEUM1.SAV", "MSGDNKM1.SAV", "MSGFRAM1.SAV", "MSGGRCM1.SAV", "MSGISRM1.SAV", "MSGITAM1.SAV", "MSGLTUM1.SAV", "MSGRUSM1.SAV", "MSGSVNM1.SAV", "MSGSWEM1.SAV", "MSGUSAM1.SAV", "MSRAUSM1.SAV", "MSRCANM1.SAV", "MSRCHEM1.SAV", "MSRCZEM1.SAV", "MSRDEUM1.SAV", "MSRDNKM1.SAV", "MSRFRAM1.SAV", "MSRRUSM1.SAV", "MSRSWEM1.SAV", "MSRUSAM1.SAV")
      )),
    TIMSS_Advanced_Physics_1995 = list(
      TIMSS_Advanced_Physics_1995_G12 = list("TA1995_IDB_SPSS_Physics/Data",
                                             c("PCGAUSM1.SAV", "PCGAUTM1.SAV", "PCGCANM1.SAV", "PCGCHEM1.SAV", "PCGCYPM1.SAV", "PCGCZEM1.SAV", "PCGDEUM1.SAV", "PCGDNKM1.SAV", "PCGFRAM1.SAV", "PCGGRCM1.SAV", "PCGISRM1.SAV", "PCGLVAM1.SAV", "PCGNORM1.SAV", "PCGRUSM1.SAV", "PCGSVNM1.SAV", "PCGSWEM1.SAV", "PCGUSAM1.SAV", "PSAAUSM1.SAV", "PSAAUTM1.SAV", "PSACANM1.SAV", "PSACHEM1.SAV", "PSACYPM1.SAV", "PSACZEM1.SAV", "PSADEUM1.SAV", "PSADNKM1.SAV", "PSAFRAM1.SAV", "PSAGRCM1.SAV", "PSAISRM1.SAV", "PSALVAM1.SAV", "PSANORM1.SAV", "PSARUSM1.SAV", "PSASVNM1.SAV", "PSASWEM1.SAV", "PSAUSAM1.SAV", "PSGAUSM1.SAV", "PSGAUTM1.SAV", "PSGCANM1.SAV", "PSGCHEM1.SAV", "PSGCYPM1.SAV", "PSGCZEM1.SAV", "PSGDEUM1.SAV", "PSGDNKM1.SAV", "PSGFRAM1.SAV", "PSGGRCM1.SAV", "PSGISRM1.SAV", "PSGLVAM1.SAV", "PSGNORM1.SAV", "PSGRUSM1.SAV", "PSGSVNM1.SAV", "PSGSWEM1.SAV", "PSGUSAM1.SAV", "PSRAUSM1.SAV", "PSRCANM1.SAV", "PSRCHEM1.SAV", "PSRCZEM1.SAV", "PSRDEUM1.SAV", "PSRDNKM1.SAV", "PSRFRAM1.SAV", "PSRNORM1.SAV", "PSRRUSM1.SAV", "PSRSWEM1.SAV", "PSRUSAM1.SAV")
      )),
    TIMSS_Advanced_Mathematics_2008 = list(
      TIMSS_Advanced_Mathematics_2008_G12 = list("TA2008_IDB_SPSS_Mathematics/Data",
                                                 c("MCGARMM2.SAV", "MCGIRNM2.SAV", "MCGITAM2.SAV", "MCGLBNM2.SAV", "MCGNLDM2.SAV", "MCGNORM2.SAV", "MCGPHLM2.SAV", "MCGRUSM2.SAV", "MCGSVNM2.SAV", "MCGSWEM2.SAV", "MSAARMM2.SAV", "MSAIRNM2.SAV", "MSAITAM2.SAV", "MSALBNM2.SAV", "MSANLDM2.SAV", "MSANORM2.SAV", "MSAPHLM2.SAV", "MSARUSM2.SAV", "MSASVNM2.SAV", "MSASWEM2.SAV", "MSGARMM2.SAV", "MSGIRNM2.SAV", "MSGITAM2.SAV", "MSGLBNM2.SAV", "MSGNLDM2.SAV", "MSGNORM2.SAV", "MSGPHLM2.SAV", "MSGRUSM2.SAV", "MSGSVNM2.SAV", "MSGSWEM2.SAV", "MSRARMM2.SAV", "MSRIRNM2.SAV", "MSRITAM2.SAV", "MSRLBNM2.SAV", "MSRNLDM2.SAV", "MSRNORM2.SAV", "MSRPHLM2.SAV", "MSRRUSM2.SAV", "MSRSVNM2.SAV", "MSRSWEM2.SAV", "MSTARMM2.SAV", "MSTIRNM2.SAV", "MSTITAM2.SAV", "MSTLBNM2.SAV", "MSTNLDM2.SAV", "MSTNORM2.SAV", "MSTPHLM2.SAV", "MSTRUSM2.SAV", "MSTSVNM2.SAV", "MSTSWEM2.SAV", "MTGARMM2.SAV", "MTGIRNM2.SAV", "MTGITAM2.SAV", "MTGLBNM2.SAV", "MTGNLDM2.SAV", "MTGNORM2.SAV", "MTGPHLM2.SAV", "MTGRUSM2.SAV", "MTGSVNM2.SAV", "MTGSWEM2.SAV")
      )),
    TIMSS_Advanced_Physics_2008 = list(
      TIMSS_Advanced_Physics_2008_G12 = list("TA2008_IDB_SPSS_Physics/Data",
                                             c("PCGARMM2.SAV", "PCGIRNM2.SAV", "PCGITAM2.SAV", "PCGLBNM2.SAV", "PCGNLDM2.SAV", "PCGNORM2.SAV", "PCGRUSM2.SAV", "PCGSVNM2.SAV", "PCGSWEM2.SAV", "PSAARMM2.SAV", "PSAIRNM2.SAV", "PSAITAM2.SAV", "PSALBNM2.SAV", "PSANLDM2.SAV", "PSANORM2.SAV", "PSARUSM2.SAV", "PSASVNM2.SAV", "PSASWEM2.SAV", "PSGARMM2.SAV", "PSGIRNM2.SAV", "PSGITAM2.SAV", "PSGLBNM2.SAV", "PSGNLDM2.SAV", "PSGNORM2.SAV", "PSGRUSM2.SAV", "PSGSVNM2.SAV", "PSGSWEM2.SAV", "PSRARMM2.SAV", "PSRIRNM2.SAV", "PSRITAM2.SAV", "PSRLBNM2.SAV", "PSRNLDM2.SAV", "PSRNORM2.SAV", "PSRRUSM2.SAV", "PSRSVNM2.SAV", "PSRSWEM2.SAV", "PSTARMM2.SAV", "PSTIRNM2.SAV", "PSTITAM2.SAV", "PSTLBNM2.SAV", "PSTNLDM2.SAV", "PSTNORM2.SAV", "PSTRUSM2.SAV", "PSTSVNM2.SAV", "PSTSWEM2.SAV", "PTGARMM2.SAV", "PTGIRNM2.SAV", "PTGITAM2.SAV", "PTGLBNM2.SAV", "PTGNLDM2.SAV", "PTGNORM2.SAV", "PTGRUSM2.SAV", "PTGSVNM2.SAV", "PTGSWEM2.SAV")
      )),
    TIMSS_Advanced_Mathematics_2015 = list(
      TIMSS_Advanced_Mathematics_2015_G12 = list("TA2015_IDB_SPSS_Mathematics/Data",
                                                 c("MCGFRAM3.sav", "MCGITAM3.sav", "MCGLBNM3.sav", "MCGNORM3.sav", "MCGPRTM3.sav", "MCGRTRM3.sav", "MCGRUSM3.sav", "MCGSVNM3.sav", "MCGSWEM3.sav", "MCGUSAM3.sav", "MSAFRAM3.sav", "MSAITAM3.sav", "MSALBNM3.sav", "MSANORM3.sav", "MSAPRTM3.sav", "MSARTRM3.sav", "MSARUSM3.sav", "MSASVNM3.sav", "MSASWEM3.sav", "MSAUSAM3.sav", "MSGFRAM3.sav", "MSGITAM3.sav", "MSGLBNM3.sav", "MSGNORM3.sav", "MSGPRTM3.sav", "MSGRTRM3.sav", "MSGRUSM3.sav", "MSGSVNM3.sav", "MSGSWEM3.sav", "MSGUSAM3.sav", "MSRFRAM3.sav", "MSRITAM3.sav", "MSRLBNM3.sav", "MSRNORM3.sav", "MSRPRTM3.sav", "MSRRTRM3.sav", "MSRRUSM3.sav", "MSRSVNM3.sav", "MSRSWEM3.sav", "MSRUSAM3.sav", "MSTFRAM3.sav", "MSTITAM3.sav", "MSTLBNM3.sav", "MSTNORM3.sav", "MSTPRTM3.sav", "MSTRTRM3.sav", "MSTRUSM3.sav", "MSTSVNM3.sav", "MSTSWEM3.sav", "MSTUSAM3.sav", "MTGFRAM3.sav", "MTGITAM3.sav", "MTGLBNM3.sav", "MTGNORM3.sav", "MTGPRTM3.sav", "MTGRTRM3.sav", "MTGRUSM3.sav", "MTGSVNM3.sav", "MTGSWEM3.sav", "MTGUSAM3.sav")
      )),
    TIMSS_Advanced_Physics_2015 = list(
      TIMSS_Advanced_Physics_2015_G12 = list("TA2015_IDB_SPSS_Physics/Data",
                                             c("PCGFRAM3.sav", "PCGITAM3.sav", "PCGLBNM3.sav", "PCGNORM3.sav", "PCGPRTM3.sav", "PCGRUSM3.sav", "PCGSVNM3.sav", "PCGSWEM3.sav", "PCGUSAM3.sav", "PSAFRAM3.sav", "PSAITAM3.sav", "PSALBNM3.sav", "PSANORM3.sav", "PSAPRTM3.sav", "PSARUSM3.sav", "PSASVNM3.sav", "PSASWEM3.sav", "PSAUSAM3.sav", "PSGFRAM3.sav", "PSGITAM3.sav", "PSGLBNM3.sav", "PSGNORM3.sav", "PSGPRTM3.sav", "PSGRUSM3.sav", "PSGSVNM3.sav", "PSGSWEM3.sav", "PSGUSAM3.sav", "PSRFRAM3.sav", "PSRITAM3.sav", "PSRLBNM3.sav", "PSRNORM3.sav", "PSRPRTM3.sav", "PSRRUSM3.sav", "PSRSVNM3.sav", "PSRSWEM3.sav", "PSRUSAM3.sav", "PSTFRAM3.sav", "PSTITAM3.sav", "PSTLBNM3.sav", "PSTNORM3.sav", "PSTPRTM3.sav", "PSTRUSM3.sav", "PSTSVNM3.sav", "PSTSWEM3.sav", "PSTUSAM3.sav", "PTGFRAM3.sav", "PTGITAM3.sav", "PTGLBNM3.sav", "PTGNORM3.sav", "PTGPRTM3.sav", "PTGRUSM3.sav", "PTGSVNM3.sav", "PTGSWEM3.sav", "PTGUSAM3.sav")
      )),
    TiPi_2011 = list(
      TiPi_2011_G4 = list("TIMSS&PIRLS2011_IDB_SPSS/Data",
                          c("acgaadB1.sav", "acgaduB1.sav", "acgareB1.sav", "acgausB1.sav", "acgautB1.sav", "acgazeB1.sav", "acgbwaB1.sav", "acgcquB1.sav", "acgczeB1.sav", "acgdeuB1.sav", "acgespB1.sav", "acgfinB1.sav", "acggeoB1.sav", "acghkgB1.sav", "acghndB1.sav", "acghrvB1.sav", "acghunB1.sav", "acgirlB1.sav", "acgirnB1.sav", "acgitaB1.sav", "acgltuB1.sav", "acgmarB1.sav", "acgmltB1.sav", "acgnirB1.sav", "acgnorB1.sav", "acgomnB1.sav", "acgpolB1.sav", "acgprtB1.sav", "acgqatB1.sav", "acgromB1.sav", "acgrusB1.sav", "acgsauB1.sav", "acgsgpB1.sav", "acgsvkB1.sav", "acgsvnB1.sav", "acgsweB1.sav", "acgtwnB1.sav", "asaaadB1.sav", "asaaduB1.sav", "asaareB1.sav", "asaausB1.sav", "asaautB1.sav", "asaazeB1.sav", "asabwaB1.sav", "asacquB1.sav", "asaczeB1.sav", "asadeuB1.sav", "asaespB1.sav", "asafinB1.sav", "asageoB1.sav", "asahkgB1.sav", "asahndB1.sav", "asahrvB1.sav", "asahunB1.sav", "asairlB1.sav", "asairnB1.sav", "asaitaB1.sav", "asaltuB1.sav", "asamarB1.sav", "asamltB1.sav", "asanirB1.sav", "asanorB1.sav", "asaomnB1.sav", "asapolB1.sav", "asaprtB1.sav", "asaqatB1.sav", "asaromB1.sav", "asarusB1.sav", "asasauB1.sav", "asasgpB1.sav", "asasvkB1.sav", "asasvnB1.sav", "asasweB1.sav", "asatwnB1.sav", "asgaadB1.sav", "asgaduB1.sav", "asgareB1.sav", "asgausB1.sav", "asgautB1.sav", "asgazeB1.sav", "asgbwaB1.sav", "asgcquB1.sav", "asgczeB1.sav", "asgdeuB1.sav", "asgespB1.sav", "asgfinB1.sav", "asggeoB1.sav", "asghkgB1.sav", "asghndB1.sav", "asghrvB1.sav", "asghunB1.sav", "asgirlB1.sav", "asgirnB1.sav", "asgitaB1.sav", "asgltuB1.sav", "asgmarB1.sav", "asgmltB1.sav", "asgnirB1.sav", "asgnorB1.sav", "asgomnB1.sav", "asgpolB1.sav", "asgprtB1.sav", "asgqatB1.sav", "asgromB1.sav", "asgrusB1.sav", "asgsauB1.sav", "asgsgpB1.sav", "asgsvkB1.sav", "asgsvnB1.sav", "asgsweB1.sav", "asgtwnB1.sav", "ashaadB1.sav", "ashaduB1.sav", "ashareB1.sav", "ashausB1.sav", "ashautB1.sav", "ashazeB1.sav", "ashbwaB1.sav", "ashcquB1.sav", "ashczeB1.sav", "ashdeuB1.sav", "ashespB1.sav", "ashfinB1.sav", "ashgeoB1.sav", "ashhkgB1.sav", "ashhndB1.sav", "ashhrvB1.sav", "ashhunB1.sav", "ashirlB1.sav", "ashirnB1.sav", "ashitaB1.sav", "ashltuB1.sav", "ashmarB1.sav", "ashmltB1.sav", "ashnirB1.sav", "ashnorB1.sav", "ashomnB1.sav", "ashpolB1.sav", "ashprtB1.sav", "ashqatB1.sav", "ashromB1.sav", "ashrusB1.sav", "ashsauB1.sav", "ashsgpB1.sav", "ashsvkB1.sav", "ashsvnB1.sav", "ashsweB1.sav", "ashtwnB1.sav", "astaadB1.sav", "astaduB1.sav", "astareB1.sav", "astausB1.sav", "astautB1.sav", "astazeB1.sav", "astbwaB1.sav", "astcquB1.sav", "astczeB1.sav", "astdeuB1.sav", "astespB1.sav", "astfinB1.sav", "astgeoB1.sav", "asthkgB1.sav", "asthndB1.sav", "asthrvB1.sav", "asthunB1.sav", "astirlB1.sav", "astirnB1.sav", "astitaB1.sav", "astltuB1.sav", "astmarB1.sav", "astmltB1.sav", "astnirB1.sav", "astnorB1.sav", "astomnB1.sav", "astpolB1.sav", "astprtB1.sav", "astqatB1.sav", "astromB1.sav", "astrusB1.sav", "astsauB1.sav", "astsgpB1.sav", "astsvkB1.sav", "astsvnB1.sav", "astsweB1.sav", "asttwnB1.sav", "atgaadB1.sav", "atgaduB1.sav", "atgareB1.sav", "atgausB1.sav", "atgautB1.sav", "atgazeB1.sav", "atgbwaB1.sav", "atgcquB1.sav", "atgczeB1.sav", "atgdeuB1.sav", "atgespB1.sav", "atgfinB1.sav", "atggeoB1.sav", "atghkgB1.sav", "atghndB1.sav", "atghrvB1.sav", "atghunB1.sav", "atgirlB1.sav", "atgirnB1.sav", "atgitaB1.sav", "atgltuB1.sav", "atgmarB1.sav", "atgmltB1.sav", "atgnirB1.sav", "atgnorB1.sav", "atgomnB1.sav", "atgpolB1.sav", "atgprtB1.sav", "atgqatB1.sav", "atgromB1.sav", "atgrusB1.sav", "atgsauB1.sav", "atgsgpB1.sav", "atgsvkB1.sav", "atgsvnB1.sav", "atgsweB1.sav", "atgtwnB1.sav")
      )),
    PISA_2015 = list(
      PISA_2015_Y15 = list(".",
                           c("CY6_MS_CM2_SCH_QQQ.sav", "CY6_MS_CM2_STU_COG.sav", "CY6_MS_CM2_STU_CPS.sav", "CY6_MS_CM2_STU_QQ2.sav", "CY6_MS_CM2_STU_QQQ.sav", "CY6_MS_CM2_STU_QTM.sav", "CY6_MS_CM2_STU_TTM.SAV", "CY6_MS_CM2_TCH_QQQ.sav", "CY6_MS_CMB_FLT_TTM.sav", "CY6_MS_CMB_SCH_QQQ.sav", "CY6_MS_CMB_STU_COG.sav", "CY6_MS_CMB_STU_CPS.sav", "CY6_MS_CMB_STU_FLT.sav", "CY6_MS_CMB_STU_QQ2.sav", "CY6_MS_CMB_STU_QQQ.sav", "CY6_MS_CMB_STU_QTM.sav", "CY6_MS_CMB_STU_TTM.sav", "CY6_MS_CMB_TCH_QQQ.sav"))
    ),
    PISA_2018 = list(
      PISA_2018_Y15 = list(".",
                           c("CY07_MSU_FLT_COG.SAV", "CY07_MSU_FLT_QQQ.SAV", "CY07_MSU_FLT_TIM.SAV", "CY07_MSU_FLT_TTM.SAV", "CY07MSU_QMC_FLT_COG.SAV", "CY07MSU_QMC_FLT_QQQ.SAV", "CY07MSU_QMC_FLT_TIM.SAV", "CY07MSU_QMC_SCH_QQQ.sav", "CY07MSU_QMC_STU_COG.sav", "CY07MSU_QMC_STU_QQQ.sav", "CY07MSU_QMC_STU_TIM.sav", "CY07_MSU_SCH_QQQ.sav", "CY07_MSU_STU_COG.sav", "CY07_MSU_STU_QQQ.sav", "CY07_MSU_STU_TIM.sav", "CY07_MSU_STU_TTM.SAV", "CY07_MSU_TCH_QQQ.sav", "CY07_QMC_FLT_TTM.SAV", "CY07_QMC_STU_TTM.SAV", "CY07_VNM_STU_COG.sav", "CY07_VNM_STU_PVS.sav"))
    ),
    PISA_2022 = list(
      PISA_2022_Y15 = list(".",
                           c("CY08MSP_SCH_QQQ.SAV", "CY08MSP_STU_COG.SAV", "CY08MSP_STU_QQQ.SAV", "CY08MSP_STU_TIM.SAV", "CY08MSP_TCH_QQQ.SAV", "CY08MSP_FLT_COG.SAV", "CY08MSP_FLT_QQQ.SAV", "CY08MSP_FLT_TIM.SAV"))
    ),
    PISA_D_2019 = list(
      PISA_D_2019_IS = list(".",
                            c("CY1MDAI_SCH_QQQ.sav", "CY1MDAI_STU_COG.sav", "CY1MDAI_STU_QQQ.sav", "CY1MDAI_TCH_QQQ.sav")),
      PISA_D_2019_OS = list(".",
                            c("CY1MDCI_COG.SAV", "CY1MDCI_QQQ.SAV", "CY1MDCI_TIM.SAV"))
    )
  ),
  ZIP.roots = list(
    "CivED" = list(
      "1999" = list(G8 = "CivED/CivED1999/CivED1999_IDB_SPSS_G8.zip", G12 = "CivED/CivED1999/CivED1999_IDB_SPSS_G12.zip")
    ),
    "ICCS" = list(
      "2009" = list(G8 = "ICCS/ICCS2009/ICCS2009_IDB_SPSS.zip", G9 = "ICCS/ICCS2009/ICCS2009_IDB_SPSS.zip"),
      "2016" = list(G8 = "ICCS/ICCS2016/ICCS2016_IDB_SPSS.zip"),
      "2022" = list(G8 = "ICCS/ICCS2022/ICCS2022_IDB_SPSS.zip")
    ),
    "ICILS" = list(
      "2013" = list(G8 = "ICILS/ICILS2013/ICILS2013_IDB_SPSS.zip"),
      "2018" = list(G8 = "ICILS/ICILS2018/ICILS2018_IDB_SPSS.zip"),
      "2023" = list(G8 = "ICILS/ICILS2023/ICILS2023_IDB_SPSS.zip")
    ),
    "PIRLS" = list(
      "2001" = list(G4 = "PIRLS/PIRLS2001/PIRLS2001_IDB_SPSS.zip"),
      "2006" = list(G4 = "PIRLS/PIRLS2006/PIRLS2006_IDB_SPSS.zip"),
      "2011" = list(G4 = "PIRLS/PIRLS2011/PIRLS2011_IDB_SPSS.zip"),
      "2016" = list(G4 = "PIRLS/PIRLS2016/PIRLS2016_IDB_SPSS.zip"),
      "2021" = list(G4 = "PIRLS/PIRLS2021/PIRLS2021_IDB_SPSS.zip")
    ),
    "prePIRLS" = list(
      "2011" = list(G4 = "PIRLS/PIRLS2011/PIRLS2011_IDB_SPSS.zip"),
      "2016" = list(G4 = "PIRLS/PIRLS2016/PIRLS_Literacy2016_IDB_SPSS.zip")
    ),
    "ePIRLS" = list(
      "2016" = list(G4 = "PIRLS/PIRLS2016/ePIRLS2016_IDB_SPSS.zip")
    ),
    "REDS" = list(
      "2021" = list(G8 = "REDS/REDS2021/REDS2021_IDB_SPSS.zip")
    ),
    "RLII" = list(
      "1991" = list(G4 = "RLII/RLII1991/RLII1991_IDB_SPSS.zip"),
      "2001" = list(G4 = "RLII/RLII2001/RLII2001_IDB_SPSS.zip")
    ),
    "SITES" = list(
      "1998" = list(M1 = "SITES/SITES1998/SITES1998_IDB_SPSS.zip"),
      "2006" = list(M2 = "SITES/SITES2006/SITES2006_IDB_SPSS.zip")
    ),
    "TIMSS Advanced Mathematics" = list(
      "1995" = list(G12 = "TIMSSAdvanced/TA1995/TA1995_IDB_SPSS_Mathematics.zip"),
      "2008" = list(G12 = "TIMSSAdvanced/TA2008/TA2008_IDB_SPSS_Mathematics.zip"),
      "2015" = list(G12 = "TIMSSAdvanced/TA2015/TA2015_IDB_SPSS_Mathematics.zip")
    ),
    "TIMSS Advanced Physics" = list(
      "1995" = list(G12 = "TIMSSAdvanced/TA1995/TA1995_IDB_SPSS_Physics.zip"),
      "2008" = list(G12 = "TIMSSAdvanced/TA2008/TA2008_IDB_SPSS_Physics.zip"),
      "2015" = list(G12 = "TIMSSAdvanced/TA2015/TA2015_IDB_SPSS_Physics.zip")
    ),
    "TiPi" = list(
      "2011" = list(G4 = "TiPi/TiPi2011/TIMSS%26PIRLS2011_IDB_SPSS.zip")
    ),
    "TIMSS" = list(
      "1995" = list(G4 = "TIMSS/TIMSS1995/TIMSS1995_IDB_SPSS_G4.zip", G8 = "TIMSS/TIMSS1995/TIMSS1995_IDB_SPSS_G8.zip"),
      "1999" = list(G8 = "TIMSS/TIMSS1999/TIMSS1999_IDB_SPSS_G8.zip"),
      "2003" = list(G4 = "TIMSS/TIMSS2003/TIMSS2003_IDB_SPSS_G4.zip", G8 = "TIMSS/TIMSS2003/TIMSS2003_IDB_SPSS_G8.zip"),
      "2007" = list(G4 = "TIMSS/TIMSS2007/TIMSS2007_IDB_SPSS_G4.zip", G8 = "TIMSS/TIMSS2007/TIMSS2007_IDB_SPSS_G8.zip"),
      "2011" = list(G4 = "TIMSS/TIMSS2011/TIMSS2011_IDB_SPSS_G4.zip", G8 = "TIMSS/TIMSS2011/TIMSS2011_IDB_SPSS_G8.zip"),
      "2015" = list(G4 = "TIMSS/TIMSS2015/TIMSS2015_IDB_SPSS_G4.zip", G8 = "TIMSS/TIMSS2015/TIMSS2015_IDB_SPSS_G8.zip"),
      "2019" = list(G4 = "TIMSS/TIMSS2019/TIMSS2019_IDB_SPSS_G4.zip", G8 = "TIMSS/TIMSS2019/TIMSS2019_IDB_SPSS_G8.zip"),
      "2023" = list(G4 = "TIMSS/TIMSS2023/TIMSS2023_IDB_SPSS_G4.zip", G8 = "TIMSS/TIMSS2023/TIMSS2023_IDB_SPSS_G8.zip")
    ),
    "preTIMSS" = list(
      "2015" = list(G4 = "TIMSS/TIMSS2015/TIMSS2015_IDB_SPSS_G4.zip")
    ),
    "eTIMSS PSI" = list(
      "2019" = list(G4 = "TIMSS/TIMSS2019/TIMSS2019_IDB_SPSS_G4.zip", G8 = "TIMSS/TIMSS2019/TIMSS2019_IDB_SPSS_G8.zip")
    ),
    "TALIS" = list(
      "2008" = list(I2 = "SPSS_2008_national.zip"),
      "2013" = list(I1 = "SPSS_2013_national.zip", I2 = "SPSS_2013_national.zip", I3 = "SPSS_2013_national.zip", P = "SPSS_2013_national.zip"),
      "2018" = list(I1 = "SPSS_2018_national.zip", I2 = "SPSS_2018_national.zip", I3 = "SPSS_2018_national.zip", P = "SPSS_2018_national.zip"),
      "2024" = list(I1 = c("TALIS2024_principals_NoESE_SPSS.zip", "TALIS2024_teachers_NoESE_SPSS.zip"), I2 = c("TALIS2024_principals_NoESE_SPSS.zip", "TALIS2024_teachers_NoESE_SPSS.zip"), I3 = c("TALIS2024_principals_NoESE_SPSS.zip", "TALIS2024_teachers_NoESE_SPSS.zip"))
    ),
    "TALIS 3S" = list(
      "2018" = list(I0.2 = "TALIS-Starting-Strong-By-country-SPSS.zip", IU3 = "TALIS-Starting-Strong-By-country-SPSS.zip")
    ),
    "PISA" = list(
      "2015" = list(Y15 = c("PUF_SPSS_COMBINED_CMB_STU_QQQ.zip", "PUF_SPSS_COMBINED_CMB_SCH_QQQ.zip", "PUF_SPSS_COMBINED_CMB_TCH_QQQ.zip", "PUF_SPSS_COMBINED_CMB_STU_COG.zip", "PUF_SPSS_COMBINED_CMB_STU_QTM.zip", "PUF_SPSS_COMBINED_CM2_STU_QQQ_COG_QTM_SCH_TCH.zip", "PUF_SPSS_COMBINED_CMB_STU_FLT.zip", "PUF_SPSS_COMBINED_CMB_STU_CPS.zip", "PUF_SPSS_STU_TTM.zip")),
      "2018" = list(Y15 = c("SPSS_QMC_ALLDB.zip", "SPSS_SCH_QQQ.zip", "SPSS_STU_COG.zip", "SPSS_STU_FLT.zip", "SPSS_STU_QQQ.zip", "SPSS_STU_TIM.zip", "SPSS_STU_TTM.zip", "SPSS_TCH_QQQ.zip", "SPSS_VNM_PV_COG.zip")),
      "2022" = list(Y15 = c("SCH_QQQ_SPSS.zip", "STU_COG_SPSS.zip", "STU_QQQ_SPSS.zip", "STU_TIM_SPSS.zip", "TCH_QQQ_SPSS.zip", "FLT_SPSS.zip"))
    ),
    "PISA D" = list(
      "2019" = list(IS = c("STU_QQQ_spss.zip", "SCH_QQQ_spss.zip", "TCH_QQQ_spsss.zip", "STU_COG_spss.zip"), OS = c("CY1MDCI_COG.SAV", "CY1MDCI_QQQ.SAV", "CY1MDCI_TIM.SAV"))
    )
  )
)
graph.custom.colors <- c("#F40040", "#72F842", "#4F45FA", "#877A77", "#E83BD1", "#FEBF32", "#00D1F8", "#32DBAB", "#BE4B3D", "#B898EC", "#B3C865", "#EC168E", "#F995BB", "#BB26EF", "#00622A", "#459AFB", "#8B600D", "#C2E7F7", "#8A0D7A", "#4B7DA8", "#F5D2A3", "#F4C9EA", "#C7EBC7", "#3D40B0", "#F79BF0", "#56F6F3", "#EEDE22", "#E69782", "#F85171", "#8FE98D", "#F0751C", "#697216", "#C33D75", "#AA77FE", "#9D69A3", "#CC5DE0", "#BDE635", "#F665C2", "#65BDAC", "#FE5635", "#A3687E", "#4F407C", "#CDC9FC", "#5C9600", "#F516F6", "#DDC1BB", "#6AB9FB", "#2ABE38", "#71AA79", "#A34F4B", "#F9A763", "#DBBB58", "#979EB6", "#53B1C9", "#49F695", "#949B78", "#F1AEB8", "#ECBBFE", "#FC7A89", "#C85CA7", "#D88DBD", "#53654B", "#4265DF", "#7276BD", "#927858", "#A0F268", "#ADA862", "#1CC072", "#C22A55", "#1C8C38", "#DDE4DC", "#D04235", "#96E6AF", "#DBD7B4", "#BE78D1", "#9B5CD1", "#8A2EE5", "#D20032", "#EBE47F", "#A53B6E", "#699294", "#F93DB7", "#865AB0", "#E78356", "#FA76E4", "#8CF2D7", "#C5E6A1", "#C09FCB", "#93E8FB", "#0DA876", "#3B8E71", "#8EA9E7", "#C70D9B", "#F54D8B", "#AEAEB0", "#E6AD7F", "#88BE0D", "#8391FC", "#CE9EB2", "#42C0C2")
produce.percentages.plots <- function(data.obj, split.vars.vector, type, perc.graph.xlab, perc.graph.ylab) {
  if(type == "ordinary") {
    if(length(split.vars.vector) == 1) {
      x.var <- sym(split.vars.vector)
      y.var <- sym(paste0("Percentages_", split.vars.vector))
      y.var.SE <- sym(paste0("Percentages_", split.vars.vector, "_SE"))
      fill.var <- y.var
    } else if(length(split.vars.vector) == 2) {
      x.var <- sym(split.vars.vector[2])
      y.var <- sym(paste0("Percentages_", split.vars.vector[2]))
      y.var.SE <- sym(paste0("Percentages_", split.vars.vector[2], "_SE"))
      fill.var <- y.var
    } else if(length(split.vars.vector) > 2) {
      x.var <- sym(split.vars.vector[2])
      y.var <- sym(paste0("Percentages_", split.vars.vector[length(split.vars.vector)]))
      y.var.SE <- sym(paste0("Percentages_", split.vars.vector[length(split.vars.vector)], "_SE"))
      fill.var <- sym("collapsed_split")
    }
  } else if(type == "bench") {
    x.var <- sym("Performance_Group")
    y.var.SE <- sym(grep(pattern = "^Percentages_.+_SE$", x = colnames(data.obj[[1]]), value = TRUE))
    y.var <- sym(gsub(pattern = "_SE$", replacement = "", x = y.var.SE))
    if(length(split.vars.vector) == 1) {
      fill.var <- y.var
    } else if(length(split.vars.vector) > 1) {
      fill.var <- sym("collapsed_split")
    }
  }
  if(is.null(perc.graph.xlab)) {
    perc.graph.xlab <- x.var
  }
  if(is.null(perc.graph.ylab)) {
    perc.graph.ylab <- y.var
  }
  if(length(split.vars.vector) <= 2) {
    lapply(X = data.obj, FUN = function(i) {
      tmp.percentages <- ggplot(data = i, aes(x = !! x.var, y = !! y.var, fill = factor(x = make.unique(as.character(!! fill.var)), levels = make.unique(as.character(!! fill.var)))))
      if(type == "ordinary" || type == "bench" & length(split.vars.vector) == 1) {
        tmp.percentages <- tmp.percentages + geom_bar(stat="identity", color="black",
                                                      position=position_dodge(),
                                                      show.legend = FALSE)
        if(nrow(i) <= 100) {
          tmp.percentages <- tmp.percentages + scale_fill_manual(labels = i[ , get(as.character(x.var))], values = graph.custom.colors)
        }
      } else if(type == "bench" && length(split.vars.vector) > 1) {
        tmp.percentages <- tmp.percentages + geom_bar(stat="identity", color="black",
                                                      position=position_dodge())
        tmp.percentages <- tmp.percentages + labs(fill = "Legend")
        tmp.percentages <- tmp.percentages + geom_vline(xintercept = 1:(length(unique(i[ , get("Performance_Group")])) - 1) + 0.5, colour = "#e2e2e2")
        tmp.percentages <- tmp.percentages + scale_fill_manual(labels = i[ , get(as.character(fill.var))], values = graph.custom.colors)
      }
      tmp.percentages <- tmp.percentages + theme(panel.background = element_rect(fill = "white"),
                                                 panel.grid.major.x = element_blank(),
                                                 panel.grid.major = element_line(colour = "black"),
                                                 panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
                                                 plot.background = element_rect(fill = "#e2e2e2"),
                                                 legend.background = element_rect(fill = "#e2e2e2"),
                                                 plot.title = element_text(hjust = 0.5))
      tmp.percentages <- tmp.percentages + geom_errorbar(aes(ymin = !!y.var - 1.96 * !!y.var.SE, ymax = !!y.var + 1.96 * !!y.var.SE),
                                                         width = 0.2,
                                                         linewidth = 1,
                                                         position = position_dodge(0.9))
      tmp.percentages <- tmp.percentages + scale_x_discrete(labels = function(j) {
        str_wrap(string = j, width = 15)
      })
      tmp.percentages <- tmp.percentages + scale_y_continuous(labels = function(j) {
        paste0(j, "%")
      },
      limits = c(0, 100),
      expand = expansion(mult = c(0, 0)),
      n.breaks = 10, minor_breaks = NULL)
      if(type == "ordinary") {
        tmp.percentages <- tmp.percentages + labs(title = unique(i[ , get(split.vars.vector[1])]), x = perc.graph.xlab, y = gsub(pattern = "_", replacement = " ", x = perc.graph.ylab))
      } else if(type == "bench") {
        tmp.percentages <- tmp.percentages + labs(title = unique(i[ , get(split.vars.vector[1])]), x = gsub(pattern = "\\_", replacement = " ", x = perc.graph.xlab), y = gsub(pattern = "_", replacement = " ", x = perc.graph.ylab))
      }
      return(tmp.percentages)
    })
  } else if(length(split.vars.vector) > 2 && length(split.vars.vector) < 5) {
    lapply(X = data.obj, FUN = function(i) {
      tmp.percentages <- ggplot(data = i, aes(x = !! x.var, y = !! y.var, fill = factor(x = make.unique(as.character(!! fill.var)), levels = make.unique(as.character(!! fill.var)))))
      tmp.percentages <- tmp.percentages + geom_bar(stat="identity", color="black",
                                                    position=position_dodge())
      if(nrow(i) <= 100) {
        tmp.percentages <- tmp.percentages + scale_fill_manual(labels = i[ , get(as.character(fill.var))], values = graph.custom.colors)
      }
      if(type == "ordinary") {
        tmp.percentages <- tmp.percentages + geom_vline(xintercept = 1:(length(unique(i[ , get(split.vars.vector[2])])) - 1) + 0.5, colour = "#e2e2e2")
      } else if(type == "bench") {
        tmp.percentages <- tmp.percentages + geom_vline(xintercept = 1:(length(unique(i[ , get("Performance_Group")])) - 1) + 0.5, colour = "#e2e2e2")
      }
      tmp.percentages <- tmp.percentages + theme(panel.background = element_rect(fill = "white"),
                                                 panel.grid.major.x = element_blank(),
                                                 panel.grid.major = element_line(colour = "black"),
                                                 panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
                                                 plot.background = element_rect(fill = "#e2e2e2"),
                                                 legend.background = element_rect(fill = "#e2e2e2"),
                                                 plot.title = element_text(hjust = 0.5))
      tmp.percentages <- tmp.percentages + geom_errorbar(aes(ymin = !!y.var - 1.96 * !!y.var.SE, ymax = !!y.var + 1.96 * !!y.var.SE),
                                                         width = 0.2,
                                                         linewidth = 1,
                                                         position = position_dodge(0.9))
      tmp.percentages <- tmp.percentages + scale_x_discrete(labels = function(j) {
        str_wrap(string = j, width = 20)
      })
      tmp.percentages <- tmp.percentages + scale_y_continuous(labels = function(j) {
        paste0(j, "%")
      },
      limits = c(0, 100),
      expand = expansion(mult = c(0, 0)),
      n.breaks = 10, minor_breaks = NULL)
      if(type == "ordinary") {
        tmp.percentages <- tmp.percentages + labs(title = unique(i[ , get(split.vars.vector[1])]), x = perc.graph.xlab, y = gsub(pattern = "_", replacement = " ", x = perc.graph.ylab), fill = "Legend")
      } else if(type == "bench") {
        tmp.percentages <- tmp.percentages + labs(title = unique(i[ , get(split.vars.vector[1])]), x = gsub(pattern = "\\_", replacement = " ", x = perc.graph.xlab), y = gsub(pattern = "_", replacement = " ", x = perc.graph.ylab), fill = "Legend")
      }
      return(tmp.percentages)
    })
  } else if(length(split.vars.vector) >= 5) {
    lapply(X = data.obj, FUN = function(i) {
      tmp.percentages <- ggplot(data = i, aes(x = !! x.var, y = !! y.var, fill = factor(x = make.unique(as.character(!! fill.var)), levels = make.unique(as.character(!! fill.var)))))
      tmp.percentages <- tmp.percentages + geom_bar(stat="identity", color="black",
                                                    position=position_dodge(),
                                                    show.legend = FALSE)
      scale_fill_manual(labels = i[ , get(as.character(x.var))], values = graph.custom.colors)
      tmp.percentages <- tmp.percentages + geom_vline(xintercept = 1:(length(unique(i[ , get(split.vars.vector[2])])) - 1) + 0.5, colour = "#e2e2e2")
      tmp.percentages <- tmp.percentages + theme(panel.background = element_rect(fill = "white"),
                                                 panel.grid.major.x = element_blank(),
                                                 panel.grid.major = element_line(colour = "black"),
                                                 panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
                                                 plot.background = element_rect(fill = "#e2e2e2"),
                                                 legend.background = element_rect(fill = "#e2e2e2"),
                                                 plot.title = element_text(hjust = 0.5))
      tmp.percentages <- tmp.percentages + geom_errorbar(aes(ymin = !!y.var - 1.96 * !!y.var.SE, ymax = !!y.var + 1.96 * !!y.var.SE),
                                                         width = 0.2,
                                                         linewidth = 1,
                                                         position = position_dodge(0.9))
      tmp.percentages <- tmp.percentages + scale_x_discrete(labels = function(j) {
        str_wrap(string = j, width = 20)
      })
      tmp.percentages <- tmp.percentages + scale_y_continuous(labels = function(j) {
        paste0(j, "%")
      },
      limits = c(0, 100),
      expand = expansion(mult = c(0, 0)),
      n.breaks = 10, minor_breaks = NULL)
      if(type == "ordinary") {
        tmp.percentages <- tmp.percentages + labs(title = unique(i[ , get(split.vars.vector[1])]), x = perc.graph.xlab, y = gsub(pattern = "_", replacement = " ", x = perc.graph.ylab))
      } else if(type == "bench") {
        tmp.percentages <- tmp.percentages + labs(title = unique(i[ , get(split.vars.vector[1])]), x = gsub(pattern = "\\_", replacement = " ", x = perc.graph.xlab), y = gsub(pattern = "_", replacement = " ", x = perc.graph.ylab))
      }
      return(tmp.percentages)
    })
  }
}
produce.means.plots <- function(data.obj, estimates.obj, split.vars.vector, type, mean.graph.xlab, mean.graph.ylab) {
  if(type == "ordinary") {
    if(length(split.vars.vector) == 1) {
      x.var <- sym(split.vars.vector)
    } else if(length(split.vars.vector) == 2) {
      x.var <- sym(split.vars.vector[2])
    } else if(length(split.vars.vector) == 2) {
      x.var <- sym(split.vars.vector[2])
    } else if(length(split.vars.vector) > 2) {
      x.var <- sym(split.vars.vector[length(split.vars.vector)])
      color.var <- sym("collapsed_split")
    }
  } else if(type == "bench") {
    x.var <- sym("Performance_Group")
    if(length(split.vars.vector) == 1) {
      color.var <- x.var
    } else if(length(split.vars.vector) > 1) {
      color.var <- sym("collapsed_split")
    }
  }
  y.var <- grep(pattern = "^Mean_[[:alnum:]]|^Median_[[:alnum:]]|^Mode_[[:alnum:]]", x = colnames(estimates.obj), value = TRUE)
  y.var <- y.var[!y.var %in% grep(pattern = "_SE$|_SVR$|_MVR$|_SVR_SE$|_MVR_SE$", x = y.var, value = TRUE)]
  y.var <- lapply(X = y.var, FUN = sym)
  if(is.null(mean.graph.xlab)) {
    mean.graph.xlab <- as.character(x.var)
    mean.graph.xlab <- as.list(rep(x = mean.graph.xlab, times = length(y.var)))
  }
  if(is.null(mean.graph.ylab)) {
    mean.graph.ylab <- as.character(y.var)
    mean.graph.ylab <- as.list(rep(x = mean.graph.ylab, times = length(y.var)))
  }
  if(length(split.vars.vector) <= 2) {
    lapply(X = data.obj, FUN = function(i) {
      tmp.means <- lapply(X = 1:length(y.var), FUN = function(j) {
        if(type == "ordinary" || type == "bench" & length(split.vars.vector) == 1) {
          cnt.plot <- ggplot(data = i, aes(x = !!x.var, y = !!y.var[[j]]))
        } else if(type == "bench" && length(split.vars.vector) > 1) {
          cnt.plot <- ggplot(data = i, aes(x = !!x.var, y = !!j, color = factor(x = !!color.var, levels = !!color.var)))
        }
        cnt.plot <- cnt.plot + geom_errorbar(aes(ymin = !!y.var[[j]] - 1.96 * !!sym(paste0(y.var[[j]], "_SE")), ymax = !!y.var[[j]] + 1.96 * !!sym(paste0(y.var[[j]], "_SE"))),
                                             width = 0.3,
                                             linewidth = 1.3,
                                             position = position_dodge(.9))
        if(type == "ordinary" || type == "bench" & length(split.vars.vector) == 1) {
          cnt.plot <- cnt.plot + geom_point(size = 3)
        } else if(type == "bench" && length(split.vars.vector) > 1) {
          cnt.plot <- cnt.plot + geom_point(size = 3, position = position_dodge(0.9))
          cnt.plot <- cnt.plot + geom_vline(xintercept = 1:(length(unique(i[ , get(as.character(x.var))])) - 1) + 0.5, colour = "#e2e2e2")
          cnt.plot <- cnt.plot + guides(color = guide_legend(title="Legend", override.aes = list(linetype = 0, size = 3.5)))
          cnt.plot <- cnt.plot + scale_color_manual(labels = i[ , get(as.character(color.var))], values = graph.custom.colors)
        }
        cnt.plot <- cnt.plot + theme(panel.background = element_rect(fill = "white"),
                                     panel.grid.major.x = element_blank(),
                                     panel.grid.major = element_line(colour = "black"),
                                     panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                                     plot.background = element_rect(fill = "#e2e2e2"),
                                     legend.background = element_rect(fill = "#e2e2e2"),
                                     legend.key = element_blank(),
                                     plot.title = element_text(hjust = 0.5))
        cnt.plot <- cnt.plot + scale_x_discrete(labels = function(k) {
          str_wrap(k, width = 20)
        })
        cnt.plot <- cnt.plot + scale_y_continuous(labels = function(k) {
          sprintf("%.2f", k)
        })
        if(type == "ordinary") {
          cnt.plot + labs(title = unique(i[ , get(split.vars.vector[1])]), x = mean.graph.xlab[j], y = gsub(pattern = "_", replacement = " ", x = mean.graph.ylab[j]))
        } else if(type == "bench") {
          cnt.plot + labs(title = unique(i[ , get(split.vars.vector[1])]), x = gsub(pattern = "\\_", replacement = " ", x = mean.graph.xlab[j]), y = gsub(pattern = "_", replacement = " ", x = mean.graph.ylab[j]))
        }
      })
      names(tmp.means) <- unlist(as.character(y.var))
      return(tmp.means)
    })
  } else if(length(split.vars.vector) > 2 && length(split.vars.vector) < 5) {
    lapply(X = data.obj, FUN = function(i) {
      tmp.means <- lapply(X = 1:length(y.var), FUN = function(j) {
        cnt.plot <- ggplot(data = i, aes(x = !!x.var, y= !!y.var[[j]], color = factor(x = !!color.var, levels = !!color.var)))
        cnt.plot <- cnt.plot + geom_errorbar(aes(ymin = !!y.var[[j]] - 1.96 * !!sym(paste0(y.var[[j]], "_SE")), ymax = !!y.var[[j]] + 1.96 * !!sym(paste0(y.var[[j]], "_SE"))),
                                             width = 0.3,
                                             linewidth = 1.3,
                                             position = position_dodge(.9),
                                             show.legend = FALSE)
        if(type == "ordinary") {
          cnt.plot <- cnt.plot + geom_vline(xintercept = 1:(length(unique(i[ , get(split.vars.vector[length(split.vars.vector)])])) - 1) + 0.5, colour = "#e2e2e2")
        } else if(type == "bench") {
          cnt.plot <- cnt.plot + geom_vline(xintercept = 1:(length(unique(i[ , get("Performance_Group")])) - 1) + 0.5, colour = "#e2e2e2")
        }
        cnt.plot <- cnt.plot + geom_point(position = position_dodge(0.9), size = 4)
        cnt.plot <- cnt.plot + scale_color_manual(labels = i[ , get(as.character(color.var))], values = graph.custom.colors)
        cnt.plot <- cnt.plot + theme(panel.background = element_rect(fill = "white"),
                                     panel.grid.major.y = element_line(colour = "black"),
                                     panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                                     plot.background = element_rect(fill = "#e2e2e2"),
                                     legend.background = element_rect(fill = "#e2e2e2"),
                                     legend.key = element_blank(),
                                     plot.title = element_text(hjust = 0.5),
                                     panel.grid.major.x = element_blank())
        cnt.plot <- cnt.plot + scale_x_discrete(labels = function(k) {
          str_wrap(k, width = 20)
        })
        cnt.plot <- cnt.plot + scale_y_continuous(labels = function(k) {
          sprintf("%.2f", k)
        })
        cnt.plot <- cnt.plot + guides(color = guide_legend(title="Legend"))
        if(type == "ordinary") {
          cnt.plot + labs(title = unique(i[ , get(split.vars.vector[1])]), x = mean.graph.xlab[j], y = gsub(pattern = "_", replacement = " ", x = mean.graph.ylab[j]))
        } else if(type == "bench") {
          cnt.plot + labs(title = unique(i[ , get(split.vars.vector[1])]), x = gsub(pattern = "\\_", replacement = " ", x = mean.graph.xlab[j]), y = gsub(pattern = "_", replacement = " ", x = mean.graph.ylab[j]))
        }
      })
      names(tmp.means) <- unlist(as.character(y.var))
      return(tmp.means)
    })
  } else if(length(split.vars.vector) >= 5) {
    lapply(X = data.obj, FUN = function(i) {
      tmp.means <- lapply(X = 1:length(y.var), FUN = function(j) {
        cnt.plot <- ggplot(data = i, aes(x = !!x.var, y= !!y.var[[j]], color = !!color.var))
        cnt.plot <- cnt.plot + geom_errorbar(aes(ymin = !!y.var[[j]] - 1.96 * !!sym(paste0(y.var[[j]], "_SE")), ymax = !!y.var[[j]] + 1.96 * !!sym(paste0(y.var[[j]], "_SE"))),
                                             width = 0.3,
                                             linewidth = 1.3,
                                             position = position_dodge(.9),
                                             show.legend = FALSE)
        if(nrow(i) <= 100) {
          cnt.plot <- cnt.plot + scale_color_manual(labels = i[ , get(as.character(color.var))], values = graph.custom.colors)
        }
        cnt.plot <- cnt.plot + geom_vline(xintercept = 1:(length(unique(i[ , get(split.vars.vector[length(split.vars.vector)])])) - 1) + 0.5, colour = "#e2e2e2")
        cnt.plot <- cnt.plot + geom_point(position = position_dodge(0.9), size = 4,
                                          show.legend = FALSE)
        cnt.plot <- cnt.plot + theme(panel.background = element_rect(fill = "white"),
                                     panel.grid.major.y = element_line(colour = "black"),
                                     panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                                     plot.background = element_rect(fill = "#e2e2e2"),
                                     legend.background = element_rect(fill = "#e2e2e2"),
                                     legend.key = element_blank(),
                                     plot.title = element_text(hjust = 0.5),
                                     panel.grid.major.x = element_blank())
        cnt.plot <- cnt.plot + scale_x_discrete(labels = function(k) {
          str_wrap(k, width = 20)
        })
        cnt.plot <- cnt.plot + scale_y_continuous(labels = function(k) {
          sprintf("%.2f", k)
        })
        cnt.plot <- cnt.plot + guides(color=guide_legend(title="Legend"))
        if(type == "ordinary") {
          cnt.plot + labs(title = unique(i[ , get(split.vars.vector[1])]), x = mean.graph.xlab[j], y = gsub(pattern = "_", replacement = " ", x = mean.graph.ylab[j]))
        } else if(type == "bench") {
          cnt.plot + labs(title = unique(i[ , get(split.vars.vector[1])]), x = gsub(pattern = "\\_", replacement = " ", x = mean.graph.xlab[j]), y = gsub(pattern = "_", replacement = " ", x = mean.graph.ylab[j]))
        }
      })
      names(tmp.means) <- unlist(as.character(y.var))
      return(tmp.means)
    })
  }
}
produce.percentiles.plots <- function(data.obj, estimates.obj, split.vars.vector, prctl.graph.xlab, prctl.graph.ylab) {
  y.var <- grep(pattern = "_SE$", x = colnames(data.obj[[1]]), value = TRUE)
  y.var <- gsub(pattern = "_SE$", replacement = "", x = y.var)
  y.var <- lapply(X = y.var, FUN = sym)
  if(is.null(prctl.graph.xlab)) {
    prctl.graph.xlab <- as.list(rep(x = "Percentiles", times = length(y.var)))
  }
  if(is.null(prctl.graph.ylab)) {
    prctl.graph.ylab <- y.var
  }
  lapply(X = data.obj, FUN = function(i) {
    if(length(split.vars.vector) <= 2) {
      x.var <- sym("variable")
      group.var <- sym(split.vars.vector[length(split.vars.vector)])
      color.var <- sym(split.vars.vector[length(split.vars.vector)])
    } else if(length(split.vars.vector) > 2) {
      x.var <- sym("collapsed_split")
      group.var <- sym(split.vars.vector[length(split.vars.vector)])
      facet.var <- split.vars.vector[length(split.vars.vector)]
    }
    tmp.prctls <- lapply(X = 1:length(y.var), FUN = function(j) {
      if(length(split.vars.vector) <= 2) {
        cnt.plot <- ggplot(data = i, aes(x = !!x.var, y = !!y.var[[j]], group = !!group.var, color = !!color.var))
      } else {
        cnt.plot <- ggplot(data = i, aes(x = !!x.var, y = !!y.var[[j]], group = !!group.var))
        cnt.plot <- cnt.plot + facet_wrap(i[ , get(facet.var)] ~ interaction(i[ , mget(split.vars.vector[2:length(split.vars.vector)])], sep = " - "), scales = "free_x", labeller = function(df) {list(str_wrap(string = as.character(df[ , 2]), width = 25))})
      }
      cnt.plot <- cnt.plot + geom_errorbar(aes(ymin = !!y.var[[j]] - 1.96 * !!sym(paste0(y.var[[j]], "_SE")), ymax = !!y.var[[j]] + 1.96 * !!sym(paste0(y.var[[j]], "_SE"))),
                                           width = 0.3,
                                           linewidth = 1.3)
      cnt.plot <- cnt.plot + geom_point(size = 3, position = position_dodge(0.1))
      cnt.plot <- cnt.plot + geom_line(linewidth = 1)
      cnt.plot <- cnt.plot + theme(panel.background = element_rect(fill = "white"),
                                   panel.grid.major.x = element_blank(),
                                   panel.grid.major = element_line(colour = "black"),
                                   panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                                   plot.background = element_rect(fill = "#e2e2e2"),
                                   strip.background = element_rect(color = "black", fill = "#9E9E9E", linewidth = 1, linetype = "solid"),
                                   legend.background = element_rect(fill = "#e2e2e2"),
                                   legend.key = element_blank(),
                                   plot.title = element_text(hjust = 0.5))
      if(length(split.vars.vector) == 1) {
        cnt.plot <- cnt.plot + theme(legend.position = "none")
      }
      if(length(split.vars.vector) > 1) {
        cnt.plot <- cnt.plot + scale_color_manual(values = graph.custom.colors)
      } else {
        cnt.plot <- cnt.plot + scale_color_manual(values = "#000000")
      }
      cnt.plot <- cnt.plot + scale_y_continuous(labels = function(k) {
        sprintf("%.2f", k)
      })
      if(length(split.vars.vector) < 2) {
        suppressMessages(cnt.plot <- cnt.plot + scale_x_discrete(labels = gsub(pattern = "Prctl_", replacement = "P", x = unique(i[ , variable]))))
      } else {
        suppressMessages(cnt.plot <- cnt.plot + scale_x_discrete(labels = unique(str_extract(string = i[ , collapsed_split], pattern = "^P[[:digit:]]+"))))
      }
      cnt.plot <- cnt.plot + guides(color = guide_legend(title = "Legend", override.aes = list(linetype = 0, size = 3.5)))
      cnt.plot <- cnt.plot + labs(title = unique(i[ , get(split.vars.vector[1])]), x = prctl.graph.xlab[j], y = prctl.graph.ylab[j])
    })
    names(tmp.prctls) <- unlist(as.character(y.var))
    return(tmp.prctls)
  })
}
produce.crosstabs.plots <- function(data.obj, split.vars.vector, row.var, col.var, name.col.var, name.row.var) {
  x.var <- sym(col.var)
  y.var <- sym(row.var)
  color.var <- sym("value")
  facet.var <- split.vars.vector[length(split.vars.vector)]
  tmp.crosstabs <- lapply(X = data.obj, FUN = function(i) {
    if(length(split.vars.vector) == 1) {
      cnt.plot <- ggplot(data = i, aes(x = !!x.var, y = ordered(!!y.var, levels = rev(levels(!!y.var))), fill = !!color.var))
    } else if(length(split.vars.vector) == 2) {
      cnt.plot <- ggplot(data = i, aes(x = !!x.var, y = ordered(!!y.var, levels = rev(levels(!!y.var))), fill = !!color.var, group = i[ , get(split.vars.vector[length(split.vars.vector)])]))
      cnt.plot <- cnt.plot + facet_wrap(i[ , get(facet.var)] ~ .)
    } else if(length(split.vars.vector) > 2) {
      cnt.plot <- ggplot(data = i, aes(x = !!x.var, y = ordered(!!y.var, levels = rev(levels(!!y.var))), fill = !!color.var, group = interaction(i[ , mget(split.vars.vector[2:length(split.vars.vector)])], sep = " - ")))
      cnt.plot <- cnt.plot + facet_wrap(i[ , get(facet.var)] ~ interaction(i[ , mget(split.vars.vector[2:length(split.vars.vector)])], sep = " - "), labeller = function(df) {list(str_wrap(string = as.character(df[ , 2]), width = 25))})
    }
    cnt.plot <- cnt.plot + geom_tile(color = "grey")
    cnt.plot <- cnt.plot + scale_fill_gradient(low = "white", high = "red")
    cnt.plot <- cnt.plot + ggtitle(unique(i[ , get(split.vars.vector[1])]))
    cnt.plot <- cnt.plot + xlab(label = name.col.var)
    cnt.plot <- cnt.plot + ylab(label = name.row.var)
    cnt.plot <- cnt.plot + geom_text(aes(label = round(x = value, digits = 0)))
    cnt.plot <- cnt.plot + scale_x_discrete(expand = c(0,0), position = "top", labels = str_wrap(string = i[ , get(as.character(x.var))], width = 10))
    if(is.factor(i[ , get(y.var)])) {
      cnt.plot <- cnt.plot + scale_y_discrete(expand = c(0,0), labels = str_wrap(string = rev(levels(droplevels(i[ , get(as.character(y.var))]))), width = 10))
    } else {
      cnt.plot <- cnt.plot + scale_y_discrete(expand = c(0,0), labels = str_wrap(string = y.var, width = 10))
    }
    cnt.plot <- cnt.plot + coord_equal()
    cnt.plot <- cnt.plot + theme(panel.background = element_rect(fill = "white"),
                                 panel.grid.major.x = element_blank(),
                                 panel.grid.major = element_line(colour = "black"),
                                 panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                                 plot.background = element_rect(fill = "#e2e2e2"),
                                 strip.background = element_rect(color = "black", fill = "#9E9E9E", linewidth = 1, linetype = "solid"),
                                 legend.background = element_rect(fill = "#e2e2e2"),
                                 legend.key = element_blank(),
                                 plot.title = element_text(hjust = 0.5))
    cnt.plot <- cnt.plot + labs(fill="Observed\ncounts")
  })
  names(tmp.crosstabs) <- paste0(names(tmp.crosstabs), "_Crosstabs")
  return(tmp.crosstabs)
}
produce.missing.patterns.plots <- function(data.obj, split.vars.vector, var.names) {
  if(length(var.names) < 10) {
    missing.pattern.scaled.dpi <- 300
    height.width <- 150
  } else if(length(var.names) >= 10 & length(var.names) < 20) {
    missing.pattern.scaled.dpi <- 450
    height.width <- 200
  } else if(length(var.names) >= 20 & length(var.names) < 30) {
    missing.pattern.scaled.dpi <- 600
    height.width <- 250
  } else if(length(var.names) >= 30 & length(var.names) < 40) {
    missing.pattern.scaled.dpi <- 750
    height.width <- 300
  } else if(length(var.names) >= 40) {
    missing.pattern.scaled.dpi <- 900
    height.width <- 350
  }
  font.scaled.size <- (length(var.names) ^ -0.3) * 15
  tmp.patterns <- lapply(data.obj, function(i) {
    i[ , (split.vars.vector) := lapply(.SD, function(j) {
      as.character(j)
    }), .SDcols = split.vars.vector]
    i[ , c(grep(pattern = "_SE$", x = colnames(i), value = TRUE)) := NULL]
    i[ , Missings_in_Pattern := as.character(Missings_in_Pattern)]
    i[ , Missings_in_Pattern := ifelse(test = is.na(Missings_in_Pattern), yes = "NA", no = Missings_in_Pattern)]
    max.nchar.missings.in.pattern <- max(nchar(i[ , Missings_in_Pattern]))
    annotation.spaces <- paste0(rep(" ", times = max.nchar.missings.in.pattern))
    i[ , Total_Missings_per_Pattern := sprintf(fmt = "%.2f", Total_Missings_per_Pattern)]
    i[, Missings_in_Pattern := paste0(strrep(x = " ", times = max(nchar(Missings_in_Pattern)) - nchar(Missings_in_Pattern)), Missings_in_Pattern)]
    i[ , Total_Missings_per_Pattern := paste0(strrep(x = " ", times = max(nchar(Total_Missings_per_Pattern)) - nchar(Total_Missings_per_Pattern)), Total_Missings_per_Pattern)]
    i[ , tmp := "    "]
    i[ , Missings_in_Pattern := paste0(Missings_in_Pattern, tmp, Total_Missings_per_Pattern)]
    i[ , c("Total_Missings_per_Pattern", "tmp") := NULL]
    i.total <- tail(i, n= 1)
    i <- head(i, n = nrow(i) - 1)
    i[ , Pattern_Counts := sprintf(fmt = "%.2f", Pattern_Counts)]
    i[ , Pattern_Counts := make.unique(names = Pattern_Counts, sep = "")]
    i[ , Pattern_Counts := factor(x = Pattern_Counts, levels = as.character(Pattern_Counts), ordered = FALSE)]
    i <- melt(data = i, id.vars = c(split.vars.vector, "Pattern_Counts", "Missings_in_Pattern"))
    i.total <- melt(data = i.total, id.vars = c(split.vars.vector, "Pattern_Counts", "Missings_in_Pattern"))
    y.secondary.labels <- unique(i[ , Missings_in_Pattern, by = "Pattern_Counts"])[ , Missings_in_Pattern]
    x.secondary.labels <- sprintf(fmt = "%.2f", i.total[ , value])
    ggplot(i, aes(x = as.numeric(variable), y = as.numeric(ordered(Pattern_Counts, levels = rev(levels(Pattern_Counts)))), fill = as.factor(value))) +
      geom_tile(color = "black", linewidth = 0.5) +
      scale_fill_manual(values = c("#CC5E85", "#4C98D4")) +
      scale_y_continuous(
        breaks = 1:length(levels(i[ , Pattern_Counts])), labels = rev(sprintf(fmt = "%.2f", as.numeric(levels(i[ , Pattern_Counts])))),
        sec.axis = sec_axis(~ ., breaks = length(y.secondary.labels):1, labels = y.secondary.labels),
        expand = c(0, 0)
      ) +
      scale_x_continuous(
        breaks = 1:length(levels(i[ , variable])), labels = levels(i[ , variable]), position = "top",
        sec.axis = sec_axis(~ ., breaks = length(x.secondary.labels):1, labels = rev(x.secondary.labels)),
        expand = c(0, 0)
      ) +
      coord_equal(clip = "off") +
      {if(length(split.vars.vector) > 1) {
        width.split <- floor(nchar(paste(unique(i[ , mget(split.vars.vector)]), collapse = " // ")) / length(var.names) * 3)
        ggtitle(label = unique(i[ , get(split.vars.vector[1])]), subtitle = str_wrap(paste(unique(i[ , mget(split.vars.vector)]), collapse = " // "), width = width.split))
      } else {
        ggtitle(label = unique(i[ , get(split.vars.vector[1])]))
      }
      } +
      annotate(geom = "text", x = Inf, y = -Inf, label = unique(i.total[ , Missings_in_Pattern]), hjust = -0.06, vjust = 1.8, family = "mono", size = font.scaled.size / .pt) +
      theme(
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#e2e2e2"),
        panel.background = element_rect(fill = "#e2e2e2", colour = "#e2e2e2", linewidth = 2),
        axis.text.x.top = element_text(angle = 90, color = "black", size = font.scaled.size, vjust = 0.5, hjust = 0),
        axis.text.x.bottom = element_text(angle = 270, color = "black", family = "mono", size = font.scaled.size, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(color = "black", family = "mono", size = font.scaled.size),
        axis.text.y.right = element_text(color = "black", family = "mono", size = font.scaled.size),
        legend.position="none",
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  })
  return(tmp.patterns)
}
produce.flux.plots <- function(data.obj, split.vars.vector, weighted) {
  x.var <- sym("Influx")
  y.var <- sym("Outflux")
  facet.var <- split.vars.vector[length(split.vars.vector)]
  if(length(split.vars.vector) == 1) {
    font.size <- 5
  } else if(length(split.vars.vector) == 2) {
    font.size <- 4
  } else if(length(split.vars.vector) > 2) {
    font.size <- 3
  }
  tmp.fluxplots <- lapply(X = data.obj, FUN = function(i) {
    if(length(split.vars.vector) == 1) {
      cnt.plot <- ggplot(data = i, aes(x = !!x.var, y = !!y.var, label = Variables))
    } else if(length(split.vars.vector) == 2) {
      cnt.plot <- ggplot(data = i, aes(x = !!x.var, y = !!y.var, group = i[ , get(split.vars.vector[length(split.vars.vector)])], label = Variables))
      cnt.plot <- cnt.plot + facet_wrap(i[ , get(facet.var)] ~ .)
    } else if(length(split.vars.vector) > 2) {
      cnt.plot <- ggplot(data = i, aes(x = !!x.var, y = !!y.var, group = interaction(i[ , mget(split.vars.vector[2:length(split.vars.vector)])], sep = " - "), label = Variables))
      cnt.plot <- cnt.plot + facet_wrap(i[ , get(facet.var)] ~ interaction(i[ , mget(split.vars.vector[2:length(split.vars.vector)])], sep = " - "), labeller = function(df) {list(str_wrap(string = as.character(df[ , 2]), width = 25))})
    }
    cnt.plot <- cnt.plot + geom_vline(aes(xintercept = .5), linetype = "longdash", color = "#bfbfbf") +
      geom_hline(aes(yintercept = .5), linetype = "longdash", color = "#bfbfbf") +
      geom_abline(aes(intercept = 1, slope = -1), linetype = "longdash", color = "black", linewidth = 0.8)
    cnt.plot <- cnt.plot + geom_point(shape = 21, fill = "red", size = 3) +
      geom_text(hjust = -0.15, vjust = -0.15, size = font.size) +
      ggtitle(label = unique(i[ , get(split.vars.vector[1])]))
    if(isTRUE(weighted)) {
      cnt.plot <- cnt.plot + geom_errorbar(aes(ymin = i[ , Outflux] - 1.96 * i[ , Outflux_SE], ymax = i[ , Outflux] + 1.96 * i[ , Outflux_SE]), width = 0.02) +
        geom_errorbarh(aes(xmin = i[ , Influx] - 1.96 * i[ , Influx_SE], xmax = i[ , Influx] + 1.96 * i[ , Influx_SE]), height = 0.02)
    }
    cnt.plot <- cnt.plot + xlab(label = "Influx")
    cnt.plot <- cnt.plot + ylab(label = "Outflux")
    cnt.plot <- cnt.plot + coord_equal(xlim = c(0, 1), ylim = c(0, 1))
    cnt.plot <- cnt.plot + theme(panel.background = element_rect(fill = "white"),
                                 panel.grid.major = element_line(colour = "#e2e2e2", linetype = "longdash"),
                                 panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                                 plot.background = element_rect(fill = "#e2e2e2"),
                                 strip.background = element_rect(color = "black", fill = "#9E9E9E", linewidth = 1, linetype = "solid"),
                                 legend.background = element_rect(fill = "#e2e2e2"),
                                 legend.key = element_blank(),
                                 plot.title = element_text(hjust = 0.5))
  })
  if(isTRUE(weighted)) {
    names(tmp.fluxplots) <- paste0(names(tmp.fluxplots), "_Fluxplots_Weighted")
  } else if(isFALSE(weighted)) {
    names(tmp.fluxplots) <- paste0(names(tmp.fluxplots), "_Fluxplots_Unweighted")
  }
  return(tmp.fluxplots)
}
save.graphs <- function(out.path) {
  key.vars.length <- length(get(x = "key.vars", envir = parent.frame()))
  if(key.vars.length <= 2) {
    plot.width <- 250
    plot.height <- 125
  } else if(key.vars.length > 2 && key.vars.length <= 4) {
    plot.width <- 350
    plot.height <- 175
  } else if(key.vars.length > 4) {
    plot.width <- 450
    plot.height <- 225
  }
  if(exists("perc.graphs.list", where = parent.frame())) {
    if(is.null(out.path)) {
      out.dir.percentages <- getwd()
    } else {
      out.dir.percentages <- dirname(out.path)
    }
    percentage.plots.files <- paste0(names(get("perc.graphs.list", pos = parent.frame())), "_Percentages.png")
    assign(x = "percentage.plots.files", value = percentage.plots.files, pos = parent.frame())
    write.percentage.plots.files <- function(input1, input2) {
      ggsave(filename = input1, plot = input2, width = plot.width, height = plot.height, units = "mm", device = "png", path = out.dir.percentages)
    }
    Map(write.percentage.plots.files, input1 = percentage.plots.files, input2 = get("perc.graphs.list", pos = parent.frame()))
  }
  if(exists("means.graphs.list", where = parent.frame())) {
    means.obj.copy <- get("means.graphs.list", pos = parent.frame())
    means.plots.files <- lapply(X = names(means.obj.copy), function(i) {
      means.plot.name <- unlist(lapply(X = i, FUN = function(j) {
        names(means.obj.copy[[j]])
      }))
      means.plot.name <- lapply(X = means.plot.name, FUN = function(k) {
        paste0(paste(i, k, sep = "_"), ".png")
      })
    })
    assign(x = "means.plots.files", value = means.plots.files, pos = parent.frame())
    write.means.plots.files <- function(input1, input2) {
      if(is.null(out.path)) {
        out.dir.means <- getwd()
      } else {
        out.dir.means <- dirname(out.path)
      }
      lapply(X = 1:length(input1), FUN = function(i) {
        suppressWarnings(ggsave(filename = unlist(input1[[i]], recursive = FALSE), plot = input2[[i]], width = plot.width, height = plot.height, units = "mm", device = "png", path = out.dir.means))
      })
    }
    Map(write.means.plots.files, input1 = means.plots.files, input2 = get("means.graphs.list", pos = parent.frame()))
  }
  if(exists("percentiles.graphs.list", where = parent.frame())) {
    percentiles.obj.copy <- get("percentiles.graphs.list", pos = parent.frame())
    percentiles.plots.files <- lapply(X = names(percentiles.obj.copy), function(i) {
      percentiles.plot.name <- unlist(lapply(X = i, FUN = function(j) {
        names(percentiles.obj.copy[[j]])
      }))
      percentiles.plot.name <- lapply(X = percentiles.plot.name, FUN = function(k) {
        paste0(paste(i, "Percentiles", k, sep = "_"), ".png")
      })
    })
    assign(x = "percentiles.plots.files", value = percentiles.plots.files, pos = parent.frame())
    write.percentiles.plots.files <- function(input1, input2) {
      if(is.null(out.path)) {
        out.dir.percentiles <- getwd()
      } else {
        out.dir.percentiles <- dirname(out.path)
      }
      lapply(X = 1:length(input1), FUN = function(i) {
        suppressWarnings(ggsave(filename = unlist(input1[[i]], recursive = FALSE), plot = input2[[i]], width = plot.width, height = plot.height, units = "mm", device = "png", path = out.dir.percentiles))
      })
    }
    Map(write.percentiles.plots.files, input1 = percentiles.plots.files, input2 = percentiles.obj.copy)
  }
  if(exists("crosstabs.graphs.list", where = parent.frame())) {
    crosstabs.obj.copy <- get("crosstabs.graphs.list", pos = parent.frame())
    crosstabs.plots.files <- paste0(names(crosstabs.obj.copy), ".png")
    assign(x = "crosstabs.plots.files", value = crosstabs.plots.files, pos = parent.frame())
    if(is.null(out.path)) {
      out.dir.crosstabs <- getwd()
    } else {
      out.dir.crosstabs <- dirname(out.path)
    }
    lapply(X = names(crosstabs.obj.copy), FUN = function(i) {
      ggsave(filename = paste0(i, ".png"), plot = crosstabs.obj.copy[[i]], width = plot.width, height = plot.height, units = "mm", device = "png", path = out.dir.crosstabs)
    })
  }
  if(exists("missing.patterns.graphs.list", where = parent.frame())) {
    missing.patterns.obj.copy <- get("missing.patterns.graphs.list", pos = parent.frame())
    missing.patterns.plots.files <- paste0(names(missing.patterns.obj.copy), ".png")
    assign(x = "missing.patterns.plots.files", value = missing.patterns.plots.files, pos = parent.frame())
    if(is.null(out.path)) {
      out.dir.missing.patterns <- getwd()
    } else {
      out.dir.missing.patterns <- dirname(out.path)
    }
    var.names.length <- get(x = "vars.list", pos = parent.frame())
    var.names.length <- length(var.names.length[["variables"]])
    if(var.names.length < 10) {
      missing.pattern.scaled.dpi <- 300
      height.width <- 150
    } else if(var.names.length >= 10 & var.names.length < 20) {
      missing.pattern.scaled.dpi <- 450
      height.width <- 200
    } else if(var.names.length >= 20 & var.names.length < 30) {
      missing.pattern.scaled.dpi <- 600
      height.width <- 250
    } else if(var.names.length >= 30 & var.names.length < 40) {
      missing.pattern.scaled.dpi <- 750
      height.width <- 300
    } else if(var.names.length >= 40) {
      missing.pattern.scaled.dpi <- 900
      height.width <- 350
    }
    lapply(X = names(missing.patterns.obj.copy), FUN = function(i) {
      ggsave(filename = paste0(i, ".png"), plot = missing.patterns.obj.copy[[i]], width = height.width, height = height.width, units = "mm", dpi = missing.pattern.scaled.dpi, device = "png", path = out.dir.missing.patterns)
    })
  }
  if(exists("unweighted.fluxplots.list", where = parent.frame())) {
    unweighted.fluxplots.obj.copy <- get("unweighted.fluxplots.list", pos = parent.frame())
    unweighted.fluxplots.files <- paste0(names(unweighted.fluxplots.obj.copy), ".png")
    assign(x = "unweighted.fluxplots.files", value = unweighted.fluxplots.files, pos = parent.frame())
    if(is.null(out.path)) {
      out.dir.fluxplots <- getwd()
    } else {
      out.dir.fluxplots <- dirname(out.path)
    }
    lapply(X = names(unweighted.fluxplots.obj.copy), FUN = function(i) {
      ggsave(filename = paste0(i, ".png"), plot = unweighted.fluxplots.obj.copy[[i]], width = plot.width, height = plot.width, units = "mm", device = "png", path = out.dir.fluxplots)
    })
  }
  if(exists("weighted.fluxplots.list", where = parent.frame())) {
    weighted.fluxplots.obj.copy <- get("weighted.fluxplots.list", pos = parent.frame())
    weighted.fluxplots.files <- paste0(names(weighted.fluxplots.obj.copy), ".png")
    assign(x = "weighted.fluxplots.files", value = weighted.fluxplots.files, pos = parent.frame())
    if(is.null(out.path)) {
      out.dir.fluxplots <- getwd()
    } else {
      out.dir.fluxplots <- dirname(out.path)
    }
    lapply(X = names(weighted.fluxplots.obj.copy), FUN = function(i) {
      ggsave(filename = paste0(i, ".png"), plot = weighted.fluxplots.obj.copy[[i]], width = plot.width, height = plot.width, units = "mm", device = "png", path = out.dir.fluxplots)
    })
  }
}
delete.graphs <- function(out.path) {
  if(is.null(out.path)) {
    out.dir.percentages <- getwd()
  } else {
    out.dir.percentages <- dirname(out.path)
  }
  if(exists("perc.graphs.list", where = parent.frame())) {
    percentage.plots.files <- paste0(names(get("perc.graphs.list", pos = parent.frame())), "_Percentages.png")
    lapply(X = percentage.plots.files, FUN = function(i) {
      file.remove(file.path(out.dir.percentages, i))
    })
  }
  if(exists("means.graphs.list", where = parent.frame())) {
    if(is.null(out.path)) {
      out.dir.means <- getwd()
    } else {
      out.dir.means <- dirname(out.path)
    }
    means.obj.copy <- get("means.graphs.list", pos = parent.frame())
    means.plots.files <- lapply(X = names(means.obj.copy), function(i) {
      means.plot.name <- unlist(lapply(X = i, FUN = function(j) {
        names(means.obj.copy[[j]])
      }))
      means.plot.name <- lapply(X = means.plot.name, FUN = function(k) {
        paste0(paste(i, k, sep = "_"), ".png")
      })
    })
    lapply(X = means.plots.files, FUN = function(i) {
      file.remove(file.path(out.dir.means, i))
    })
  }
  if(exists("percentiles.plots.files", where = parent.frame())) {
    if(is.null(out.path)) {
      out.dir.percentiles <- getwd()
    } else {
      out.dir.percentiles <- dirname(out.path)
    }
    lapply(X = get("percentiles.plots.files", pos = parent.frame()), FUN = function(i) {
      file.remove(file.path(out.dir.percentiles, i))
    })
  }
  if(exists("crosstabs.plots.files", where = parent.frame())) {
    if(is.null(out.path)) {
      out.dir.crosstabs <- getwd()
    } else {
      out.dir.crosstabs <- dirname(out.path)
    }
    lapply(X = get("crosstabs.plots.files", pos = parent.frame()), FUN = function(i) {
      file.remove(file.path(out.dir.crosstabs, i))
    })
  }
  if(exists("missing.patterns.plots.files", where = parent.frame())) {
    if(is.null(out.path)) {
      out.dir.explore.missings <- getwd()
    } else {
      out.dir.explore.missings <- dirname(out.path)
    }
    lapply(X = get("missing.patterns.plots.files", pos = parent.frame()), FUN = function(i) {
      file.remove(file.path(out.dir.explore.missings, i))
    })
  }
  if(exists("unweighted.fluxplots.files", where = parent.frame())) {
    if(is.null(out.path)) {
      out.dir.explore.missings <- getwd()
    } else {
      out.dir.explore.missings <- dirname(out.path)
    }
    lapply(X = get("unweighted.fluxplots.files", pos = parent.frame()), FUN = function(i) {
      file.remove(file.path(out.dir.explore.missings, i))
    })
  }
  if(exists("weighted.fluxplots.files", where = parent.frame())) {
    if(is.null(out.path)) {
      out.dir.explore.missings <- getwd()
    } else {
      out.dir.explore.missings <- dirname(out.path)
    }
    lapply(X = get("weighted.fluxplots.files", pos = parent.frame()), FUN = function(i) {
      file.remove(file.path(out.dir.explore.missings, i))
    })
  }
}
produce.analysis.data.table <- function(data.object, object.variables, action.arguments, imported.file.attributes) {
  if(imported.file.attributes[["lsa.study"]] %in% design.weight.variables[["IEA.JK2.studies"]]) {
    DESIGN <- "JRR"
  } else if(imported.file.attributes[["lsa.study"]] %in% c(design.weight.variables[["IEA.BRR.studies"]], design.weight.variables[["OECD.BRR.studies"]])) {
    DESIGN <- "BRR"
  }
  assign(x = "DESIGN", value = DESIGN, envir = parent.frame())
  data.object <- data.object[ , mget(unique(c(key(data.object), unname(unlist(object.variables[!names(object.variables) %in% c("PV.root.avg", "PV.root.prctls", "PV.root.bench", "PV.root.corr", "PV.root.dep", "PV.root.indep")])))))]
  bckg.vars.in.data <- unname(unlist(Filter(Negate(is.null), object.variables[c("bckg.var", "bckg.corr.vars", "bckg.avg.vars", "bckg.prctls.vars", "bckg.dep.var", "bckg.indep.cont.vars", "bckg.indep.cat.vars", "bin.dep.var", "bckg.row.var", "bckg.col.var")])))
  if(!is.null(bckg.vars.in.data)) {
    data.object[ , (bckg.vars.in.data) := lapply(.SD, function(i) {
      if(!is.null(attr(i, "missings"))) {
        i[i %in% attr(i, "missings")] <- NA
        return(i)
      } else {
        return(i)
      }
    }), .SDcols = bckg.vars.in.data]
  }
  if(!is.null(bckg.vars.in.data)) {
    factor.bckg.avg.vars <- names(Filter(isTRUE, sapply(X = data.object[ , mget(bckg.vars.in.data)], FUN = is.factor)))
    if(length(factor.bckg.avg.vars) > 0) {
      data.object[ , (factor.bckg.avg.vars) := lapply(.SD, function(i) {
        as.numeric(i)
      }), .SDcols = factor.bckg.avg.vars]
    }
  }
  if(!is.null(object.variables[["split.vars"]]) & action.arguments[["include.missing"]] == FALSE) {
    data.object[ , (object.variables[["split.vars"]]) := lapply(.SD, function(i) {
      if(!is.null(attr(i, "missings"))) {
        i[i %in% attr(i, "missings")] <- NA
        return(i)
      } else {
        return(i)
      }
    }), .SDcols = object.variables[["split.vars"]]]
  }
  which.bckg.vars.argument.passed <- grep(pattern = "^bckg", x = names(object.variables), value = TRUE)
  if(length(which.bckg.vars.argument.passed)) {
    data.object[ , unlist(object.variables[which.bckg.vars.argument.passed]) := lapply(.SD, function(i) {
      if(!is.null(attr(i, "missings"))) {
        i <- ifelse(test = i %in% attr(i, "missings"), yes = NA, no = i)
      } else {
        i
      }
    }), .SDcols = unlist(object.variables[which.bckg.vars.argument.passed])]
  }
  if(!"split.vars" %in% object.variables) {
    setkeyv(x = data.object, cols = c(key(data.object), object.variables[["split.vars"]]))
  }
  key.vars <- key(data.object)
  assign(x = "key.vars", value = key.vars, envir = parent.frame())
  if(action.arguments[["include.missing"]] == TRUE) {
    data.object[ , (key.vars) := lapply(.SD, function(i) {
      if(is.factor(i) == FALSE) {
        i <- factor(i)
      }
      if(anyNA(i) == TRUE) {
        `levels<-` (addNA(i), c(levels(i), "<NA>"))
      } else {
        i
      }
    }), .SDcols = key.vars]
    setkeyv(data.object, key.vars)
  }
  if(length(key.vars) >= 2) {
    object.variables[["group.vars"]] <- key.vars[1:(length(key.vars) - 1)]
    object.variables[["pcts.var"]] <- key.vars[length(key.vars)]
  }
  assign(x = "tmp.pcts.var", value = object.variables[["pcts.var"]], envir = parent.frame())
  assign(x = "tmp.group.vars", value = object.variables[["group.vars"]], envir = parent.frame())
  data.object <- split(data.object, by = key.vars[1])
  data.all.wgts.NA <- which(lapply(X = data.object, FUN = function(i) {
    all(is.na(i[ , get(object.variables[["weight.var"]])]))
  }) == TRUE)
  if(length(data.all.wgts.NA) > 0) {
    data.object[data.all.wgts.NA] <- NULL
  }
  if(length(object.variables[["split.vars"]]) > 0) {
    any.split.vars.only.NA <- lapply(X = data.object, FUN = function(i) {
      nrow(Filter(function(j) {all(is.na(j))}, i[ , mget(object.variables[["split.vars"]])]))
    })
    any.split.vars.only.NA <- names(Filter(function(i) {i > 0}, any.split.vars.only.NA))
    data.object <- data.object[!names(data.object) %in% any.split.vars.only.NA]
    assign(x = "removed.countries.where.any.split.var.is.all.NA", value = any.split.vars.only.NA, envir = parent.frame())
  }
  if(imported.file.attributes[["lsa.study"]] %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi")) {
    data.object <- produce.jk.reps.data(data = data.object, weight.var = object.variables[["weight.var"]], jk.zones = object.variables[["jk.zones"]], jk.replicates = object.variables[["rep.ind"]], shortcut = action.arguments[["shortcut"]])
  } else if(imported.file.attributes[["lsa.study"]] %in% c("CivED", "ICCS", "ICILS", "SITES", "REDS")) {
    data.object <- produce.jk.reps.data(data = data.object, weight.var = object.variables[["weight.var"]], jk.zones = object.variables[["jk.zones"]], jk.replicates = object.variables[["rep.ind"]], shortcut = TRUE)
  }
  return(data.object)
}
all.missing.values.combinations <- list("Invalid", "mis.", "miss.", "missing", "MISSING", "not administered", "NOT ADMINISTERED", "NOT EXCLUDED", "Omitted or invalid", "OMITTED OR INVALID", "omitted", "Omitted", "OMITTED",
                                        c(" not appl.", "not admin.", "omitted"),
                                        c("Default missing code", "Not administered/missing by design", "Presented but not answered/invalid"),
                                        c("Default", "Not applicable", "Not stated"),
                                        c("DO NOT KNOW", "OMITTED"),
                                        c("DOES NOT APPLY", "OMITTED"),
                                        c("DONT KNOW", "LOGICALLY NOT APPLICABLE", "OMITTED"),
                                        c("DONT KNOW", "OMITTED"),
                                        c("INVALID RESPONSE", "OMITTED"),
                                        c("Invalid", "Not administered", "Omitted"),
                                        c("Not administered/not scored/score out of range", "Omitted"),
                                        c("Invalid", "Not administered/not scored/score out of range", "Omitted"),
                                        c("INVALID", "OMITTED OR INVALID"),
                                        c("INVALID", "OMITTED"),
                                        c("log.not appl.", "not admin.", "omitted"),
                                        c("Logically not applicable", "Invalid", "Not administered", "Omitted"),
                                        c("LOGICALLY NOT APPLICABLE", "INVALID", "OMITTED OR INVALID"),
                                        c("LOGICALLY NOT APPLICABLE", "INVALID", "OMITTED"),
                                        c("LOGICALLY NOT APPLICABLE", "MISSING"),
                                        c("Logically not applicable", "Not administered or missing by design", "Presented but not answered or invalid"),
                                        c("Logically not applicable", "Not administered/missing by design", "Presented but not answered/invalid"),
                                        c("Logically not applicable", "Not reached", "Not administered", "Omitted or Invalid"),
                                        c("Logically not applicable", "Not Reached", "Not administered", "Omitted or invalid"),
                                        c("LOGICALLY NOT APPLICABLE", "NOT REACHED", "OMITTED"),
                                        c("Logically not applicable", "Omitted or invalid"),
                                        c("LOGICALLY NOT APPLICABLE", "OMITTED OR INVALID"),
                                        c("LOGICALLY NOT APPLICABLE", "OMITTED"),
                                        c("N/A, Yes: Not administered", "N/A, N/A: Not administered"),
                                        c("not admin.", "missing"),
                                        c("not admin.", "omitted"),
                                        c("not admin", "omitted"),
                                        c("Not administered or missing by design", "Presented but not answered or invalid"),
                                        c("Not administered", "Omitted or invalid"),
                                        c("Not administered", "Omitted"),
                                        c("Not administered/missing by design", "Presented but not answered/invalid"),
                                        c("not appl.", "not admin.", "omitted"),
                                        c("not appl", "not admin", "omitted"),
                                        c("Not applicable", "Invalid", "Missing"),
                                        c("Not Applicable", "Invalid", "Missing"),
                                        c("Not applicable", "Invalid", "No response"),
                                        c("Not Applicable", "Invalid", "No Response"),
                                        c("NOT APPLICABLE", "MISSING"),
                                        c("not applicable", "not admin.", "missing"),
                                        c("not applicable", "not admin.", "omitted"),
                                        c("Not Applicable", "Not Administered", "Omitted"),
                                        c("not applicable", "not applicable", "not admin.", "missing"),
                                        c("Not applicable", "Not stated"),
                                        c("Not reached (default during data processing)", "Not administered", "Omitted or Invalid"),
                                        c("NOT REACHED", "INVALID RESPONSE", "OMITTED"),
                                        c("not reached", "INVALID", "not admin.", "missing"),
                                        c("Not reached", "INVALID", "not admin.", "missing"),
                                        c("Not reached", "Invalid", "Not administered", "Omitted"),
                                        c("NOT REACHED", "INVALID", "OMITTED"),
                                        c("NOT REACHED", "MISSING (BLANK ONLY)"),
                                        c("NOT REACHED", "MISSING"),
                                        c("not reached", "not admin.", "blank(miss)"),
                                        c("not reached", "not admin.", "blank(missing)"),
                                        c("not reached", "not admin.", "blank(omitted)"),
                                        c("not reached", "not admin.", "missing (blank only)"),
                                        c("not reached", "not admin.", "missing"),
                                        c("not reached", "not admin.", "omitted (blank only)"),
                                        c("not reached", "not admin.", "omitted"),
                                        c("not reached", "not admin", "blank(miss)"),
                                        c("not reached", "not admin", "blank(missing)"),
                                        c("not reached", "not admin", "omitted"),
                                        c("Not Reached", "Not administered or missing by design", "Presented but not answered or invalid"),
                                        c("Not Reached", "Not administered", "Missing"),
                                        c("Not Reached", "Not administered", "Omitted or invalid"),
                                        c("NOT REACHED", "NOT ADMINISTERED"),
                                        c("Not reached", "Not administered/missing by design", "Presented but not answered/invalid"),
                                        c("Not Reached", "Not Applicable", "Invalid", "No Response"),
                                        c("Not reached", "Omitted or invalid"),
                                        c("Not reached", "Omitted"),
                                        c("NOT REACHED", "OMITTED"),
                                        c("not reached", "uninterpretable", "not admin.", "omitted"),
                                        c("not reached", "uninterpretable", "not admin", "omitted"),
                                        c("Notadministered/missing by design", "Presented but not answered/invalid"),
                                        c("noz admin", "omitted"),
                                        c("Omitted or invalid", "Omitted or invalid"),
                                        c("OMITTED OR INVALID", "OMITTED OR INVALID"),
                                        c("OMITTED", "OMITTED"),
                                        c("Valid Skip", "Not Applicable", "Invalid", "No Response"),
                                        c("Valid Skip", "Not Reached", "Not Applicable", "Invalid", "No Response"),
                                        c("Data not available", "Invalid", "Not adminstered"),
                                        c("Data not available", "Invalid", "Not administered", "Missing")
)
fac.to.num.missing.codes <- c("not admin." = 8, "missing" = 9, "Not reached" = 6, "INVALID" = 7,
                              "not reached" = 6, "Omitted or invalid" = 999999, "Logically not applicable" = 6,
                              "Omitted" = 9, "OMITTED" = 99999, "OMITTED OR INVALID" = 9, "NOT REACHED" = 6,
                              "LOGICALLY NOT APPLICABLE" = 5, "Not administered" = 999998,
                              "Invalid" = 7, "Not administered/not scored/score out of range" = 9998,
                              "Not administered/missing by design" = 8, "Presented but not answered/invalid" = 9,
                              "Not applicable" = 8, "Not stated" = 9, "Notadministered/missing by design" = 8,
                              "Default missing code" = 9997, "Not administered or missing by design" = 9998,
                              "Presented but not answered or invalid" = 9999, "Not Reached" = 7,
                              "INVALID RESPONSE" = 7, "Missing" = 9, "non-scalable" = 8, "Mis" = 9,
                              "Miss" = 9, "Unreached" = 8, "N/A, Yes: Not administered" = 71, "N/A, N/A: Not administered" = 77,
                              "Valid Skip" = 995, "Not Applicable" = 997, "No Response" = 999,
                              "No response" = 9999, "NOT EXCLUDED" = 9, "Default" = 9997, "Not reached (default during data processing)" = 7,
                              "Omitted or Invalid" = 9, "DONT KNOW" = 11, "DOES NOT APPLY" = 5,
                              "NOT ADMINISTERED" = 8, "not applicable" = 6, "missing (blank only)" = 9,
                              "blank(missing)" = 99, "not admin" = 98, "blank(miss)" = 99,
                              "omitted" = 9, "omitted (blank only)" = 9, "blank(omitted)" = 99,
                              "log.not appl." = 6, "Not Administered" = 8, "not appl." = 6,
                              "miss." = 9, "uninterpretable" = 7, "mis." = 99, "not administered" = 8,
                              "noz admin" = 8, "not appl" = 96, " not appl." = 6, "DO NOT KNOW" = 6,
                              "MISSING" = 999, "NOT APPLICABLE" = 5, "MISSING (BLANK ONLY)" = 9,
                              "Data not available" = 9996
)
fac.to.num.missing.codes <- c(99999991, 99999991, 99999991, 99999991, 99999991, 99999991, 99999991, 99999991, 99999991, 99999991, 99999992, 99999994, 99999995, 99999996, 99999996, 99999996, 99999996, 99999996, 99999996, 99999996, 99999996, 99999996, 99999996, 99999997, 99999997, 99999997, 99999997, 99999997, 99999997, 99999997, 99999997, 99999997, 99999997, 99999997, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999999, 99999998, 99999998, 99999998, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999)
names(fac.to.num.missing.codes) <- c(" not appl.", "Default", "log.not appl.", "LOGICALLY NOT APPLICABLE", "Logically not applicable", "not appl", "not appl.", "Not Applicable", "Not applicable", "not applicable", "don't know", "No Response", "Valid Skip", "n. rea.", "n. reach.", "n. reached", "n.rea.", "n.reach.", "NOT REACHED", "Not Reached", "Not reached", "not reached", "Not reached (default during data processing)", "crossed out, not interpretable", "INVALID", "Invalid", "INVALID RESPONSE", "Presented but not answered/invalid", "Presented but not answered or invalid", "two or more responses, not interpretable", "two or more responses, uninterpretable", "TWO OR MORE RESPONSES, UNINTERPRETABLE", "two or more responses, uninterpretable, ...", "uninterpretable", "n. adm.", "n. admin.", "not admin", "not admin.", "Not administered", "not administered", "Not administered/missing by design", "Not administered or missing by design", "Not stated", "Notadministered/missing by design", "noz admin", "missing", "MISSING", "MISSING (BLANK ONLY)", "NOT EXCLUDED", "OMITTED", "Omitted", "omitted", "omitted (blank only)", "OMITTED OR INVALID", "Omitted or invalid", "Omitted or Invalid")
get.analysis.and.design.vars <- function(x) {
  passed.args <- as.list(sys.call(which = -1))
  passed.args <- passed.args[c("split.vars", "group.vars", "bckg.var", "bckg.avg.vars", "bckg.prctls.vars", "bckg.corr.vars", "bckg.dep.var", "bckg.indep.cont.vars", "bckg.indep.cat.vars", "bin.dep.var", "PV.root.avg", "PV.root.prctls", "PV.root.bench", "PV.root.corr", "PV.root.dep", "PV.root.indep", "bckg.row.var", "bckg.col.var", "weight.var", "interactions", "variables", "src.variables")]
  if(is.na(names(passed.args["weight.var"])) == TRUE) {
    if(attr(x, "study") %in% design.weight.variables[["IEA.JK2.studies"]]) {
      if(attr(x, "file.type") %in% design.weight.variables[["IEA.JK2.dflt.std.bckg.types"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["IEA.JK2.dflt.std.bckg.wgts"]], names(x))
        passed.args["jk.zones"] <- intersect(design.weight.variables[["IEA.JK2.dflt.std.bckg.zones"]], names(x))
        passed.args["rep.ind"] <- intersect(design.weight.variables[["IEA.JK2.dflt.std.bckg.rep.ind"]], names(x))[1]
      } else if(attr(x, "file.type") %in% design.weight.variables[["IEA.JK2.dflt.sch.bckg.types"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["IEA.JK2.dflt.sch.bckg.wgts"]], names(x))
        passed.args["jk.zones"] <- intersect(design.weight.variables[["IEA.JK2.dflt.sch.bckg.zones"]], names(x))
        passed.args["rep.ind"] <- intersect(design.weight.variables[["IEA.JK2.dflt.sch.bckg.rep.ind"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["IEA.JK2.dflt.tch.bckg.types"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["IEA.JK2.dflt.tch.bckg.wgts"]], names(x))[1]
        passed.args["jk.zones"] <- intersect(design.weight.variables[["IEA.JK2.dflt.tch.bckg.zones"]], names(x))
        passed.args["rep.ind"] <- intersect(design.weight.variables[["IEA.JK2.dflt.tch.bckg.rep.ind"]], names(x))
      }
    } else if(attr(x, "study") %in% design.weight.variables[["IEA.BRR.studies"]]) {
      if(attr(x, "file.type") %in% design.weight.variables[["IEA.BRR.dflt.inst.bckg.types"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["IEA.BRR.dflt.inst.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["IEA.BRR.dflt.inst.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["IEA.BRR.dflt.prim.tch.bckg.types"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["IEA.BRR.dflt.prim.tch.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["IEA.BRR.dflt.prim.tch.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["IEA.BRR.dflt.low_sec.tch.bckg.types"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["IEA.BRR.dflt.low_sec.tch.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["IEA.BRR.dflt.low_sec.tch.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["IEA.BRR.dflt.educ.bckg.types"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["IEA.BRR.dflt.educ.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["IEA.BRR.dflt.educ.bckg.rep.wgts"]], names(x))
      }
    } else if(attr(x, "study") %in% design.weight.variables[["OECD.BRR.studies"]]) {
      if(attr(x, "file.type") %in% design.weight.variables[["OECD.BRR.dflt.std.bckg"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["OECD.BRR.dflt.std.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["OECD.BRR.dflt.std.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["OECD.BRR.dflt.out.of.school.bckg"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["OECD.BRR.dflt.out.of.school.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["OECD.BRR.dflt.out.of.school.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["OECD.BRR.dflt.sch.bckg"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["OECD.BRR.dflt.sch.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["OECD.dflt.sch.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["OECD.BRR.dflt.tch.bckg"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["OECD.BRR.dflt.tch.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["OECD.BRR.dflt.tch.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["OECD.BRR.dflt.lead.bckg"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["OECD.BRR.dflt.lead.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["OECD.BRR.dflt.lead.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["OECD.BRR.dflt.staff.bckg"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["OECD.BRR.dflt.staff.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["OECD.BRR.dflt.staff.bckg.rep.wgts"]], names(x))
      }
    }
  } else if(is.na(names(passed.args["weight.var"])) == FALSE) {
    if(attr(x, "study") %in% design.weight.variables[["IEA.JK2.studies"]]) {
      passed.args["jk.zones"] <- intersect(unlist(unname(design.weight.variables[c("IEA.JK2.dflt.std.bckg.zones", "IEA.JK2.dflt.sch.bckg.zones", "IEA.JK2.dflt.tch.bckg.zones")])), colnames(x))[1]
      passed.args["rep.ind"] <- intersect(unlist(unname(design.weight.variables[c("IEA.JK2.dflt.std.bckg.rep.ind", "IEA.JK2.dflt.sch.bckg.rep.ind", "IEA.JK2.dflt.tch.bckg.rep.ind")])), colnames(x))[1]
    } else if(attr(x, "study") %in% design.weight.variables[["IEA.BRR.studies"]]) {
      if(attr(x, "file.type") %in% design.weight.variables[["IEA.BRR.dflt.inst.bckg.types"]]
         && passed.args[["weight.var"]] %in% design.weight.variables[["IEA.BRR.other.inst.bckg.wgts"]]) {
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["IEA.BRR.other.inst.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["IEA.BRR.dflt.prim.tch.bckg.types"]]
                && passed.args[["weight.var"]] %in% design.weight.variables[["IEA.BRR.other.prim.tch.bckg.wgts"]]) {
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["IEA.BRR.other.prim.tch.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["IEA.BRR.dflt.low_sec.tch.bckg.types"]]
                && passed.args[["weight.var"]] %in% design.weight.variables[["IEA.BRR.other.low_sec.tch.bckg.wgts"]]) {
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["IEA.BRR.other.low_sec.tch.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["IEA.BRR.dflt.educ.bckg.types"]]
                && passed.args[["weight.var"]] %in% design.weight.variables[["IEA.BRR.other.educ.bckg.wgts"]]) {
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["IEA.BRR.other.educ.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["IEA.BRR.dflt.inst.bckg.types"]]
                && passed.args[["weight.var"]] %in% design.weight.variables[["IEA.BRR.dflt.inst.bckg.wgts"]]) {
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["IEA.BRR.dflt.inst.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["IEA.BRR.dflt.prim.tch.bckg.types"]]
                && passed.args[["weight.var"]] %in% design.weight.variables[["IEA.BRR.dflt.prim.tch.bckg.wgts"]]) {
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["IEA.BRR.dflt.prim.tch.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["IEA.BRR.dflt.low_sec.tch.bckg.types"]]
                && passed.args[["weight.var"]] %in% design.weight.variables[["IEA.BRR.dflt.low_sec.tch.bckg.wgts"]]) {
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["IEA.BRR.dflt.low_sec.tch.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["IEA.BRR.dflt.educ.bckg.types"]]
                && passed.args[["weight.var"]] %in% design.weight.variables[["IEA.BRR.dflt.educ.bckg.wgts"]]) {
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["IEA.BRR.dflt.educ.bckg.rep.wgts"]], names(x))
      }
    } else if(attr(x, "study") %in% design.weight.variables[["OECD.BRR.studies"]]) {
      if(attr(x, "file.type") %in% design.weight.variables[["OECD.BRR.dflt.std.bckg"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["OECD.BRR.dflt.std.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["OECD.BRR.dflt.std.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["OECD.BRR.dflt.out.of.school.bckg"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["OECD.BRR.dflt.out.of.school.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["OECD.BRR.dflt.out.of.school.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["OECD.BRR.dflt.sch.bckg"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["OECD.BRR.dflt.sch.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["OECD.dflt.sch.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["OECD.BRR.dflt.tch.bckg"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["OECD.BRR.dflt.tch.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["OECD.BRR.dflt.tch.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["OECD.BRR.dflt.lead.bckg"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["OECD.BRR.dflt.lead.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["OECD.BRR.dflt.lead.bckg.rep.wgts"]], names(x))
      } else if(attr(x, "file.type") %in% design.weight.variables[["OECD.BRR.dflt.staff.bckg"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["OECD.BRR.dflt.staff.bckg.wgts"]], names(x))
        passed.args[["rep.wgts"]] <- intersect(design.weight.variables[["OECD.BRR.dflt.staff.bckg.rep.wgts"]], names(x))
      }
    }
  }
  passed.args <- passed.args[!is.na(names(passed.args))]
  passed.args <- lapply(X = passed.args, FUN = eval)
  PV.root.passed <- c("PV.root.avg", "PV.root.prctls", "PV.root.bench", "PV.root.corr", "PV.root.dep", "PV.root.indep")[which(c("PV.root.avg", "PV.root.prctls", "PV.root.bench", "PV.root.corr", "PV.root.dep", "PV.root.indep") %in% names(passed.args))]
  if(any(unname(unlist(passed.args[PV.root.passed])) %in% gsub(pattern = "[[:digit:]]+", replacement = "#", x = unname(unlist(studies.all.design.variables[["PV.roots"]])), fixed = TRUE) == FALSE)) {
    stop('One or more names passed as a PV root is does not represent a root of set of PVs for the study as required (refer to the documentation). All operations stop here. Check your input.\n\n', call. = FALSE)
  }
  if(length(PV.root.passed) > 0) {
    if(grepl(pattern = "#", x = paste(unname(unlist(passed.args[PV.root.passed])), sep = "", collapse = "|")) == TRUE) {
      passed.args[PV.root.passed] <- lapply(X = passed.args[PV.root.passed], FUN = function(i) {
        gsub(pattern = "#", replacement = "[[:digit:]]+", x = i)
      })
    }
    passed.args[["PV.names"]] <- lapply(X = unname(unlist(passed.args[PV.root.passed])), FUN = function(i) {
      grep(pattern = i, x = names(x), value = TRUE)
    })
  } else {
    passed.args <- passed.args
  }
  if(all(unlist(unname(passed.args[!names(passed.args) %in% c("PV.root.avg", "PV.root.prctls", "PV.root.bench", "PV.root.corr", "PV.root.dep", "PV.root.indep", "interactions")])) %in% names(x)) != TRUE
     || any(unlist(lapply(X = passed.args, FUN = function(i) {
       lapply(i, length)
     })) < 1) == TRUE) {
    stop(paste("One or more variables specified in the call to the function do not exist in the data. All operations stop here. Check your input.\n\n"), call. = FALSE)
  } else {
    return(passed.args)
  }
}
get.action.arguments <- function() {
  passed.args <- as.list(sys.call(which = -1))
  passed.args[["executed.analysis.function"]] <- as.character(Filter(is.symbol, passed.args))[[1]]
  passed.args <- passed.args[c("central.tendency", "include.missing", "graphs", "shortcut", "output.file", "open.output", "executed.analysis.function", "prctls")]
  if(is.null(passed.args[["include.missing"]])) {
    passed.args[["include.missing"]] <- FALSE
  }
  if(passed.args[["executed.analysis.function"]] == "lsa.prctls" & is.null(passed.args[["prctls"]])) {
    passed.args[["prctls"]] <- c(5, 25, 50, 75, 95)
  }
  return(passed.args[Filter(Negate(is.na), names(passed.args))])
}
get.file.attributes <- function(imported.object) {
  list(lsa.study = attr(x = imported.object, which = "study"),
       lsa.cycle = as.numeric(attr(x = imported.object, which = "cycle")),
       lsa.file.type = attr(x = imported.object, which = "file.type"))
}
merge.combinations <- list(
  CivED = list(
    c("bc_", "sch.bckg"),
    c("bc_", "bs_", "std.bckg.sch.bckg"),
    c("bc_", "bt_", "sch.bckg.tch.bckg"),
    c("bc_", "bs_", "bt_", "std.bckg.sch.bckg.tch.bckg"),
    c("bs_", "bt_", "std.bckg.tch.bckg"),
    c("cs_", "std.bckg"),
    c("bs_", "std.bckg")
  ),
  ICCS = list(
    c("isa", "std.ach"),
    c("icg", "sch.bckg"),
    c("ise", "std.EUM"),
    c("isl", "std.LAM"),
    c("iss", "std.AM"),
    c("isg", "std.bckg"),
    c("itg", "tch.bckg"),
    c("isg", "std.bckg"),
    c("icg", "isa", "std.ach.sch.bckg"),
    c("isa", "ise", "std.ach.EUM"),
    c("isa", "isl", "std.ach.LAM"),
    c("isa", "iss", "std.ach.AM"),
    c("icg", "isg", "std.bckg.sch.bckg"),
    c("isg", "ise", "std.bckg.EUM"),
    c("isg", "isl", "std.bckg.LAM"),
    c("isg", "iss", "std.bckg.AM"),
    c("icg", "itg", "sch.bckg.tch.bckg"),
    c("icg", "ise", "std.EUM.sch.bckg"),
    c("icg", "isl", "std.LAM.sch.bckg"),
    c("icg", "iss", "std.AM.sch.bckg"),
    c("isl", "isg", "isa", "std.bckg.ach.LAM"),
    c("ise", "isg", "isa", "std.bckg.ach.EUM"),
    c("iss", "isg", "isa", "std.bckg.ach.AM"),
    c("icg", "isg", "isa", "std.bckg.ach.sch.bckg"),
    c("icg", "isg", "ise", "std.bckg.EUM.sch.bckg"),
    c("icg", "isg", "isl", "std.bckg.LAM.sch.bckg"),
    c("icg", "isg", "iss", "std.bckg.AM.sch.bckg"),
    c("icg", "isa", "ise", "std.ach.EUM.sch.bckg"),
    c("icg", "isa", "isl", "std.ach.LAM.sch.bckg "),
    c("icg", "isa", "iss", "std.ach.AM.sch.bckg"),
    c("icg", "ise", "isg", "isa", "std.bckg.std.ach.EUM.sch.bckg"),
    c("icg", "isl", "isg", "isa", "std.bckg.std.ach.LAM.sch.bckg"),
    c("icg", "iss", "isg", "isa", "std.bckg.std.ach.AM.sch.bckg"),
    c("isa", "isp", "std.ach.proc"),
    c("isg", "isp", "std.bckg.proc"),
    c("ise", "isp", "std.proc.EUM"),
    c("isl", "isp", "std.proc.LAM"),
    c("iss", "isp", "std.proc.AM"),
    c("icg", "isp", "std.proc.sch.bckg"),
    c("icg", "isa", "isp", "std.ach.proc.sch.bckg"),
    c("isa", "ise", "isp", "std.ach.proc.EUM"),
    c("isa", "isl", "isp", "std.ach.proc.LAM"),
    c("isa", "iss", "isp", "std.ach.proc.AM"),
    c("icg", "isg", "isp", "std.bckg.proc.sch.bckg"),
    c("isg", "ise", "isp", "std.bckg.proc.EUM"),
    c("isg", "isl", "isp", "std.bckg.proc.LAM"),
    c("isg", "iss", "isp", "std.bckg.proc.AM"),
    c("icg", "ise", "isp", "std.proc.EUM.sch.bckg"),
    c("icg", "isl", "isp", "std.proc.LAM.sch.bckg"),
    c("icg", "iss", "isp", "std.proc.AM.sch.bckg"),
    c("isl", "isg", "isa", "isp", "std.bckg.proc.ach.LAM"),
    c("ise", "isg", "isa", "isp", "std.bckg.proc.ach.EUM"),
    c("iss", "isg", "isa", "isp", "std.bckg.proc.ach.AM"),
    c("icg", "isg", "isa", "isp", "std.bckg.proc.ach.sch.bckg"),
    c("icg", "isg", "ise", "isp", "std.bckg.proc.EUM.sch.bckg"),
    c("icg", "isg", "isl", "isp", "std.bckg.proc.LAM.sch.bckg"),
    c("icg", "isg", "iss", "isp", "std.bckg.proc.AM.sch.bckg"),
    c("icg", "isa", "ise", "isp", "std.ach.proc.EUM.sch.bckg"),
    c("icg", "isa", "isl", "isp", "std.ach.proc.LAM.sch.bckg"),
    c("icg", "isa", "iss", "isp", "std.ach.proc.AM.sch.bckg"),
    c("icg", "ise", "isg", "isa", "isp", "std.bckg.std.ach.proc.EUM.sch.bckg"),
    c("icg", "isl", "isg", "isa", "isp", "std.bckg.std.ach.proc.LAM.sch.bckg"),
    c("icg", "iss", "isg", "isa", "isp", "std.bckg.std.ach.proc.AM.sch.bckg"),
    c("jsa", "std.ach"),
    c("jse", "std.EUM"),
    c("jsg", "std.bckg"),
    c("jsa", "jse", "std.ach.EUM"),
    c("jsa", "jsg", "std.bckg.ach"),
    c("jsg", "jse", "std.bckg.EUM"),
    c("jse", "jsg", "jsa", "std.bckg.ach.EUM")
  ),
  ICILS = list(
    c("bcg", "sch.bckg"),
    c("bsg", "std.bckg"),
    c("btg", "tch.bckg"),
    c("bcg", "bsg", "std.bckg.sch.bckg"),
    c("bcg", "btg", "sch.bckg.tch.bckg")
  ),
  REDS = list(
    c("bcg", "sch.bckg"),
    c("bsg", "std.bckg"),
    c("btg", "tch.bckg"),
    c("bcg", "bsg", "std.bckg.sch.bckg"),
    c("bcg", "btg", "sch.bckg.tch.bckg")
  ),
  PIRLS = list(
    c("acg", "sch.bckg"),
    c("asg", "std.bckg"),
    c("asa", "std.ach"),
    c("asg", "ash", "std.bckg.home"),
    c("asg", "asa", "std.bckg.ach"),
    c("ash", "asa", "std.ach.home"),
    c("acg", "asg", "std.bckg.sch.bckg"),
    c("acg", "ash", "std.home.sch.bckg"),
    c("acg", "asa", "std.ach.sch.bckg"),
    c("acg", "atg", "sch.bckg.tch.bckg"),
    c("asg", "atg", "std.bckg.tch.bckg"),
    c("asa", "atg", "std.ach.tch.bckg"),
    c("ash", "atg", "std.home.tch.bckg"),
    c("asg", "ash", "asa", "std.bckg.ach.home"),
    c("asg", "ash", "atg", "std.bckg.home.tch.bckg"),
    c("asg", "asa", "atg", "std.bckg.ach.tch.bckg"),
    c("ash", "asa", "atg", "std.ach.home.tch.bckg"),
    c("acg", "asg", "asa", "std.bckg.ach.sch.bckg"),
    c("acg", "asg", "ash", "std.bckg.home.sch.bckg"),
    c("acg", "ash", "asa", "std.ach.home.sch.bckg"),
    c("acg", "asg", "atg", "std.bckg.sch.bckg.tch.bckg"),
    c("acg", "ash", "atg", "std.home.sch.bckg.tch.bckg"),
    c("acg", "asa", "atg", "std.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "asa", "std.bckg.ach.home.sch.bckg"),
    c("acg", "asg", "asa", "atg", "std.bckg.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "atg", "std.bckg.home.sch.bckg.tch.bckg"),
    c("acg", "ash", "asa", "atg", "std.ach.home.sch.bckg.tch.bckg"),
    c("asg", "ash", "asa", "atg", "std.bckg.ach.home.tch.bckg"),
    c("acg", "asg", "ash", "asa", "atg", "std.bckg.ach.home.sch.bckg.tch.bckg"),
    c("asa", "asp", "std.ach.proc"),
    c("asg", "asp", "std.bckg.proc"),
    c("acg", "asp", "std.proc.sch.bckg"),
    c("atg", "asp", "std.proc.tch.bckg"),
    c("asg", "ash", "asp", "std.bckg.proc.home"),
    c("asg", "asa", "asp", "std.bckg.proc.ach"),
    c("ash", "asa", "asp", "std.ach.proc.home"),
    c("acg", "asg", "asp", "std.bckg.proc.sch.bckg"),
    c("acg", "ash", "asp", "std.proc.home.sch.bckg"),
    c("acg", "asa", "asp", "std.ach.proc.sch.bckg"),
    c("asg", "atg", "asp", "std.bckg.proc.tch.bckg"),
    c("asa", "atg", "asp", "std.ach.proc.tch.bckg"),
    c("ash", "atg", "asp", "std.proc.home.tch.bckg"),
    c("asg", "ash", "asa", "asp", "std.bckg.proc.ach.home"),
    c("asg", "ash", "atg", "asp", "std.bckg.proc.home.tch.bckg"),
    c("asg", "asa", "atg", "asp", "std.bckg.proc.ach.tch.bckg"),
    c("ash", "asa", "atg", "asp", "std.ach.proc.home.tch.bckg"),
    c("acg", "asg", "asa", "asp", "std.bckg.proc.ach.sch.bckg"),
    c("acg", "asg", "ash", "asp", "std.bckg.proc.home.sch.bckg"),
    c("acg", "ash", "asa", "asp", "std.ach.proc.home.sch.bckg"),
    c("acg", "asg", "atg", "asp", "std.bckg.proc.sch.bckg.tch.bckg"),
    c("acg", "ash", "atg", "asp", "std.proc.home.sch.bckg.tch.bckg"),
    c("acg", "asa", "atg", "asp", "std.ach.proc.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "asa", "asp", "std.bckg.proc.ach.home.sch.bckg"),
    c("acg", "asg", "asa", "atg", "asp", "std.bckg.proc.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "atg", "asp", "std.bckg.proc.home.sch.bckg.tch.bckg"),
    c("acg", "ash", "asa", "atg", "asp", "std.ach.proc.home.sch.bckg.tch.bckg"),
    c("asg", "ash", "asa", "atg", "asp", "std.bckg.proc.ach.home.tch.bckg"),
    c("acg", "asg", "ash", "asa", "atg", "asp", "std.bckg.proc.ach.home.sch.bckg.tch.bckg")
  ),
  ePIRLS = list(
    c("acg", "sch.bckg"),
    c("asg", "std.bckg"),
    c("asa", "std.ach"),
    c("asg", "ash", "std.bckg.home"),
    c("asg", "asa", "std.bckg.ach"),
    c("ash", "asa", "std.ach.home"),
    c("acg", "asg", "std.bckg.sch.bckg"),
    c("acg", "ash", "std.home.sch.bckg"),
    c("acg", "asa", "std.ach.sch.bckg"),
    c("acg", "atg", "sch.bckg.tch.bckg"),
    c("asg", "atg", "std.bckg.tch.bckg"),
    c("asa", "atg", "std.ach.tch.bckg"),
    c("ash", "atg", "std.home.tch.bckg"),
    c("asg", "ash", "asa", "std.bckg.ach.home"),
    c("asg", "ash", "atg", "std.bckg.home.tch.bckg"),
    c("asg", "asa", "atg", "std.bckg.ach.tch.bckg"),
    c("ash", "asa", "atg", "std.ach.home.tch.bckg"),
    c("acg", "asg", "asa", "std.bckg.ach.sch.bckg"),
    c("acg", "asg", "ash", "std.bckg.home.sch.bckg"),
    c("acg", "ash", "asa", "std.ach.home.sch.bckg"),
    c("acg", "asg", "atg", "std.bckg.sch.bckg.tch.bckg"),
    c("acg", "ash", "atg", "std.home.sch.bckg.tch.bckg"),
    c("acg", "asa", "atg", "std.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "asa", "std.bckg.ach.home.sch.bckg"),
    c("acg", "asg", "asa", "atg", "std.bckg.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "atg", "std.bckg.home.sch.bckg.tch.bckg"),
    c("acg", "ash", "asa", "atg", "std.ach.home.sch.bckg.tch.bckg"),
    c("asg", "ash", "asa", "atg", "std.bckg.ach.home.tch.bckg"),
    c("acg", "asg", "ash", "asa", "atg", "std.bckg.ach.home.sch.bckg.tch.bckg")
  ),
  prePIRLS = list(
    c("acg", "sch.bckg"),
    c("asg", "std.bckg"),
    c("asa", "std.ach"),
    c("asg", "ash", "std.bckg.home"),
    c("asg", "asa", "std.bckg.ach"),
    c("ash", "asa", "std.ach.home"),
    c("acg", "asg", "std.bckg.sch.bckg"),
    c("acg", "ash", "std.home.sch.bckg"),
    c("acg", "asa", "std.ach.sch.bckg"),
    c("acg", "atg", "sch.bckg.tch.bckg"),
    c("asg", "atg", "std.bckg.tch.bckg"),
    c("asa", "atg", "std.ach.tch.bckg"),
    c("ash", "atg", "std.home.tch.bckg"),
    c("asg", "ash", "asa", "std.bckg.ach.home"),
    c("asg", "ash", "atg", "std.bckg.home.tch.bckg"),
    c("asg", "asa", "atg", "std.bckg.ach.tch.bckg"),
    c("ash", "asa", "atg", "std.ach.home.tch.bckg"),
    c("acg", "asg", "asa", "std.bckg.ach.sch.bckg"),
    c("acg", "asg", "ash", "std.bckg.home.sch.bckg"),
    c("acg", "ash", "asa", "std.ach.home.sch.bckg"),
    c("acg", "asg", "atg", "std.bckg.sch.bckg.tch.bckg"),
    c("acg", "ash", "atg", "std.home.sch.bckg.tch.bckg"),
    c("acg", "asa", "atg", "std.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "asa", "std.bckg.ach.home.sch.bckg"),
    c("acg", "asg", "asa", "atg", "std.bckg.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "atg", "std.bckg.home.sch.bckg.tch.bckg"),
    c("acg", "ash", "asa", "atg", "std.ach.home.sch.bckg.tch.bckg"),
    c("asg", "ash", "asa", "atg", "std.bckg.ach.home.tch.bckg"),
    c("acg", "asg", "ash", "asa", "atg", "std.bckg.ach.home.sch.bckg.tch.bckg")
  ),
  TiPi = list(
    c("acg", "sch.bckg"),
    c("asg", "std.bckg"),
    c("asa", "std.ach"),
    c("asg", "ash", "std.bckg.home"),
    c("asg", "asa", "std.bckg.ach"),
    c("ash", "asa", "std.ach.home"),
    c("acg", "asg", "std.bckg.sch.bckg"),
    c("acg", "ash", "std.home.sch.bckg"),
    c("acg", "asa", "std.ach.sch.bckg"),
    c("acg", "atg", "sch.bckg.tch.bckg"),
    c("asg", "atg", "std.bckg.tch.bckg"),
    c("asa", "atg", "std.ach.tch.bckg"),
    c("ash", "atg", "std.home.tch.bckg"),
    c("asg", "ash", "asa", "std.bckg.ach.home"),
    c("asg", "ash", "atg", "std.bckg.home.tch.bckg"),
    c("asg", "asa", "atg", "std.bckg.ach.tch.bckg"),
    c("ash", "asa", "atg", "std.ach.home.tch.bckg"),
    c("acg", "asg", "asa", "std.bckg.ach.sch.bckg"),
    c("acg", "asg", "ash", "std.bckg.home.sch.bckg"),
    c("acg", "ash", "asa", "std.ach.home.sch.bckg"),
    c("acg", "asg", "atg", "std.bckg.sch.bckg.tch.bckg"),
    c("acg", "ash", "atg", "std.home.sch.bckg.tch.bckg"),
    c("acg", "asa", "atg", "std.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "asa", "std.bckg.ach.home.sch.bckg"),
    c("acg", "asg", "asa", "atg", "std.bckg.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "atg", "std.bckg.home.sch.bckg.tch.bckg"),
    c("acg", "ash", "asa", "atg", "std.ach.home.sch.bckg.tch.bckg"),
    c("asg", "ash", "asa", "atg", "std.bckg.ach.home.tch.bckg"),
    c("acg", "asg", "ash", "asa", "atg", "std.bckg.ach.home.sch.bckg.tch.bckg")
  ),
  RLII = list(
    c("asc", "std.bckg")
  ),
  SITES = list(
    c("bcg", "sch.bckg"),
    c("btm", "math.tch.bckg"),
    c("bts", "sci.tch.bckg"),
    c("bcg", "btm", "sch.bckg.math.tch.bckg"),
    c("bcg", "bts", "sch.bckg.sci.tch.bckg")
  ),
  TALIS = list(
    c("acg", "sch.bckg"),
    c("atg", "tch.bckg"),
    c("acg", "atg", "sch.bckg.tch.bckg"),
    c("bcg", "sch.bckg"),
    c("btg", "tch.bckg"),
    c("bcg", "btg", "sch.bckg.tch.bckg"),
    c("ccg", "sch.bckg"),
    c("ctg", "tch.bckg"),
    c("ccg", "ctg", "sch.bckg.tch.bckg"),
    c("pcg", "sch.bckg"),
    c("ptg", "tch.bckg"),
    c("pcg", "ptg", "sch.bckg.tch.bckg")
  ),
  "TALIS 3S" = list(
    c("alg", "leader.bckg"),
    c("asg", "staff.bckg"),
    c("alg", "asg", "leader.bckg.staff.bckg"),
    c("blg", "leader.bckg"),
    c("bsg", "staff.bckg"),
    c("blg", "bsg", "leader.bckg.staff.bckg")
  ),
  "TEDS-M" = list(
    c("dig", "inst.bckg"),
    c("dpg", "prim.tch.bckg"),
    c("dsg", "low-sec.tch.bckg"),
    c("deg", "educ.bckg"),
    c("dig", "dpg", "inst.bckg.prim.tch.bckg"),
    c("dig", "dsg", "inst.bckg.low-sec.tch.bckg")
  ),
  TIMSS = list(
    c("acg", "sch.bckg"),
    c("asg", "std.bckg"),
    c("asa", "std.ach"),
    c("asg", "asa", "std.bckg.ach"),
    c("asg", "ash", "std.bckg.home"),
    c("ash", "asa", "std.ach.home"),
    c("asg", "ash", "asa", "std.bckg.ach.home"),
    c("acg", "asg", "std.bckg.sch.bckg"),
    c("acg", "ash", "std.home.sch.bckg"),
    c("acg", "asa", "std.ach.sch.bckg"),
    c("acg", "asg", "ash", "asa", "std.bckg.ach.home.sch.bckg"),
    c("acg", "asg", "asa", "std.bckg.ach.sch.bckg"),
    c("acg", "asg", "ash", "std.bckg.home.sch.bckg"),
    c("acg", "ash", "asa", "std.ach.home.sch.bckg"),
    c("acg", "atg", "sch.bckg.tch.bckg"),
    c("asg", "atg", "std.bckg.tch.bckg"),
    c("asa", "atg", "std.ach.tch.bckg"),
    c("ash", "atg", "std.home.tch.bckg"),
    c("acg", "asg", "atg", "std.bckg.sch.bckg.tch.bckg"),
    c("acg", "ash", "atg", "std.home.sch.bckg.tch.bckg"),
    c("acg", "asa", "atg", "std.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "asa", "atg", "std.bckg.ach.home.sch.bckg.tch.bckg"),
    c("acg", "asg", "asa", "atg", "std.bckg.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "atg", "std.bckg.home.sch.bckg.tch.bckg"),
    c("acg", "ash", "asa", "atg", "std.ach.home.sch.bckg.tch.bckg"),
    c("asg", "ash", "asa", "atg", "std.bckg.ach.home.tch.bckg"),
    c("asg", "ash", "atg", "std.bckg.home.tch.bckg"),
    c("asg", "asa", "atg", "std.bckg.ach.tch.bckg"),
    c("ash", "asa", "atg", "std.ach.home.tch.bckg"),
    c("asp", "std.proc"),
    c("asa", "asp", "std.ach.proc"),
    c("asg", "asp", "std.bckg.proc"),
    c("acg", "asp", "std.proc.sch.bckg"),
    c("atg", "asp", "std.proc.tch.bckg"),
    c("asg", "ash", "asp", "std.bckg.proc.home"),
    c("asg", "asa", "asp", "std.bckg.proc.ach"),
    c("ash", "asa", "asp", "std.ach.proc.home"),
    c("acg", "asg", "asp", "std.bckg.proc.sch.bckg"),
    c("acg", "ash", "asp", "std.proc.home.sch.bckg"),
    c("acg", "asa", "asp", "std.ach.proc.sch.bckg"),
    c("asg", "atg", "asp", "std.bckg.proc.tch.bckg"),
    c("asa", "atg", "asp", "std.ach.proc.tch.bckg"),
    c("ash", "atg", "asp", "std.proc.home.tch.bckg"),
    c("asg", "ash", "asa", "asp", "std.bckg.proc.ach.home"),
    c("asg", "ash", "atg", "asp", "std.bckg.proc.home.tch.bckg"),
    c("asg", "asa", "atg", "asp", "std.bckg.proc.ach.tch.bckg"),
    c("ash", "asa", "atg", "asp", "std.ach.proc.home.tch.bckg"),
    c("acg", "asg", "asa", "asp", "std.bckg.proc.ach.sch.bckg"),
    c("acg", "asg", "ash", "asp", "std.bckg.proc.home.sch.bckg"),
    c("acg", "ash", "asa", "asp", "std.ach.proc.home.sch.bckg"),
    c("acg", "asg", "atg", "asp", "std.bckg.proc.sch.bckg.tch.bckg"),
    c("acg", "ash", "atg", "asp", "std.proc.home.sch.bckg.tch.bckg"),
    c("acg", "asa", "atg", "asp", "std.ach.proc.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "asa", "asp", "std.bckg.proc.ach.home.sch.bckg"),
    c("acg", "asg", "asa", "atg", "asp", "std.bckg.proc.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "atg", "asp", "std.bckg.proc.home.sch.bckg.tch.bckg"),
    c("acg", "ash", "asa", "atg", "asp", "std.ach.proc.home.sch.bckg.tch.bckg"),
    c("asg", "ash", "asa", "atg", "asp", "std.bckg.proc.ach.home.tch.bckg"),
    c("acg", "asg", "ash", "asa", "atg", "asp", "std.bckg.proc.ach.home.sch.bckg.tch.bckg"),
    c("bcg", "sch.bckg"),
    c("bsg", "std.bckg"),
    c("bsa", "std.ach"),
    c("bsg", "bsa", "std.bckg.ach"),
    c("bcg", "bsg", "std.bckg.sch.bckg"),
    c("bcg", "bsa", "std.ach.sch.bckg"),
    c("bcg", "bsg", "bsa", "std.bckg.ach.sch.bckg"),
    c("bcg", "btm", "sch.bckg.math.tch.bckg"),
    c("bcg", "bts", "sch.bckg.sci.tch.bckg"),
    c("bsg", "btm", "std.bckg.math.tch.bckg"),
    c("bsg", "bts", "std.bckg.sci.tch.bckg"),
    c("bsa", "btm", "std.ach.math.tch.bckg"),
    c("bsa", "bts", "std.ach.sci.tch.bckg"),
    c("bcg", "bsg", "btm", "std.bckg.sch.bckg.math.tch.bckg"),
    c("bcg", "bsg", "bts", "std.bckg.sch.bckg.sci.tch.bckg"),
    c("bcg", "bsa", "btm", "std.ach.sch.bckg.math.tch.bckg"),
    c("bcg", "bsa", "bts", "std.ach.sch.bckg.sci.tch.bckg"),
    c("bsg", "bsa", "btm", "std.bckg.ach.math.tch.bckg"),
    c("bsg", "bsa", "bts", "std.bckg.ach.sci.tch.bckg"),
    c("bcg", "bsg", "bsa", "btm", "std.bckg.ach.sch.bckg.math.tch.bckg"),
    c("bcg", "bsg", "bsa", "bts", "std.bckg.ach.sch.bckg.sci.tch.bckg"),
    c("bsp", "std.proc"),
    c("bsa", "bsp", "std.ach.proc"),
    c("bsg", "bsp", "std.bckg.proc"),
    c("bcg", "bsp", "std.proc.sch.bckg"),
    c("btm", "bsp", "std.proc.math.tch.bckg"),
    c("bts", "bsp", "std.proc.sci.tch.bckg"),
    c("bsg", "bsa", "bsp", "std.bckg.proc.ach"),
    c("bcg", "bsg", "bsp", "std.bckg.proc.sch.bckg"),
    c("bcg", "bsa", "bsp", "std.ach.proc.sch.bckg"),
    c("bcg", "bsg", "bsp", "bsa", "std.bckg.proc.ach.sch.bckg"),
    c("bcg", "btm", "bsp", "sch.bckg.proc.math.tch.bckg"),
    c("bcg", "bts", "bsp", "sch.bckg.proc.sci.tch.bckg"),
    c("bsg", "btm", "bsp", "std.bckg.proc.math.tch.bckg"),
    c("bsg", "bts", "bsp", "std.bckg.proc.sci.tch.bckg"),
    c("bsa", "btm", "bsp", "std.ach.proc.math.tch.bckg"),
    c("bsa", "bts", "bsp", "std.ach.proc.sci.tch.bckg"),
    c("bcg", "bsg", "btm", "bsp", "std.bckg.proc.sch.bckg.math.tch.bckg"),
    c("bcg", "bsg", "bts", "bsp", "std.bckg.proc.sch.bckg.sci.tch.bckg"),
    c("bcg", "bsa", "btm", "bsp", "std.ach.proc.sch.bckg.math.tch.bckg"),
    c("bcg", "bsa", "bts", "bsp", "std.ach.proc.sch.bckg.sci.tch.bckg"),
    c("bsg", "bsa", "btm", "bsp", "std.bckg.proc.ach.math.tch.bckg"),
    c("bsg", "bsa", "bts", "bsp", "std.bckg.proc.ach.sci.tch.bckg"),
    c("bcg", "bsg", "bsa", "btm", "bsp", "std.bckg.proc.ach.sch.bckg.math.tch.bckg"),
    c("bcg", "bsg", "bsa", "bts", "bsp", "std.bckg.proc.ach.sch.bckg.sci.tch.bckg")
  ),
  preTIMSS = list(
    c("acg", "sch.bckg"),
    c("asg", "std.bckg"),
    c("asa", "std.ach"),
    c("asg", "asa", "std.bckg.ach"),
    c("asg", "ash", "std.bckg.home"),
    c("ash", "asa", "std.ach.home"),
    c("asg", "ash", "asa", "std.bckg.ach.home"),
    c("acg", "asg", "std.bckg.sch.bckg"),
    c("acg", "ash", "std.home.sch.bckg"),
    c("acg", "asa", "std.ach.sch.bckg"),
    c("acg", "asg", "ash", "asa", "std.bckg.ach.home.sch.bckg"),
    c("acg", "asg", "asa", "std.bckg.ach.sch.bckg"),
    c("acg", "asg", "ash", "std.bckg.home.sch.bckg"),
    c("acg", "ash", "asa", "std.ach.home.sch.bckg"),
    c("acg", "atg", "sch.bckg.tch.bckg"),
    c("asg", "atg", "std.bckg.tch.bckg"),
    c("asa", "atg", "std.ach.tch.bckg"),
    c("ash", "atg", "std.home.tch.bckg"),
    c("acg", "asg", "atg", "std.bckg.sch.bckg.tch.bckg"),
    c("acg", "ash", "atg", "std.home.sch.bckg.tch.bckg"),
    c("acg", "asa", "atg", "std.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "asa", "atg", "std.bckg.ach.home.sch.bckg.tch.bckg"),
    c("acg", "asg", "asa", "atg", "std.bckg.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "atg", "std.bckg.home.sch.bckg.tch.bckg"),
    c("acg", "ash", "asa", "atg", "std.ach.home.sch.bckg.tch.bckg"),
    c("asg", "ash", "asa", "atg", "std.bckg.ach.home.tch.bckg"),
    c("asg", "ash", "atg", "std.bckg.home.tch.bckg"),
    c("asg", "asa", "atg", "std.bckg.ach.tch.bckg"),
    c("ash", "asa", "atg", "std.ach.home.tch.bckg")
  ),
  "eTIMSS PSI" = list(
    c("acg", "sch.bckg"),
    c("asg", "std.bckg"),
    c("asa", "std.ach"),
    c("asg", "asa", "std.bckg.ach"),
    c("asg", "ash", "std.bckg.home"),
    c("ash", "asa", "std.ach.home"),
    c("asg", "ash", "asa", "std.bckg.ach.home"),
    c("acg", "asg", "std.bckg.sch.bckg"),
    c("acg", "ash", "std.home.sch.bckg"),
    c("acg", "asa", "std.ach.sch.bckg"),
    c("acg", "asg", "ash", "asa", "std.bckg.ach.home.sch.bckg"),
    c("acg", "asg", "asa", "std.bckg.ach.sch.bckg"),
    c("acg", "asg", "ash", "std.bckg.home.sch.bckg"),
    c("acg", "ash", "asa", "std.ach.home.sch.bckg"),
    c("acg", "atg", "sch.bckg.tch.bckg"),
    c("asg", "atg", "std.bckg.tch.bckg"),
    c("asa", "atg", "std.ach.tch.bckg"),
    c("ash", "atg", "std.home.tch.bckg"),
    c("acg", "asg", "atg", "std.bckg.sch.bckg.tch.bckg"),
    c("acg", "ash", "atg", "std.home.sch.bckg.tch.bckg"),
    c("acg", "asa", "atg", "std.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "asa", "atg", "std.bckg.ach.home.sch.bckg.tch.bckg"),
    c("acg", "asg", "asa", "atg", "std.bckg.ach.sch.bckg.tch.bckg"),
    c("acg", "asg", "ash", "atg", "std.bckg.home.sch.bckg.tch.bckg"),
    c("acg", "ash", "asa", "atg", "std.ach.home.sch.bckg.tch.bckg"),
    c("asg", "ash", "asa", "atg", "std.bckg.ach.home.tch.bckg"),
    c("asg", "ash", "atg", "std.bckg.home.tch.bckg"),
    c("asg", "asa", "atg", "std.bckg.ach.tch.bckg"),
    c("ash", "asa", "atg", "std.ach.home.tch.bckg"),
    c("bcg", "sch.bckg"),
    c("bsg", "std.bckg"),
    c("bsa", "std.ach"),
    c("bsg", "bsa", "std.bckg.ach"),
    c("bcg", "bsg", "std.bckg.sch.bckg"),
    c("bcg", "bsa", "std.ach.sch.bckg"),
    c("bcg", "bsg", "bsa", "std.bckg.ach.sch.bckg"),
    c("bcg", "btm", "sch.bckg.math.tch.bckg"),
    c("bcg", "bts", "sch.bckg.sci.tch.bckg"),
    c("bsg", "btm", "std.bckg.math.tch.bckg"),
    c("bsg", "bts", "std.bckg.sci.tch.bckg"),
    c("bsa", "btm", "std.ach.math.tch.bckg"),
    c("bsa", "bts", "std.ach.sci.tch.bckg"),
    c("bcg", "bsg", "btm", "std.bckg.sch.bckg.math.tch.bckg"),
    c("bcg", "bsg", "bts", "std.bckg.sch.bckg.sci.tch.bckg"),
    c("bcg", "bsa", "btm", "std.ach.sch.bckg.math.tch.bckg"),
    c("bcg", "bsa", "bts", "std.ach.sch.bckg.sci.tch.bckg"),
    c("bsg", "bsa", "btm", "std.bckg.ach.math.tch.bckg"),
    c("bsg", "bsa", "bts", "std.bckg.ach.sci.tch.bckg"),
    c("bcg", "bsg", "bsa", "btm", "std.bckg.ach.sch.bckg.math.tch.bckg"),
    c("bcg", "bsg", "bsa", "bts", "std.bckg.ach.sch.bckg.sci.tch.bckg")
  ),
  "TIMSS Advanced" = list(
    c("mcg", "math.sch.bckg"),
    c("msg", "math.std.bckg"),
    c("msa", "math.std.ach"),
    c("msg", "msa", "math.std.bckg.ach"),
    c("mcg", "msg", "math.std.bckg.math.sch.bckg"),
    c("mcg", "msa", "math.std.ach.math.sch.bckg"),
    c("mcg", "msg", "msa", "math.std.bckg.ach.math.sch.bckg"),
    c("mcg", "mtg", "math.tch.bckg.math.sch.bckg"),
    c("msg", "mtg", "math.std.bckg.math.tch.bckg"),
    c("msa", "mtg", "math.std.ach.math.tch.bckg"),
    c("mcg", "msg", "mtg", "math.std.bckg.math.sch.bckg.math.tch.bckg"),
    c("mcg", "msa", "mtg", "math.std.ach.math.sch.bckg.math.tch.bckg"),
    c("msg", "msa", "mtg", "math.std.bckg.ach.math.tch.bckg"),
    c("mcg", "msg", "msa", "mtg", "math.std.bckg.ach.math.sch.bckg.math.tch.bckg"),
    c("pcg", "phys.sch.bckg"),
    c("psg", "phys.std.bckg"),
    c("psa", "phys.std.ach"),
    c("psg", "psa", "phys.std.bckg.ach"),
    c("pcg", "psg", "phys.std.bckg.phys.sch.bckg"),
    c("pcg", "psa", "phys.std.ach.phys.sch.bckg"),
    c("pcg", "psg", "psa", "phys.std.bckg.ach.phys.sch.bckg"),
    c("pcg", "ptg", "phys.tch.bckg.phys.sch.bckg"),
    c("psg", "ptg", "phys.std.bckg.phys.tch.bckg"),
    c("psa", "ptg", "phys.std.ach.phys.tch.bckg"),
    c("pcg", "psg", "ptg", "phys.std.bckg.phys.sch.bckg.phys.tch.bckg"),
    c("pcg", "psa", "ptg", "phys.std.ach.phys.sch.bckg.phys.tch.bckg"),
    c("psg", "psa", "ptg", "phys.std.bckg.ach.phys.tch.bckg"),
    c("pcg", "psg", "psa", "ptg", "phys.std.bckg.ach.phys.sch.bckg.phys.tch.bckg")
  )
)
studies.all.design.variables <- list(
  sampling.vars = list(
    bc_ = c("SCHWGT", "STDWGT"),
    bl_ = c("TOTWGT", "SENWGT", "JKZONE", "JKREP", "TOTWGTCH", "SENWGTCH"),
    bs_ = c("TOTWGT", "SENWGT", "JKZONE", "JKREP"),
    isa = c("TOTWGTS", "JKZONES", "JKREPS", paste0("SRWGT", 1:75)),
    icg = c("TOTWGTC", "JKZONEC", "JKREPC", paste0("CRWGT", 1:75)),
    ise = c("TOTWGTS", "JKZONES", "JKREPS", paste0("SRWGT", 1:75)),
    isg = c("TOTWGTS", "JKZONES", "JKREPS", paste0("SRWGT", 1:75)),
    isl = c("TOTWGTS", "JKZONES", "JKREPS"),
    iss = c("TOTWGTS", "JKZONES", "JKREPS"),
    itg = c("TOTWGTT", "JKZONET", "JKREPT", paste0("TRWGT", 1:75)),
    jsa = c("TOTWGTS", "JKZONES", "JKREPS", paste0("SRWGT", 1:75)),
    jse = c("TOTWGTS", "JKZONES", "JKREPS", paste0("SRWGT", 1:75)),
    jsg = c("TOTWGTS", "JKZONES", "JKREPS", paste0("SRWGT", 1:75)),
    asa = c("TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP", "JKINDIC"),
    acg = c("SCHWGT", "STOTWGTU", "STOTWGTL", "JKCZONE", "JKCREP", paste0("SRWGT", 1:100)),
    asc = c("TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP"),
    asg = c("TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP", "JKINDIC", "STAFFWGT", paste0("SRWGT", 1:92)),
    ast = c("TCHWGT", "MATWGT", "SCIWGT", "REAWGT", "JKZONE", "JKREP"),
    asp = c("TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP", "JKINDIC"),
    atg = c("TCHWGT", paste0("TRWGT", 1:100)),
    alg = c("CNTRWGT", paste0("CRWGT", 1:92)),
    bsa = c("TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP"),
    bcg = c("TOTWGTC", paste0("CRWGT", 1:75), "JKZONEC", "JKREPC", "SCHWGT", "STOTWGTL", "STOTWGTU", "STOTWGTE", "JKCZONE", "JKCREP", paste0("SRWGT", 1:100)),
    bsg = c("TOTWGTS", paste0("SRWGT", 1:75), "JKZONES", "JKREPS", "TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP", "STAFFWGT", paste0("SRWGT", 1:92)),
    btg = c("TOTWGTT", paste0("TRWGT", 1:75), "JKZONET", "JKREPT", "TCHWGT", paste0("TRWGT", 1:100)),
    btm = c("MTOTWGT", "JKZONE", "JKREP"),
    bts = c("STOTWGT", "JKZONE", "JKREP"),
    bst = c("MATWGT", "SCIWGT", "TCHWGT", "TOTWGT", "JKZONE", "JKREP"),
    bsp = c("TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP"),
    blg = c("CNTRWGT", paste0("CRWGT", 1:92)),
    mcg = c("SCHWGT", "STOTWGT", "STOTWGTU", "JKCZONE", "JKCREP"),
    msa = c("TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP"),
    msg = c("TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP"),
    mst = c("MATWGT", "JKZONE", "JKREP"),
    psa = c("TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP"),
    psg = c("TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP"),
    pst = c("PHYWGT", "JKZONE", "JKREP"),
    ccg = c("SCHWGT", paste0("SRWGT", 1:100)),
    ctg = c("TCHWGT", paste0("TRWGT", 1:100)),
    pcg = c("SCHWGT", "STOTWGT", "STOTWGTU", "JKCZONE", "JKCREP", paste0("SRWGT", 1:100)),
    ptg = c("TCHWGT", paste0("TRWGT", 1:100)),
    deg = c("INSWGTE", "FINWGTE", paste0("INSRWE", 1:32), paste0("FINRWE", 1:32)),
    dig = c("INSWGTI", "FINWGTI", paste0("INSRWI", 1:32), paste0("FINRWI", 1:32)),
    dpg = c("INSWGTP", "FINWGTP", paste0("INSRWP", 1:32), paste0("FINRWP", 1:32)),
    dsg = c("INSWGTS", "FINWGTS", paste0("INSRWS", 1:32), paste0("FINRWS", 1:32))
  ),
  PV.roots = list(
    TIMSS = list(
      G4 = c("ASMPV", "ASSPV", "ASMMAT", "ASMWHO", "ASMFAP", "ASMGEM", "ASMDAP", "ASSSCI", "ASSEAS", "ASSLIS", "ASSPHS", "ASMALG", "ASMFNS", "ASMGEO", "ASMMEA", "ASSPHY", "ASMAPP", "ASMKNO", "ASMREA", "ASMDAT", "ASMNUM", "ASSEAR", "ASSLIF", "ASSKNO", "ASSAPP", "ASSREA", "ASSENV"),
      G8 = c("BSMMAT", "BSSSCI", "BSMALG", "BSMDAP", "BSMFNS", "BSMGEO", "BSMMEA", "BSSCHE", "BSSEAS", "BSSLIS", "BSSPHY", "BSSERI", "BSSNOS", "BSMNBM", "BSSNBM", "BSMAPP", "BSMKNO", "BSMREA", "BSMDAT", "BSMNUM", "BSSEAR", "BSSBIO", "BSSKNO", "BSSAPP", "BSSREA", "BSSENV")
    ),
    "TIMSS Advanced" = list(
      Mathematics = c("PSPPHY", "PSPPHY", "PSPPHY", "PSPELE", "PSPMEC", "PSPWAV", "PSPAPP", "PSPKNO", "PSPREA"),
      Physics = c("MSMMAT", "MSMMAT", "MSMMAT", "MSMALG", "MSMCAL", "MSMGEO", "MSMKNO", "MSMAPP", "MSMREA")
    ),
    TiPi = list(
      G4 = c("ASMMAT", "ASSSCI", "ASRREA")
    ),
    PIRLS = list(
      G4 = c("ASRINF", "ASRLIT", "ASRREA", "ASRIIE", "ASRRSI", "ASRREA", "ASRLIT", "ASRINF", "ASRIIE", "ASRRSI", "ASRREA", "ASRLIT", "ASRINF", "ASRIIE", "ASRRSI", "ASRREA", "ASRLIT", "ASRINF", "ASRIIE", "ASRRSI", "ASEREA", "ASERSI", "ASEIIE", "ASRREA", "ASRLIT", "ASRINF", "ASRIIE", "ASRRSI", "ASRREA", "ASRLIT", "ASRINF", "ASRIIE", "ASRRSI", "ASRREA", "ASRLIT", "ASRINF", "ASRIIE", "ASRRSI")
    ),
    RLII = list(
      G4 = c("ASRDOC", "ASRDOC", "ASRDOC", "ASRDOC", "ASRDOC", "ASREXP", "ASREXP", "ASREXP", "ASREXP", "ASREXP", "ASRNAR", "ASRNAR", "ASRNAR", "ASRNAR", "ASRNAR", "ASRREA", "ASRREA", "ASRREA", "ASRREA", "ASRREA", "ASRDOC", "ASRDOC", "ASRDOC", "ASRDOC", "ASRDOC", "ASREXP", "ASREXP", "ASREXP", "ASREXP", "ASREXP", "ASRNAR", "ASRNAR", "ASRNAR", "ASRNAR", "ASRNAR", "ASRREA", "ASRREA", "ASRREA", "ASRREA", "ASRREA")
    ),
    ICCS = list(
      G8 = c("PV[[:digit:]]+CIV"),
      G9 = c("PV[[:digit:]]+CIV")
    ),
    ICILS = list(
      G8 = c("PV[[:digit:]]+CIL", "PV[[:digit:]]+CT")
    ),
    PISA = list(
      fifteen.year.old = c("PV[[:digit:]]+MATH", "PV[[:digit:]]+READ", "PV[[:digit:]]+SCIE", "PV[[:digit:]]+PROB", "PV[[:digit:]]+INTR", "PV[[:digit:]]+SUPP", "PV[[:digit:]]+EPS", "PV[[:digit:]]+ISI", "PV[[:digit:]]+USE", "PV[[:digit:]]+MACC", "PV[[:digit:]]+MACQ", "PV[[:digit:]]+MACS", "PV[[:digit:]]+MACU", "PV[[:digit:]]+MAPE", "PV[[:digit:]]+MAPF", "PV[[:digit:]]+MAPI", "PV[[:digit:]]+SCEP", "PV[[:digit:]]+SCED", "PV[[:digit:]]+SCID", "PV[[:digit:]]+SKCO", "PV[[:digit:]]+SKPE", "PV[[:digit:]]+SSPH", "PV[[:digit:]]+SSLI", "PV[[:digit:]]+SSES", "PV[[:digit:]]+GLCM", "PV[[:digit:]]+RCLI", "PV[[:digit:]]+RCUN", "PV[[:digit:]]+RCER", "PV[[:digit:]]+RTSN", "PV[[:digit:]]+RTML", "PV[[:digit:]]+MCCR", "PV[[:digit:]]+MCQN", "PV[[:digit:]]+MCSS", "PV[[:digit:]]+MCUD", "PV[[:digit:]]+MPEM", "PV[[:digit:]]+MPFS", "PV[[:digit:]]+MPIN", "PV[[:digit:]]+MPRE")
    )
  )
)
design.weight.variables <- list(
  IEA.JK2.studies = c("CivED",
                      "ICCS",
                      "ICILS",
                      "PIRLS",
                      "prePIRLS",
                      "ePIRLS",
                      "RLII",
                      "SITES",
                      "TIMSS",
                      "preTIMSS",
                      "eTIMSS PSI",
                      "TIMSS Advanced",
                      "TiPi",
                      "REDS"),
  IEA.JK2.dflt.std.bckg.types = c("std.bckg",
                                  "std.bckg.sch.bckg",
                                  "std.EUM",
                                  "std.AM",
                                  "std.LAM",
                                  "std.ach.EUM",
                                  "std.ach.AM",
                                  "std.ach.LAM",
                                  "std.EUM.sch.bckg",
                                  "std.AM.sch.bckg",
                                  "std.LAM.sch.bckg",
                                  "std.bckg.EUM",
                                  "std.bckg.ach.EUM",
                                  "std.bckg.ach.AM",
                                  "std.bckg.ach.LAM",
                                  "std.ach.sch.bckg",
                                  "std.bckg.AM",
                                  "std.bckg.LAM",
                                  "std.ach",
                                  "std.bckg.ach",
                                  "std.bckg.ach.sch.bckg",
                                  "std.bckg.EUM.sch.bckg",
                                  "std.bckg.AM.sch.bckg",
                                  "std.bckg.LAM.sch.bckg",
                                  "std.ach.EUM.sch.bckg",
                                  "std.ach.AM.sch.bckg",
                                  "std.ach.LAM.sch.bckg",
                                  "std.bckg.std.ach.EUM.sch.bckg",
                                  "std.bckg.std.ach.AM.sch.bckg",
                                  "std.bckg.std.ach.LAM.sch.bckg",
                                  "std.bckg.home",
                                  "std.bckg.ach.home",
                                  "std.ach.home",
                                  "std.ach.home.sch.bckg",
                                  "std.bckg.home.sch.bckg",
                                  "std.bckg.ach.home.sch.bckg",
                                  "math.std.bckg",
                                  "math.std.ach",
                                  "math.std.bckg.ach",
                                  "math.std.bckg.math.sch.bckg",
                                  "math.std.ach.math.sch.bckg",
                                  "math.std.bckg.ach.math.sch.bckg",
                                  "phys.std.bckg",
                                  "phys.std.ach",
                                  "phys.std.bckg.ach",
                                  "phys.std.bckg.phys.sch.bckg",
                                  "phys.std.ach.phys.sch.bckg",
                                  "phys.std.bckg.ach.phys.sch.bckg",
                                  "std.proc",
                                  "std.bckg.proc",
                                  "std.bckg.proc.home",
                                  "std.bckg.proc.ach",
                                  "std.bckg.proc.sch.bckg",
                                  "std.ach.proc.sch.bckg",
                                  "std.bckg.proc.ach.home",
                                  "std.bckg.proc.ach.sch.bckg",
                                  "std.bckg.proc.home.sch.bckg",
                                  "std.ach.proc.home.sch.bckg",
                                  "std.bckg.proc.ach.home.sch.bckg",
                                  "std.ach.proc.EUM",
                                  "std.ach.proc.LAM",
                                  "std.ach.proc.AM",
                                  "std.bckg.proc.EUM",
                                  "std.bckg.proc.LAM",
                                  "std.bckg.proc.AM",
                                  "std.proc.EUM.sch.bckg",
                                  "std.proc.LAM.sch.bckg",
                                  "std.proc.AM.sch.bckg",
                                  "std.bckg.proc.ach.LAM",
                                  "std.bckg.proc.ach.EUM",
                                  "std.bckg.proc.ach.AM",
                                  "std.bckg.proc.EUM.sch.bckg",
                                  "std.bckg.proc.LAM.sch.bckg",
                                  "std.ach.proc.LAM.sch.bckg",
                                  "std.ach.proc.AM.sch.bckg",
                                  "std.ach.proc.EUM.sch.bckg",
                                  "std.bckg.std.ach.proc.EUM.sch.bckg",
                                  "std.bckg.std.ach.proc.LAM.sch.bckg",
                                  "std.bckg.std.ach.proc.AM.sch.bckg"
  ),
  IEA.JK2.dflt.std.bckg.wgts = c("TOTWGT",
                                 "TOTWGTS"),
  IEA.JK2.dflt.std.bckg.zones = c("JKZONE",
                                  "JKZONES"),
  IEA.JK2.dflt.std.bckg.rep.ind = c("JKREP",
                                    "JKREPS",
                                    "JKINDIC"),
  IEA.JK2.dflt.sch.bckg.types = c("sch.bckg",
                                  "std.home.sch.bckg",
                                  "math.sch.bckg",
                                  "math.sch.bckg.math.tch.bckg",
                                  "phys.sch.bckg",
                                  "phys.sch.bckg.phys.tch.bckg",
                                  "std.proc.home.sch.bckg"
  ),
  IEA.JK2.dflt.sch.bckg.wgts = c("TOTWGTC",
                                 "SCHWGT",
                                 "TOTWGTT"),
  IEA.JK2.dflt.sch.bckg.zones = c("JKZONEC",
                                  "JKCZONE",
                                  "JKZONE",
                                  "JKZONET"),
  IEA.JK2.dflt.sch.bckg.rep.ind = c("JKREPC",
                                    "JKCREP",
                                    "JKREPT"),
  IEA.JK2.dflt.tch.bckg.types = c("tch.bckg",
                                  "sch.bckg.tch.bckg",
                                  "std.bckg.tch.bckg",
                                  "std.bckg.sch.bckg.tch.bckg",
                                  "std.ach.sch.bckg.tch.bckg",
                                  "std.bckg.ach.sch.bckg.tch.bckg",
                                  "std.bckg.ach.home.tch.bckg",
                                  "std.bckg.ach.home.sch.bckg.tch.bckg",
                                  "std.ach.tch.bckg",
                                  "std.home.tch.bckg",
                                  "std.ach.home.tch.bckg",
                                  "math.tch.bckg",
                                  "math.tch.bckg.math.sch.bckg",
                                  "math.std.bckg.math.tch.bckg",
                                  "math.std.bckg.ach.math.tch.bckg",
                                  "math.std.ach.math.tch.bckg",
                                  "math.std.bckg.math.sch.bckg.math.tch.bckg",
                                  "math.std.ach.math.sch.bckg.math.tch.bckg",
                                  "math.std.bckg.ach.math.sch.bckg.math.tch.bckg",
                                  "phys.tch.bckg.phys.sch.bckg",
                                  "phys.std.bckg.phys.tch.bckg",
                                  "phys.std.bckg.ach.phys.tch.bckg",
                                  "phys.std.ach.phys.tch.bckg",
                                  "phys.std.bckg.phys.sch.bckg.phys.tch.bckg",
                                  "phys.std.ach.phys.sch.bckg.phys.tch.bckg",
                                  "phys.std.bckg.ach.phys.sch.bckg.phys.tch.bckg",
                                  "sci.tch.bckg.sci.sch.bckg",
                                  "sci.tch.bckg",
                                  "std.bckg.ach.tch.bckg",
                                  "std.bckg.home.tch.bckg",
                                  "std.bckg.sch.bckg.math.tch.bckg",
                                  "std.bckg.sch.bckg.sci.tch.bckg",
                                  "std.ach.sch.bckg.math.tch.bckg",
                                  "std.ach.sch.bckg.sci.tch.bckg",
                                  "std.home.sch.bckg.tch.bckg",
                                  "std.bckg.home.sch.bckg.tch.bckg",
                                  "std.ach.home.sch.bckg.tch.bckg",
                                  "std.bckg.math.tch.bckg",
                                  "std.ach.math.tch.bckg",
                                  "std.bckg.ach.math.tch.bckg",
                                  "std.bckg.ach.sch.bckg.math.tch.bckg",
                                  "sch.bckg.math.tch.bckg",
                                  "std.bckg.sci.tch.bckg",
                                  "std.ach.sci.tch.bckg",
                                  "std.bckg.ach.sci.tch.bckg",
                                  "std.bckg.ach.sch.bckg.sci.tch.bckg",
                                  "sch.bckg.sci.tch.bckg",
                                  "std.bckg.proc.tch.bckg",
                                  "std.ach.proc.tch.bckg",
                                  "std.proc.home.tch.bckg",
                                  "std.bckg.proc.home.tch.bckg",
                                  "std.bckg.proc.ach.tch.bckg",
                                  "std.ach.proc.home.tch.bckg",
                                  "std.bckg.proc.sch.bckg.tch.bckg",
                                  "std.proc.home.sch.bckg.tch.bckg",
                                  "std.ach.proc.sch.bckg.tch.bckg",
                                  "std.bckg.proc.ach.sch.bckg.tch.bckg",
                                  "std.bckg.proc.home.sch.bckg.tch.bckg",
                                  "std.ach.proc.home.sch.bckg.tch.bckg",
                                  "std.bckg.proc.ach.home.tch.bckg",
                                  "std.bckg.proc.ach.home.sch.bckg.tch.bckg",
                                  "sch.bckg.proc.math.tch.bckg",
                                  "sch.bckg.proc.sci.tch.bckg",
                                  "std.bckg.proc.math.tch.bckg",
                                  "std.bckg.proc.sci.tch.bckg",
                                  "std.ach.proc.math.tch.bckg",
                                  "std.ach.proc.sci.tch.bckg",
                                  "std.bckg.proc.sch.bckg.math.tch.bckg",
                                  "std.bckg.proc.sch.bckg.sci.tch.bckg",
                                  "std.ach.proc.sch.bckg.math.tch.bckg",
                                  "std.ach.proc.sch.bckg.sci.tch.bckg",
                                  "std.bckg.proc.ach.math.tch.bckg",
                                  "std.bckg.proc.ach.sci.tch.bckg",
                                  "std.bckg.proc.ach.sch.bckg.math.tch.bckg",
                                  "std.bckg.proc.ach.sch.bckg.sci.tch.bckg"
  ),
  IEA.JK2.dflt.tch.bckg.wgts = c("TOTWGT",
                                 "TOTWGTT",
                                 "TCHWGT",
                                 "MTOTWGT",
                                 "STOTWGT",
                                 "MATWGT",
                                 "SCIWGT"),
  IEA.JK2.dflt.tch.bckg.zones = c("JKZONET",
                                  "JKZONE"),
  IEA.JK2.dflt.tch.bckg.rep.ind = c("JKREPT",
                                    "JKREP",
                                    "JKINDIC"),
  IEA.BRR.studies = "TEDS-M",
  IEA.BRR.dflt.inst.bckg.types = "inst.bckg",
  IEA.BRR.dflt.inst.bckg.wgts = "FINWGTI",
  IEA.BRR.dflt.inst.bckg.rep.wgts = paste0("FINRWI", 1:32),
  IEA.BRR.other.inst.bckg.wgts = "INSWGTI",
  IEA.BRR.other.inst.bckg.rep.wgts = paste0("INSRWI", 1:32),
  IEA.BRR.dflt.prim.tch.bckg.types = c("prim.tch.bckg",
                                       "inst.bckg.prim.tch.bckg"),
  IEA.BRR.dflt.prim.tch.bckg.wgts = "FINWGTP",
  IEA.BRR.dflt.prim.tch.bckg.rep.wgts = paste0("FINRWP", 1:32),
  IEA.BRR.other.prim.tch.bckg.wgts = "INSWGTP",
  IEA.BRR.other.prim.tch.bckg.rep.wgts = paste0("INSRWP", 1:32),
  IEA.BRR.dflt.low_sec.tch.bckg.types = c("low-sec.tch.bckg",
                                          "inst.bckg.low-sec.tch.bckg"),
  IEA.BRR.dflt.low_sec.tch.bckg.wgts = "FINWGTS",
  IEA.BRR.dflt.low_sec.tch.bckg.rep.wgts = paste0("FINRWS", 1:32),
  IEA.BRR.other.low_sec.tch.bckg.wgts = "INSWGTS",
  IEA.BRR.other.low_sec.tch.bckg.rep.wgts = paste0("INSRWS", 1:32),
  IEA.BRR.dflt.educ.bckg.types = "educ.bckg",
  IEA.BRR.dflt.educ.bckg.wgts = "FINWGTE",
  IEA.BRR.dflt.educ.bckg.rep.wgts = paste0("FINRWE", 1:32),
  IEA.BRR.other.educ.bckg.wgts = "INSWGTE",
  IEA.BRR.other.educ.bckg.rep.wgts = paste0("INSRWE", 1:32),
  OECD.BRR.studies = c("PISA",
                       "PISA for Development",
                       "TALIS",
                       "TALIS 3S"),
  OECD.BRR.dflt.std.bckg = "std.bckg",
  OECD.BRR.dflt.std.bckg.wgts = "W_FSTUWT",
  OECD.BRR.dflt.std.bckg.rep.wgts = c(paste0("W_FSTR", 1:80), paste0("W_FSTURWT", 1:80)),
  OECD.BRR.dflt.out.of.school.bckg = "out.of.school.bckg",
  OECD.BRR.dflt.out.of.school.bckg.wgts = "SPFWT0",
  OECD.BRR.dflt.out.of.school.bckg.rep.wgts = paste0("SPFWT0", 1:30),
  OECD.BRR.dflt.sch.bckg = "sch.bckg",
  OECD.BRR.dflt.sch.bckg.wgts = "SCHWGT",
  OECD.dflt.sch.bckg.rep.wgts = paste0("SRWGT", 1:100),
  OECD.BRR.dflt.tch.bckg = c("tch.bckg",
                             "sch.bckg.tch.bckg"),
  OECD.BRR.dflt.tch.bckg.wgts = "TCHWGT",
  OECD.BRR.dflt.tch.bckg.rep.wgts = paste0("TRWGT", 1:100),
  OECD.BRR.dflt.lead.bckg = c("leader.bckg"),
  OECD.BRR.dflt.lead.bckg.wgts = "CNTRWGT",
  OECD.BRR.dflt.lead.bckg.rep.wgts = paste0("CRWGT", 1:92),
  OECD.BRR.dflt.staff.bckg = c("staff.bckg",
                               "leader.bckg.staff.bckg"),
  OECD.BRR.dflt.staff.bckg.wgts = "STAFFWGT",
  OECD.BRR.dflt.staff.bckg.rep.wgts = paste0("SRWGT", 1:92)
)
default.benchmarks <- list(
  ICCS = list(
    "2009" = c(395, 479, 563),
    "2016" = c(311, 395, 479, 563),
    "2022" = c(311, 395, 479, 563)
  ),
  ICILS = c(407.001, 492.001, 576.001, 661.001),
  PIRLS = c(400, 475, 550, 625),
  RLII = c(400, 475, 550, 625),
  ePIRLS = c(400, 475, 550, 625),
  prePIRLS = c(400, 475, 550, 625),
  TIMSS = c(400, 475, 550, 625),
  preTIMSS = c(400, 475, 550, 625),
  "eTIMSS PSI" = c(400, 475, 550, 625),
  "TIMSS Advanced" = c(475, 550, 625),
  TiPi = c(400, 475, 550, 625),
  PISA = list(
    Reading = list(
      "2022" = c(189.33, 262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32),
      "2018" = c(189.33, 262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32),
      "2015" = c(262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32),
      "2012" = c(262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32),
      "2009" = c(262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32),
      "2006" = c(334.75, 407.47, 480.18, 552.89, 625.61),
      "2003" = c(334.75, 407.47, 480.18, 552.89, 625.61),
      "2000" = c(334.75, 407.47, 480.18, 552.89, 625.61)
    ),
    Science = list(
      "2022" = c(260.54, 334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
      "2018" = c(260.54, 334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
      "2015" = c(260.54, 334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
      "2012" = c(334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
      "2009" = c(334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
      "2006" = c(334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
      "2003" = c(334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
      "2000" = c(334.94, 409.54, 484.14, 558.73, 633.33, 707.93)
    ),
    Mathematics = list(
      "2022" = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30),
      "2018" = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30),
      "2015" = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30),
      "2012" = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30),
      "2009" = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30),
      "2006" = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30),
      "2003" = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30),
      "2000" = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30)
    ),
    Problem.Solving = list(
      "2003" = c(358.49, 423.42, 488.35, 553.28, 618.21, 683.14)
    ),
    Collaborative.Problem.Solving = list(
      "2015" = c(340, 440, 540, 640)
    ),
    Financial.Literacy = list(
      "2022" = c(326, 399, 474, 549, 624),
      "2015" = c(325.57, 400.33, 475.10, 549.86, 624.63)
    ),
    Global.Competency = list(
      "2018" = c(340, 440, 540, 640),
      "2018" = c(340, 440, 540, 640)
    ),
    Reading.root.PVs = c("PV#READ", "PV#READ1", "PV#READ2", "PV#READ3", "PV#READ4", "PV#READ5", "PV#RCLI", "PV#RCUN", "PV#RCER", "PV#RTSN", "PV#RTML"),
    Science.root.PVs = c("PV#SCIE", "PV#INTR", "PV#SUPP", "PV#EPS", "PV#ISI", "PV#USE", "PV#SCEP", "PV#SCED", "PV#SCID", "PV#SKCO", "PV#SKPE", "PV#SSPH", "PV#SSLI", "PV#SSES"),
    Mathematics.root.PVs = c("PV#MATH", "PV#MATH1", "PV#MATH2", "PV#MATH3", "PV#MATH4", "PV#MACC", "PV#MACQ", "PV#MACS", "PV#MACU", "PV#MAPE", "PV#MAPF", "PV#MAPI", "PV#MCCR", "PV#MCQN", "PV#MCSS", "PV#MCUD", "PV#MPEM", "PV#MPFS", "PV#MPIN", "PV#MPRE"),
    Problem.Solving.root.PVs = c("PV#PROB"),
    Collaborative.Problem.Solving.root.PVs = c("PV#CLPS"),
    Financial.Literacy.root.PVs = c("PV#FLIT"),
    Global.Competency.root.PVs = c("PV#GLCM")
  ),
  "PISA for Development" = list(
    Reading = list(
      "2019" = c(188.33, 262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32)
    ),
    Science = list(
      "2019" = c(185.94, 260.54, 334.94, 409.54, 484.14, 558.73, 633.33, 707.93)
    ),
    Mathematics = list(
      "2019" = c(233.17, 295.47, 357.77, 420.07, 482.38, 544.68, 606.99, 669.30)
    ),
    Reading.root.PVs = "PV#READ",
    Science.root.PVs = "PV#SCIE",
    Mathematics.root.PVs = "PV#MATH"
  )
)
produce.jk.reps.data <- function(data, weight.var, jk.zones, jk.replicates, shortcut = FALSE) {
  data <- lapply(X = data, FUN = function(i) {
    unique.jk.zones <- unique(i[[jk.zones]])
    rep.names1 <- eval(paste0("REPWGT", unique.jk.zones))
    rep.names2 <- paste0("REPWGT", unique.jk.zones + length(unique.jk.zones))
    if(shortcut == FALSE) {
      i[ , eval(rep.names1) := lapply(unique.jk.zones, function(j) {
        i[[weight.var]] * ifelse(j == i[[jk.zones]], ifelse(i[[jk.replicates]] == 1, 2, 0), 1)})]
      i[ , eval(rep.names2) := lapply(unique.jk.zones, function(j) {
        i[[weight.var]] * ifelse(j == i[[jk.zones]], ifelse(i[[jk.replicates]] == 0, 2, 0), 1)})]
    } else if(shortcut == TRUE) {
      i[ , eval(rep.names1) := lapply(unique.jk.zones, function(j) {
        i[[weight.var]] * ifelse(j == i[[jk.zones]], ifelse(i[[jk.replicates]] == 1, 2, 0), 1)})]
    }
  })
  return(data)
}
weighted.variance.or.SD <- function (x, weights = weight.var, statistics = c("variance", "SD"), na.rm) {
  if(na.rm == TRUE) {
    complete.cases <- !is.na(x + weights)
    x <- x[complete.cases]
    weights <- weights[complete.cases]
  }
  wgt.sum <- sum(weights)
  mean.x <- sum(weights * x)/wgt.sum
  estimate <- sum(weights * ((x - mean.x)^2))/wgt.sum
  if(statistics == "variance") {
    estimate <- estimate
  } else if(statistics == "SD") {
    estimate <- sqrt(estimate)
  }
  return(estimate)
}
wgt.prctl <- function(variable, weight, prctls.values) {
  ranked.variable <- order(variable)
  sum.of.weights <- cumsum(x = weight[ranked.variable])
  all.person.prctls <- sum.of.weights / sum.of.weights[length(sum.of.weights)]
  sapply(X = prctls.values, FUN = function(i) {
    variable[ranked.variable[which.max(!is.na(all.person.prctls) & all.person.prctls >= i)]]
  })
}
wgt.MAD <- function(variable, weight, constant = 1.4826022185056) {
  tmp.median <- wgt.prctl(variable = variable, weight = weight, prctls.values = 0.5)
  person.diff <- abs(variable - tmp.median)
  wgt.prctl(variable = person.diff, weight = weight, prctls.values = 0.5) * constant
}
wgt.pct <- function(variable, weight, na.rm = TRUE) {
  if(na.rm == FALSE & is.factor(variable) & any(is.na(variable))) {
    variable <- addNA(variable)
  } else if(na.rm == FALSE & is.numeric(variable) & any(is.na(variable))) {
    variable <- addNA(as.factor(variable))
  }  else if(na.rm == FALSE & is.character(variable) & any(is.na(variable))) {
    variable <- addNA(as.factor(variable))
  }
  tmp.table <- wtd.table(x = variable, weights = weight, na.rm = na.rm, type = "table")
  tmp.names <- names(tmp.table)
  tmp.table <- as.numeric(tmp.table)
  names(tmp.table) <- tmp.names
  (tmp.table/sum(tmp.table))*100
}
compute.multiple.means.all.repwgt <- function(data.object, vars.vector, weight.var, keys, include.missing.arg) {
  if(all(is.na(data.object[ , mget(vars.vector)])) | include.missing.arg == TRUE) {
    tmp <- data.object[ , Map(f = weighted.mean,
                              x = .(get(vars.vector)),
                              w = mget(weight.var),
                              na.rm = TRUE),
                        by = keys]
  } else {
    tmp <- data.object[ , Map(f = weighted.mean,
                              x = .(get(vars.vector)),
                              w = mget(weight.var),
                              na.rm = TRUE),
                        by = keys]
  }
  if(include.missing.arg == FALSE) {
    tmp <- na.omit(object = tmp, cols = key(tmp))
  } else {
    tmp
  }
  if(include.missing.arg == FALSE && nrow(na.omit(data.object[ , .N, by = keys][N == 1, ])) > 0) {
    tmp.single.case <- na.omit(data.object[ , .N, by = keys][N == 1, ])
    tmp.single.case[ , N := NULL]
    V.columns <- grep(pattern = "^V[[:digit:]]+", x = colnames(tmp), value = TRUE)
    tmp.single.case <- merge(x = tmp.single.case, y = data.object[ , mget(c(keys, vars.vector))], all.y = FALSE)
    tmp.single.case[ , (V.columns) := mget(vars.vector)]
    tmp.single.case[ , eval(vars.vector) := NULL]
    tmp <- rbindlist(l = list(tmp, tmp.single.case))
  } else if(include.missing.arg == TRUE) {
    tmp
  }
  return(tmp)
}
compute.unweighted.mode <- function(x, na.rm = TRUE) {
  if(isTRUE(na.rm)) {
    tmp <- table(x, useNA = "no")
  } else {
    tmp <- table(x, useNA = "always")
  }
  tmp <- as.numeric(names(which.max(x = tmp)))
  if(length(tmp) > 1) {
    tmp <- tmp[1]
  }
  return(tmp)
}
compute.multiple.modes.all.repwgt <- function(data.object, vars.vector, weight.var, keys) {
  all.modes <- lapply(X = vars.vector, FUN = function(i) {
    modes.tmp <- lapply(X = weight.var, FUN = function(j) {
      tmp <- na.omit(data.object[ , .(Values = .N), by = mget(c(keys, i))])
      tmp <- tmp[tmp[ , .I[Values == max(Values, na.rm = TRUE)], by = keys]$V1]
      setkeyv(x = tmp, cols = c(keys, i))
      if(any(tmp[ , .N, by = keys][ , N] > 1)) {
        tmp <- tmp[tmp[ , .I[1], by = keys]$V1]
        assign(x = "warnings.collector.multimodal", value = unique(as.character(tmp[ , get(keys[1])])), envir = parent.frame(n = 2))
      }
      tmp[ , Values := NULL]
      setkeyv(x = tmp, cols = keys)
    })
    if(exists("warnings.collector.multimodal")) {
      assign(x = "warnings.collector.multimodal", value = warnings.collector.multimodal, envir = parent.frame(n = 2))
    }
    modes.tmp <- Reduce(function(X, Y) X[Y], modes.tmp)
    setnames(x = modes.tmp, c(keys, paste0("V", 1:length(weight.var))))
  })
  if(exists("warnings.collector.multimodal")) {
    assign(x = paste0("warnings.collector.multimodal.", warnings.collector.multimodal), value = warnings.collector.multimodal, envir = parent.frame(n = 5))
  }
  return(all.modes)
}
compute.dispersion.all.repwgt <- function(data.object, vars.vector, dispersion.type, weight.var, keys, include.missing.arg) {
  if(all(is.na(data.object[ , mget(vars.vector)])) | include.missing.arg == TRUE) {
    tmp <- data.object[ , Map(f = weighted.variance.or.SD,
                              x = .(get(vars.vector)),
                              weights = mget(weight.var),
                              statistics = dispersion.type,
                              na.rm = TRUE),
                        by = keys]
  } else {
    tmp <- data.object[ , Map(f = weighted.variance.or.SD,
                              x = .(get(vars.vector)),
                              weights = mget(weight.var),
                              statistics = dispersion.type,
                              na.rm = TRUE),
                        by = keys]
  }
  if(include.missing.arg == FALSE) {
    tmp <- na.omit(object = tmp, cols = key(tmp))
  } else {
    tmp
  }
  if(include.missing.arg == FALSE && nrow(na.omit(data.object[ , .N, by = keys][N == 1, ])) > 0) {
    tmp.single.case <- na.omit(data.object[ , .N, by = keys][N == 1, ])
    tmp.single.case[ , N := NULL]
    V.columns <- grep(pattern = "^V[[:digit:]]+", x = colnames(tmp), value = TRUE)
    tmp.single.case.zeroes <- setnames(setDT(lapply(rep("numeric", times = length(V.columns)), function(x) eval(call(x)))), V.columns)
    tmp.single.case.zeroes <- rbindlist(l = list(tmp.single.case.zeroes, as.list(rep(0, times = length(V.columns)))))
    tmp.single.case <- cbind(tmp.single.case, tmp.single.case.zeroes)
    tmp <- rbindlist(l = list(tmp, tmp.single.case))
  } else if(include.missing.arg == TRUE) {
    tmp
  }
  return(tmp)
}
compute.correlations.all.repwgt <- function (data.object, vars.vector, weight.var, keys, method) {
  data.object[ , (vars.vector) := lapply(.SD, function(j) {
    if(all(is.na(j))) {
      j <- 0
    } else {
      j
    }
  }), .SDcols = vars.vector]
  all.correlations <- lapply(X = weight.var, FUN = function(i) {
    key.cols.table <- unique(data.object[ , keys, with = FALSE])
    key.cols.table <- split(x = key.cols.table, by = keys, drop = TRUE)
    if(method == "Spearman") {
      data.object[ , (vars.vector) := lapply(.SD, function(j) {
        rank(x = j, ties.method = "average")
      }), .SDcols = vars.vector]
    }
    correlations <- data.object[ , .(.(cov.wt(do.call(cbind, mget(vars.vector)), wt = get(i), cor = TRUE)["cor"])), by = keys]
    correlations <- lapply(X = correlations[["V1"]], FUN = function(j) {
      j <- j[[1]]
      diag(x = j) <- 1
      return(j)
    })
    names.combinations <- lapply(X = correlations, FUN = function(j) {
      rbindlist(lapply(X = colnames(j), FUN = function(k) {
        unique(data.table(V1 = factor(k), V2 = factor(rownames(j))))
      }))
    })
    correlations <- Map(f = function(input1, input2, input3) {
      cbind(input1, input2, as.vector(input3))
    }, input1 = key.cols.table, input2 = names.combinations, input3 = correlations)
    correlations <- rbindlist(correlations)
    setkeyv(x = correlations, cols = c(keys, "V1", "V2"))
  })
  all.correlations <- Reduce(function(X,Y) X[Y], all.correlations)
  setnames(x = all.correlations, old = grep(pattern = paste(c(keys, "V1", "V2"), collapse = "|"), x = colnames(all.correlations), value = TRUE, invert = TRUE), new = weight.var)
}
compute.linear.regression.all.repwgt <- function(data.object, vars.vector, weight.var, keys, reg.formula) {
  all.regressions <- lapply(X = weight.var, FUN = function(i) {
    key.cols.table <- unique(data.object[ , keys, with = FALSE])
    key.cols.table <- split(x = key.cols.table, by = keys, drop = TRUE)
    regression <- data.object[ , .(.(eval(bquote(lm(formula = reg.formula, data = do.call(data.table, mget(c(vars.vector, i))), weights = .(as.name(i))))))), by = keys]
    regression <- regression[ , V1]
    regression <- lapply(X = regression, FUN = function(j) {
      reg.coefficients <- data.table(names(j[["coefficients"]]), j[["coefficients"]])
      model.stats <- summary(j)[c("r.squared", "adj.r.squared")]
      model.stats[["r.squared"]] <- data.table(V1 = "r.squared", V2 = model.stats[["r.squared"]])
      model.stats[["adj.r.squared"]] <- data.table(V1 = "adj.r.squared", V2 = model.stats[["adj.r.squared"]])
      model.stats <- rbindlist(model.stats, fill = TRUE)
      return(rbindlist(l = list(reg.coefficients, model.stats), fill = TRUE))
    })
    regression <- rbindlist(l = Map(f = cbind, key.cols.table, regression))
    new.var.names <- grep(pattern = paste(vars.vector, collapse = "|"), x = regression[ , V1], value = TRUE)
    regression[ , V1 := factor(x = V1, levels = c("(Intercept)", unique(new.var.names), "r.squared", "adj.r.squared"))]
    setkeyv(x = regression, cols = c(keys, "V1"))
    setnames(x = regression, old = "V2", new = i)
  })
  all.regressions <- Reduce(function(...) merge(..., all = TRUE), all.regressions)
}
compute.Wald.test.all.repwgt <- function(Wald.object, vars.vector, weight.var, reps.names, keys, study.name) {
  if(study.name %in% c("TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "PIRLS", "ePIRLS", "prePIRLS", "TiPi", "RLII")) {
    resamp.coef <- 0.5
  } else if(study.name %in% c("CivED", "ICCS", "ICILS", "SITES", "REDS")) {
    resamp.coef <- 1
  } else if(study.name %in% c("PISA", "PISA for Development", "TALIS", "TALIS 3S", "TEDS-M")) {
    resamp.coef.corrected <- (1/(length(reps.names)/2))*2
    resamp.coef <- 1
  }
  Wald.estimate <- lapply(X = Wald.object, FUN = function(i) {
    interactions.and.contrasts <- unique(grep(pattern = ":|_DY[[:digit:]]+|_DN[[:digit:]]+|_SC[[:digit:]]+", x = i[ , Variable], value = TRUE))
    vars.vector <- c(vars.vector, interactions.and.contrasts)
    cleaned.object <- droplevels(i[Variable %in% c("(Intercept)", vars.vector), ])
    cleaned.object <- split(x = cleaned.object, by = keys)
    estimates <- lapply(X = cleaned.object, FUN = function(j) {
      coef.var.covar.matrix <- copy(j)
      coef.var.covar.matrix[ , (reps.names) := lapply(.SD, function(k) {
        k - get(weight.var)
      }), .SDcols = reps.names]
      coef.var.covar.matrix[ , c(keys, "Variable", weight.var) := NULL]
      coef.var.covar.matrix <- lapply(X = coef.var.covar.matrix, FUN = function(k) {
        k %o% k
      })
      if(study.name %in% c("TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "PIRLS", "ePIRLS", "prePIRLS", "TiPi", "RLII", "CivED", "ICCS", "ICILS", "SITES", "REDS")) {
        coef.var.covar.matrix <- as.data.table(Reduce("+", coef.var.covar.matrix) * resamp.coef)
      } else if(study.name %in% c("PISA", "PISA for Development", "TALIS", "TALIS 3S", "TEDS-M")) {
        coef.var.covar.matrix <- as.data.table(Reduce("+", coef.var.covar.matrix) * resamp.coef.corrected)
      }
      model.terms <- rbindlist(sapply(X = 1:(length(vars.vector) - 1), FUN = function(j) {
        setDT(as.list(rep(x = 0, times = length(vars.vector))))
      }, simplify = FALSE))
      setnames(x = model.terms, vars.vector)
      diag(model.terms[ , 2:ncol(model.terms)]) <- 1
      model.coefficients <- copy(j[ , mget(c("Variable", weight.var))])
      Wald.matrix <- as.matrix(model.terms) %*% model.coefficients[ , get(weight.var)]
      Wald.matrix.inversed <- qr.solve(as.matrix(model.terms) %*% as.matrix(coef.var.covar.matrix) %*% as.matrix(transpose(model.terms)))
      chi.square.test <- as.vector(t(Wald.matrix) %*% Wald.matrix.inversed %*% Wald.matrix)
      chi.square.test.p.value <- as.vector(pchisq(q = chi.square.test, df = length(vars.vector) - 1, lower.tail = FALSE))
      DF1 <- length(vars.vector) - 1
      DF2 <- (resamp.coef * length(reps.names)) - DF1
      SDF <- (resamp.coef * length(reps.names)) - 1
      Wald.F <- as.vector((DF2 * chi.square.test) / (DF1 * SDF))
      Wald.F.p.value <- as.vector(pf(q = Wald.F, df1 = DF1, df2 = DF2, lower.tail = FALSE))
      Wald.F <- data.table(head(x = j[ , mget(keys)], n = 1L), Statistic = c("Chi-Square", "Chi-Square DF", "Wald F-Statistic", "Wald F-Statistic DF1", "Wald F-Statistic DF2"), Estimate = c(chi.square.test, DF1, Wald.F, DF1, DF2), Estimate_SE = rep(NA_real_, times = 5), p_value = c(chi.square.test.p.value, NA_real_, Wald.F.p.value, NA_real_, NA_real_))
    })
    estimates <- rbindlist(l = estimates)
  })
}
compute.Wald.test.all.repwgt.PV <- function(Wald.object, vars.vector, weight.var, reps.names, keys, study.name) {
  vars.vector <- gsub(pattern = "[[:digit:]]+", replacement = "N", x = vars.vector, fixed = TRUE)
  if(study.name %in% c("TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "PIRLS", "ePIRLS", "prePIRLS", "TiPi", "RLII")) {
    resamp.coef <- 0.5
  } else if(study.name %in% c("CivED", "ICCS", "ICILS", "SITES", "REDS")) {
    resamp.coef <- 1
  } else if(study.name %in% c("PISA", "PISA for Development", "TALIS", "TALIS 3S", "TEDS-M")) {
    resamp.coef.corrected <- (1/(length(reps.names)/2))*2
    resamp.coef <- 1
  }
  interactions.and.contrasts <- unique(grep(pattern = ":|_DY[[:digit:]]+|_DN[[:digit:]]+|_SC[[:digit:]]+", x = Wald.object[[1]][[1]][ , Variable], value = TRUE))
  vars.vector <- c(vars.vector, interactions.and.contrasts)
  Wald.object <- lapply(X = Wald.object, FUN = function(i) {
    i <- lapply(X = i, FUN = function(j) {
      j <- j[Variable %in% c("(Intercept)", vars.vector), ]
      j <- droplevels(j[Variable %in% c("(Intercept)", vars.vector), ])
      setnames(x = j, c(keys, "Variable", weight.var, reps.names))
      split(x = j, by = keys)
    })
    names.i <- unique(unlist(lapply(X = i, FUN = names)))
    i <- sapply(X = names.i, FUN = function(j) {
      lapply(X = i, FUN = function(k) {
        k[[j]]
      })
    }, simplify = FALSE)
  })
  Wald.estimates <- lapply(X = Wald.object, FUN = function(i) {
    coef.sampling.var.matrix <- lapply(X = i, FUN = function(j) {
      tmp <- copy(j)
      tmp <- lapply(X = tmp, FUN = function(k) {
        k[ , (reps.names) := lapply(.SD, function(l) {
          l - get(weight.var)
        }), .SDcols = reps.names]
        k[ , c(keys, "Variable", weight.var) := NULL]
        k <- lapply(X = k, FUN = function(l) {
          l %o% l
        })
        if(study.name %in% c("TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "PIRLS", "ePIRLS", "prePIRLS", "TiPi", "RLII", "CivED", "ICCS", "ICILS", "SITES", "REDS")) {
          k <- Reduce("+", k) * resamp.coef
        } else if(study.name %in% c("PISA", "PISA for Development", "TALIS", "TALIS 3S", "TEDS-M")) {
          k <- Reduce("+", k) * resamp.coef.corrected
        }
      })
    })
    coef.sampling.var.matrix <- lapply(X = coef.sampling.var.matrix, FUN = function(j) {
      (1 / length(j)) * Reduce("+", j)
    })
    coef.full.wgt.matrix <- lapply(X = i, FUN = function(j) {
      rbindlist(l = lapply(X = j, FUN = function(k) {
        k <- copy(k)
        k[ , (reps.names) := NULL]
      }))
    })
    coef.full.wgt.matrix <- lapply(X = coef.full.wgt.matrix, FUN = function(j) {
      split(x = j, by = c(keys, "Variable"), keep.by = FALSE, drop = TRUE)
    })
    coef.full.wgt.matrix <- lapply(X = coef.full.wgt.matrix, FUN = function(j) {
      as.matrix(setDT(unlist(j, recursive = FALSE), check.names = TRUE)[])
    })
    N.PVs <- unique(sapply(X = coef.full.wgt.matrix, FUN = nrow))
    coef.full.wgt.matrix.averaged <- copy(coef.full.wgt.matrix)
    coef.full.wgt.matrix.averaged <- lapply(X = coef.full.wgt.matrix.averaged, FUN = function(j) {
      colMeans(j)
    })
    coef.full.wgt.matrix.averaged <- lapply(X = coef.full.wgt.matrix.averaged, FUN = function(j) {
      names(j) <- vars.vector
      return(j)
    })
    compute.differences <- function(coef.matrix, averaged.coef.matrix) {
      tmp <- apply(X = coef.matrix, MARGIN = 1, FUN = function(i) {
        i - averaged.coef.matrix
      })
      return(t(tmp))
    }
    coef.full.wgt.matrix.differences <- Map(f = compute.differences, coef.matrix = coef.full.wgt.matrix, averaged.coef.matrix = coef.full.wgt.matrix.averaged)
    coef.imputation.var.matrix <- lapply(X = coef.full.wgt.matrix.differences, FUN = function(j) {
      (1 / (N.PVs - 1)) * Reduce(f = "+", apply(X = j, MARGIN = 1, FUN = function(k) {k %o% k}, simplify = FALSE))
    })
    compute.var.covar.matrix <- function(samp.var.mat, imp.var.mat) {
      samp.var.mat + ((N.PVs + 1) / N.PVs) * imp.var.mat
    }
    coef.var.covar.matrix <- Map(f = compute.var.covar.matrix, samp.var.mat = coef.sampling.var.matrix, imp.var.mat = coef.imputation.var.matrix)
    model.terms <- rbindlist(sapply(X = 1:(length(vars.vector) - 1), FUN = function(j) {
      setDT(as.list(rep(x = 0, times = length(vars.vector))))
    }, simplify = FALSE))
    setnames(x = model.terms, vars.vector)
    diag(model.terms[ , 2:ncol(model.terms)]) <- 1
    Wald.matrix <- lapply(X = coef.full.wgt.matrix.averaged, FUN = function(j) {
      as.matrix(model.terms) %*% j
    })
    Wald.matrix.inversed <- lapply(X = coef.var.covar.matrix, FUN = function(j) {
      qr.solve(as.matrix(model.terms) %*% as.matrix(j) %*% as.matrix(transpose(model.terms)))
    })
    compute.chi.square <- function(w.mat, w.mat.inv) {
      as.vector(t(w.mat) %*% w.mat.inv %*% w.mat)
    }
    chi.square <- Map(f = compute.chi.square, w.mat = Wald.matrix, w.mat.inv = Wald.matrix.inversed)
    chi.square.p.value <- lapply(X = chi.square, FUN = function(j) {
      as.vector(pchisq(q = j, df = length(vars.vector) - 1, lower.tail = FALSE))
    })
    DF1 <- length(vars.vector) - 1
    DF2 <- (resamp.coef * length(reps.names)) - DF1
    SDF <- (resamp.coef * length(reps.names)) - 1
    Wald.F <- lapply(X = chi.square, FUN = function(j) {
      as.vector((DF2 * j) / (DF1 * SDF))
    })
    Wald.F.p.value <- lapply(X = Wald.F, FUN = function(j) {
      as.vector(pf(q = j, df1 = DF1, df2 = DF2, lower.tail = FALSE))
    })
    chi.square <- lapply(X = chi.square, FUN = function(j) {
      data.table(Statistic = "Chi-Square", Estimate = j)
    })
    chi.square.p.value <- lapply(X = chi.square.p.value, FUN = function(j) {
      data.table(p_value = j)
    })
    chi.square.DF <- list(data.table(Statistic = "Chi-Square DF", Estimate = DF1))
    Wald.F <- lapply(X = Wald.F, FUN = function(j) {
      data.table(Statistic = "Wald F-Statistic", Estimate = j)
    })
    Wald.F.p.value <- lapply(X = Wald.F.p.value, FUN = function(j) {
      data.table(p_value = j)
    })
    Wald.F.DF1 <- list(data.table(Statistic = "Wald F-Statistic DF1", Estimate = DF1))
    Wald.F.DF2 <- list(data.table(Statistic = "Wald F-Statistic DF2", Estimate = DF2))
    splits.table <- lapply(X = i, FUN = function(j) {
      rbindlist(unique(lapply(X = j, FUN = function(k) {
        unique(k[ , mget(keys)])
      })))
    })
    chi.square <- Map(f = `cbind`, chi.square, chi.square.p.value)
    Wald.F <- Map(f = `cbind`, Wald.F, Wald.F.p.value)
    chi.square <- rbindlist(l = Map(f = `cbind`, splits.table, chi.square))
    chi.square.DF <- rbindlist(l = Map(f = `cbind`, splits.table, chi.square.DF))
    Wald.F <- rbindlist(l = Map(f = `cbind`, splits.table, Wald.F))
    Wald.F.DF1 <- rbindlist(l = Map(f = `cbind`, splits.table, Wald.F.DF1))
    Wald.F.DF2 <- rbindlist(l = Map(f = `cbind`, splits.table, Wald.F.DF2))
    Wald.F <- rbindlist(l = list(chi.square, chi.square.DF, Wald.F, Wald.F.DF1, Wald.F.DF2), fill = TRUE)
    setkeyv(x = Wald.F, cols = keys)
  })
}
compute.logistic.regression.all.repwgt <- function(data.object, vars.vector, weight.var, keys, reg.formula) {
  all.regressions <- lapply(X = weight.var, FUN = function(i) {
    key.cols.table <- unique(data.object[ , keys, with = FALSE])
    key.cols.table <- split(x = key.cols.table, by = keys, drop = TRUE)
    regression <- suppressWarnings(data.object[ , .(.(eval(bquote(glm(formula = reg.formula, data = do.call(data.table, mget(c(vars.vector, i))), weights = .(as.name(i)), family = "binomial"))))), by = keys])
    regression <- regression[ , V1]
    regression <- lapply(X = regression, FUN = function(j) {
      reg.coefficients <- data.table(names(j[["coefficients"]]), j[["coefficients"]])
      odds.ratios <- data.table(V1 = reg.coefficients[ , V1], V2 = exp(reg.coefficients[ , V2]))
      odds.ratios[ , V1 := paste0(V1, "_odds")]
      reg.coefficients <- rbindlist(l = list(reg.coefficients, odds.ratios))
      odds.ratios <- NULL
      model.stats <- summary(j)[c("null.deviance", "deviance", "df.null", "df.residual", "aic")]
      model.stats[["null.deviance"]] <- data.table(V1 = "null.deviance", V2 = model.stats[["null.deviance"]])
      model.stats[["deviance"]] <- data.table(V1 = "deviance", V2 = model.stats[["deviance"]])
      model.stats[["df.null"]] <- data.table(V1 = "df.null", V2 = model.stats[["df.null"]])
      model.stats[["df.residual"]] <- data.table(V1 = "df.residual", V2 = model.stats[["df.residual"]])
      model.stats[["aic"]] <- data.table(V1 = "aic", V2 = model.stats[["aic"]])
      model.stats[["n.cases"]] <- data.table(V1 = "n.cases", V2 = j[["df.null"]] + 1)
      model.stats[["df.difference"]] <- data.table(V1 = "df.difference", V2 = model.stats[["df.null"]][ , V2] - model.stats[["df.residual"]][ , V2] + 1)
      model.stats[["log.likelihood"]] <- data.table(V1 = "log.likelihood", V2 = model.stats[["df.difference"]][ , V2] - model.stats[["aic"]][ , V2]/2)
      model.stats[["bic"]] <- data.table(V1 = "bic", V2 = -2 * model.stats[["log.likelihood"]][ , V2] + model.stats[["df.difference"]][ , V2] * log(model.stats[["n.cases"]][ , V2]))
      model.stats[["chi.square"]] <- data.table(V1 = "chi.square", V2 = model.stats[["null.deviance"]][ , V2] - model.stats[["deviance"]][ , V2])
      model.stats[["r2hl"]] <- data.table(V1 = "r2hl", V2 = model.stats[["chi.square"]][ , V2]/model.stats[["null.deviance"]][ , V2])
      model.stats[["r2cs"]] <- data.table(V1 = "r2cs", V2 = 1 - exp((model.stats[["deviance"]][ , V2] - model.stats[["null.deviance"]][ , V2])/model.stats[["n.cases"]][ , V2]))
      model.stats[["r2n"]] <- data.table(V1 = "r2n", V2 = model.stats[["r2cs"]][ , V2]/(1 - exp(-(model.stats[["null.deviance"]][ , V2]/model.stats[["n.cases"]][ , V2]))))
      model.stats[["n.cases"]] <- NULL
      model.stats[["df.difference"]] <- NULL
      model.stats[["log.likelihood"]] <- NULL
      model.stats <- rbindlist(model.stats, fill = TRUE)
      return(rbindlist(l = list(reg.coefficients, model.stats), fill = TRUE))
    })
    regression <- rbindlist(l = Map(f = cbind, key.cols.table, regression))
    new.var.names <- grep(pattern = paste(vars.vector, collapse = "|"), x = regression[ , V1], value = TRUE)
    regression[ , V1 := factor(x = V1, levels = c("(Intercept)", "(Intercept)_odds", unique(new.var.names), "null.deviance", "deviance", "df.null", "df.residual", "aic", "bic", "chi.square", "r2hl", "r2cs", "r2n"))]
    setkeyv(x = regression, cols = c(keys, "V1"))
    setnames(x = regression, old = "V2", new = i)
  })
  all.regressions <- Reduce(function(...) merge(..., all = TRUE), all.regressions)
}
compute.crosstabs.all.repwgt <- function(data.object, var1, var2, keys, weight, exp.cnts, pcts.in.rows, pcts.in.cols, pcts.total) {
  crosstabs.formula <- paste0(paste(c(keys, var1), collapse = " + "), " ~ ", var2)
  all.crosstabs <- lapply(X = weight, FUN = function(i) {
    na.omit(data.object[ , .(N = sum(get(i))), keyby = c(keys, var1, var2)])
  })
  all.crosstabs <- lapply(X = all.crosstabs, FUN = function(i) {
    split(x = i, by = keys, drop = TRUE)
  })
  all.crosstabs <- lapply(X = all.crosstabs, FUN = function(i) {
    lapply(X = i, FUN = function(j) {
      dcast(data = j, formula = as.formula(crosstabs.formula), value.var = "N")
    })
  })
  all.crosstabs <- lapply(X = all.crosstabs, FUN = function(i) {
    rbindlist(l = i, fill = TRUE)
  })
  lapply(X = all.crosstabs, FUN = function(i) {
    setkeyv(x = i, cols = c(keys, var1))
  })
  all.crosstabs <- lapply(X = all.crosstabs, FUN = function(k) {
    rollup(x = k, j = lapply(.SD, sum), by = c(keys, var1))
  })
  all.crosstabs <- lapply(X = all.crosstabs, FUN = function(i) {
    i[i[ , Reduce(`|`, lapply(.SD, Negate(`is.na`))), .SDcols = keys], ]
  })
  lapply(X = all.crosstabs, FUN = function(i) {
    i[is.na(get(var1)), (var1) := "Total"]
  })
  if(length(keys) > 1) {
    lapply(X = all.crosstabs, FUN = function(i) {
      i[ , (keys[2:length(keys)]) := lapply(.SD, function(j) {
        levels.j <- levels(droplevels(j))
        j <- ifelse(test = is.na(j), yes = "Total", no = j)
        j <- factor(x = j, labels = c(levels.j, "Total"))
      }), .SDcols = grep(pattern = paste(keys[2:length(keys)], collapse = "|"), x = colnames(i), value = TRUE)]
    })
  }
  lapply(X = all.crosstabs, FUN = function(i) {
    i[ , Total := rowSums(.SD, na.rm = TRUE), .SDcols = grep(pattern = paste(c(keys, var1), collapse = "|"), x = colnames(i), invert = TRUE, value = TRUE)]
    i[ , Type := factor("Observed count")]
  })
  var1.groups <- lapply(X = all.crosstabs, FUN = function(i) {
    unique(i[get(var1) != "Total", mget(var1)])
  })
  if(exp.cnts == TRUE) {
    expected.counts <- lapply(X = all.crosstabs, FUN = function(i) {
      i[ , lapply(.SD, function(j) {
        (j[length(j)] * Total[1:length(j) - 1]) / Total[length(Total)]
      }), .SDcols = grep(pattern = paste(c(keys, var1, "Total", "Type"), collapse = "|"), x = colnames(i), invert = TRUE, value = TRUE), by = keys]
    })
    expected.counts <- Map(f = cbind, var1.groups, expected.counts)
    lapply(X = expected.counts, FUN = function(i) {
      setcolorder(x = i, neworder = c(keys, var1, grep(pattern = paste(c(keys, var1), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE)))
      setkeyv(x = i, cols = c(keys, var1))
    })
    expected.counts <- lapply(X = expected.counts, FUN = function(i) {
      rollup(x = i, j = lapply(.SD, sum), by = c(keys, var1))
    })
    expected.counts <- lapply(X = expected.counts, FUN = function(i) {
      i[i[ , Reduce(`|`, lapply(.SD, Negate(`is.na`))), .SDcols = keys], ]
    })
    lapply(X = expected.counts, FUN = function(i) {
      i[ , Total := rowSums(.SD, na.rm = TRUE), .SDcols = grep(pattern = paste(c(keys, var1), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE)]
    })
    lapply(X = expected.counts, FUN = function(i) {
      i[is.na(get(var1)), (var1) := "Total"]
    })
    lapply(X = expected.counts, FUN = function(i) {
      i[ , Type := "Expected count"]
    })
    if(length(keys) > 1) {
      lapply(X = expected.counts, FUN = function(i) {
        i[ , (keys[2:length(keys)]) := lapply(.SD, function(i) {
          i <- droplevels(i)
          levels.i <- levels(i)
          i <- ifelse(test = is.na(i), yes = "Total", no = i)
          i <- factor(x = i, labels = c(levels.i, "Total"))
        }), .SDcols = grep(pattern = paste(keys[2:length(keys)], collapse = "|"), x = colnames(i), value = TRUE)]
      })
    }
    all.crosstabs <- Map(f = function(list1, list2) {
      rbindlist(l = list(list1, list2))
    }, list1 = all.crosstabs, list2 = expected.counts)
  }
  lapply(X = all.crosstabs, FUN = function(i) {
    setcolorder(x = i, neworder = c(keys, var1, "Type", grep(pattern = paste(c(keys, var1, "Type"), collapse = "|"), x = colnames(i), invert = TRUE, value = TRUE)))
  })
  if(pcts.in.rows == TRUE) {
    row.percentages <- lapply(X = all.crosstabs, FUN = function(i) {
      i[get(var1) != "Total" & Type == "Observed count", lapply(.SD, function(j) {
        (j/Total)*100
      }), .SDcols = grep(pattern = paste(c(keys, var1, "Type", "Total"), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE), key = c(keys, var1, "Type")]
    })
    lapply(X = row.percentages, FUN = function(i) {
      i[ , Total := rowSums(.SD), .SDcols = grep(pattern = paste(c(keys, var1, "Type"), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE)]
      i[ , Type := "Row percent"]
      i[ , Type := factor(Type)]
    })
    all.crosstabs <- Map(f = function(list1, list2) {
      rbindlist(l = list(list1, list2))
    }, list1 = all.crosstabs, list2 = row.percentages)
  }
  if(pcts.total == TRUE || pcts.in.cols == TRUE) {
    table.Total <- lapply(X = all.crosstabs, FUN = function(i) {
      i[get(var1) == "Total" & Type == "Observed count", mget(grep(pattern = paste(c(var1, "Type"), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE)), keyby = var1]
    })
    table.Total <- lapply(X = table.Total, FUN = function(i) {
      i[i[ , Reduce(`&`, lapply(.SD, `!=`, "Total")), .SDcols = keys], ]
    })
    lapply(X = table.Total, FUN = function(i) {
      i[ , (var1) := NULL]
    })
  }
  if(pcts.in.cols == TRUE) {
    column.percentages <- lapply(X = all.crosstabs, FUN = function(i) {
      i[get(var1) != "Total" & Type == "Observed count", ]
    })
    column.percentages.var1.type <- lapply(X = column.percentages, FUN = function(i) {
      i[ , mget(c(var1, "Type"))]
    })
    lapply(X = column.percentages, FUN = function(i) {
      i[ , c(var1, "Type") := NULL]
    })
    xcols <- grep(pattern = paste(keys, collapse = "|"), x = colnames(table.Total[[1]]), value = TRUE, invert = TRUE)
    icols <- paste0("i.", xcols)
    column.percentages <- lapply(X = 1:length(column.percentages), FUN = function(i) {
      column.percentages[[i]][table.Total[[i]], on = keys, Map(`/`, mget(xcols), mget(icols)), by = .EACHI]
    })
    lapply(X = column.percentages, FUN = function(i) {
      i[ , (grep(pattern = paste(c(keys, var1), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE)) := lapply(.SD, function(j) {
        j*100
      }), .SDcols = grep(pattern = paste(c(keys, var1), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE)]
    })
    column.percentages <- Map(f = cbind, column.percentages, column.percentages.var1.type)
    lapply(X = column.percentages, FUN = function(i) {
      setcolorder(x = i, neworder = c(keys, var1, "Type", grep(pattern = paste(c(keys, var1, "Type", "Total"), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE), "Total"))
      i[ , Type := "Column percent"]
      i[ , Type := factor(Type)]
    })
    all.crosstabs <- Map(f = function(list1, list2) {
      rbindlist(l = list(list1, list2))
    }, list1 = all.crosstabs, list2 = column.percentages)
  }
  if(pcts.total == TRUE || pcts.in.rows == TRUE || pcts.in.cols == TRUE) {
    grand.total.N <- lapply(X = all.crosstabs, FUN = function(i) {
      i[get(var1) == "Total" & Type == "Observed count", ]
    })
    lapply(X = grand.total.N, FUN = function(i) {
      i[ , (grep(pattern = paste(c(keys, "Total"), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE)) := NULL]
    })
    grand.total.N <- lapply(X = grand.total.N, FUN = function(i) {
      i[rowSums(i == "Total") == 0, ]
    })
    pct.of.total <- lapply(X = all.crosstabs, FUN = function(i) {
      i[get(var1) != "Total" & Type == "Observed count", mget(grep(pattern = paste(c(var1, "Type"), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE))]
    })
    if(pcts.in.cols == TRUE || pcts.total == TRUE) {
      xcols <- grep(pattern = paste(keys, collapse = "|"), x = colnames(table.Total[[1]]), value = TRUE, invert = TRUE)
      icols <- "i.Total"
      pct.of.total <- lapply(X = 1:length(pct.of.total), FUN = function(i) {
        pct.of.total[[i]][grand.total.N[[i]], on = keys, Map(`/`, mget(xcols), mget(icols)), by = .EACHI]
      })
      lapply(X = pct.of.total, FUN = function(i) {
        i[ , (grep(pattern = paste(c(keys, var1), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE)) := lapply(.SD, function(i) {
          i * 100
        }), .SDcols = grep(pattern = paste(c(keys, var1), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE)]
      })
    }
    pct.of.total.tot.row <- lapply(X = pct.of.total, FUN = function(i) {
      groupingsets(x = i, j = lapply(.SD, sum), .SDcols = grep(pattern = paste(c(keys, var1), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE), by = c(keys), sets = list(c(keys)))
    })
    pct.of.total.tot.row <- lapply(X = pct.of.total.tot.row, FUN = function(i) {
      i <- cbind(i[ , mget(keys)], data.table("Total"), data.table(Type = factor("Percent of total")), i[ , mget(grep(pattern = paste(keys, collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE))])
    })
    pct.of.total.tot.row <- lapply(X = pct.of.total.tot.row, FUN = function(i) {
      setnames(x = i, old = "V1", new = var1)
    })
    pct.of.total <- Map(f = cbind, pct.of.total, var1.groups, data.table(factor("Percent of total")))
    lapply(X = pct.of.total, FUN = function(i) {
      setnames(x = i, old = grep(pattern = "V[[:digit:]]+", x = colnames(i), value = TRUE), new = "Type")
      setcolorder(x = i, neworder = c(keys, var1, "Type", grep(pattern = paste(c(keys, var1, "Type", "Total"), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE), "Total"))
    })
    pct.of.total <- Map(f = function(list1, list2) {
      rbindlist(l = list(list1, list2), fill = TRUE)
    }, list1 = pct.of.total, list2 = pct.of.total.tot.row)
    if(pcts.in.rows == TRUE) {
      row.pct.of.total <- lapply(X = all.crosstabs, FUN = function(i) {
        i[get(var1) == "Total" & Type == "Observed count", ]
      })
      row.pct.of.total <- lapply(X = row.pct.of.total, FUN = function(i) {
        i[i[ , Reduce(`|`, lapply(.SD, `!=`, "Total")), .SDcols = keys], ]
      })
      row.pct.of.total <- lapply(X = row.pct.of.total, FUN = function(i) {
        i[get(var1) == "Total" & Type == "Observed count", lapply(.SD, function(j) {
          (j/Total) * 100
        }), .SDcols = grep(pattern = paste(c(keys, var1, "Type"), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE), by = keys]
      })
      row.pct.of.total <- Map(f = function(list1, list2, list3) {
        cbind(list1, list2, list3)
      }, list1 = row.pct.of.total, list2 = data.table("Total"), list3 = data.table("Row percent"))
      lapply(X = row.pct.of.total, FUN = function(i) {
        setnames(x = i, old = "list2", new = var1)
        setnames(x = i, old = "list3", new = "Type")
        setcolorder(x = i, neworder = c(keys, var1, "Type", grep(pattern = paste(c(keys, var1, "Type", "Total"), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE), "Total"))
      })
    }
    if(pcts.in.cols == TRUE) {
      column.pct.of.total <- lapply(X = column.percentages, FUN = function(i) {
        i[get(var1) != "Total", lapply(.SD, sum), .SDcols = grep(pattern = paste(c(keys, var1, "Type"), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE), by = c(keys, "Type")]
      })
      column.pct.of.total <- lapply(X = column.pct.of.total, FUN = function(i) {
        i <- cbind(i, data.table("Total"))
        setnames(x = i, old = grep(pattern = "^V[[:digit:]]+", x = colnames(i), value = TRUE), new = var1)
        setcolorder(x = i, neworder = c(keys, var1, "Type", grep(pattern = paste(c(keys, var1, "Type", "Total"), collapse = "|"), x = colnames(i), value = TRUE, invert = TRUE), "Total"))
      })
    }
    if(pcts.total == FALSE && pcts.in.rows == FALSE && pcts.in.cols == FALSE) {
      return(NULL)
    } else if(pcts.total == TRUE && pcts.in.rows == FALSE && pcts.in.cols == FALSE) {
      pct.of.total <- pct.of.total
    } else if(pcts.total == TRUE && pcts.in.rows == TRUE && pcts.in.cols == FALSE) {
      pct.of.total <- Map(f = function(list1, list2) {
        rbindlist(l = list(list1, list2))
      }, list1 = pct.of.total, list2 = row.pct.of.total)
    } else if(pcts.total == TRUE && pcts.in.rows == FALSE && pcts.in.cols == TRUE) {
      pct.of.total <- Map(f = function(list1, list2) {
        rbindlist(l = list(list1, list2))
      }, list1 = pct.of.total, list2 = column.pct.of.total)
    } else if(pcts.total == FALSE && pcts.in.rows == FALSE && pcts.in.cols == TRUE) {
      pct.of.total <- column.pct.of.total
    }  else if(pcts.total == FALSE && pcts.in.rows == TRUE && pcts.in.cols == FALSE) {
      pct.of.total <- row.pct.of.total
    } else if(pcts.total == TRUE && pcts.in.rows == TRUE && pcts.in.cols == TRUE) {
      pct.of.total <- Map(f = function(list1, list2) {
        rbindlist(l = list(list1, list2))
      }, list1 = pct.of.total, list2 = row.pct.of.total)
      pct.of.total <- Map(f = function(list1, list2) {
        rbindlist(l = list(list1, list2))
      }, list1 = pct.of.total, list2 = column.pct.of.total)
    } else if(pcts.total == FALSE && pcts.in.rows == TRUE && pcts.in.cols == TRUE) {
      pct.of.total <- Map(f = function(list1, list2) {
        rbindlist(l = list(list1, list2))
      }, list1 = row.pct.of.total, list2 = column.pct.of.total)
    }
    pct.of.total <- lapply(X = pct.of.total, FUN = function(i) {
      i[ , Type := factor(x = Type, levels = c("Row percent", "Column percent", "Percent of total"))]
      setkeyv(x = i, cols = c(keys, var1, "Type"))
    })
  }
  if(exists("pct.of.total")) {
    all.crosstabs <- Map(f = function(list1, list2) {
      rbindlist(l = list(list1, list2))
    }, list1 = all.crosstabs, list2 = pct.of.total)
  }
  lapply(X = all.crosstabs, FUN = function(i) {
    setkeyv(x = i, cols = c(keys, var1))
  })
  all.crosstabs <- lapply(X = all.crosstabs, FUN = function(i) {
    melt(data = i, id.vars = c(keys, var1, "Type"), variable.name = var2)
  })
  Map(f = function(list.of.estimates, weight.names) {
    setnames(x = list.of.estimates, old = "value", new = weight.names)
  }, list.of.estimates = all.crosstabs, weight.names = as.list(weight))
  all.crosstabs <- Reduce(function(...) merge(..., by = c(keys, var1, "Type", var2)), all.crosstabs)
  return(all.crosstabs)
}
compute.Rao.Scott.adj.chi.sq <- function(data.obj, var1, var2, weights, des.scale.fac, deg.freedom, miss.to.include, keys) {
  data.obj[ , (colnames(data.obj)) := lapply(.SD, function(i) {
    if(is.factor(i)) {
      i <- droplevels(i)
    } else {
      i
    }
  })]
  if(miss.to.include == FALSE) {
    data.obj <- na.omit(object = data.obj, cols = keys)
  }
  cnt.name <- as.character(unique(data.obj[ , get(keys[1])]))
  data.obj <- split(x = data.obj, by = keys)
  Rao.Scott.adjustment <- lapply(X = data.obj, FUN = function(i) {
    interaction.formula <- formula(x = paste0("~interaction(factor(", var1, "), factor(", var2, ")) - 1"))
    Rao.Scott.design.matrix <- model.matrix(object = interaction.formula, model.frame(formula = interaction.formula, na.omit(i)))
    rows.and.cols.seq.values <- expand.grid(var1 = 1:length(unique(i[ , get(var1)])), var2 = 1:length(unique(i[ , get(var2)])))
    indic.var.rows <- model.matrix(~factor(var1) + factor(var2), rows.and.cols.seq.values)
    indic.var.cols <- model.matrix(~factor(var1) * factor(var2), rows.and.cols.seq.values)
    covar.matrix <- qr.resid(qr(indic.var.rows), indic.var.cols[ , -(1:(length(unique(i[ , get(var1)])) + length(unique(i[ , get(var2)])) - 1))])
    replicated.averages <- rbindlist(l = lapply(X = i[ , mget(weights[2:length(weights)])], FUN = function(j) {
      setDT(as.list(colSums(j * Rao.Scott.design.matrix) / sum(j)))
    }))
    rescaled.wgt <- 1/i[ , get(weights[1])]
    wgt.sums <- sum(rescaled.wgt)
    wgt.average <- colSums(Rao.Scott.design.matrix * rescaled.wgt / wgt.sums)
    mean.replicate.averages <- unlist(lapply(X = replicated.averages, FUN = function(j) {
      na.omit(mean(j)[wgt.average > 0])
    }))
    averages.variances <- unname(obj = crossprod(x = sweep(x = as.matrix(replicated.averages), MARGIN = 2, mean.replicate.averages, FUN = "-"))*des.scale.fac)
    diag.prop.averages.matrix <- diag(ifelse(test = wgt.average == 0, yes = 0, no = 1/mean.replicate.averages))
    deff.var.d <- t(covar.matrix) %*% (diag.prop.averages.matrix/nrow(Rao.Scott.design.matrix)) %*% covar.matrix
    deff.cov.n <- t(covar.matrix) %*% diag.prop.averages.matrix %*% averages.variances %*% diag.prop.averages.matrix %*% covar.matrix
    Rao.Scott.deff <- solve(a = deff.var.d, b = deff.cov.n, tol = 1e-17)
    pearson.chi.sq <- suppressWarnings(xtabs(i[ , get(weights[1])] ~ i[ , get(var1)] + i[ , get(var2)]))
    pearson.chi.sq <- summary(pearson.chi.sq * (length(i[ , get(weights[1])])/sum(pearson.chi.sq)))
    pearson.stats.1st.order <- data.table(Chi_Square = rep("First-Order Rao-Scott", times = 3), Statistic = c("Chi-Square", "Degrees of Freedom", "p-value"), Value = rep(NA_real_, times = 3))
    pearson.stats.1st.order[Statistic == "Chi-Square", Value := pearson.chi.sq[["statistic"]]]
    pearson.stats.1st.order[Statistic == "Degrees of Freedom", Value := pearson.chi.sq[["parameter"]]]
    pearson.stats.1st.order[Statistic == "p-value", Value := pchisq(pearson.chi.sq[["statistic"]] / mean(diag(Rao.Scott.deff)), df=pearson.chi.sq[["parameter"]], lower.tail=FALSE)]
    second.ord.des.correction <- sum(diag(x = Rao.Scott.deff))^2 / (sum(diag(x = Rao.Scott.deff %*% Rao.Scott.deff)))
    pearson.stats.2nd.order <-data.table(Chi_Square = rep("Second-Order Rao-Scott", times = 4), Statistic = c("Chi-Square", "Sample Degrees of Freedom", "Design Degrees of Freedom", "p-value"), Value = rep(NA_real_, times = 4))
    pearson.stats.2nd.order[Statistic == "Chi-Square", Value := pearson.chi.sq[["statistic"]]/sum(diag(Rao.Scott.deff))]
    pearson.stats.2nd.order[Statistic == "Sample Degrees of Freedom", Value := second.ord.des.correction]
    pearson.stats.2nd.order[Statistic == "Design Degrees of Freedom", Value := second.ord.des.correction * deg.freedom]
    pearson.stats.2nd.order[Statistic == "p-value", Value := pf(q = pearson.chi.sq[["statistic"]]/sum(diag(Rao.Scott.deff)), df1 = second.ord.des.correction, df2 = second.ord.des.correction * deg.freedom, lower.tail=FALSE)]
    Rao.Scott.table.lead.cols <- unique(x = i[ , mget(keys)])
    setkeyv(x = Rao.Scott.table.lead.cols, cols = keys)
    pearson.stats <- rbindlist(l = list(pearson.stats.1st.order, pearson.stats.2nd.order))
    return(cbind(Rao.Scott.table.lead.cols, pearson.stats))
  })
  return(rbindlist(l = Rao.Scott.adjustment))
}
compute.cont.vars.pct.miss <- function(data.object, vars.vector, weight.var, keys) {
  cont.vars.pct.miss <- data.object[ , lapply(.SD, function(i) {
    if(!all(!is.na(i))) {
      wgt.pct(variable = is.na(i), weight = get(weight.var), na.rm = TRUE)[sort(unique(is.na(i)))]
    } else {
      return(0)
    }
  }), by = eval(keys), .SDcols = vars.vector]
  setnames(x = cont.vars.pct.miss, c(keys, paste0("Percent_Missing_", vars.vector)))
}
recode.missing.data.values <- function(data.obj, vars.to.recode, user.missing.to.NA) {
  data.obj[ , (vars.to.recode) := lapply(.SD, function(i) {
    var.label.i <- attr(x = i, which = "variable.label")
    missings.i <- attr(x = i, which = "missings")
    if(is.factor(i)) {
      levels.i <- levels(i)
      i <- as.character(i)
      if(!is.null(missings.i)) {
        i.valid <- grep(pattern = paste(missings.i, collapse = "|"), x = i, ignore.case = TRUE, value = TRUE, invert = TRUE)
      } else{
        i.valid <- na.omit(unique(i))
      }
      i.valid <- i.valid[!is.na(i.valid)]
      i[i %in% i.valid] <- "Valid values"
      if(isTRUE(user.missing.to.NA)) {
        i[i %in% missings.i] <- NA
        i <- factor(x = i, levels = "Valid values")
      } else {
        i <- factor(x = i, levels = c("Valid values", missings.i))
      }
    } else if(is.numeric(i)) {
      if(!is.null(missings.i)) {
        i[!i %in% missings.i & !is.na(i)] <- "Valid values"
        if(isTRUE(user.missing.to.NA)) {
          i[i %in% missings.i] <- NA
          i <- factor(x = i, levels = "Valid values")
        } else {
          i[i %in% missings.i] <- names(missings.i)[match(x = i, table = missings.i, nomatch = 0)]
          i <- factor(x = i, levels = c("Valid values", names(missings.i)))
        }
      } else {
        i[!is.na(i)] <- "Valid values"
      }
    }
    i <- addNA(x = i, ifany = TRUE)
    if(!is.null(missings.i)) {
      if(is.numeric(missings.i)) {
        missings.i <- names(missings.i)
      }
      if(isFALSE(user.missing.to.NA)) {
        setattr(x = i, name = "missings", value = missings.i)
      }
    }
    setattr(x = i, name = "variable.label", value = var.label.i)
  }), .SDcols = vars.to.recode]
}
compute.missing.data.patterns.unweighted <- function(data.obj, var.names, keys, keys.no.NA, pattern.cell.counts.miss = FALSE, pattern.cell.counts.valid = FALSE) {
  if(isTRUE(pattern.cell.counts.miss) & isTRUE(pattern.cell.counts.valid)) {
    stop('Both "pattern.cell.counts.miss" and "pattern.cell.counts.valid" are true, choose just one of them.', call. = FALSE)
  }
  data.missings <- copy(data.obj[ , mget(c(keys, var.names))])
  if(isFALSE(keys.no.NA)) {
    data.missings <- na.omit(object = data.missings, cols = keys)
    data.missings[ , (keys) := lapply(.SD, droplevels), .SDcols = keys]
  }
  cols.to.use <- colnames(data.missings)[!colnames(data.missings) %in% keys]
  setcolorder(x = data.missings, neworder = c(colnames(data.missings)[!colnames(data.missings) %in% cols.to.use], cols.to.use))
  data.missings[ , (keys) := lapply(.SD, function(i) {
    levels.i <- c(levels(i), "NA")
    i <- as.character(i)
    i <- ifelse(test = is.na(i), yes = "NA", no = i)
    i <- factor(x = i, levels = levels.i)
  }), .SDcols = keys]
  data.missings <- split(x = data.missings, by = keys, drop = TRUE, sorted = TRUE)
  table.missing.patterns <- lapply(X = data.missings, FUN = function(i) {
    i[ , (cols.to.use) := lapply(.SD, function(j) {
      ifelse(test = j == "Valid values", yes = 1, no = 0)
    }), .SDcols = cols.to.use]
    head.keys <- unique(i[ , mget(keys)])
    missing.matrix <- i[ , .N, by = eval(cols.to.use)]
    missing.matrix <- cbind(head.keys, missing.matrix)
    setnames(x = missing.matrix, old = "N", new = "Pattern_Counts")
    setcolorder(x = missing.matrix, neworder = c(keys, "Pattern_Counts", cols.to.use))
    missing.matrix[ , Missings_in_Pattern := rowSums(.SD == 0, na.rm = TRUE), .SDcols = cols.to.use]
    split.i.per.pattern <- split(x = i, by = c(keys, cols.to.use), drop = TRUE)
    missings.per.pattern <- rbindlist(l = lapply(X = split.i.per.pattern, FUN = function(j) {
      cbind(unique(j[ , mget(keys)]), data.table(Total_Missings_per_Pattern = sum(j == 0)))
    }))
    missing.matrix <- cbind(missing.matrix, data.table(Total_Missings_per_Pattern = missings.per.pattern[ , Total_Missings_per_Pattern]))
    bottom.row <- i[ , lapply(.SD, function(j) {sum(j == 0, na.rm = TRUE)}), by = keys]
    bottom.row <- cbind(Pattern_Counts = NA_integer_, bottom.row, data.table(Missings_in_Pattern = NA_integer_), data.table(Total_Missings_per_Pattern = rowSums(bottom.row[ , mget(colnames(bottom.row)[!colnames(bottom.row) %in% keys])], na.rm = TRUE)))
    bottom.row[ , (keys) := lapply(.SD, droplevels), .SDcols = keys]
    bottom.row[ , (keys[length(keys)]) := factor(x = get(keys[length(keys)]), labels = paste0(levels(get(keys[length(keys)])), " Total"))]
    setcolorder(x = bottom.row, neworder = c(keys, "Pattern_Counts", names(sort(unlist(bottom.row[ , mget(cols.to.use)]))), "Missings_in_Pattern", "Total_Missings_per_Pattern"))
    setcolorder(x = missing.matrix, neworder = c(keys, "Pattern_Counts", grep(pattern = paste(cols.to.use, collapse = "|"), x = colnames(bottom.row), value = TRUE), "Missings_in_Pattern", "Total_Missings_per_Pattern"))
    setkeyv(x = missing.matrix, cols = grep(pattern = paste(cols.to.use, collapse = "|"), x = colnames(missing.matrix), value = TRUE))
    setkey(x = missing.matrix, NULL)
    missing.matrix[ , seq_row := nrow(missing.matrix):1]
    setorderv(x = missing.matrix, cols = "seq_row")
    missing.matrix[ , seq_row := NULL]
    missing.matrix <- rbindlist(l = list(missing.matrix, bottom.row))
    if(isTRUE(pattern.cell.counts.miss)) {
      patterns.count.miss <- rbindlist(lapply(X = split.i.per.pattern, FUN = function(j) {
        j[ , lapply(.SD, function(k) {
          sum(k == 0)
        }), .SDcols = cols.to.use, keyby = c(keys, cols.to.use)]
      }))
      setnames(x = patterns.count.miss, c(keys, cols.to.use, paste0(cols.to.use, "_M_Counts")))
      colorder.pattern <- grep(pattern = paste0(paste(cols.to.use, collapse = "$|"), "$"), x = colnames(bottom.row), value = TRUE)
      colorder.miss.counts <- paste0(colorder.pattern, "_M_Counts")
      setcolorder(x = patterns.count.miss, neworder = c(keys, colorder.pattern, colorder.miss.counts))
      setorderv(x = patterns.count.miss, cols = grep(pattern = paste(cols.to.use, collapse = "|"), x = colnames(patterns.count.miss), value = TRUE), order = -1L)
    }
    if(isTRUE(pattern.cell.counts.valid)) {
      patterns.count.valid <- rbindlist(lapply(X = split.i.per.pattern, FUN = function(j) {
        j[ , lapply(.SD, function(k) {
          sum(k == 1)
        }), .SDcols = cols.to.use, keyby = c(keys, cols.to.use)]
      }))
      setnames(x = patterns.count.valid, c(keys, cols.to.use, paste0(cols.to.use, "_V_Counts")))
      colorder.pattern <- grep(pattern = paste0(paste(cols.to.use, collapse = "$|"), "$"), x = colnames(bottom.row), value = TRUE)
      colorder.valid.counts <- paste0(colorder.pattern, "_V_Counts")
      setcolorder(x = patterns.count.valid, neworder = c(keys, colorder.pattern, colorder.valid.counts))
      setorderv(x = patterns.count.valid, cols = grep(pattern = paste(cols.to.use, collapse = "|"), x = colnames(patterns.count.valid), value = TRUE), order = -1L)
    }
    if(isTRUE(pattern.cell.counts.miss)) {
      return(patterns.count.miss)
    } else if(isTRUE(pattern.cell.counts.valid)) {
      return(patterns.count.valid)
    } else if(isFALSE(pattern.cell.counts.miss) & isFALSE(pattern.cell.counts.valid)) {
      return(missing.matrix)
    }
  })
  return(table.missing.patterns)
}
compute.missing.data.patterns.all.repwgt <- function(data.object, var.names, keys, weight.var, keys.no.NA, pattern.cell.counts.miss = FALSE, pattern.cell.counts.valid = FALSE) {
  if(isTRUE(pattern.cell.counts.miss) & isTRUE(pattern.cell.counts.valid)) {
    stop('Both "pattern.cell.counts.miss" and "pattern.cell.counts.valid" are true, choose just one of them.', call. = FALSE)
  }
  data.missings <- copy(data.object[ , mget(c(keys, var.names, weight.var))])
  if(isFALSE(keys.no.NA)) {
    data.missings <- na.omit(object = data.missings, cols = keys)
    data.missings[ , (keys) := lapply(.SD, droplevels), .SDcols = keys]
  }
  data.missings[ , (keys) := lapply(.SD, function(i) {
    i <- droplevels(i)
    levels.i <- c(levels(i), "NA")
    i <- as.character(i)
    i <- ifelse(test = is.na(i), yes = "NA", no = i)
    i <- factor(x = i, levels = levels.i)
  }), .SDcols = keys]
  data.missings[ , (var.names) := lapply(.SD, function(i) {
    i <- as.numeric(i)
    ifelse(test = i == 1, yes = 1, no = 0)
  }), .SDcols = var.names]
  missing.patterns.N <- data.missings[ , lapply(.SD, sum), .SDcols = weight.var, by = c(keys, var.names)]
  missing.patterns.N[ , Missings_in_Pattern := unlist(apply(.SD, MARGIN = 1, function(i) {sum(i == 0)}, simplify = FALSE)), .SDcols = var.names]
  setcolorder(x = missing.patterns.N, neworder = c(keys, var.names, "Missings_in_Pattern", weight.var))
  data.missings[ , (var.names) := lapply(.SD, function(i) {
    ifelse(test = i == 1, yes = NA, no = 1)
  }), .SDcols = var.names]
  data.missings.per.var <- lapply(X = var.names, FUN = function(i) {
    data.missings[ , mget(c(keys, i, weight.var))]
  })
  total.rows <- lapply(X = data.missings.per.var, FUN = function(i) {
    head.cols <- unique(i[ , mget(keys)])
    i <- na.omit(i)
    var.col <- colnames(i)[colnames(i) %in% var.names]
    if(nrow(i) > 0) {
      i <- i[ , lapply(.SD, sum), .SDcols = weight.var, by = keys]
    } else {
      i[ , (keys) := NULL]
      i <- cbind(head.cols, i)
      i[ , (weight.var) := lapply(.SD, function(j) {j <- as.double(0)}), .SDcols = weight.var]
      i <- i[ , lapply(.SD, sum), .SDcols = weight.var, by = keys]
    }
    i[ , Variables := var.col]
    setcolorder(x = i, neworder = c(keys, "Variables", weight.var))
  })
  total.rows <- rbindlist(l = total.rows)
  total.rows[ , (keys) := lapply(.SD, droplevels), .SDcols = keys]
  total.rows[ , (keys[length(keys)]) := factor(x = get(keys[length(keys)]), labels = paste0(levels(get(keys[length(keys)])), " Total"))]
  data.missings[ , (var.names) := lapply(.SD, function(i) {
    ifelse(test = is.na(i), yes = 0, no = 1)
  }), .SDcols = var.names]
  data.missings.per.pattern <- split(x = data.missings, by = c(keys, var.names), drop = TRUE)
  data.missings.pattern.rows <- lapply(X = data.missings.per.pattern, FUN = function(i) {
    unique(i[ , mget(var.names)])
  })
  data.missings.per.pattern <- lapply(X = data.missings.per.pattern, FUN = function(i) {
    i <- lapply(X = var.names, FUN = function(j) {
      i[ , mget(c(keys, j, weight.var))]
    })
    i <- rbindlist(l = i, use.names = FALSE)
    setnames(x = i, old = colnames(i)[colnames(i) %in% var.names], new = "Variables_Patterns")
  })
  data.missings.per.pattern <- lapply(X = data.missings.per.pattern, FUN = function(i) {
    if(nrow(i[Variables_Patterns != 0, ]) == 0) {
      i <- cbind(unique(i[ , mget(keys)]), setnames(x = data.table(matrix(data = rep(x = 0, times = length(weight.var)), ncol = length(weight.var))), weight.var))
    } else {
      i <- i[Variables_Patterns != 0, ]
    }
  })
  missings.per.pattern <- lapply(X = data.missings.per.pattern, FUN = function(i) {
    if("Variables_Patterns" %in% colnames(i)) {
      i[ , Variables_Patterns := NULL]
    }
    i[ , lapply(.SD, function(j) {
      sum(j)
    }), .SDcols = weight.var, by = keys]
  })
  missings.per.pattern <- Map(f = cbind, data.missings.pattern.rows, missings.per.pattern)
  missings.per.pattern <- rbindlist(l = missings.per.pattern)
  setcolorder(x = missings.per.pattern, neworder = c(keys, var.names, weight.var))
  missings.per.pattern <- split(x = missings.per.pattern, by = keys, drop = TRUE)
  missings.per.pattern <- lapply(X = missings.per.pattern, FUN = function(i) {
    i[ , (keys) := lapply(.SD, function(j) {j <- droplevels(j)}), .SDcols = keys]
    tot.row <- i[ , lapply(.SD, sum), .SDcols = weight.var, by = keys]
    tot.row[ , (keys[length(keys)]) := paste0(get(keys[length(keys)]), " Total")]
    rbindlist(l = list(i, tot.row), fill = TRUE)
  })
  lapply(X = 1:length(names(missings.per.pattern)), FUN = function(i) {
    missings.per.pattern[[i]][ , Index := i]
    setcolorder(x = missings.per.pattern[[i]], neworder = c("Index", colnames(missings.per.pattern[[i]])[!colnames(missings.per.pattern[[i]]) %in% "Index"]))
  })
  missings.per.pattern <- rbindlist(l = missings.per.pattern)
  missings.per.pattern[ , (var.names) := lapply(.SD, function(i) {
    ifelse(test = i == 0, yes = 1, no = 0)
  }), .SDcols = var.names]
  missing.patterns.N[ , (keys) := lapply(.SD, droplevels), .SDcols = keys]
  total.rows[ , (keys) := lapply(.SD, droplevels), .SDcols = keys]
  missings.per.pattern[ , (keys) := lapply(.SD, droplevels), .SDcols = keys]
  setkeyv(x = missing.patterns.N, cols = keys)
  setkeyv(x = total.rows, cols = c(keys, "Variables"))
  setkeyv(x = missings.per.pattern, cols = keys)
  all.missing.pattern.estimates <- list(Missing_Patterns = missing.patterns.N, Total_Rows = total.rows, Missings_per_Pattern = missings.per.pattern)
  if(isTRUE(pattern.cell.counts.valid)) {
    data.missings[ , (var.names) := lapply(.SD, function(i) {
      ifelse(test = i == 0, yes = 1, no = 0)
    }), .SDcols = var.names]
    data.valid.per.pattern <- split(x = data.missings, by = c(keys, var.names), drop = TRUE)
    patterns.count.valid <- lapply(X = data.valid.per.pattern, FUN = function(i) {
      lapply(X = var.names, FUN = function(j) {
        i <- i[ , lapply(.SD, function(k) {
          if(unique(get(j)) == 1) {
            sum(k)
          } else {
            0
          }
        }), .SDcols = weight.var, keyby = c(keys, colnames(i)[colnames(i) %in% var.names])]
        i[ , (colnames(i)[!colnames(i) %in% c(keys, j, weight.var)]) := NULL]
      })
    })
  }
  if(isTRUE(pattern.cell.counts.miss)) {
    data.missings[ , (var.names) := lapply(.SD, function(i) {
      ifelse(test = i == 0, yes = 1, no = 0)
    }), .SDcols = var.names]
    data.miss.per.pattern <- split(x = data.missings, by = c(keys, var.names), drop = TRUE)
    patterns.count.miss <- lapply(X = data.miss.per.pattern, FUN = function(i) {
      lapply(X = var.names, FUN = function(j) {
        i <- i[ , lapply(.SD, function(k) {
          if(unique(get(j)) == 0) {
            sum(k)
          } else {
            0
          }
        }), .SDcols = weight.var, keyby = c(keys, colnames(i)[colnames(i) %in% var.names])]
        i[ , (colnames(i)[!colnames(i) %in% c(keys, j, weight.var)]) := NULL]
      })
    })
  }
  if(isTRUE(pattern.cell.counts.valid)) {
    return(patterns.count.valid)
  } else if(isTRUE(pattern.cell.counts.miss)) {
    return(patterns.count.miss)
  } else if(isFALSE(pattern.cell.counts.valid) & isFALSE(pattern.cell.counts.miss)) {
    return(all.missing.pattern.estimates)
  }
}
compute.unweighted.missing.data.pairs <- function(data.object, var.names, keys, keys.no.NA) {
  data.object <- data.object[ , mget(c(keys, var.names))]
  if(isFALSE(keys.no.NA)) {
    data.object <- na.omit(object = data.object, cols = keys)
    data.object[ , (keys) := lapply(.SD, droplevels), .SDcols = keys]
  }
  data.object[ , wgt := 1]
  data.object[ , (keys) := lapply(.SD, function(i) {
    i <- droplevels(i)
  }), .SDcols = keys]
  data.object[ , (var.names) := lapply(.SD, function(i) {
    i <- as.numeric(i)
    ifelse(test = i == 2, yes = NA_real_, no = i)
  }), .SDcols = var.names]
  var.pairs <- split(x = data.table(expand.grid(var.names, var.names)), by = c("Var1", "Var2"))
  var.pairs <- lapply(X = var.pairs, FUN = function(i) {
    as.character(unname(unlist(i)))
  })
  data.object[ , (keys) := lapply(.SD, function(i) {
    i <- droplevels(i)
    if(any(is.na(i))) {
      i <- addNA(i)
    } else {
      i
    }
  }), .SDcols = keys]
  response.response.data <- copy(data.object)
  response.response.data <- lapply(X = var.pairs, FUN = function(i) {
    i <- response.response.data[ , mget(c(keys, i, "wgt"))]
    setnames(x = i, make.unique(names = colnames(i), sep = "_"))
    i[ , colnames(i)[!colnames(i) %in% c(keys, "wgt")] := lapply(.SD, function(i) {
      ifelse(test = is.na(i), yes = NA_real_, no = 1)
    }), .SDcols = colnames(i)[!colnames(i) %in% c(keys, "wgt")], by = keys]
  })
  missing.missing.data <- copy(data.object)
  missing.missing.data <- lapply(X = var.pairs, FUN = function(i) {
    i <- missing.missing.data[ , mget(c(keys, i, "wgt"))]
    setnames(x = i, make.unique(names = colnames(i), sep = "_"))
    i[ , colnames(i)[!colnames(i) %in% c(keys, "wgt")] := lapply(.SD, function(i) {
      ifelse(test = is.na(i), yes = 1, no = NA_real_)
    }), .SDcols = colnames(i)[!colnames(i) %in% c(keys, "wgt")], by = keys]
  })
  missing.response.data <- copy(data.object)
  missing.response.data <- lapply(X = var.pairs, FUN = function(i) {
    i <- missing.response.data[ , mget(c(keys, i, "wgt"))]
    setnames(x = i, make.unique(names = colnames(i), sep = "_"))
    i[ , colnames(i)[length(keys) + 1] := ifelse(test = is.na(get(colnames(i)[length(keys) + 1])), yes = 1, no = NA_real_), by = keys]
    i[ , colnames(i)[length(keys) + 2] := ifelse(test = is.na(get(colnames(i)[length(keys) + 2])), yes = NA_real_, no = 1), by = keys]
  })
  response.missing.data <- copy(data.object)
  response.missing.data <- lapply(X = var.pairs, FUN = function(i) {
    i <- response.missing.data[ , mget(c(keys, i, "wgt"))]
    setnames(x = i, make.unique(names = colnames(i), sep = "_"))
    i[ , colnames(i)[length(keys) + 1] := ifelse(test = is.na(get(colnames(i)[length(keys) + 1])), yes = NA_real_, no = 1), by = keys]
    i[ , colnames(i)[length(keys) + 2] := ifelse(test = is.na(get(colnames(i)[length(keys) + 2])), yes = 1, no = NA_real_), by = keys]
  })
  zero.the.weights.in.pairs <- function(pairs.object) {
    lapply(X = pairs.object, FUN = function(i) {
      var.cols <- colnames(i)[!colnames(i) %in% c(keys, "wgt")]
      i[is.na(eval(parse(text = var.cols[1]))) | is.na(eval(parse(text = var.cols[2]))), ("wgt") := lapply(.SD, function(j) {
        j <- 0
      }), .SDcols = "wgt"]
    })
  }
  response.response.data <- zero.the.weights.in.pairs(pairs.object = response.response.data)
  missing.missing.data <- zero.the.weights.in.pairs(pairs.object = missing.missing.data)
  response.missing.data <- zero.the.weights.in.pairs(pairs.object = response.missing.data)
  missing.response.data <- zero.the.weights.in.pairs(pairs.object = missing.response.data)
  compute.pair.stats <- function(pair.data, weights.vector) {
    lapply(X = pair.data, FUN = function(i) {
      pair.cols <- colnames(i)[!colnames(i) %in% c(keys, weights.vector)]
      pair.cols <- gsub(pattern = "_1$", replacement = "", x = pair.cols)
      i <- i[ , lapply(.SD, function(j) {
        sum(j)
      }), .SDcols = weights.vector, keyby = keys]
      i <- cbind(i, Variables = pair.cols[1], variable = pair.cols[2])
      setcolorder(x = i, neworder = c(keys, "Variables", "variable", weights.vector))
    })
  }
  response.response.stat <- cbind(data.table(Missing_Pairs = "Response-Response"), rbindlist(l = compute.pair.stats(pair.data = response.response.data, weights.vector = "wgt")))
  missing.missing.stat <- cbind(data.table(Missing_Pairs = "Missing-Missing"), rbindlist(l = compute.pair.stats(pair.data = missing.missing.data, weights.vector = "wgt")))
  response.missing.stat <- cbind(data.table(Missing_Pairs = "Response-Missing"), rbindlist(l = compute.pair.stats(pair.data = response.missing.data, weights.vector = "wgt")))
  missing.response.stat <- cbind(data.table(Missing_Pairs = "Missing-Response"), rbindlist(l = compute.pair.stats(pair.data = missing.response.data, weights.vector = "wgt")))
  all.pairs.stats <- rbindlist(l = list(response.response.stat, response.missing.stat, missing.response.stat, missing.missing.stat))
  all.pairs.stats[ , Missing_Pairs := factor(x = Missing_Pairs, levels = c("Response-Response", "Response-Missing", "Missing-Response", "Missing-Missing"))]
  dcast.formula <- formula(paste0(paste(keys, collapse = " + "), " + Missing_Pairs + Variables ~ variable"))
  all.pairs.stats <- dcast(data = all.pairs.stats, formula = dcast.formula, value.var = "wgt")
  all.pairs.stats[ , Missing_Pairs := factor(x = Missing_Pairs, levels = c("Response-Response", "Response-Missing", "Missing-Response", "Missing-Missing"))]
  all.pairs.stats[ , Variables := factor(x = Variables, levels = var.names)]
  setkeyv(x = all.pairs.stats, cols = c("Missing_Pairs", keys, "Variables"))
  setcolorder(x = all.pairs.stats, neworder = c(keys, "Missing_Pairs", "Variables", var.names))
  return(all.pairs.stats)
}
compute.missing.data.pairs.all.repwgt <- function(data.object, var.names, keys, weight.var) {
  data.object <- data.object[ , mget(c(keys, var.names, weight.var))]
  data.object[ , (keys) := lapply(.SD, function(i) {
    i <- droplevels(i)
  }), .SDcols = keys]
  data.object[ , (var.names) := lapply(.SD, function(i) {
    i <- as.numeric(i)
    ifelse(test = i == 2, yes = NA_real_, no = i)
  }), .SDcols = var.names]
  var.pairs <- split(x = data.table(expand.grid(var.names, var.names)), by = c("Var1", "Var2"))
  var.pairs <- lapply(X = var.pairs, FUN = function(i) {
    as.character(unname(unlist(i)))
  })
  data.object[ , (keys) := lapply(.SD, function(i) {
    i <- droplevels(i)
    if(any(is.na(i))) {
      i <- addNA(i)
    } else {
      i
    }
  }), .SDcols = keys]
  compute.pairs.stat <- function(obj, pairs.obj, pairs.type, all.weights.vector, all.keys) {
    stat <- lapply(X = pairs.obj, FUN = function(i) {
      tmp <- copy(obj[ , mget(c(all.keys, i, weight.var))])
      setnames(x = tmp, make.unique(names = colnames(tmp), sep = "_"))
      if(pairs.type == "Response-Response") {
        tmp[ , colnames(tmp)[!colnames(tmp) %in% c(all.keys, weight.var)] := lapply(.SD, function(j) {
          ifelse(test = is.na(j), yes = NA_real_, no = 1)
        }), .SDcols = colnames(tmp)[!colnames(tmp) %in% c(all.keys, weight.var)], by = all.keys]
      } else if(pairs.type == "Missing-Missing") {
        tmp[ , colnames(tmp)[!colnames(tmp) %in% c(all.keys, weight.var)] := lapply(.SD, function(j) {
          ifelse(test = is.na(j), yes = 1, no = NA_real_)
        }), .SDcols = colnames(tmp)[!colnames(tmp) %in% c(all.keys, weight.var)], by = all.keys]
      } else if(pairs.type == "Missing-Response") {
        tmp[ , colnames(tmp)[length(all.keys) + 1] := ifelse(test = is.na(get(colnames(tmp)[length(all.keys) + 1])), yes = 1, no = NA_real_), by = all.keys]
        tmp[ , colnames(tmp)[length(all.keys) + 2] := ifelse(test = is.na(get(colnames(tmp)[length(all.keys) + 2])), yes = NA_real_, no = 1), by = all.keys]
      } else if(pairs.type == "Response-Missing") {
        tmp[ , colnames(tmp)[length(all.keys) + 1] := ifelse(test = is.na(get(colnames(tmp)[length(all.keys) + 1])), yes = NA_real_, no = 1), by = all.keys]
        tmp[ , colnames(tmp)[length(all.keys) + 2] := ifelse(test = is.na(get(colnames(tmp)[length(all.keys) + 2])), yes = 1, no = NA_real_), by = all.keys]
      }
      var.cols <- colnames(tmp)[!colnames(tmp) %in% c(keys, weight.var)]
      tmp[is.na(eval(parse(text = var.cols[1]))) | is.na(eval(parse(text = var.cols[2]))), (weight.var) := lapply(.SD, function(j) {
        j <- 0
      }), .SDcols = weight.var]
      tmp <- tmp[ , lapply(.SD, function(j) {
        sum(j)
      }), .SDcols = weight.var, keyby = keys]
      tmp <- cbind(tmp, Variables = i[1], variable = i[2])
      setcolorder(x = tmp, neworder = c(keys, "Variables", "variable", weight.var))
    })
    stat <- rbindlist(l = stat)
    stat[ , Missing_Pairs := pairs.type]
    setcolorder(x = stat, neworder = c(keys, "Missing_Pairs", "Variables", "variable", weight.var))
  }
  response.response.stat <- compute.pairs.stat(obj = data.object, pairs.obj = var.pairs, pairs.type = "Response-Response", all.keys = keys, all.weights.vector = weight.var)
  missing.missing.stat <- compute.pairs.stat(obj = data.object, pairs.obj = var.pairs, pairs.type = "Missing-Missing", all.keys = keys, all.weights.vector = weight.var)
  response.missing.stat <- compute.pairs.stat(obj = data.object, pairs.obj = var.pairs, pairs.type = "Response-Missing", all.keys = keys, all.weights.vector = weight.var)
  missing.response.stat <- compute.pairs.stat(obj = data.object, pairs.obj = var.pairs, pairs.type = "Missing-Response", all.keys = keys, all.weights.vector = weight.var)
  all.pairs.stats <- rbindlist(l = list(response.response.stat, response.missing.stat, missing.response.stat, missing.missing.stat))
  all.pairs.stats[ , Missing_Pairs := factor(x = Missing_Pairs, levels = c("Response-Response", "Response-Missing", "Missing-Response", "Missing-Missing"))]
  all.pairs.stats[ , Variables := factor(x = Variables, levels = Variables, labels = Variables)]
  all.pairs.stats[ , variable := factor(x = variable, levels = variable, labels = variable)]
  setkeyv(x = all.pairs.stats, cols = c("Missing_Pairs", keys, "Variables", "variable"))
  return(all.pairs.stats)
}
compute.weighted.proportion.of.usable.cases.inbound.all.repwgt <- function(statistics.object, keys, weight.var) {
  statistics.object <- split(x = statistics.object, by = keys, drop = TRUE)
  percentages.of.usable.cases <- lapply(statistics.object, FUN = function(i) {
    head.cols <- unique(i[ , mget(c(keys, "Variables", "variable"))])
    i <- (i[Missing_Pairs == "Missing-Response", mget(weight.var)] / (i[Missing_Pairs == "Missing-Response", mget(weight.var)] + i[Missing_Pairs == "Missing-Missing", mget(weight.var)])) * 100
    cbind(head.cols, i)
  })
  return(percentages.of.usable.cases)
}
compute.weighted.proportion.of.usable.cases.outbound.all.repwgt <- function(statistics.object, keys, weight.var) {
  statistics.object <- split(x = statistics.object, by = keys, drop = TRUE)
  percentages.of.usable.cases <- lapply(statistics.object, FUN = function(i) {
    head.cols <- unique(i[ , mget(c(keys, "Variables", "variable"))])
    i <- (i[Missing_Pairs == "Response-Missing", mget(weight.var)] / (i[Missing_Pairs == "Response-Missing", mget(weight.var)] + i[Missing_Pairs == "Response-Response", mget(weight.var)])) * 100
    cbind(head.cols, i)
  })
  return(percentages.of.usable.cases)
}
compute.influx.outflux.unweighted <- function(data.object, miss.pairs.object, var.names, keys, keys.no.NA) {
  if(!missing(keys)) {
    keys <- keys
  } else {
    keys <- NULL
  }
  data.object <- copy(data.object[ , mget(c(keys, var.names))])
  if(!is.null(keys) & isFALSE(keys.no.NA)) {
    data.object <- na.omit(object = data.object, cols = keys)
    data.object[ , (keys) := lapply(.SD, droplevels), .SDcols = keys]
  }
  data.object[ , colnames(data.object)[!colnames(data.object) %in% keys] := lapply(.SD, function(i) {
    i <- as.numeric(i)
    i <- ifelse(test = i >= 2, yes = NA, no = i)
  }), .SDcols = colnames(data.object)[!colnames(data.object) %in% keys]]
  data.object[ , (keys) := lapply(.SD, function(i) {
    i <- droplevels(i)
    if(any(is.na(i))) {
      i <- addNA(i)
    } else {
      i
    }
  }), .SDcols = keys]
  proportion.valid.values <- data.object[ , lapply(.SD, function(i) {
    (length(na.omit(i)) / length(i)) * 100
  }), by = keys]
  proportion.valid.values <- melt(data = proportion.valid.values, id.vars = keys, measure.vars = var.names)
  setnames(x = proportion.valid.values, c(keys, "Variables", "Percent_of_Valid_Values"))
  setkeyv(x = proportion.valid.values, cols = c(keys, "Variables"))
  miss.pairs.object <- split(x = miss.pairs.object, by = keys, drop = TRUE)
  influx.stats <- rbindlist(l = lapply(X = miss.pairs.object, FUN = function(i) {
    key.cols <- unique(i[ , mget(keys)])
    influx.stat <- cbind(key.cols, data.table(Variables = var.names, Influx = rowSums(i[Missing_Pairs == "Missing-Response", mget(var.names)])/(rowSums(i[Missing_Pairs == "Missing-Response", mget(var.names)] + i[Missing_Pairs == "Response-Response", mget(var.names)]))))
    outflux.stat <- cbind(key.cols, data.table(Variables = var.names, Outflux = rowSums(i[Missing_Pairs == "Response-Missing", mget(var.names)])/(rowSums(i[Missing_Pairs == "Response-Missing", mget(var.names)] + i[Missing_Pairs == "Missing-Missing", mget(var.names)]))))
    average.inbound.stat <- i[Missing_Pairs == "Missing-Response", mget(var.names)]/(i[Missing_Pairs == "Missing-Response", mget(var.names)] + i[Missing_Pairs == "Missing-Missing", mget(var.names)])
    average.inbound.stat[ , Average_Inbound := (rowSums(x = .SD, na.rm = TRUE)/(length(var.names) - 1)) * 100]
    average.inbound.stat[ , grep(pattern = "Average_Inbound", x = colnames(average.inbound.stat), value = TRUE, invert = TRUE) := NULL]
    average.inbound.stat <- cbind(key.cols, Variables = var.names, average.inbound.stat)
    average.outbound.stat <- i[Missing_Pairs == "Response-Missing", mget(var.names)]/(i[Missing_Pairs == "Response-Missing", mget(var.names)] + i[Missing_Pairs == "Response-Response", mget(var.names)])
    average.outbound.stat[ , Average_Outbound := (rowSums(x = .SD, na.rm = TRUE)/(length(var.names) - 1)) * 100]
    average.outbound.stat[ , grep(pattern = "Average_Outbound", x = colnames(average.outbound.stat), value = TRUE, invert = TRUE) := NULL]
    average.outbound.stat <- cbind(key.cols, Variables = var.names, average.outbound.stat)
    setkeyv(x = influx.stat, cols = c(keys, "Variables"))
    setkeyv(x = outflux.stat, cols = c(keys, "Variables"))
    setkeyv(x = average.inbound.stat, cols = c(keys, "Variables"))
    setkeyv(x = average.outbound.stat, cols = c(keys, "Variables"))
    Reduce(function(...) merge(..., all = TRUE), list(influx.stat, outflux.stat, average.inbound.stat, average.outbound.stat))
  }))
  data.object <- split(x = data.object, by = keys, drop = TRUE)
  FICO.stat <- rbindlist(l = lapply(X = data.object, FUN = function(i) {
    incomplete.case.indicator <- !complete.cases(i)
    key.cols <- unique(i[ , mget(keys)])
    FICO <- transpose(l = i[ , lapply(.SD, function(j) {
      (sum((!is.na(j)) & incomplete.case.indicator)/sum(!is.na(j)))*100
    }), .SDcols = var.names], keep.names = "Variables")
    FICO <- cbind(key.cols, FICO)
    setnames(x = FICO, old = "V1", new = "FICO")
    setkeyv(x = FICO, cols = c(keys, "Variables"))
  }))
  all.influx.stats <- Reduce(function(...) merge(..., all = TRUE), list(proportion.valid.values, influx.stats, FICO.stat))
  all.influx.stats[ , (keys) := lapply(.SD, function(i) {
    i <- droplevels(i)
    if(any(is.na(i))) {
      i <- addNA(i)
    } else {
      i
    }
  }), .SDcols = keys]
  setkeyv(x = all.influx.stats, cols = c(keys, "Variables"))
  return(all.influx.stats)
}
compute.influx.outflux.all.repwgt <- function(data.object, miss.pairs.object, var.names, keys, weight.var, keys.no.NA) {
  if(!missing(keys)) {
    keys <- keys
  } else {
    keys <- NULL
  }
  data.object <- copy(data.object[ , mget(c(keys, var.names, weight.var))])
  if(!is.null(keys) & isFALSE(keys.no.NA)) {
    data.object <- na.omit(object = data.object, cols = keys)
    data.object[ , (keys) := lapply(.SD, droplevels), .SDcols = keys]
  }
  data.object[ , colnames(data.object)[!colnames(data.object) %in% c(keys, weight.var)] := lapply(.SD, function(i) {
    i <- as.numeric(i)
    i <- ifelse(test = i >= 2, yes = NA, no = i)
  }), .SDcols = colnames(data.object)[!colnames(data.object) %in% c(keys, weight.var)]]
  data.object[ , (keys) := lapply(.SD, function(i) {
    i <- droplevels(i)
    if(any(is.na(i))) {
      tmp.levels <- levels(i)
      i <- as.character(i)
      i <- ifelse(test = is.na(i), yes = "<NA>", no = i)
      i <- factor(x = i, levels = c(tmp.levels, "<NA>"))
    } else {
      i
    }
  }), .SDcols = keys]
  proportion.valid.values <- lapply(X = weight.var, FUN = function(i) {
    data.object[ , lapply(.SD, function(j) {
      j <- na.omit(data.table(j, get(i)))
      (sum(j[ , V2]) / sum(get(i))) * 100
    }), .SDcols = var.names, by = keys]
  })
  proportion.valid.values <- lapply(X = proportion.valid.values, FUN = function(i) {
    melt(data = i, id.vars = keys)
  })
  names(proportion.valid.values) <- weight.var
  lapply(X = names(proportion.valid.values), FUN = function(i) {
    setnames(x = proportion.valid.values[[i]], old = c("variable", "value"), new = c("Variables", i))
    setkeyv(x = proportion.valid.values[[i]], cols = c(keys, "Variables"))
  })
  proportion.valid.values <- list(Proportions_Valid_Values = Reduce(function(...) merge(..., all = TRUE), proportion.valid.values))
  miss.pairs.object <- lapply(X = weight.var, FUN = function(i) {
    dcast.formula <- formula(paste0("Missing_Pairs + ", paste(keys, collapse = " + "), " + Variables ~ variable"))
    dcast(data = miss.pairs.object[ , mget(c(key(miss.pairs.object), i))], formula = dcast.formula, value.var = i)
  })
  lapply(X = miss.pairs.object, FUN = function(i) {
    i[ , Variables := factor(x = Variables, levels = var.names)]
  })
  names(miss.pairs.object) <- weight.var
  influx.stats <- lapply(X = miss.pairs.object, FUN = function(i) {
    influx.numerator <- i[Missing_Pairs == "Missing-Response", rowSums(.SD), .SDcols = var.names, keyby = c(keys, "Variables")]
    influx.denominator <- i[Missing_Pairs %in% c("Missing-Response", "Response-Response"), sum(.SD), .SDcols = var.names, keyby = c(keys, "Variables")]
    influx <- merge(x = influx.numerator, y = influx.denominator, by = c(keys, "Variables"))
    influx[ , Influx := V1.x / V1.y, keyby = c(keys, "Variables")]
    influx[ , colnames(influx)[!colnames(influx) %in% c(keys, "Variables", "Influx")] := NULL]
    outflux.numerator <- i[Missing_Pairs == "Response-Missing", rowSums(.SD), .SDcols = var.names, keyby = c(keys, "Variables")]
    outflux.denominator <- i[Missing_Pairs %in% c("Response-Missing", "Missing-Missing"), sum(.SD), .SDcols = var.names, keyby = c(keys, "Variables")]
    outflux <- merge(x = outflux.numerator, y = outflux.denominator, by = c(keys, "Variables"))
    outflux[ , Outflux := V1.x / V1.y, keyby = c(keys, "Variables")]
    outflux[ , colnames(outflux)[!colnames(outflux) %in% c(keys, "Variables", "Outflux")] := NULL]
    average.inbound.numerator <- i[Missing_Pairs == "Missing-Response", mget(var.names), keyby = c(keys, "Variables")]
    average.inbound.denominator <- i[Missing_Pairs %in% c("Missing-Response", "Missing-Missing"), lapply(.SD, sum), .SDcols = var.names, keyby = c(keys, "Variables")]
    average.inbound.head.cols <- average.inbound.numerator[ , mget(c(keys, "Variables"))]
    average.inbound <- (average.inbound.numerator[ , mget(var.names)] / average.inbound.denominator[ , mget(var.names)]) / (length(var.names) - 1)
    average.inbound <- cbind(average.inbound.head.cols, average.inbound)
    average.inbound[ , Average_Inbound := rowSums(.SD) * 100, .SDcols = var.names, keyby = c(keys, "Variables")]
    average.inbound[ , (var.names) := NULL]
    average.outbound.numerator <- i[Missing_Pairs == "Response-Missing", mget(var.names), keyby = c(keys, "Variables")]
    average.outbound.denominator <- i[Missing_Pairs %in% c("Response-Missing", "Response-Response"), lapply(.SD, sum), .SDcols = var.names, keyby = c(keys, "Variables")]
    average.outbound.head.cols <- average.outbound.numerator[ , mget(c(keys, "Variables"))]
    average.outbound <- (average.outbound.numerator[ , mget(var.names)] / average.outbound.denominator[ , mget(var.names)]) / (length(var.names) - 1)
    average.outbound <- cbind(average.outbound.head.cols, average.outbound)
    average.outbound[ , Average_Outbound := rowSums(.SD) * 100, .SDcols = var.names, keyby = c(keys, "Variables")]
    average.outbound[ , (var.names) := NULL]
    return(list(influx, outflux, average.inbound, average.outbound))
  })
  influx <- lapply(X = influx.stats, FUN = function(i) {
    Filter(Negate(is.null), lapply(X = i, FUN = function(j) {if("Influx" %in% colnames(j)) {j}}))
  })
  outflux <- lapply(X = influx.stats, FUN = function(i) {
    Filter(Negate(is.null), lapply(X = i, FUN = function(j) {if("Outflux" %in% colnames(j)) {j}}))
  })
  average.inbound <- lapply(X = influx.stats, FUN = function(i) {
    Filter(Negate(is.null), lapply(X = i, FUN = function(j) {if("Average_Inbound" %in% colnames(j)) {j}}))
  })
  average.outbound <- lapply(X = influx.stats, FUN = function(i) {
    Filter(Negate(is.null), lapply(X = i, FUN = function(j) {if("Average_Outbound" %in% colnames(j)) {j}}))
  })
  influx <- lapply(X = names(influx), FUN = function(i) {
    rbindlist(l = lapply(X = influx[[i]], FUN = function(j) {
      setnames(x = j, old = "Influx", new = i)
    }))
  })
  outflux <- lapply(X = names(outflux), FUN = function(i) {
    rbindlist(l = lapply(X = outflux[[i]], FUN = function(j) {
      setnames(x = j, old = "Outflux", new = i)
    }))
  })
  average.inbound <- lapply(X = names(average.inbound), FUN = function(i) {
    rbindlist(l = lapply(X = average.inbound[[i]], FUN = function(j) {
      setnames(x = j, old = "Average_Inbound", new = i)
    }))
  })
  average.outbound <- lapply(X = names(average.outbound), FUN = function(i) {
    rbindlist(l = lapply(X = average.outbound[[i]], FUN = function(j) {
      setnames(x = j, old = "Average_Outbound", new = i)
    }))
  })
  influx <- Reduce(function(...) merge(..., all = TRUE), influx)
  outflux <- Reduce(function(...) merge(..., all = TRUE), outflux)
  average.inbound <- Reduce(function(...) merge(..., all = TRUE), average.inbound)
  average.outbound <- Reduce(function(...) merge(..., all = TRUE), average.outbound)
  influx.stats <- list(Influx = influx, Outflux = outflux, Average_Inbound = average.inbound, Average_Outbound = average.outbound)
  data.object <- split(x = data.object, by = keys, drop = TRUE)
  FICO <- lapply(X = data.object, FUN = function(i) {
    incomplete.case.indicator <- !complete.cases(i[ , mget(var.names)])
    key.cols <- unique(i[ , mget(keys)])
    stat <- i[ , lapply(.SD, function(j) {
      tmp <- transpose(l = i[ , lapply(.SD, function(k) {
        (sum((as.numeric(!is.na(k)) * j) & incomplete.case.indicator * j)/sum(!is.na(k) * j))*100
      }), .SDcols = var.names], keep.names = "Variables")
    }), .SDcols = weight.var]
    cols.to.delete <- grep(pattern = "\\.Variables$", x = colnames(stat), value = TRUE)
    stat[ , cols.to.delete[2:length(cols.to.delete)] := NULL]
    setnames(x = stat, old = grep(pattern = "\\.Variables$", x = colnames(stat), value = TRUE), new = "Variables")
    setnames(x = stat, old = grep(pattern = "\\.V1$", x = colnames(stat), value = TRUE), new = gsub(pattern = "\\.V1", replacement = "", x = grep(pattern = "\\.V1$", x = colnames(stat), value = TRUE)))
    stat <- cbind(key.cols, stat)
  })
  FICO <- rbindlist(l = FICO)
  setkeyv(x = FICO, cols = c(keys, "Variables"))
  FICO <- list(FICO = FICO)
  all.influx.stats <- c(proportion.valid.values, influx.stats, FICO)
  return(all.influx.stats)
}
compute.predictor.correlations <- function(pairs, data, weights, keys, correlation.method, weighted) {
  lapply(X = pairs, FUN = function(i) {
    tmp.data <- na.omit(data[ , mget(c(keys, i, weights))])
    if(identical(i[1], i[2])) {
      setnames(x = tmp.data, c(keys, i[1], paste0(i[2], "dupl"), weights))
    }
    if(isFALSE(weighted)) {
      tmp.data[ , (weights[2:length(weights)]) := NULL]
      tmp.data[ , (weights[1]) := 1]
    }
    if(isTRUE(weighted)) {
      all.cors <- compute.correlations.all.repwgt(data.object = tmp.data, vars.vector = i, weight.var = weights, keys = keys, method = correlation.method)
    } else if(isFALSE(weighted)) {
      all.cors <- compute.correlations.all.repwgt(data.object = tmp.data, vars.vector = colnames(tmp.data)[!colnames(tmp.data) %in% c(keys, weights)], weight.var = weights[1], keys = keys, method = correlation.method)
    }
  })
}
reshape.list.statistics.bckg <- function(estimate.object, estimate.name, bckg.vars.vector, weighting.variable, data.key.variables, new.names.vector, replication.weights, study.name, SE.design, multiply.columns = 1) {
  object.type <- deparse(substitute(estimate.object))
  if(object.type == "percentages" && !is.null(bckg.vars.vector)) {
    lapply(X = estimate.object, FUN = function(i) {
      setnames(x = i, grep(pattern = "V[[:digit:]]+", x = names(i), ignore.case = TRUE, value = TRUE), c(bckg.vars.vector, weighting.variable, replication.weights), skip_absent = )
    })
  } else if(object.type == "percentages" && is.null(bckg.vars.vector)) {
    lapply(X = estimate.object, FUN = function(i) {
      setnames(x = i, grep(pattern = "V[[:digit:]]+", x = names(i), ignore.case = TRUE, value = TRUE), c(data.key.variables, weighting.variable, replication.weights))
    })
  } else if(object.type %in% c("bckg.means", "bckg.variances", "bckg.SDs", "bckg.modes")) {
    lapply(X = estimate.object, FUN = function(i) {
      setnames(x = i, grep(pattern = "V[[:digit:]]+", x = names(i), ignore.case = TRUE, value = TRUE), c(weighting.variable, replication.weights))
    })
  } else if(object.type %in% c("bckg.prctls", "bckg.medians", "bckg.MADs") && !is.null(bckg.vars.vector)) {
    split.estimate.object <- lapply(X = estimate.object, FUN = function(i) {
      V.columns <- grep(pattern = "V[[:digit:]]+", x = colnames(i), value = TRUE)
      split.V.columns <- split(x = V.columns, f = rep_len(1:(length(multiply.columns) * length(bckg.vars.vector)), length(V.columns)))
      tmp <- lapply(X = split.V.columns, FUN = function(j) {
        i[ , mget(c(data.key.variables, j))]
      })
      list.of.percentiles <- rep(x = multiply.columns, times = length(bckg.vars.vector))
      list.of.percentiles <- lapply(X = list.of.percentiles, FUN = function(j) {
        data.table(Percentiles = paste0("Prctl_", j * 100))
      })
      list.of.prctls.vars <- rep(x = bckg.vars.vector, each = length(multiply.columns))
      list.of.prctls.vars <- lapply(X = list.of.prctls.vars, FUN = function(j) {
        data.table(PRCTLS.VARS = j)
      })
      lapply(X = seq_along(tmp), FUN = function(j) {
        cbind(tmp[[j]][ , mget(data.key.variables)], list.of.percentiles[[j]], list.of.prctls.vars[[j]], tmp[[j]][ , mget(grep(pattern = "^V[[:digit:]]+$", x = colnames(tmp[[j]]), value = TRUE))])
      })
    })
    split.estimate.object <- lapply(X = split.estimate.object, FUN = function(i) {
      lapply(X = i, FUN = function(j) {
        original.names <- grep(pattern = "^V[[:digit:]]+$", x = colnames(j), value = TRUE)
        setnames(x = j, old = original.names, new = c(weighting.variable, replication.weights))
      })
    })
    estimate.object <- lapply(X = split.estimate.object, FUN = function(i) {
      setkeyv(rbindlist(l = i, use.names = TRUE), data.key.variables)
    })
  }
  lapply(X = estimate.object, function(i) {
    i[ , (replication.weights) := Map(f = `-`, mget(replication.weights), .(get(weighting.variable)))]
    i[ , (replication.weights) := lapply(X = .SD, FUN = function(i) {i^2}), .SDcols = replication.weights]
    i[ , sum.of.squares := Reduce(`+`, .SD), .SDcols = replication.weights]
    if(study.name %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi") && SE.design == FALSE) {
      i[ , paste0("_SE") := sqrt(0.5*sum.of.squares)]
    } else if(study.name %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi") && SE.design == TRUE || study.name %in% c("CivED", "ICCS", "ICILS", "SITES", "REDS")) {
      i[ , paste0("_SE") := sqrt(sum.of.squares)]
    } else if(study.name %in% c("TEDS-M", "PISA", "PISA for Development", "TALIS", "TALIS 3S")) {
      i[ , paste0("_SE") := sqrt(sum.of.squares/(length(replication.weights)*(1 - 0.5)^2))]
    }
    i[ , c(replication.weights, "sum.of.squares") := NULL]
  })
  assign.vars.names <- function(list.of.data.tables, old.names, new.names, stat.name) {
    setnames(list.of.data.tables, old.names, paste0(stat.name, new.names), skip_absent = TRUE)
  }
  mapply(assign.vars.names, list.of.data.tables = estimate.object, old.names = weighting.variable, new.names = new.names.vector, stat.name = estimate.name)
  mapply(assign.vars.names, list.of.data.tables = estimate.object, old.names = "_SE", new.names = paste0(new.names.vector, "_SE"), stat.name = estimate.name)
  if(object.type %in% c("bckg.prctls", "bckg.medians", "bckg.MADs")) {
    estimate.object <- lapply(X = estimate.object, FUN = function(i) {
      tmp <- split(x = i, by = c("Percentiles", "PRCTLS.VARS"))
      if(object.type == "bckg.prctls") {
        tmp <- lapply(X = tmp, FUN = function(j) {
          prctls.column <- grep(pattern = paste(paste0(estimate.name, bckg.vars.vector, "$"), collapse = "|"), x = colnames(j), value = TRUE)
          setnames(x = j, old = prctls.column, new = paste0(j[ , Percentiles][1], "_", j[ , PRCTLS.VARS][1]))
        })
        tmp <- lapply(X = tmp, FUN = function(j) {
          prctls.column <- grep(pattern = paste(paste0(estimate.name, bckg.vars.vector, "_SE$"), collapse = "|"), x = colnames(j), value = TRUE)
          setnames(x = j, old = prctls.column, new = paste0(j[ , Percentiles][1], "_", j[ , PRCTLS.VARS][1], "_SE"))
          j[ , c("Percentiles", "PRCTLS.VARS") := NULL]
        })
      } else if(object.type == "bckg.medians") {
        tmp <- lapply(X = tmp, FUN = function(j) {
          medians.column <- grep(pattern = paste(paste0(estimate.name, bckg.vars.vector, "$"), collapse = "|"), x = colnames(j), value = TRUE)
          setnames(x = j, old = medians.column, new = paste0(estimate.name, j[ , PRCTLS.VARS][1]))
        })
        tmp <- lapply(X = tmp, FUN = function(j) {
          medians.column <- grep(pattern = paste(paste0(estimate.name, bckg.vars.vector, "_SE$"), collapse = "|"), x = colnames(j), value = TRUE)
          setnames(x = j, old = medians.column, new = paste0(estimate.name, j[ , PRCTLS.VARS][1], "_SE"))
          j[ , c("Percentiles", "PRCTLS.VARS") := NULL]
        })
      } else if(object.type == "bckg.MADs") {
        tmp <- lapply(X = tmp, FUN = function(j) {
          medians.column <- grep(pattern = paste(paste0(estimate.name, bckg.vars.vector, "$"), collapse = "|"), x = colnames(j), value = TRUE)
          setnames(x = j, old = medians.column, new = paste0(estimate.name, j[ , PRCTLS.VARS][1]))
        })
        tmp <- lapply(X = tmp, FUN = function(j) {
          medians.column <- grep(pattern = paste(paste0(estimate.name, bckg.vars.vector, "_SE$"), collapse = "|"), x = colnames(j), value = TRUE)
          setnames(x = j, old = medians.column, new = paste0(estimate.name, j[ , PRCTLS.VARS][1], "_SE"))
          j[ , c("Percentiles", "PRCTLS.VARS") := NULL]
        })
      }
      tmp <- Reduce(function(...) merge(..., all = TRUE, no.dups = FALSE), tmp)
      return(tmp)
    })
    if(object.type == "bckg.prctls") {
      assign(x = "bckg.prctls", value = estimate.object, envir = parent.frame())
    } else if(object.type == "bckg.medians") {
      assign(x = "bckg.medians", value = estimate.object, envir = parent.frame())
    } else if(object.type == "bckg.MADs") {
      assign(x = "bckg.MADs", value = estimate.object, envir = parent.frame())
    }
  } else {
    return(estimate.object)
  }
}
reshape.list.statistics.PV <- function(estimate.object, estimate.name, PV.vars.vector, weighting.variable, replication.weights, study.name, SE.design, multiply.columns = 1) {
  object.type <- deparse(substitute(estimate.object))
  if(object.type %in% c("PV.prctls", "PV.medians", "PV.MADs") && !is.null(PV.vars.vector)) {
    estimate.object <- lapply(X = estimate.object, FUN = function(i) {
      lapply(X = i, FUN = function(j) {
        V.columns <- grep(pattern = "V[[:digit:]]+", x = colnames(j), value = TRUE)
        split.V.columns <- split.V.columns <- split(x = V.columns, f = rep_len(1:(length(multiply.columns)), length(V.columns)))
        tmp <- lapply(X = split.V.columns, FUN = function(k) {
          j[ , mget(c(key(j), k))]
        })
        list.of.percentiles <- rep(x = multiply.columns, times = length(PV.vars.vector))
        list.of.percentiles <- lapply(X = list.of.percentiles, FUN = function(k) {
          data.table(Percentiles = paste0("Prctl_", k * 100))
        })
        tmp <- lapply(X = seq_along(tmp), FUN = function(k) {
          k <- cbind(tmp[[k]][ , mget(key(j))], list.of.percentiles[[k]], tmp[[k]][ , mget(grep(pattern = "^V[[:digit:]]+$", x = colnames(tmp[[k]]), value = TRUE))])
        })
      })
    })
    estimate.object <- lapply(X = estimate.object, FUN = function(i) {
      lapply(X = i, FUN = function(j) {
        rbindlist(lapply(X = j, FUN = function(k) {
          setnames(x = k, grep(pattern = "V[[:digit:]]+", x = names(k), ignore.case = TRUE, value = TRUE), c(weighting.variable, replication.weights))
        }))
      })
    })
  } else if(object.type == "PV.correlations") {
    estimate.object
  } else {
    lapply(X = estimate.object, FUN = function(i) {
      lapply(X = i, FUN = function(j) {
        setnames(x = j, grep(pattern = "V[[:digit:]]+", x = names(j), ignore.case = TRUE, value = TRUE), c(weighting.variable, replication.weights))
      })
    })
  }
  lapply(X = estimate.object, function(i) {
    lapply(X = i, FUN = function(j) {
      j[ , (replication.weights) := Map(f = `-`, mget(replication.weights), .(get(weighting.variable)))]
      j[ , (replication.weights) := lapply(X = .SD, FUN = function(k) {k^2}), .SDcols = replication.weights]
      j[ , sum.of.squares := Reduce(`+`, .SD), .SDcols = replication.weights]
      if(study.name %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi") && SE.design == FALSE) {
        j[ , sum.of.squares := 0.5*sum.of.squares]
      } else if(study.name %in% c("TEDS-M", "PISA", "PISA for Development", "TALIS")) {
        j[ , sum.of.squares := sum.of.squares/(length(replication.weights)*(1 - 0.5)^2)]
      }
      j[ , c(replication.weights) := NULL]
    })
  })
  assign.vars.names <- function(list.of.data.tables, old.names, new.names, stat.name, append.string = "") {
    Map(f = setnames, x = list.of.data.tables, old = old.names, new = paste0(stat.name, new.names, append.string), skip_absent = TRUE)
  }
  mapply(FUN = assign.vars.names, list.of.data.tables = estimate.object, old.names = weighting.variable, new.names = PV.vars.vector, stat.name = estimate.name)
  mapply(FUN = assign.vars.names, list.of.data.tables = estimate.object, old.names = "sum.of.squares", new.names = PV.vars.vector, stat.name = estimate.name, append.string = "_SumSq")
  if(object.type == "PV.prctls") {
    assign(x = "PV.prctls", value = estimate.object, envir = parent.frame())
  } else if(object.type == "PV.medians") {
    lapply(X = estimate.object, FUN = function(i) {
      lapply(X = i, FUN = function(j) {
        j[ , Percentiles := NULL]
      })
    })
    assign(x = "PV.medians", value = estimate.object, envir = parent.frame())
  } else if(object.type == "PV.MADs") {
    lapply(X = estimate.object, FUN = function(i) {
      lapply(X = i, FUN = function(j) {
        j[ , Percentiles := NULL]
      })
    })
    assign(x = "PV.MADs", value = estimate.object, envir = parent.frame())
  }
}
#' @exportS3Method pkg::aggregate.PV
aggregate.PV.estimates <- function(estimate.object, estimate.name, root.PV, PV.vars.vector, data.key.variables, study.name, SE.design) {
  object.type <- deparse(substitute(estimate.object))
  if(study.name %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi") && SE.design == FALSE || study.name %in% c("ICCS", "ICILS")) {
    lapply(X = estimate.object, FUN = function(i) {
      i[ , sampling.variance := Reduce(`+`, .SD), .SDcols = grep(pattern = "_SumSq", x = names(i))]
      i[ , sampling.variance := sampling.variance*(1/length(grep(pattern = "_SumSq", x = colnames(i))))]
    })
  } else if(study.name %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi") && SE.design == TRUE || study.name %in% c("CivED", "SITES")) {
    lapply(X = estimate.object, FUN = function(i) {
      i[ , sampling.variance := Reduce(`+`, .SD), .SDcols = grep(pattern = "_SumSq", x = names(i))[1]]
    })
  } else if(study.name %in% c("PISA", "PISA for Development")) {
    lapply(X = estimate.object, FUN = function(i) {
      i[ , sampling.variance := Reduce(`+`, .SD), .SDcols = grep(pattern = "_SumSq", x = names(i))]
      i[ , sampling.variance := sampling.variance*(1/length(grep(pattern = "_SumSq", x = colnames(i))))]
    })
  }
  lapply(X = estimate.object, FUN = function(i) {
    tmp.names <- grep(pattern = paste0(estimate.name), x = colnames(i), value = TRUE)
    tmp.names <- grep(pattern = "_SumSq", x = tmp.names, value = TRUE, invert = TRUE)
    if(object.type %in% c("bckg.means", "bckg.variances", "bckg.SDs")) {
      i[ , mean.of.PV.estimates := rowMeans(.SD, na.rm = TRUE), .SDcols = tmp.names]
    } else {
      i[ , mean.of.PV.estimates := rowMeans(.SD), .SDcols = tmp.names]
    }
    i[ , paste0(tmp.names, "_diff") := Map(`-`, mget(tmp.names), .(mean.of.PV.estimates))]
    tmp.names <- grep(pattern = "_diff", x = names(i), value = TRUE)
    i[ , (tmp.names) := lapply(.SD, function(i) {i^2}), .SDcols = tmp.names]
    i[ , sum.of.PV.diff := rowSums(.SD), .SDcols = tmp.names]
    i[ , measurement.variance := (sum.of.PV.diff/(length(tmp.names) - 1))*(1 + 1/length(tmp.names))]
    if(study.name %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi")) {
      root.PV.name <- unique(unlist(lapply(root.PV, function(j) {
        as.character(na.omit(str_extract(string = colnames(i), pattern = j)))
      })))
    } else if(study.name %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
      root.PV.name <- unique(unlist(lapply(colnames(i), function(j) {
        gsub(pattern = "[[:digit:]]+", replacement = "N", na.omit(str_extract(string = j, pattern = root.PV)))
      })))
    }
    i[ , paste0(estimate.name, root.PV.name, "_SE") := sqrt(sampling.variance + measurement.variance)]
    i[ , grep(pattern = paste(paste(unlist(PV.vars.vector), collapse = "|"), ".diff", sep = "|"), x = names(i), value = TRUE) := NULL]
    setcolorder(x = i, neworder = c(key(i), "mean.of.PV.estimates", paste0(estimate.name, root.PV.name, "_SE"), "sampling.variance", "measurement.variance"))
    if("Percentiles" %in% colnames(i)) {
      setnames(i, c(data.key.variables, "Percentiles", paste0(estimate.name, root.PV.name), paste0(estimate.name, root.PV.name, "_SE"), paste0(estimate.name, root.PV.name, "_SVR"), paste0(estimate.name, root.PV.name, "_MVR")))
    } else if ("Performance_Group" %in% colnames(i)) {
      setnames(i, c(data.key.variables, "Performance_Group", paste0(estimate.name, root.PV.name), paste0(estimate.name, root.PV.name, "_SE"), paste0(estimate.name, root.PV.name, "_SVR"), paste0(estimate.name, root.PV.name, "_MVR")))
    } else {
      setnames(i, c(data.key.variables, paste0(estimate.name, root.PV.name), paste0(estimate.name, root.PV.name, "_SE"), paste0(estimate.name, root.PV.name, "_SVR"), paste0(estimate.name, root.PV.name, "_MVR")))
    }
    lapply(X = i, FUN = function(j) {
      setnames(i, gsub(pattern = "[[:digit:]]+", replacement = "N", x = names(i), fixed = TRUE))
    })
    setkeyv(x = i, cols = data.key.variables)
  })
}
compute.table.average <- function(output.obj, object.variables, data.key.variables, data.properties) {
  all.estimate.columns <- grep(pattern = "^Percentages$|Percentages_.*|Mean_.*|Median_.*|MAD_.*|Mode_.*|Prctl_.*|Variance_.*|SD_.*|Correlation_|Coefficients|Odds_|Wald_|t_value|p_value|Estimate|Crosstab_", x = names(output.obj), value = TRUE)
  all.estimate.columns <- grep(pattern = "_SVR$|_MVR$|_SE$", x = all.estimate.columns, value = TRUE, invert = TRUE)
  PV.estimate.root <- grep(pattern = "^PV.root", x = names(object.variables), value = TRUE)
  if(length(PV.estimate.root) != 0) {
    if(any(!PV.estimate.root %in% c("PV.root.dep", "PV.root.indep"))) {
      if(data.properties[["lsa.study"]] %in% c("ICCS", "PISA", "PISA for Development", "ICILS")) {
        PV.estimate.columns <- grep(pattern = "_PVN", x = all.estimate.columns, value = TRUE)
      } else {
        PV.estimate.columns <- grep(pattern = paste(object.variables[[PV.estimate.root]], sep = "", collapse = "|"), x = all.estimate.columns, value = TRUE)
      }
      if(data.properties[["lsa.study"]] %in% c("PISA", "PISA for Development", "ICCS", "ICILS")) {
        non.PV.estimate.columns <- grep(pattern = "_PVN", x = all.estimate.columns, value = TRUE, invert = TRUE)
      } else {
        non.PV.estimate.columns <- grep(pattern = paste(object.variables[[PV.estimate.root]], sep = "", collapse = "|"), x = all.estimate.columns, value = TRUE, invert = TRUE)
      }
    } else {
      PV.estimate.columns <- grep(pattern = "^Coefficients|^Estimate|t_value|p_value", x = all.estimate.columns, value = TRUE)
      non.PV.estimate.columns <- grep(pattern = "^Coefficients|^Estimate|t_value|p_value", x = all.estimate.columns, value = TRUE, invert = TRUE)
    }
  } else {
    non.PV.estimate.columns <- all.estimate.columns
  }
  SE.columns <- grep(pattern = "^[^Sum_].*_SE$|SD_.+_SE", x = names(output.obj), value = TRUE)
  if(length(data.key.variables) >= 2) {
    output.obj <- na.omit(object = output.obj, cols = data.key.variables)
    estimates.by.split.vars <- split(x = output.obj, by = data.key.variables[2:length(data.key.variables)])
  } else {
    estimates.by.split.vars <- list(output.obj)
  }
  if(length(PV.estimate.root) != 0) {
    if(length(data.key.variables) >= 2) {
      PV.composites <- rbindlist(l = lapply(X = estimates.by.split.vars, FUN = function(i) {
        i[ , lapply(.SD, mean, na.rm = TRUE), .SDcols = PV.estimate.columns, by = eval(data.key.variables[2:length(data.key.variables)])]
      }))
    } else {
      PV.composites <- rbindlist(l = lapply(X = estimates.by.split.vars, FUN = function(i) {
        i[ , lapply(.SD, mean, na.rm = TRUE), .SDcols = PV.estimate.columns]
      }))
    }
  }
  if(length(data.key.variables) >= 2) {
    non.PV.composites <- rbindlist(l = lapply(X = estimates.by.split.vars, FUN = function(i) {
      i[ , lapply(.SD, function(j) {
        sum(j, na.rm = TRUE)/(length(j))
      }), .SDcols = non.PV.estimate.columns, by = eval(data.key.variables[2:length(data.key.variables)])]
    }))
    SE.composites <- rbindlist(l = lapply(X = estimates.by.split.vars, FUN = function(i) {
      i[ , lapply(.SD, function(j) {
        sqrt(sum(j^2, na.rm = TRUE)/(length(j)^2))
      }), .SDcols = SE.columns, by = eval(data.key.variables[2:length(data.key.variables)])]
    }))
  } else {
    non.PV.composites <- rbindlist(l = lapply(X = estimates.by.split.vars, FUN = function(i) {
      i[ , lapply(.SD, function(j) {
        sum(j, na.rm = TRUE)/(length(unique(output.obj[ , get(data.key.variables[1])])))
      }), .SDcols = non.PV.estimate.columns]
    }))
    SE.composites <- rbindlist(l = lapply(X = estimates.by.split.vars, FUN = function(i) {
      i[ , lapply(.SD, function(j) {
        sqrt(sum(j^2, na.rm = TRUE)/(length(unique(output.obj[ , get(data.key.variables[1])]))^2))
      }), .SDcols = SE.columns]
    }))
  }
  if(length(PV.estimate.root) != 0) {
    if(length(data.key.variables) > 1) {
      if(length(non.PV.estimate.columns) > 0) {
        all.composites <- Reduce(function(...) merge(..., by = eval(data.key.variables[2:length(data.key.variables)])), list(PV.composites, non.PV.composites, SE.composites))
      } else {
        all.composites <- Reduce(function(...) merge(..., by = eval(data.key.variables[2:length(data.key.variables)])), list(PV.composites, SE.composites))
      }
    } else {
      if(length(non.PV.estimate.columns) > 0) {
        all.composites <- cbind(PV.composites, non.PV.composites, SE.composites)
      } else {
        all.composites <- cbind(PV.composites, SE.composites)
      }
    }
  } else {
    if(length(data.key.variables) > 1) {
      all.composites <- Reduce(function(...) merge(..., by = eval(data.key.variables[2:length(data.key.variables)])), list(non.PV.composites, SE.composites))
    } else {
      all.composites <- cbind(non.PV.composites, SE.composites)
    }
  }
  all.composites[ , (data.key.variables[1]) := rep(x = "Table Average", times = nrow(all.composites))]
  output.obj <- rbindlist(l = list(output.obj, all.composites), use.names = TRUE, fill = TRUE)
}
produce.analysis.info <- function(data, cnt.ID, study, cycle, weight.variable, rep.design, used.shortcut, number.of.reps, in.time) {
  tmp <- data.table(COUNTRY = cnt.ID,
                    DATA = data,
                    STUDY = study,
                    CYCLE = cycle,
                    WEIGHT = eval(weight.variable),
                    DESIGN = rep.design,
                    if(!missing(used.shortcut) && study %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi")) {
                      SHORTCUT = used.shortcut
                    },
                    NREPS = length(number.of.reps),
                    ANALYSIS_DATE = format(Sys.Date(), "%B %d, %Y"),
                    START_TIME = in.time,
                    END_TIME = format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"))
  if(!missing(used.shortcut) && study %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "eTIMSS PSI", "TIMSS Advanced", "TiPi")) {
    setnames(x = tmp, old = "V7", new = "SHORTCUT")
  }
  time.difference <- difftime(time1 = in.time, time2 = tmp[ , END_TIME], units = "secs")
  tmp[ , DURATION :=
         if(abs(time.difference) < 1) {
           format(as.POSIXct("0001-01-01 00:00:00") - time.difference, "%H:%M:%OS3")
         } else {
           format(as.POSIXct("0001-01-01 00:00:00") - time.difference - 1, "%H:%M:%OS3")
         }
  ]
  tmp[ , c("START_TIME", "END_TIME") := lapply(.SD, function(i) {
    gsub(pattern = "^[[:digit:]]+\\-[[:digit:]]+\\-[[:digit:]]+[[:space:]]|\\.[[:digit:]]+$", replacement = "", x = i)
  }), .SDcols = c("START_TIME", "END_TIME")]
}
export.results <- function(output.object, analysis.type, add.graphs = FALSE, perc.graphs = NULL, non.perc.graphs = NULL, simple.desc.graphs = NULL, unwgt.miss.pattern.graphs = NULL, wgt.miss.pattern.graphs = NULL, unwgt.flux.graphs = NULL, wgt.flux.graphs = NULL, analysis.info.obj, model.stats.obj, Rao.Scott.adj.chi.sq.obj, destination.file, warns.list, open.exported.file) {
  if(missing(destination.file)) {
    destination.file <- file.path(getwd(), "Analysis.xlsx")
  }
  does.file.exist <- file.exists(destination.file)
  export.workbook <- createWorkbook(title = "Analysis created using RALSA (www.ralsa.ineri.org) provided by INERI (www.ineri.org)")
  header.row.style <- createStyle(fontColour = "#FFFFFF", bgFill = "#000000", border = c("left", "right"), borderColour = "#FFFFFF")
  call.cell.style <- createStyle(valign = "top", wrapText = TRUE)
  three.decimals.style <- createStyle(numFmt = "0.000")
  two.decimals.style <- createStyle(numFmt = "0.00")
  no.decimals.style <- createStyle(numFmt = "0")
  if(analysis.type %in% c("lsa.pcts.means", "lsa.prctls", "lsa.bench", "lsa.corr", "lsa.lin.reg", "lsa.bin.log.reg", "lsa.crosstabs")) {
    addWorksheet(wb = export.workbook, sheetName = "Estimates", tabColour = "#FF0000")
    setColWidths(wb = export.workbook, sheet = "Estimates", cols = 1:ncol(output.object), widths = "auto")
    addStyle(wb = export.workbook, sheet = "Estimates", style = header.row.style, rows = 1, cols = 1:ncol(output.object))
    freezePane(wb = export.workbook, sheet = "Estimates", firstRow = TRUE)
    cols.with.decimals <- names(Filter(is.numeric, output.object))
    cols.with.decimals <- grep(pattern = "n_Cases|CYCLE|NREPS|DURATION", x = cols.with.decimals, ignore.case = TRUE, invert = TRUE, value = TRUE)
    cols.with.decimals <- grep(pattern = paste(cols.with.decimals, collapse = "|"), x = colnames(output.object), ignore.case = TRUE)
    addStyle(wb = export.workbook, sheet = "Estimates", style = two.decimals.style, cols = cols.with.decimals, rows = 2:(nrow(output.object) + 1), gridExpand = TRUE)
    addStyle(wb = export.workbook, sheet = "Estimates", style = no.decimals.style, cols = grep(pattern = "n_cases", x = colnames(output.object)), rows = 2:(nrow(output.object) + 1), gridExpand = TRUE)
    addStyle(wb = export.workbook, sheet = "Estimates", style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object)), rows = 2:(nrow(output.object) + 1), gridExpand = TRUE)
    writeData(wb = export.workbook, x = output.object, sheet = "Estimates")
    if(!missing(model.stats.obj)) {
      cols.with.decimals <- grep(pattern = paste(c("Estimate", "Estimate_SE", "t_value", "p_value"), collapse = "|"), x = colnames(model.stats.obj))
      addWorksheet(wb = export.workbook, sheetName = "Model statistics")
      writeData(wb = export.workbook, sheet = "Model statistics", x = model.stats.obj)
      addStyle(wb = export.workbook, sheet = "Model statistics", style = header.row.style, rows = 1, cols = 1:ncol(model.stats.obj))
      addStyle(wb = export.workbook, sheet = "Model statistics", style = two.decimals.style, cols = cols.with.decimals, rows = 2:(nrow(model.stats.obj) + 1), gridExpand = TRUE)
      setColWidths(wb = export.workbook, sheet = "Model statistics", cols = 1:ncol(model.stats.obj), widths = "auto")
    }
    if(!missing(Rao.Scott.adj.chi.sq.obj)) {
      cols.with.decimals <- grep(pattern = "Value", x = colnames(Rao.Scott.adj.chi.sq.obj))
      addWorksheet(wb = export.workbook, sheetName = "Rao-Scott Adjusted Chi-Square")
      writeData(wb = export.workbook, sheet = "Rao-Scott Adjusted Chi-Square", x = Rao.Scott.adj.chi.sq.obj)
      addStyle(wb = export.workbook, sheet = "Rao-Scott Adjusted Chi-Square", style = header.row.style, rows = 1, cols = 1:ncol(Rao.Scott.adj.chi.sq.obj))
      addStyle(wb = export.workbook, sheet = "Rao-Scott Adjusted Chi-Square", style = three.decimals.style, cols = cols.with.decimals, rows = 2:(nrow(Rao.Scott.adj.chi.sq.obj) + 1), gridExpand = TRUE)
      setColWidths(wb = export.workbook, sheet = "Rao-Scott Adjusted Chi-Square", cols = 1:ncol(Rao.Scott.adj.chi.sq.obj), widths = "auto")
    }
  } else if(analysis.type %in% "lsa.explore.missings") {
    get.row.numbers <- function(list.object) {
      tmp.numbers <- unname(sapply(X = list.object, FUN = nrow))
      tmp.numbers <- cumsum(tmp.numbers)[1:(length(cumsum(tmp.numbers)) - 1)]
      tmp.numbers <- tmp.numbers + ((1:(length(tmp.numbers)) * 4) + 1)
      if(length(list.object) > 1) {
        tmp.numbers <- c(1, tmp.numbers)
      } else {
        tmp.numbers <- 1
      }
    }
    apply.header.row.style <- function(list.object, estimate.name, sheet.name, first.row.positions) {
      lapply(X = first.row.positions, FUN = function(i) {
        addStyle(wb = export.workbook, sheet = sheet.name, style = header.row.style, rows = i, cols = 1:ncol(list.object[[estimate.name]][[1]]))
      })
    }
    write.multiple.tables <- function(list.object, estimate.name, sheet.name, first.row.positions) {
      lapply(X = 1:length(first.row.positions), FUN = function(i) {
        writeData(wb = export.workbook, sheet = sheet.name, x = output.object[[estimate.name]][[i]], startRow = first.row.positions[i], colNames = TRUE)
      })
    }
    addWorksheet(wb = export.workbook, sheetName = "Simple Descriptives", tabColour = "#FF0000")
    setColWidths(wb = export.workbook, sheet = "Simple Descriptives", cols = 1:ncol(output.object[["Simple_Descriptives"]]), widths = "auto")
    addStyle(wb = export.workbook, sheet = "Simple Descriptives", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["Simple_Descriptives"]]))
    freezePane(wb = export.workbook, sheet = "Simple Descriptives", firstRow = TRUE)
    cols.with.decimals <- names(Filter(is.numeric, output.object[["Simple_Descriptives"]]))
    cols.with.decimals <- grep(pattern = paste(cols.with.decimals, collapse = "|"), x = colnames(output.object[["Simple_Descriptives"]]), ignore.case = TRUE)
    addStyle(wb = export.workbook, sheet = "Simple Descriptives", style = two.decimals.style, cols = cols.with.decimals, rows = 2:(nrow(output.object[["Simple_Descriptives"]]) + 1), gridExpand = TRUE)
    addStyle(wb = export.workbook, sheet = "Simple Descriptives", style = no.decimals.style, cols = grep(pattern = "n_cases", x = colnames(output.object[["Simple_Descriptives"]])), rows = 2:(nrow(output.object[["Simple_Descriptives"]]) + 1), gridExpand = TRUE)
    addStyle(wb = export.workbook, sheet = "Simple Descriptives", style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object[["Simple_Descriptives"]])), rows = 2:(nrow(output.object[["Simple_Descriptives"]]) + 1), gridExpand = TRUE)
    writeData(wb = export.workbook, x = output.object[["Simple_Descriptives"]], sheet = "Simple Descriptives")
    if("Unweighted_Missing_Patterns" %in% names(output.object)) {
      addWorksheet(wb = export.workbook, sheetName = "Unweighted Missing Patterns")
      setColWidths(wb = export.workbook, sheet = "Unweighted Missing Patterns", cols = 1:ncol(output.object[["Unweighted_Missing_Patterns"]][[1]]), widths = "auto")
      row.numbers <- get.row.numbers(list.object = output.object[["Unweighted_Missing_Patterns"]])
      apply.header.row.style(list.object = output.object, estimate.name = "Unweighted_Missing_Patterns", sheet.name = "Unweighted Missing Patterns", first.row.positions = row.numbers)
      write.multiple.tables(list.object = output.object, estimate.name = "Unweighted_Missing_Patterns", sheet.name = "Unweighted Missing Patterns", first.row.positions = row.numbers)
      addWorksheet(wb = export.workbook, sheetName = "Unwgt Miss Patterns N Miss")
      setColWidths(wb = export.workbook, sheet = "Unwgt Miss Patterns N Miss", cols = 1:ncol(output.object[["Unwgt_Miss_Patterns_N_Miss"]][[1]]), widths = "auto")
      row.numbers <- get.row.numbers(list.object = output.object[["Unwgt_Miss_Patterns_N_Miss"]])
      apply.header.row.style(list.object = output.object, estimate.name = "Unwgt_Miss_Patterns_N_Miss", sheet.name = "Unwgt Miss Patterns N Miss", first.row.positions = row.numbers)
      write.multiple.tables(list.object = output.object, estimate.name = "Unwgt_Miss_Patterns_N_Miss", sheet.name = "Unwgt Miss Patterns N Miss", first.row.positions = row.numbers)
      addWorksheet(wb = export.workbook, sheetName = "Unwgt Miss Patterns N Valid")
      setColWidths(wb = export.workbook, sheet = "Unwgt Miss Patterns N Valid", cols = 1:ncol(output.object[["Unwgt_Miss_Patterns_N_Valid"]][[1]]), widths = "auto")
      row.numbers <- get.row.numbers(list.object = output.object[["Unwgt_Miss_Patterns_N_Valid"]])
      apply.header.row.style(list.object = output.object, estimate.name = "Unwgt_Miss_Patterns_N_Valid", sheet.name = "Unwgt Miss Patterns N Valid", first.row.positions = row.numbers)
      write.multiple.tables(list.object = output.object, estimate.name = "Unwgt_Miss_Patterns_N_Valid", sheet.name = "Unwgt Miss Patterns N Valid", first.row.positions = row.numbers)
      addWorksheet(wb = export.workbook, sheetName = "Weighted Missing Patterns")
      setColWidths(wb = export.workbook, sheet = "Weighted Missing Patterns", cols = 1:ncol(output.object[["Weighted_Missing_Patterns"]][[1]]), widths = "auto")
      row.numbers <- get.row.numbers(list.object = output.object[["Weighted_Missing_Patterns"]])
      row.sequences <- row.numbers + sapply(X = output.object[["Weighted_Missing_Patterns"]], FUN = nrow)
      row.sequences <- lapply(X = 1:length(row.sequences), FUN = function(i) {
        tmp <- row.numbers[i] + 1
        tmp:row.sequences[i]
      })
      addStyle(wb = export.workbook, sheet = "Weighted Missing Patterns", style = two.decimals.style, rows = unlist(row.sequences), cols = grep(pattern = paste(c("Pattern_Counts", "Pattern_Counts_SE", "Total_Missings_per_Pattern", "Total_Missings_per_Pattern_SE"), collapse = "|"), x = colnames(output.object[["Weighted_Missing_Patterns"]][[1]])), gridExpand = TRUE)
      row.sequences <- lapply(X = row.sequences, FUN = function(i) {i[length(i)]})
      col.sequences <- unlist(unique(lapply(X = output.object[["Weighted_Missing_Patterns"]], FUN = function(i) {
        names(Filter(f = isTRUE, x = lapply(X = tail(i, 1), FUN = function(j) {
          !is.na(j) & is.numeric(j)
        })))
      })))
      col.sequences <- which(colnames(output.object[["Weighted_Missing_Patterns"]][[1]]) %in% col.sequences)
      addStyle(wb = export.workbook, sheet = "Weighted Missing Patterns", style = two.decimals.style, rows = unlist(row.sequences), cols = col.sequences, gridExpand = TRUE)
      apply.header.row.style(list.object = output.object, estimate.name = "Weighted_Missing_Patterns", sheet.name = "Weighted Missing Patterns", first.row.positions = row.numbers)
      write.multiple.tables(list.object = output.object, estimate.name = "Weighted_Missing_Patterns", sheet.name = "Weighted Missing Patterns", first.row.positions = row.numbers)
      addWorksheet(wb = export.workbook, sheetName = "Wgt Miss Patterns N Miss")
      setColWidths(wb = export.workbook, sheet = "Wgt Miss Patterns N Miss", cols = 1:ncol(output.object[["Wgt_Miss_Patterns_N_Miss"]][[1]]), widths = "auto")
      row.numbers <- get.row.numbers(list.object = output.object[["Wgt_Miss_Patterns_N_Miss"]])
      row.sequences <- row.numbers + sapply(X = output.object[["Wgt_Miss_Patterns_N_Miss"]], FUN = nrow)
      row.sequences <- lapply(X = 1:length(row.sequences), FUN = function(i) {
        tmp <- row.numbers[i] + 1
        tmp:row.sequences[i]
      })
      addStyle(wb = export.workbook, sheet = "Wgt Miss Patterns N Miss", style = two.decimals.style, rows = unlist(row.sequences), cols = grep(pattern = "M_Counts", x = colnames(output.object[["Wgt_Miss_Patterns_N_Miss"]][[1]])), gridExpand = TRUE)
      apply.header.row.style(list.object = output.object, estimate.name = "Wgt_Miss_Patterns_N_Miss", sheet.name = "Wgt Miss Patterns N Miss", first.row.positions = row.numbers)
      write.multiple.tables(list.object = output.object, estimate.name = "Wgt_Miss_Patterns_N_Miss", sheet.name = "Wgt Miss Patterns N Miss", first.row.positions = row.numbers)
      addWorksheet(wb = export.workbook, sheetName = "Wgt Miss Patterns N Valid")
      setColWidths(wb = export.workbook, sheet = "Wgt Miss Patterns N Valid", cols = 1:ncol(output.object[["Wgt_Miss_Patterns_N_Valid"]][[1]]), widths = "auto")
      row.numbers <- get.row.numbers(list.object = output.object[["Wgt_Miss_Patterns_N_Valid"]])
      row.sequences <- row.numbers + sapply(X = output.object[["Wgt_Miss_Patterns_N_Valid"]], FUN = nrow)
      row.sequences <- lapply(X = 1:length(row.sequences), FUN = function(i) {
        tmp <- row.numbers[i] + 1
        tmp:row.sequences[i]
      })
      addStyle(wb = export.workbook, sheet = "Wgt Miss Patterns N Valid", style = two.decimals.style, rows = unlist(row.sequences), cols = grep(pattern = "V_Counts", x = colnames(output.object[["Wgt_Miss_Patterns_N_Valid"]][[1]])), gridExpand = TRUE)
      apply.header.row.style(list.object = output.object, estimate.name = "Wgt_Miss_Patterns_N_Valid", sheet.name = "Wgt Miss Patterns N Valid", first.row.positions = row.numbers)
      write.multiple.tables(list.object = output.object, estimate.name = "Wgt_Miss_Patterns_N_Valid", sheet.name = "Wgt Miss Patterns N Valid", first.row.positions = row.numbers)
    }
    if("Unweighted_Missing_Pairs" %in% names(output.object)) {
      addWorksheet(wb = export.workbook, sheetName = "Unweighted Missing Pairs")
      setColWidths(wb = export.workbook, sheet = "Unweighted Missing Pairs", cols = 1:ncol(output.object[["Unweighted_Missing_Pairs"]]), widths = "auto")
      addStyle(wb = export.workbook, sheet = "Unweighted Missing Pairs", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["Unweighted_Missing_Pairs"]]))
      freezePane(wb = export.workbook, sheet = "Unweighted Missing Pairs", firstRow = TRUE)
      cols.with.decimals <- names(Filter(is.numeric, output.object[["Unweighted_Missing_Pairs"]]))
      cols.with.decimals <- grep(pattern = paste(cols.with.decimals, collapse = "|"), x = colnames(output.object[["Unweighted_Missing_Pairs"]]), ignore.case = TRUE)
      addStyle(wb = export.workbook, sheet = "Unweighted Missing Pairs", style = two.decimals.style, cols = cols.with.decimals, rows = 2:(nrow(output.object[["Unweighted_Missing_Pairs"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Unweighted Missing Pairs", style = no.decimals.style, cols = grep(pattern = "n_cases", x = colnames(output.object[["Unweighted_Missing_Pairs"]])), rows = 2:(nrow(output.object[["Unweighted_Missing_Pairs"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Unweighted Missing Pairs", style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object[["Unweighted_Missing_Pairs"]])), rows = 2:(nrow(output.object[["Unweighted_Missing_Pairs"]]) + 1), gridExpand = TRUE)
      writeData(wb = export.workbook, x = output.object[["Unweighted_Missing_Pairs"]], sheet = "Unweighted Missing Pairs")
      addWorksheet(wb = export.workbook, sheetName = "Weighted Missing Pairs")
      setColWidths(wb = export.workbook, sheet = "Weighted Missing Pairs", cols = 1:ncol(output.object[["Weighted_Missing_Pairs"]]), widths = "auto")
      addStyle(wb = export.workbook, sheet = "Weighted Missing Pairs", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["Weighted_Missing_Pairs"]]))
      freezePane(wb = export.workbook, sheet = "Weighted Missing Pairs", firstRow = TRUE)
      cols.with.decimals <- names(Filter(is.numeric, output.object[["Weighted_Missing_Pairs"]]))
      cols.with.decimals <- grep(pattern = paste(cols.with.decimals, collapse = "|"), x = colnames(output.object[["Weighted_Missing_Pairs"]]), ignore.case = TRUE)
      addStyle(wb = export.workbook, sheet = "Weighted Missing Pairs", style = two.decimals.style, cols = cols.with.decimals, rows = 2:(nrow(output.object[["Weighted_Missing_Pairs"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Weighted Missing Pairs", style = no.decimals.style, cols = grep(pattern = "n_cases", x = colnames(output.object[["Weighted_Missing_Pairs"]])), rows = 2:(nrow(output.object[["Weighted_Missing_Pairs"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Weighted Missing Pairs", style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object[["Weighted_Missing_Pairs"]])), rows = 2:(nrow(output.object[["Weighted_Missing_Pairs"]]) + 1), gridExpand = TRUE)
      writeData(wb = export.workbook, x = output.object[["Weighted_Missing_Pairs"]], sheet = "Weighted Missing Pairs")
      addWorksheet(wb = export.workbook, sheetName = "Unwgt Prop of Usable Cases")
      setColWidths(wb = export.workbook, sheet = "Unwgt Prop of Usable Cases", cols = 1:ncol(output.object[["Unweighted_Proportion_Usable_InOutBound"]]), widths = "auto")
      addStyle(wb = export.workbook, sheet = "Unwgt Prop of Usable Cases", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["Unweighted_Proportion_Usable_InOutBound"]]))
      freezePane(wb = export.workbook, sheet = "Unwgt Prop of Usable Cases", firstRow = TRUE)
      cols.with.decimals <- names(Filter(is.numeric, output.object[["Unweighted_Proportion_Usable_InOutBound"]]))
      cols.with.decimals <- grep(pattern = paste(cols.with.decimals, collapse = "|"), x = colnames(output.object[["Unweighted_Proportion_Usable_InOutBound"]]), ignore.case = TRUE)
      addStyle(wb = export.workbook, sheet = "Unwgt Prop of Usable Cases", style = two.decimals.style, cols = cols.with.decimals, rows = 2:(nrow(output.object[["Unweighted_Proportion_Usable_InOutBound"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Unwgt Prop of Usable Cases", style = no.decimals.style, cols = grep(pattern = "n_cases", x = colnames(output.object[["Unweighted_Proportion_Usable_InOutBound"]])), rows = 2:(nrow(output.object[["Unweighted_Proportion_Usable_InOutBound"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Unwgt Prop of Usable Cases", style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object[["Unweighted_Proportion_Usable_InOutBound"]])), rows = 2:(nrow(output.object[["Unweighted_Proportion_Usable_InOutBound"]]) + 1), gridExpand = TRUE)
      writeData(wb = export.workbook, x = output.object[["Unweighted_Proportion_Usable_InOutBound"]], sheet = "Unwgt Prop of Usable Cases")
      addWorksheet(wb = export.workbook, sheetName = "Wgt Prop of Usable Cases")
      setColWidths(wb = export.workbook, sheet = "Wgt Prop of Usable Cases", cols = 1:ncol(output.object[["Weighted_Proportion_Usable_InOutBound"]]), widths = "auto")
      addStyle(wb = export.workbook, sheet = "Wgt Prop of Usable Cases", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["Weighted_Proportion_Usable_InOutBound"]]))
      freezePane(wb = export.workbook, sheet = "Wgt Prop of Usable Cases", firstRow = TRUE)
      cols.with.decimals <- names(Filter(is.numeric, output.object[["Weighted_Proportion_Usable_InOutBound"]]))
      cols.with.decimals <- grep(pattern = paste(cols.with.decimals, collapse = "|"), x = colnames(output.object[["Weighted_Proportion_Usable_InOutBound"]]), ignore.case = TRUE)
      addStyle(wb = export.workbook, sheet = "Wgt Prop of Usable Cases", style = two.decimals.style, cols = cols.with.decimals, rows = 2:(nrow(output.object[["Weighted_Proportion_Usable_InOutBound"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Wgt Prop of Usable Cases", style = no.decimals.style, cols = grep(pattern = "n_cases", x = colnames(output.object[["Weighted_Proportion_Usable_InOutBound"]])), rows = 2:(nrow(output.object[["Weighted_Proportion_Usable_InOutBound"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Wgt Prop of Usable Cases", style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object[["Weighted_Proportion_Usable_InOutBound"]])), rows = 2:(nrow(output.object[["Weighted_Proportion_Usable_InOutBound"]]) + 1), gridExpand = TRUE)
      writeData(wb = export.workbook, x = output.object[["Weighted_Proportion_Usable_InOutBound"]], sheet = "Wgt Prop of Usable Cases")
      addWorksheet(wb = export.workbook, sheetName = "Unweighted Influx and Outflux")
      setColWidths(wb = export.workbook, sheet = "Unweighted Influx and Outflux", cols = 1:ncol(output.object[["Unweighted_InOutFlux"]]), widths = "auto")
      addStyle(wb = export.workbook, sheet = "Unweighted Influx and Outflux", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["Unweighted_InOutFlux"]]))
      freezePane(wb = export.workbook, sheet = "Unweighted Influx and Outflux", firstRow = TRUE)
      cols.with.decimals <- names(Filter(is.numeric, output.object[["Unweighted_InOutFlux"]]))
      cols.with.decimals <- grep(pattern = paste(cols.with.decimals, collapse = "|"), x = colnames(output.object[["Unweighted_InOutFlux"]]), ignore.case = TRUE)
      addStyle(wb = export.workbook, sheet = "Unweighted Influx and Outflux", style = two.decimals.style, cols = cols.with.decimals, rows = 2:(nrow(output.object[["Unweighted_InOutFlux"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Unweighted Influx and Outflux", style = no.decimals.style, cols = grep(pattern = "n_cases", x = colnames(output.object[["Unweighted_InOutFlux"]])), rows = 2:(nrow(output.object[["Unweighted_InOutFlux"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Unweighted Influx and Outflux", style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object[["Unweighted_InOutFlux"]])), rows = 2:(nrow(output.object[["Unweighted_InOutFlux"]]) + 1), gridExpand = TRUE)
      writeData(wb = export.workbook, x = output.object[["Unweighted_InOutFlux"]], sheet = "Unweighted Influx and Outflux")
      addWorksheet(wb = export.workbook, sheetName = "Weighted Influx and Outflux")
      setColWidths(wb = export.workbook, sheet = "Weighted Influx and Outflux", cols = 1:ncol(output.object[["Weighted_InOutFlux"]]), widths = "auto")
      addStyle(wb = export.workbook, sheet = "Weighted Influx and Outflux", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["Weighted_InOutFlux"]]))
      freezePane(wb = export.workbook, sheet = "Weighted Influx and Outflux", firstRow = TRUE)
      cols.with.decimals <- names(Filter(is.numeric, output.object[["Weighted_InOutFlux"]]))
      cols.with.decimals <- grep(pattern = paste(cols.with.decimals, collapse = "|"), x = colnames(output.object[["Weighted_InOutFlux"]]), ignore.case = TRUE)
      addStyle(wb = export.workbook, sheet = "Weighted Influx and Outflux", style = two.decimals.style, cols = cols.with.decimals, rows = 2:(nrow(output.object[["Weighted_InOutFlux"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Weighted Influx and Outflux", style = no.decimals.style, cols = grep(pattern = "n_cases", x = colnames(output.object[["Weighted_InOutFlux"]])), rows = 2:(nrow(output.object[["Weighted_InOutFlux"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Weighted Influx and Outflux", style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object[["Weighted_InOutFlux"]])), rows = 2:(nrow(output.object[["Weighted_InOutFlux"]]) + 1), gridExpand = TRUE)
      writeData(wb = export.workbook, x = output.object[["Weighted_InOutFlux"]], sheet = "Weighted Influx and Outflux")
    }
  } else if(analysis.type %in% "lsa.quick.pred") {
    addWorksheet(wb = export.workbook, sheetName = "Simple Descriptives", tabColour = "#FF0000")
    setColWidths(wb = export.workbook, sheet = "Simple Descriptives", cols = 1:ncol(output.object[["Simple_Descriptives"]]), widths = "auto")
    addStyle(wb = export.workbook, sheet = "Simple Descriptives", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["Simple_Descriptives"]]))
    freezePane(wb = export.workbook, sheet = "Simple Descriptives", firstRow = TRUE)
    addStyle(wb = export.workbook, sheet = "Simple Descriptives", style = two.decimals.style, cols = grep(pattern = "^Sum_|^Percentages", x = colnames(output.object[["Simple_Descriptives"]])), rows = 2:(nrow(output.object[["Simple_Descriptives"]]) + 1), gridExpand = TRUE)
    addStyle(wb = export.workbook, sheet = "Simple Descriptives", style = no.decimals.style, cols = grep(pattern = "n_cases", x = colnames(output.object[["Simple_Descriptives"]])), rows = 2:(nrow(output.object[["Simple_Descriptives"]]) + 1), gridExpand = TRUE)
    writeData(wb = export.workbook, x = output.object[["Simple_Descriptives"]], sheet = "Simple Descriptives")
    addWorksheet(wb = export.workbook, sheetName = "Paired Descriptives")
    setColWidths(wb = export.workbook, sheet = "Paired Descriptives", cols = 1:ncol(output.object[["Paired_Descriptives"]]), widths = "auto")
    addStyle(wb = export.workbook, sheet = "Paired Descriptives", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["Paired_Descriptives"]]))
    freezePane(wb = export.workbook, sheet = "Paired Descriptives", firstRow = TRUE)
    addStyle(wb = export.workbook, sheet = "Paired Descriptives", style = two.decimals.style, cols = grep(pattern = "^Sum_|^Percentages", x = colnames(output.object[["Paired_Descriptives"]])), rows = 2:(nrow(output.object[["Paired_Descriptives"]]) + 1), gridExpand = TRUE)
    addStyle(wb = export.workbook, sheet = "Paired Descriptives", style = no.decimals.style, cols = grep(pattern = "n_cases", x = colnames(output.object[["Paired_Descriptives"]])), rows = 2:(nrow(output.object[["Paired_Descriptives"]]) + 1), gridExpand = TRUE)
    writeData(wb = export.workbook, x = output.object[["Paired_Descriptives"]], sheet = "Paired Descriptives")
    addWorksheet(wb = export.workbook, sheetName = "Unweighted V-correlations")
    setColWidths(wb = export.workbook, sheet = "Unweighted V-correlations", cols = 1:ncol(output.object[["V_Correlations_Unwgt"]]), widths = "auto")
    addStyle(wb = export.workbook, sheet = "Unweighted V-correlations", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["V_Correlations_Unwgt"]]))
    freezePane(wb = export.workbook, sheet = "Unweighted V-correlations", firstRow = TRUE)
    addStyle(wb = export.workbook, sheet = "Unweighted V-correlations", style = two.decimals.style, cols = grep(pattern = "^Correlation_", x = colnames(output.object[["V_Correlations_Unwgt"]])), rows = 2:(nrow(output.object[["V_Correlations_Unwgt"]]) + 1), gridExpand = TRUE)
    writeData(wb = export.workbook, x = output.object[["V_Correlations_Unwgt"]], sheet = "Unweighted V-correlations")
    addWorksheet(wb = export.workbook, sheetName = "Weighted V-correlations")
    setColWidths(wb = export.workbook, sheet = "Weighted V-correlations", cols = 1:ncol(output.object[["V_Correlations_Wgt"]]), widths = "auto")
    addStyle(wb = export.workbook, sheet = "Weighted V-correlations", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["V_Correlations_Wgt"]]))
    freezePane(wb = export.workbook, sheet = "Weighted V-correlations", firstRow = TRUE)
    addStyle(wb = export.workbook, sheet = "Weighted V-correlations", style = two.decimals.style, cols = grep(pattern = "^Correlation_|^t_", x = colnames(output.object[["V_Correlations_Wgt"]])), rows = 2:(nrow(output.object[["V_Correlations_Wgt"]]) + 1), gridExpand = TRUE)
    addStyle(wb = export.workbook, sheet = "Weighted V-correlations", style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object[["V_Correlations_Wgt"]])), rows = 2:(nrow(output.object[["V_Correlations_Wgt"]]) + 1), gridExpand = TRUE)
    writeData(wb = export.workbook, x = output.object[["V_Correlations_Wgt"]], sheet = "Weighted V-correlations")
    addWorksheet(wb = export.workbook, sheetName = "Unweighted U-correlations")
    setColWidths(wb = export.workbook, sheet = "Unweighted U-correlations", cols = 1:ncol(output.object[["U_Correlations_Unwgt"]]), widths = "auto")
    addStyle(wb = export.workbook, sheet = "Unweighted U-correlations", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["U_Correlations_Unwgt"]]))
    freezePane(wb = export.workbook, sheet = "Unweighted U-correlations", firstRow = TRUE)
    cols <- names(sapply(X = output.object[["U_Correlations_Unwgt"]], FUN = class)[sapply(X = output.object[["U_Correlations_Unwgt"]], FUN = class) == "numeric"])
    cols <- grep(pattern = paste(cols, collapse = "|"), x = colnames(output.object[["U_Correlations_Unwgt"]]))
    addStyle(wb = export.workbook, sheet = "Unweighted U-correlations", style = two.decimals.style, cols = cols, rows = 2:(nrow(output.object[["U_Correlations_Unwgt"]]) + 1), gridExpand = TRUE)
    writeData(wb = export.workbook, x = output.object[["U_Correlations_Unwgt"]], sheet = "Unweighted U-correlations")
    addWorksheet(wb = export.workbook, sheetName = "Weighted U-correlations")
    setColWidths(wb = export.workbook, sheet = "Weighted U-correlations", cols = 1:ncol(output.object[["U_Correlations_Wgt"]]), widths = "auto")
    addStyle(wb = export.workbook, sheet = "Weighted U-correlations", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["U_Correlations_Wgt"]]))
    freezePane(wb = export.workbook, sheet = "Weighted U-correlations", firstRow = TRUE)
    addStyle(wb = export.workbook, sheet = "Weighted U-correlations", style = two.decimals.style, cols = grep(pattern = "^Correlation_|^t_", x = colnames(output.object[["U_Correlations_Wgt"]])), rows = 2:(nrow(output.object[["U_Correlations_Wgt"]]) + 1), gridExpand = TRUE)
    addStyle(wb = export.workbook, sheet = "Weighted U-correlations", style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object[["U_Correlations_Wgt"]])), rows = 2:(nrow(output.object[["U_Correlations_Wgt"]]) + 1), gridExpand = TRUE)
    writeData(wb = export.workbook, x = output.object[["U_Correlations_Wgt"]], sheet = "Weighted U-correlations")
    addWorksheet(wb = export.workbook, sheetName = "Unweighted VU-correlations")
    setColWidths(wb = export.workbook, sheet = "Unweighted VU-correlations", cols = 1:ncol(output.object[["VU_Max_Unweighted"]]), widths = "auto")
    addStyle(wb = export.workbook, sheet = "Unweighted VU-correlations", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["VU_Max_Unweighted"]]))
    freezePane(wb = export.workbook, sheet = "Unweighted VU-correlations", firstRow = TRUE)
    addStyle(wb = export.workbook, sheet = "Unweighted VU-correlations", style = two.decimals.style, cols = grep(pattern = "^Correlation_", x = colnames(output.object[["VU_Max_Unweighted"]])), rows = 2:(nrow(output.object[["VU_Max_Unweighted"]]) + 1), gridExpand = TRUE)
    writeData(wb = export.workbook, x = output.object[["VU_Max_Unweighted"]], sheet = "Unweighted VU-correlations")
    addWorksheet(wb = export.workbook, sheetName = "Weighted VU-correlations")
    setColWidths(wb = export.workbook, sheet = "Weighted VU-correlations", cols = 1:ncol(output.object[["VU_Max_Weighted"]]), widths = "auto")
    addStyle(wb = export.workbook, sheet = "Weighted VU-correlations", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["VU_Max_Weighted"]]))
    freezePane(wb = export.workbook, sheet = "Weighted VU-correlations", firstRow = TRUE)
    addStyle(wb = export.workbook, sheet = "Weighted VU-correlations", style = two.decimals.style, cols = grep(pattern = "^Correlation_|^t_", x = colnames(output.object[["VU_Max_Weighted"]])), rows = 2:(nrow(output.object[["VU_Max_Weighted"]]) + 1), gridExpand = TRUE)
    addStyle(wb = export.workbook, sheet = "Weighted VU-correlations", style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object[["VU_Max_Weighted"]])), rows = 2:(nrow(output.object[["VU_Max_Weighted"]]) + 1), gridExpand = TRUE)
    writeData(wb = export.workbook, x = output.object[["VU_Max_Weighted"]], sheet = "Weighted VU-correlations")
    addWorksheet(wb = export.workbook, sheetName = "Unweighted Predictor Matrix")
    setColWidths(wb = export.workbook, sheet = "Unweighted Predictor Matrix", cols = 1:ncol(output.object[["Predictor_Matrix_Unwgt"]]), widths = "auto")
    addStyle(wb = export.workbook, sheet = "Unweighted Predictor Matrix", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["Predictor_Matrix_Unwgt"]]))
    freezePane(wb = export.workbook, sheet = "Unweighted Predictor Matrix", firstRow = TRUE)
    writeData(wb = export.workbook, x = output.object[["Predictor_Matrix_Unwgt"]], sheet = "Unweighted Predictor Matrix")
    addWorksheet(wb = export.workbook, sheetName = "Weighted Predictor Matrix")
    setColWidths(wb = export.workbook, sheet = "Weighted Predictor Matrix", cols = 1:ncol(output.object[["Predictor_Matrix_Wgt"]]), widths = "auto")
    addStyle(wb = export.workbook, sheet = "Weighted Predictor Matrix", style = header.row.style, rows = 1, cols = 1:ncol(output.object[["Predictor_Matrix_Wgt"]]))
    freezePane(wb = export.workbook, sheet = "Weighted Predictor Matrix", firstRow = TRUE)
    if(length(grep(pattern = "^t_", x = colnames(output.object[["Predictor_Matrix_Wgt"]]))) > 0) {
      addStyle(wb = export.workbook, sheet = "Weighted Predictor Matrix", style = two.decimals.style, cols = grep(pattern = "^t_", x = colnames(output.object[["Predictor_Matrix_Wgt"]])), rows = 2:(nrow(output.object[["Predictor_Matrix_Wgt"]]) + 1), gridExpand = TRUE)
      addStyle(wb = export.workbook, sheet = "Weighted Predictor Matrix", style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object[["Predictor_Matrix_Wgt"]])), rows = 2:(nrow(output.object[["Predictor_Matrix_Wgt"]]) + 1), gridExpand = TRUE)
    }
    writeData(wb = export.workbook, x = output.object[["Predictor_Matrix_Wgt"]], sheet = "Weighted Predictor Matrix")
  }
  if(!missing(add.graphs) && add.graphs == TRUE) {
    if(analysis.type %in% c("lsa.pcts.means", "lsa.prctls", "lsa.bench", "lsa.crosstabs")) {
      addWorksheet(wb = export.workbook, sheetName = "Graphs")
      files.to.import <- file.path(dirname(destination.file), perc.graphs)
      if(length(files.to.import) > 0) {
        lapply(X = 1:length(files.to.import), FUN = function(i) {
          insertImage(wb = export.workbook, sheet = "Graphs", file = files.to.import[i], width = 8, height = 4, dpi = 600, startRow = (i * 20) - 19)
        })
      }
      if(!is.null(non.perc.graphs)) {
        files.to.import <- file.path(dirname(destination.file), unlist(non.perc.graphs))
        lapply(X = 1:length(files.to.import), FUN = function(i) {
          if(length(grep(pattern = "_Crosstab", x = files.to.import[i])) == 0) {
            insertImage(wb = export.workbook, sheet = "Graphs", file = files.to.import[i], width = 8, height = 4, dpi = 600, startRow = (i * 20) - 19, startCol = 13)
          } else {
            insertImage(wb = export.workbook, sheet = "Graphs", file = files.to.import[i], width = 8, height = 4, dpi = 600, startRow = (i * 20) - 19, startCol = 1)
          }
        })
      }
    } else if(analysis.type == "lsa.explore.missings") {
      unwgt.miss.pattern.graphs.files.to.import <- grep(pattern = "_Unweighted_Missing_Patterns", x = file.path(dirname(destination.file), unwgt.miss.pattern.graphs), value = TRUE)
      wgt.miss.pattern.graphs.files.to.import <- grep(pattern = "_Weighted_Missing_Patterns", x = file.path(dirname(destination.file), unwgt.miss.pattern.graphs), value = TRUE)
      unwgt.flux.graphs.files.to.import <- file.path(dirname(destination.file), unwgt.flux.graphs)
      wgt.flux.graphs.files.to.import <- file.path(dirname(destination.file), wgt.flux.graphs)
      addWorksheet(wb = export.workbook, sheetName = "Simple Descr Graphs")
      if(length(unwgt.miss.pattern.graphs.files.to.import) > 0) {
        addWorksheet(wb = export.workbook, sheetName = "Unwgt Miss Patterns Graphs")
      }
      if(length(wgt.miss.pattern.graphs.files.to.import) > 0) {
        addWorksheet(wb = export.workbook, sheetName = "Wgt Miss Patterns Graphs")
      }
      if(length(unwgt.flux.graphs.files.to.import) > 0) {
        addWorksheet(wb = export.workbook, sheetName = "Unwgt Flux Stats Graphs")
      }
      if(length(wgt.flux.graphs.files.to.import) > 0) {
        addWorksheet(wb = export.workbook, sheetName = "Wgt Flux Stats Graphs")
      }
      simple.desc.graphs.files.to.import <- file.path(dirname(destination.file), simple.desc.graphs)
      if(length(simple.desc.graphs.files.to.import) > 0) {
        lapply(X = 1:length(simple.desc.graphs.files.to.import), FUN = function(i) {
          insertImage(wb = export.workbook, sheet = "Simple Descr Graphs", file = simple.desc.graphs.files.to.import[i], width = 8, height = 4, dpi = 600, startRow = (i * 20) - 19)
        })
      }
      if(length(unwgt.miss.pattern.graphs.files.to.import) > 0) {
        lapply(X = 1:length(unwgt.miss.pattern.graphs.files.to.import), FUN = function(i) {
          insertImage(wb = export.workbook, sheet = "Unwgt Miss Patterns Graphs", file = unwgt.miss.pattern.graphs.files.to.import[i], width = 8, height = 8, dpi = 600, startRow = (i * 39) - 38)
        })
      }
      if(length(wgt.miss.pattern.graphs.files.to.import) > 0) {
        lapply(X = 1:length(wgt.miss.pattern.graphs.files.to.import), FUN = function(i) {
          insertImage(wb = export.workbook, sheet = "Wgt Miss Patterns Graphs", file = wgt.miss.pattern.graphs.files.to.import[i], width = 8, height = 8, dpi = 600, startRow = (i * 39) - 38)
        })
      }
      if(length(unwgt.flux.graphs.files.to.import) > 0) {
        lapply(X = 1:length(unwgt.flux.graphs.files.to.import), FUN = function(i) {
          insertImage(wb = export.workbook, sheet = "Unwgt Flux Stats Graphs", file = unwgt.flux.graphs.files.to.import[i], width = 8, height = 8, dpi = 600, startRow = (i * 39) - 38)
        })
      }
      if(length(wgt.flux.graphs.files.to.import) > 0) {
        lapply(X = 1:length(wgt.flux.graphs.files.to.import), FUN = function(i) {
          insertImage(wb = export.workbook, sheet = "Wgt Flux Stats Graphs", file = wgt.flux.graphs.files.to.import[i], width = 8, height = 8, dpi = 600, startRow = (i * 39) - 38)
        })
      }
    } else if(analysis.type == "lsa.quick.pred") {
      addWorksheet(wb = export.workbook, sheetName = "Simple Descr Graphs")
      simple.desc.graphs.files.to.import <- file.path(dirname(destination.file), simple.desc.graphs)
      if(length(simple.desc.graphs.files.to.import) > 0) {
        lapply(X = 1:length(simple.desc.graphs.files.to.import), FUN = function(i) {
          insertImage(wb = export.workbook, sheet = "Simple Descr Graphs", file = simple.desc.graphs.files.to.import[i], width = 8, height = 4, dpi = 600, startRow = (i * 20) - 19)
        })
      }
    }
  }
  addWorksheet(wb = export.workbook, sheetName = "Analysis information")
  writeData(wb = export.workbook, sheet = "Analysis information", x = analysis.info.obj)
  addStyle(wb = export.workbook, sheet = "Analysis information", style = header.row.style, rows = 1, cols = 1:ncol(analysis.info.obj))
  setColWidths(wb = export.workbook, sheet = "Analysis information", cols = 1:ncol(analysis.info.obj), widths = "auto")
  if(!is.null(warns.list)) {
    addWorksheet(wb = export.workbook, sheetName = "Warnings")
    setColWidths(wb = export.workbook, sheet = "Warnings", cols = 1, widths = 100)
    warnings.style <- createStyle(wrapText = TRUE)
    addStyle(wb = export.workbook, sheet = "Warnings", style = warnings.style, cols = 1, rows = 1:1000)
    writeData(wb = export.workbook, sheet = "Warnings", x = warns.list)
  }
  called.analysis.function <- paste0(gsub(pattern = "\\s+", replacement = " ", x = deparse(sys.call(1))))
  addWorksheet(wb = export.workbook, "Calling syntax")
  setRowHeights(wb = export.workbook, sheet = "Calling syntax", rows = 2, heights = 75)
  setColWidths(wb = export.workbook, sheet = "Calling syntax", cols = 1, widths = 120)
  addStyle(wb = export.workbook, sheet = "Calling syntax", style = call.cell.style, rows = 2, cols = 1)
  writeData(wb = export.workbook, sheet = "Calling syntax", x = "The following call can be used in R/Rstudio to perform the same analysis again:", startCol = 1, startRow = 1)
  writeData(wb = export.workbook, sheet = "Calling syntax", x = paste(called.analysis.function, collapse = ""), startCol = 1, startRow = 2)
  withCallingHandlers(
    saveWorkbook(wb = export.workbook, file = destination.file, overwrite = TRUE),
    warning = function(w){
      if(grepl("reason 'Permission denied'", w$message)){
        stop('The file in "output.file" (', destination.file, ') exists and is open, it cannot be overwritten. Please close the file and try again.', call. = FALSE)
      } else {
        message(w$message)
      }
    })
  if(does.file.exist == TRUE) {
    warning('The destination file in "output.file" already existed. It was overwritten.', call. = FALSE)
  }
  if(open.exported.file == TRUE && file.exists(destination.file)) {
    openXL(file = destination.file)
  }
}

file.merged.respondents <- list(
  "educ.bckg"                                     = "Educator background",
  "inst.bckg"                                     = "Institutional background",
  "inst.bckg.low-sec.tch.bckg"                    = "Institutional background<br/>Lower-secondary future teacher background",
  "inst.bckg.prim.tch.bckg"                       = "Institutional background<br/>Primary future teacher background",
  "leader.bckg"                                   = "Leader background",
  "leader.bckg.staff.bckg"                        = "Leader background<br/>Staff background",
  "low-sec.tch.bckg"                              = "Future lower-secondary teacher background",
  "math.sch.bckg"                                 = "Advanced mathematics school background",
  "math.std.ach"                                  = "Advanced mathematics student achievement",
  "math.std.ach.math.sch.bckg"                    = "Advanced mathematics student achievement<br/>Advanced mathematics school background",
  "math.std.ach.math.sch.bckg.math.tch.bckg"      = "Advanced mathematics student achievement<br/>Advanced mathematics school background<br/>Advanced mathematics teacher background",
  "math.std.ach.math.tch.bckg"                    = "Advanced mathematics student achievement<br/>Advanced mathematics teacher background",
  "math.std.bckg"                                 = "Advanced mathematics student background",
  "math.std.bckg.ach"                             = "Advanced mathematics student background<br/>Advanced mathematics student achievement",
  "math.std.bckg.ach.math.sch.bckg"               = "Advanced mathematics student background<br/>Advanced mathematics student achievement<br/>Advanced mathematics school background",
  "math.std.bckg.ach.math.sch.bckg.math.tch.bckg" = "Advanced mathematics student background<br/>Advanced mathematics student achievement<br/>Advanced mathematics school background<br/>Advanced mathematics teacher background",
  "math.std.bckg.ach.math.tch.bckg"               = "Advanced mathematics student background<br/>Advanced mathematics student achievement<br/>Advanced mathematics teacher background",
  "math.std.bckg.math.sch.bckg"                   = "Advanced mathematics student background<br/>Advanced mathematics school background",
  "math.std.bckg.math.sch.bckg.math.tch.bckg"     = "Advanced mathematics student background<br/>Advanced mathematics school background<br/>Advanced mathematics teacher background",
  "math.std.bckg.math.tch.bckg"                   = "Advanced mathematics student background<br/>Advanced mathematics teacher background",
  "math.tch.bckg"                                 = "Advanced mathematics teacher background",
  "math.tch.bckg.math.sch.bckg"                   = "Advanced mathematics teacher background<br/>Advanced mathematics school background",
  "phys.sch.bckg"                                 = "Advanced physics school background",
  "phys.std.ach"                                  = "Advanced physics student achievement",
  "phys.std.ach.phys.sch.bckg"                    = "Advanced physics student achievement<br/>Advanced physics school background",
  "phys.std.ach.phys.sch.bckg.phys.tch.bckg"      = "Advanced physics student achievement<br/>Advanced physics school background<br/>Advanced physics teacher background",
  "phys.std.ach.phys.tch.bckg"                    = "Advanced physics student achievement<br/>Advanced physics teacher background",
  "phys.std.bckg"                                 = "Advanced physics student background",
  "phys.std.bckg.ach"                             = "Advanced physics student background<br/>Advanced physics student achievement",
  "phys.std.bckg.ach.phys.sch.bckg"               = "Advanced physics student background<br/>Advanced physics student achievement<br/>Advanced physics school background",
  "phys.std.bckg.ach.phys.sch.bckg.phys.tch.bckg" = "Advanced physics student background<br/>Advanced physics student achievement<br/>Advanced physics school background<br/>Advanced physics teacher background",
  "phys.std.bckg.ach.phys.tch.bckg"               = "Advanced physics student background<br/>Advanced physics student achievement<br/>Advanced physics teacher background",
  "phys.std.bckg.phys.sch.bckg"                   = "Advanced physics student background<br/>Advanced physics school background",
  "phys.std.bckg.phys.sch.bckg.phys.tch.bckg"     = "Advanced physics student background<br/>Advanced physics school background<br/>Advanced physics teacher background",
  "phys.std.bckg.phys.tch.bckg"                   = "Advanced physics student background<br/>Advanced physics teacher background",
  "phys.tch.bckg.phys.sch.bckg"                   = "Advanced physics teacher background<br/>Advanced physics school background",
  "prim.tch.bckg"                                 = "Future primary teacher background",
  "sch.bckg"                                      = "School background",
  "sch.bckg.math.tch.bckg"                        = "School background<br/>Mathematics teacher background",
  "sch.bckg.sci.tch.bckg"                         = "School background<br/>Science teacher background",
  "sch.bckg.tch.bckg"                             = "School background<br/>Teacher background",
  "sci.tch.bckg"                                  = "Science teacher background",
  "staff.bckg"                                    = "Staff background",
  "std.AM"                                        = "Student Asian module",
  "std.AM.sch.bckg"                               = "Student Asian module<br/>School background",
  "std.EUM"                                       = "Student European module",
  "std.EUM.sch.bckg"                              = "Student European module<br/>School background",
  "std.LAM"                                       = "Student Latin American module",
  "std.LAM.sch.bckg"                              = "Student Latin American module<br/>School background",
  "std.ach"                                       = "Student achievement",
  "std.ach.AM"                                    = "Student achievement<br/>Asian module",
  "std.ach.AM.sch.bckg"                           = "Student achievement<br/>Asian module<br/>School background",
  "std.ach.EUM"                                   = "Student achievement<br/>European module",
  "std.ach.EUM.sch.bckg"                          = "Student achievement<br/>European module<br/>School background",
  "std.ach.LAM"                                   = "Student achievement<br/>Latin American module",
  "std.ach.LAM.sch.bckg "                         = "Student achievement<br/>Latin American module<br/>School background",
  "std.ach.home"                                  = "Student achievement<br/>Student home background",
  "std.ach.home.sch.bckg"                         = "Student achievement<br/>Student home background<br/>School background",
  "std.ach.home.sch.bckg.tch.bckg"                = "Student achievement<br/>Student home background<br/>School background<br/>Teacher background",
  "std.ach.home.tch.bckg"                         = "Student achievement<br/>Student home background<br/>Teacher background",
  "std.ach.math.tch.bckg"                         = "Student achievement<br/>Mathematics teacher background",
  "std.ach.sch.bckg"                              = "Student achievement<br/>School background",
  "std.ach.sch.bckg.math.tch.bckg"                = "Student achievement<br/>School background<br/>Mathematics teacher background",
  "std.ach.sch.bckg.sci.tch.bckg"                 = "Student achievement<br/>School background<br/>Science teacher background",
  "std.ach.sch.bckg.tch.bckg"                     = "Student achievement<br/>School background<br/>Teacher background",
  "std.ach.sci.tch.bckg"                          = "Student achievement<br/>Science teacher background",
  "std.ach.tch.bckg"                              = "Student achievement<br/>Teacher background",
  "std.bckg"                                      = "Student background",
  "std.bckg.AM"                                   = "Student background<br/>Asian module",
  "std.bckg.AM.sch.bckg"                          = "Student background<br/>Asian module<br/>School background",
  "std.bckg.EUM"                                  = "Student background<br/>European module",
  "std.bckg.EUM.sch.bckg"                         = "Student background<br/>European module<br/>School background",
  "std.bckg.LAM"                                  = "Student background<br/>Latin American module",
  "std.bckg.LAM.sch.bckg"                         = "Student background<br/>Latin American module<br/>School background",
  "std.bckg.ach"                                  = "Student background<br/>Student achievement",
  "std.bckg.ach.AM"                               = "Student background<br/>Student achievement<br/>Asian module",
  "std.bckg.ach.EUM"                              = "Student background<br/>Student achievement<br/>European module",
  "std.bckg.ach.LAM"                              = "Student background<br/>Student achievement<br/>Latin American module",
  "std.bckg.ach.home"                             = "Student background<br/>Student achievement<br/>Student home background",
  "std.bckg.ach.home.sch.bckg"                    = "Student background<br/>Student achievement<br/>Student home background<br/>School background",
  "std.bckg.ach.home.sch.bckg.tch.bckg"           = "Student background<br/>Student achievement<br/>Student home background<br/>School background<br/>Teacher background",
  "std.bckg.ach.home.tch.bckg"                    = "Student background<br/>Student achievement<br/>Student home background<br/>Teacher background",
  "std.bckg.ach.math.tch.bckg"                    = "Student background<br/>Student achievement<br/>Mathematics teacher background",
  "std.bckg.ach.sch.bckg"                         = "Student background<br/>Student achievement<br/>School background",
  "std.bckg.ach.sch.bckg.math.tch.bckg"           = "Student background<br/>Student achievement<br/>School background<br/>Mathematics teacher background",
  "std.bckg.ach.sch.bckg.sci.tch.bckg"            = "Student background<br/>Student achievement<br/>School background<br/>Science teacher background",
  "std.bckg.ach.sch.bckg.tch.bckg"                = "Student background<br/>Student achievement<br/>School background<br/>Teacher background",
  "std.bckg.ach.sci.tch.bckg"                     = "Student background<br/>Student achievement<br/>Science teacher background",
  "std.bckg.ach.tch.bckg"                         = "Student background<br/>Student achievement<br/>Teacher background",
  "std.bckg.home"                                 = "Student background<br/>Student home background",
  "std.bckg.home.sch.bckg"                        = "Student background<br/>Student home background<br/>School background",
  "std.bckg.home.sch.bckg.tch.bckg"               = "Student background<br/>Student home background<br/>School background<br/>Teacher background",
  "std.bckg.home.tch.bckg"                        = "Student background<br/>Student home background<br/>Teacher background",
  "std.bckg.math.tch.bckg"                        = "Student background<br/>Mathematics teacher background",
  "std.bckg.sch.bckg"                             = "Student background<br/>School background",
  "std.bckg.sch.bckg.math.tch.bckg"               = "Student background<br/>School background<br/>Mathematics teacher background",
  "std.bckg.sch.bckg.sci.tch.bckg"                = "Student background<br/>School background<br/>Science teacher background",
  "std.bckg.sch.bckg.tch.bckg"                    = "Student background<br/>School background<br/>Teacher background",
  "std.bckg.sci.tch.bckg"                         = "Student background<br/>Science teacher background",
  "std.bckg.std.ach.AM.sch.bckg"                  = "Student background<br/>Student achievement<br/>Asian module<br/>School background",
  "std.bckg.std.ach.EUM.sch.bckg"                 = "Student background<br/>Student achievement<br/>European module<br/>School background",
  "std.bckg.std.ach.LAM.sch.bckg"                 = "Student background<br/>Student achievement<br/>Latin American module<br/>School background",
  "std.bckg.tch.bckg"                             = "Student background<br/>Teacher background",
  "std.home.sch.bckg"                             = "Student home background<br/>School background",
  "std.home.sch.bckg.tch.bckg"                    = "Student home background<br/>School background<br/>Teacher background",
  "std.home.tch.bckg"                             = "Student home background<br/>Teacher background",
  "tch.bckg"                                      = "Teacher background",
  "std.home"                                      = "Student home background",
  "std.proc"                                      = "Student process data",
  "std.bckg.proc"                                 = "Student background<br/>Student process data",
  "std.ach.proc"                                  = "Student achievement<br/>Student process data",
  "std.proc.sch.bckg"                             = "Student process data<br/>School background",
  "std.proc.tch.bckg"                             = "Student process data<br/>Teacher background",
  "std.proc.math.tch.bckg"                        = "Student process data<br/>Mathematics teacher background",
  "std.proc.sci.tch.bckg"                        = "Student process data<br/>Science teacher background",
  "std.bckg.proc.home"                            = "Student background<br/>Student process data",
  "std.bckg.proc.ach"                             = "Student background<br/>Student achievement<br/>Student process data",
  "std.bckg.proc.sch.bckg"                        = "Student background<br/>Student process data<br/>School background",
  "std.ach.proc.sch.bckg"                         = "Student achievement<br/>Student process data<b/>School background",
  "std.bckg.proc.ach.home"                        = "Student background<br/>Student achievement<br/>Student home background<br/>Student process data",
  "std.bckg.proc.ach.sch.bckg"                    = "Student background<br/>Student achievement<br/>Student process data<br/>School background",
  "std.bckg.proc.home.sch.bckg"                   = "Student background<br/>Student home background<br/>Student process data<br/>School background",
  "std.ach.proc.home.sch.bckg"                    = "Student achievement<br/>Student process data<br/>Student home background<br/>School background",
  "std.bckg.proc.ach.home.sch.bckg"               = "Student background<br/>Student achievement<br/>Student home background<br/>Student process data<br/>School background",
  "std.proc.EUM"                                  = "European module<br/>Student process data",
  "std.proc.LAM"                                  = "Latin American module<br/>Student process data",
  "std.proc.AM"                                  = "Asian module<br/>Student process data",
  "std.ach.proc.EUM"                              = "Student achievement<br/>European module<br/>Student process data",
  "std.ach.proc.LAM"                              = "Student achievement<br/>Latin American module<br/>Student process data",
  "std.ach.proc.AM"                               = "Student achievement<br/>Asian module<br/>Student process data",
  "std.bckg.proc.EUM"                             = "Student background<br/>European module<br/>Student process data",
  "std.bckg.proc.LAM"                             = "Student background<br/>Latin American module<br/>Student process data",
  "std.bckg.proc.AM"                              = "Student background<br/>Asian module<br/>Student process data",
  "std.proc.EUM.sch.bckg"                         = "European module<br/>Student process data<br/>School background",
  "std.proc.LAM.sch.bckg"                         = "Latin American module<br/>Student process data<br/>School background",
  "std.proc.AM.sch.bckg"                          = "Asian module<br/>Student process data<br/>School background",
  "std.bckg.proc.ach.LAM"                         = "Student background<br/>Student achievement<br/>Latin American module<br/>Student process data",
  "std.bckg.proc.ach.EUM"                         = "Student background<br/>Student achievement<br/>European module<br/>Student process data",
  "std.bckg.proc.ach.AM"                          = "Student background<br/>Student achievement<br/>Asian module<br/>Student process data",
  "std.bckg.proc.EUM.sch.bckg"                    = "Student background<br/>European module<br/>Student process data<br/>School background",
  "std.bckg.proc.LAM.sch.bckg"                    = "Student background<br/>Latin American module<br/>Student process data<br/>School background",
  "std.ach.proc.LAM.sch.bckg"                     = "Student achievement<br/>Latin American module<br/>Student process data<br/>School background",
  "std.ach.proc.AM.sch.bckg"                      = "Student achievement<br/>Asian module<br/>Student process data<br/>School background",
  "std.ach.proc.EUM.sch.bckg"                     = "Student achievement<br/>European module<br/>Student process data<br/>School background",
  "std.bckg.std.ach.proc.EUM.sch.bckg"            = "Student background<br/>Student achievement<br/>European module<br/>Student process data<br/>School background",
  "std.bckg.std.ach.proc.LAM.sch.bckg"            = "Student background<br/>Student achievement<br/>Latin American module<br/>Student process data<br/>School background",
  "std.bckg.std.ach.proc.AM.sch.bckg"             = "Student background<br/>Student achievement<br/>Asian module<br/>Student process data<br/>School background",
  "std.proc.home.sch.bckg"                        = "Student home background<br/>Student processdata<br/>School background",
  "std.bckg.proc.tch.bckg"                        = "Student background<br/>Student process data<br/>Teacher background",
  "std.ach.proc.tch.bckg"                         = "Student achievement<br/>Student process data<br/>Teacher background",
  "std.proc.home.tch.bckg"                        = "Student home background<br/>Student process data<br/>Teacher background",
  "std.bckg.proc.home.tch.bckg"                   = "Student background<br/>Student home background<br/>Student process data<br/>Teacher background",
  "std.bckg.proc.ach.tch.bckg"                    = "Student background<br/>Student achievement<br/>Student process data<br/>Teacher background",
  "std.ach.proc.home.tch.bckg"                    = "Student achievement<br/>Student home background<br/>Student process data<br/>Teacher background",
  "std.bckg.proc.sch.bckg.tch.bckg"               = "Student background<br/>Student process data<br/>School background<br/>Teacher background",
  "std.proc.home.sch.bckg.tch.bckg"               = "Student home background<br/>Student process data<br/>School background<br/>Teacher background",
  "std.ach.proc.sch.bckg.tch.bckg"                = "Student achievement<br/>Student process data<br/>School background<br/>Teacher background",
  "std.bckg.proc.ach.sch.bckg.tch.bckg"           = "Student background<br/>Student achievement<br/>Student process data<br/>School background<br/>Teacher background",
  "std.bckg.proc.home.sch.bckg.tch.bckg"          = "Student background<br/>Student home background<br/>Student process data<br/>School background<br/>Teacher background",
  "std.ach.proc.home.sch.bckg.tch.bckg"           = "Student achievement<br/>Student home background<br/>Student process data<br/>School background<br/>Teacher background",
  "std.bckg.proc.ach.home.tch.bckg"               = "Student background<br/>Student achievement<br/>Student home background<br/>Student process data<br/>Teacher background",
  "std.bckg.proc.ach.home.sch.bckg.tch.bckg"      = "Student background<br/>Student achievement<br/>Student home background<br/>Student process data<br/>School backround<br/>Teacher background",
  "sch.bckg.proc.math.tch.bckg"                   = "Student process data<br/>School background<br/>Mathematics teacher data",
  "sch.bckg.proc.sci.tch.bckg"                    = "Student process data<br/>School background<br/>Science teacher data",
  "std.bckg.proc.math.tch.bckg"                   = "Student background<br/>Student process data<br/>Mathematics teacher data",
  "std.bckg.proc.sci.tch.bckg"                    = "Student background<br/>Student process data<br/>Science teacher data",
  "std.ach.proc.math.tch.bckg"                    = "Student achievement<br/>Student process data<br/>Mathematics teacher data",
  "std.ach.proc.sci.tch.bckg"                     = "Student achievement<br/>Student process data<br/>Science teacher data",
  "std.bckg.proc.sch.bckg.math.tch.bckg"          = "Student background<br/>Student process data<br/>School background<br/>Mathematics teacher data",
  "std.bckg.proc.sch.bckg.sci.tch.bckg"           = "Student background<br/>Student process data<br/>School background<br/>Science teacher data",
  "std.ach.proc.sch.bckg.math.tch.bckg"           = "Student achievement<br/>Student process data<br/>School background<br/>Mathematics teacher data",
  "std.ach.proc.sch.bckg.sci.tch.bckg"            = "Student achievement<br/>Student process data<br/>School background<br/>Science teacher data",
  "std.bckg.proc.ach.math.tch.bckg"               = "Student background<br/>Student achievement<br/>Student process data<br/>Mathematics teacher data",
  "std.bckg.proc.ach.sci.tch.bckg"                = "Student background<br/>Student achievement<br/>Student process data<br/>Science teacher data",
  "std.bckg.proc.ach.sch.bckg.math.tch.bckg" = "Student background<br/>Student achievement<br/>Student process data<br/>School background<br/>Mathematics teacher data",
  "std.bckg.proc.ach.sch.bckg.sci.tch.bckg" = "Student background<br/>Student achievement<br/>Student process data<br/>School background<br/>Science teacher data"
)
