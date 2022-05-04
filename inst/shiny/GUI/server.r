server <- function(input, output, session) {
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # Define common objects
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
      atg = c("TCHWGT", paste0("TRWGT", 1:100)),
      alg = c("CNTRWGT", paste0("CRWGT", 1:92)),
      bsa = c("TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP"),
      bcg = c("TOTWGTC", paste0("CRWGT", 1:75), "JKZONEC", "JKREPC", "SCHWGT", "STOTWGTL", "STOTWGTU", "STOTWGTE", "JKCZONE", "JKCREP", paste0("SRWGT", 1:100)),
      bsg = c("TOTWGTS", paste0("SRWGT", 1:75), "JKZONES", "JKREPS", "TOTWGT", "HOUWGT", "SENWGT", "JKZONE", "JKREP", "STAFFWGT", paste0("SRWGT", 1:92)),
      btg = c("TOTWGTT", paste0("TRWGT", 1:75), "JKZONET", "JKREPT", "TCHWGT", paste0("TRWGT", 1:100)),
      btm = c("MTOTWGT", "JKZONE", "JKREP"),
      bts = c("STOTWGT", "JKZONE", "JKREP"),
      bst = c("MATWGT", "SCIWGT", "TCHWGT", "TOTWGT", "JKZONE", "JKREP"),
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
        G4 = c("ASMPV", "ASSPV", "ASMMAT", "ASMWHO", "ASMFAP", "ASMGEM", "ASMDAP", "ASSSCI", "ASSEAS", "ASSLIS", "ASSPHS", "ASMALG", "ASMFNS", "ASMGEO", "ASMMEA", "ASSPHY", "ASMAPP", "ASMKNO", "ASMREA", "ASMDAT", "ASMNUM", "ASSEAR", "ASSLIF", "ASSKNO", "ASSAPP", "ASSREA"),
        G8 = c("BSMMAT", "BSSSCI", "BSMALG", "BSMDAP", "BSMFNS", "BSMGEO", "BSMMEA", "BSSCHE", "BSSEAS", "BSSLIS", "BSSPHY", "BSSERI", "BSSNOS", "BSMNBM", "BSSNBM", "BSMAPP", "BSMKNO", "BSMREA", "BSMDAT", "BSMNUM", "BSSEAR", "BSSBIO", "BSSKNO", "BSSAPP", "BSSREA")
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
        fifteen.year.old = c("PV[[:digit:]]+MATH", "PV[[:digit:]]+READ", "PV[[:digit:]]+SCIE", "PV[[:digit:]]+PROB", "PV[[:digit:]]+INTR", "PV[[:digit:]]+SUPP", "PV[[:digit:]]+EPS", "PV[[:digit:]]+ISI", "PV[[:digit:]]+USE", "PV[[:digit:]]+MACC", "PV[[:digit:]]+MACQ", "PV[[:digit:]]+MACS", "PV[[:digit:]]+MACU", "PV[[:digit:]]+MAPE", "PV[[:digit:]]+MAPF", "PV[[:digit:]]+MAPI", "PV[[:digit:]]+SCEP", "PV[[:digit:]]+SCED", "PV[[:digit:]]+SCID", "PV[[:digit:]]+SKCO", "PV[[:digit:]]+SKPE", "PV[[:digit:]]+SSPH", "PV[[:digit:]]+SSLI", "PV[[:digit:]]+SSES", "PV[[:digit:]]+GLCM", "PV[[:digit:]]+RCLI", "PV[[:digit:]]+RCUN", "PV[[:digit:]]+RCER", "PV[[:digit:]]+RTSN", "PV[[:digit:]]+RTML")
      )
    )
  )
  
  default.benchmarks <- list(
    ICCS = list(
      "2009" = c(395, 479, 563),
      "2016" = c(311, 395, 479, 563)
    ),
    ICILS = c(407.001, 492.001, 576.001, 661.001),
    PIRLS = c(400, 475, 550, 625),
    ePIRLS = c(400, 475, 550, 625),
    prePIRLS = c(400, 475, 550, 625),
    TIMSS = c(400, 475, 550, 625),
    preTIMSS = c(400, 475, 550, 625),
    "eTIMSS PSI" = c(400, 475, 550, 625),
    "TIMSS Advanced" = c(475, 550, 625),
    TiPi = c(400, 475, 550, 625),
    PISA = list(
      Reading = list(
        "2018" = c(189.33, 262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32),
        "2015" = c(262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32),
        "2012" = c(262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32),
        "2009" = c(262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32),
        "2006" = c(334.75, 407.47, 480.18, 552.89, 625.61),
        "2003" = c(334.75, 407.47, 480.18, 552.89, 625.61),
        "2000" = c(334.75, 407.47, 480.18, 552.89, 625.61)
      ),
      Science = list(
        "2018" = c(260.54, 334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
        "2015" = c(260.54, 334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
        "2012" = c(334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
        "2009" = c(334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
        "2006" = c(334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
        "2003" = c(334.94, 409.54, 484.14, 558.73, 633.33, 707.93),
        "2000" = c(334.94, 409.54, 484.14, 558.73, 633.33, 707.93)
      ),
      Mathematics = list(
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
        "2015" = c(325.57, 400.33, 475.10, 549.86, 624.63)
      ),
      Global.Competency = list(
        "2018" = c(340, 440, 540, 640)
      ),
      Reading.root.PVs = c("PV#READ", "PV#READ1", "PV#READ2", "PV#READ3", "PV#READ4", "PV#READ5", "PV#RCLI", "PV#RCUN", "PV#RCER", "PV#RTSN", "PV#RTML"),
      Science.root.PVs = c("PV#SCIE", "PV#INTR", "PV#SUPP", "PV#EPS", "PV#ISI", "PV#USE", "PV#SCEP", "PV#SCED", "PV#SCID", "PV#SKCO", "PV#SKPE", "PV#SSPH", "PV#SSLI", "PV#SSES"),
      Mathematics.root.PVs = c("PV#MATH", "PV#MATH1", "PV#MATH2", "PV#MATH3", "PV#MATH4", "PV#MACC", "PV#MACQ", "PV#MACS", "PV#MACU", "PV#MAPE", "PV#MAPF", "PV#MAPI"),
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
      c("acg", "asa", "atg", "std.ach.sch.bckg.tch.bck"),
      c("acg", "asg", "ash", "asa", "std.bckg.ach.home.sch.bckg"),
      c("acg", "asg", "asa", "atg", "std.bckg.ach.sch.bckg.tch.bckg"),
      c("acg", "asg", "ash", "atg", "std.bckg.home.sch.bckg.tch.bckg"),
      c("acg", "ash", "asa", "atg", "std.ach.home.sch.bckg.tch.bckg"),
      c("asg", "ash", "asa", "atg", "std.bckg.ach.home.tch.bckg"),
      c("acg", "asg", "ash", "asa", "atg", "std.bckg.ach.home.sch.bckg.tch.bckg")
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
      c("acg", "asa", "atg", "std.ach.sch.bckg.tch.bck"),
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
      c("acg", "asa", "atg", "std.ach.sch.bckg.tch.bck"),
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
      c("acg", "asa", "atg", "std.ach.sch.bckg.tch.bck"),
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
                                    "std.bckg.AM",
                                    "std.bckg.LAM",
                                    "std.bckg.ach.EUM",
                                    "std.bckg.ach.AM",
                                    "std.bckg.ach.LAM",
                                    "std.ach.sch.bckg",
                                    "std.bckg.EUM",
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
                                    "phys.std.bckg.ach.phys.sch.bckg"),
    IEA.JK2.dflt.std.bckg.wgts = c("TOTWGT",
                                   "TOTWGTS"),
    IEA.JK2.dflt.std.bckg.zones = c("JKZONE",
                                    "JKZONES"),
    IEA.JK2.dflt.std.bckg.rep.ind = c("JKREP",
                                      "JKREPS",
                                      "JKINDIC"),
    IEA.JK2.dflt.sch.bckg.types = c("sch.bckg",
                                    "sch.bckg.tch.bckg",
                                    "sch.bckg.math.tch.bckg",
                                    "sch.bckg.sci.tch.bckg",
                                    "std.home.sch.bckg",
                                    "math.sch.bckg",
                                    "math.sch.bckg.math.tch.bckg",
                                    "phys.sch.bckg",
                                    "phys.sch.bckg.phys.tch.bckg"),
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
                                    "sch.bckg.sci.tch.bckg"),
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
  
  
  ######################################################
  # From Global
  load.app.CSS.screen <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 1;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"


jscode.close.RALSA.GUI <- "shinyjs.closeWindow = function() { window.close(); }"


jscode.scroll.tab.to.top <- 'shinyjs.scrolltop = function() {window.scrollTo(0, 0);}'


country.ISO.and.names <- data.table(ISOs = c("AAD", "ABA", "ADU", "ALB", "ARE", "ARG", "ARM", "AUS", "AUT", "AZE", "BEL", "BFA", "BFL", "BFR", "BGR", "BHR", "BIH", "BLZ", "BRA", "BSQ", "BWA", "CAB", "CAN", "CBC", "CHE", "CHL", "CNL", "CNS", "COL", "COT", "CQU", "CRI", "CSH", "CSK", "CYP", "CZE", "DEU", "DN3", "DNK", "DNW", "DOM", "DZA", "EAN", "ECN", "ECT", "ECU", "EGY", "EMA", "EMB", "ENG", "ESP", "EST", "ETH", "FI7", "FIN", "FRA", "GBR", "GEO", "GHA", "GMX", "GRC", "GTM", "HKG", "HND", "HRV", "HUN", "IDN", "IND", "IRL", "IRN", "IS5", "ISL", "ISR", "ITA", "JOR", "JPN", "KAZ", "KEN", "KOR", "KWT", "LBN", "LIE", "LTU", "LUX", "LVA", "MA6", "MAC", "MAR", "MDA", "MDF", "MET", "MEX", "MJA", "MKD", "MLN", "MLT", "MNE", "MNG", "MNL", "MQR", "MSL", "MTM", "MXT", "MYS", "NIC", "NIR", "NLD", "NLN", "NO1", "NO2", "NO3", "NO4", "NO5", "NO8", "NOM", "NOR", "NZ1", "NZL", "OMN", "PAK", "PER", "PHL", "PO2", "POL", "PRT", "PRY", "PSE", "QAT", "RMO", "ROM", "ROU", "RTR", "RUM", "RUS", "RWA", "SAU", "SCG", "SCO", "SE3", "SG7", "SGP", "SLV", "SRB", "SVK", "SVN", "SWE", "SYR", "TDF", "THA", "TJA", "TMX", "TNL", "TQR", "TSL", "TTM", "TTO", "TUN", "TUR", "TWN", "UAL", "UCA", "UCO", "UCT", "UFL", "UGA", "UIN", "UK1", "UKR", "UMA", "UMN", "UNC", "URY", "USA", "UZB", "VNM", "XKX", "YE6", "YEM", "ZA4", "ZA5", "ZAF", "ZGT", "ZWC"), Names = c("United Arab Emirates (Abu Dhabi)", "Argentina, Buenos Aires", "United Arab Emirates (Dubai)", "Albania", "United Arab Emirates", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan, Republic of", "Belgium", "Burkina Faso", "Belgium (Flemish)", "Belgium (French)", "Bulgaria", "Bahrain", "Bosnia and Herzegovina", "Belize", "Brazil", "Spain (Basque Country)", "Botswana", "Canada (Alberta)", "Canada", "Canada (British Columbia)", "Switzerland", "Chile", "Canada (Newfoundland and Labrador)", "Canada (Nova Scotia)", "Colombia", "Canada (Ontario)", "Canada (Quebec)", "Costa Rica", "China (Shanghai)", "Czech Republic", "Cyprus", "Czech Republic", "Germany", "Denmark (Grade 3)", "Denmark", "Germany, North-Rhine Westphalia", "Dominican Republic", "Algeria", "Spain (Andalucia)", "Spain (Canary Islands)", "Spain (Catalonia)", "Ecuador", "Egypt", "Spain, Madrid", "Spain, Madrid, Bilingual", "England", "Spain", "Estonia", "Ethiopia", "Finland (Grade 7)", "Finland", "France", "United Kingdom", "Georgia", "Ghana", "Mexico (Generales/Tecnicas/Privadas)", "Greece", "Guatemala", "Hong Kong, SAR", "Honduras, Republic of", "Croatia", "Hungary", "Indonesia", "India", "Ireland", "Iran, Islamic Republic of", "Iceland (Grade 5)", "Iceland", "Israel", "Italy", "Jordan", "Japan", "Kazakhstan", "Kenya", "Korea, Republic of", "Kuwait", "Lebanon", "Liechtenstein", "Lithuania", "Luxembourg", "Latvia", "Morocco (Grade 6)", "Macao SAR", "Morocco", "Moldova", "Mexico (Distrito Federal)", "Mexico (International Telesecundaria)", "Mexico", "Mexico (Jalisco)", "North Macedonia", "Malta (Maltese)", "Malta", "Montenegro", "Mongolia", "Mexico (Nuevo Leon)", "Mexico (Quintana Roo)", "Mexico (San Luis Potosi)", "Mexico (Tamaulipas)", "Mexico (Talis-Nacional)", "Malaysia", "Nicaragua", "Northern Ireland", "Netherlands", "The Netherlands (50 additional schools)", "Norway (ALU)", "Norway (ALU +)", "Norway (PPU)", "Norway (4)", "Norway (Grade 5)", "Norway (8)", "Norway (MASTERS)", "Norway", "New Zealand (TIMSS data processing)", "New Zealand", "Oman", "Pakistan", "Peru", "Philippines", "Poland (Second-Cycle Programs)", "Poland", "Portugal", "Paraguay", "Palestinian National Authority", "Qatar", "Russian Federation, Moscow", "Romania", "Romania", "Russia (8+ sample)", "Russian Federation (Moscow)", "Russian Federation", "Rwanda", "Saudi Arabia", "Serbia", "Scotland", "Sweden (Grade 3)", "Singapore (Chinese Grade 7)", "Singapore", "El Salvador", "Serbia", "Slovak Republic", "Slovenia", "Sweden", "Syria, Arab Republic of", "Mexico (Telesecundaria-Distrito Federal)", "Thailand", "Mexico (Telesecundaria-Jalisco)", "Mexico (Telesecundarias)", "Mexico (Telesecundaria-Nuevo Leon)", "Mexico (Telesecundaria-Quintana Roo)", "Mexico (Telesecundaria-San Luis Potosi)", "Mexico (Telesecundaria-Tamaulipas)", "Trinidad And Tobago", "Tunisia", "Turkey", "Chinese Taipei", "United States (Alabama)", "United States (California)", "United States (Colorado)", "United States (Connecticut)", "United States (Florida)", "Uganda", "United States (Indiana)", "England and Northern Ireland (UK)", "Ukraine", "United States (Massachusetts)", "United States (Minnesota)", "United States (North Carolina)", "Uruguay", "United States", "Uzbekistan", "Vietnam", "Kosovo", "Yemen (Grade 6)", "Yemen", "South Africa (Grade 4)", "South Africa (Eng/Afr)", "South Africa", "South Africa (Gauteng)", "South Africa (Western Cape Province)"))




PISA.data.files <- list(
  PISA.pre2015.TXT.files = list(
    "2000" = c("intcogn_v4.txt",
               "intscho.txt",
               "intstud_math_v3.txt",
               "intstud_read_v3.txt",
               "intstud_scie_v3.txt"),
    "2003" = c("INT_cogn_2003_v2.txt",
               "INT_schi_2003.txt",
               "INT_stui_2003_v2.txt"),
    "2006" = c("INT_Cogn06_S_Dec07.txt",
               "INT_Cogn06_T_Dec07.txt",
               "INT_Par06_Dec07.txt",
               "INT_Sch06_Dec07.txt",
               "INT_Stu06_Dec07.txt"),
    "2009" = c("INT_COG09_S_DEC11.txt",
               "INT_COG09_TD_DEC11.txt",
               "INT_PAR09_DEC11.txt",
               "INT_SCQ09_Dec11.txt",
               "INT_STQ09_DEC11.txt"),
    "2012" = c("INT_COG12_DEC03.txt",
               "INT_COG12_S_DEC03.txt",
               "INT_PAQ12_DEC03.txt",
               "INT_SCQ12_DEC03.txt",
               "INT_STU12_DEC03.txt")
  ),
  
  PISA.pre2015.SPS.files = list(
    "2000" = c("PISA2000_SPSS_cognitive_item.sps",
               "PISA2000_SPSS_school_questionnaire.sps",
               "PISA2000_SPSS_student_mathematics.sps",
               "PISA2000_SPSS_student_reading.sps",
               "PISA2000_SPSS_student_science.sps"),
    "2003" = c("PISA2003_SPSS_cognitive_item.sps",
               "PISA2003_SPSS_school.sps",
               "PISA2003_SPSS_student.sps"),
    "2006" = c("PISA2006_SPSS_scored_cognitive_item.sps",
               "PISA2006_SPSS_cognitive_item.sps",
               "PISA2006_SPSS_parent.sps",
               "PISA2006_SPSS_school.sps",
               "PISA2006_SPSS_student.sps"),
    "2009" = c("PISA2009_SPSS_score_cognitive_item.sps",
               "PISA2009_SPSS_cognitive_item.sps",
               "PISA2009_SPSS_parent.sps",
               "PISA2009_SPSS_school.sps",
               "PISA2009_SPSS_student.sps"),
    "2012" = c("PISA2012_SPSS_cognitive_item.sps",
               "PISA2012_SPSS_scored_cognitive_item.sps",
               "PISA2012_SPSS_parent.sps",
               "PISA2012_SPSS_school.sps",
               "PISA2012_SPSS_student.sps")
  ),
  
  PISA.2015.plus.SPSS.files = list(
    "2015" = c("CY6_MS_CM2_SCH_QQQ.sav",
               "CY6_MS_CM2_STU_COG.sav",
               "CY6_MS_CM2_STU_QQQ.sav",
               "CY6_MS_CM2_STU_QTM.sav",
               "CY6_MS_CM2_TCH_QQQ.sav",
               "CY6_MS_CMB_SCH_QQQ.sav",
               "Cy6_ms_cmb_stu_cog.sav",
               "CY6_MS_CMB_STU_CPS.sav",
               "CY6_MS_CMB_STU_FLT.sav",
               "CY6_MS_CMB_STU_QQ2.sav",
               "CY6_MS_CMB_STU_QQQ.sav",
               "Cy6_ms_cmb_stu_qtm.sav",
               "Cy6_ms_cmb_tch_qqq.sav"),
    "2018" = c("CY07_MSU_SCH_QQQ.sav",
               "CY07_MSU_STU_COG.sav",
               "CY07_MSU_STU_QQQ.sav",
               "CY07_MSU_STU_TIM.sav",
               "CY07_MSU_TCH_QQQ.sav",
               "CY07_VNM_STU_COG.sav",
               "CY07_VNM_STU_PVS.sav")
  )
)



PISA.for.Development.data.files <- list(
  PISA.for.Development.2019.files = list(
    "2019" = c("CY1MDAI_SCH_QQQ.sav",
               "CY1MDAI_STU_COG.sav",
               "CY1MDAI_STU_QQQ.sav",
               "CY1MDAI_TCH_QQQ.sav",
               "CY1MDCI_COG.SAV",
               "CY1MDCI_QQQ.SAV",
               "CY1MDCI_TIM.SAV")
  )
)




studies.and.cycles <- list(
  TIMSS = list(
    first.chars = c("acg", "asa", "asg", "ash", "asr", "ast", "atg", "bcg", "bsa", "bsg", "bsr", "bst", "btm", "bts"), "1995" = "m1", "1999" = "m2", "2003" = "m3", "2007" = "m4", "2011" = "m5", "2015" = "m6", "2019" = "m7", "2023" = "m8", "2027" = "m9"
  ),
  preTIMSS = list(
    first.chars = c("acg", "asa", "asg", "ash", "asr", "ast", "atg"), "2015" = "n1"
  ),
  "eTIMSS PSI" = list(
    first.chars = c("acg", "asa", "asg", "ash", "asr", "ast", "atg", "bcg", "bsa", "bsg", "bsr", "bst", "btm", "bts"), "2019" = "z7", "2023" = "z8", "2027" = "z9"
  ),
  "TIMSS Advanced" = list(
    first.chars = c("mcg", "msa", "msg", "msr", "mst", "mtg", "pcg", "psa", "psg", "psr", "pst", "ptg"), "1995" = "m1", "2008" = "m2", "2015" = "m3"
  ),
  PIRLS = list(
    first.chars = c("acg", "asa", "asg", "ash", "asr", "ast", "atg"), "2001" = "r1", "2006" = "r2", "2011" = "r3", "2016" = "r4", "2021" = "r5", "2026" = "r6"
  ),
  prePIRLS = list(
    first.chars = c("acg", "asa", "asg", "ash", "asr", "ast", "atg"), "2011" = "l1", "2016" = "l2"
  ),
  ePIRLS = list(
    first.chars = c("acg", "asa", "asg", "ash", "asr", "ast", "atg"), "2016" = "e1"
  ),
  TiPi = list(
    first.chars = c("acg", "asa", "asg", "ash", "ast", "atg"), "2011" = "b1"
  ),
  RLII = list(
    first.chars = "asc", "1991" = "t1", "2001" = "t2"
  ),
  SITES = list(
    first.chars = c("axg", "bxg", "cxg", "bcg", "btm", "bts"), "1998" = "s0", "2006" = "s1"
  ),
  CivED = list(
    first.chars = c("bc_", "bl_", "bs_", "bt_", "cs_"), "1999" = "f2"
  ),
  ICCS = list(
    first.chars = c("icg", "isa", "ise", "isg", "isl", "isr", "iss", "itg", "jsa", "jse", "jsg", "jsl", "jss", "jsr"), "2009" = "c2", "2016" = "c3"
  ),
  ICILS = list(
    first.chars = c("bcg", "bsg", "btg"), "2013" = "i1", "2018" = "i2"
  ),
  REDS = list(
    first.chars = c("bcg", "bsg", "btg"), "2021" = "v1"
  ),
  TALIS = list(
    first.chars = c("acg", "atg", "bcg", "btg", "ccg", "ctg", "pcg", "ptg"), "2008" = "t1", "2013" = "t2", "2018" = "t3"
  ),
  "TALIS 3S" = list(
    first.chars = c("alg", "asg", "blg", "bsg"), "2018" = "s1"
  ),
  "TEDS-M" = list(
    first.chars = c("dig", "deg", "dpg", "dsg", "dpr", "dsr"), "2008" = "t1"
  )
)




respondents.and.cycles <- list(
  
  "Student background" = list(
    resp.type = c("asc", "asg", "bsg", "isg", "jsg", "bs_", "cs_"),
    round = c(
      "b1",
      "c2", "c3", "c4", "c5", "c6",
      "e1", "e2", "e3", "e4", "e5", "e6",
      "f2",
      "i1", "i2", "i3", "i4", "i5", "i6",
      "v1",
      "l1", "l2", "l3", "l4", "l5", "l6",
      "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9",
      "n1", "n2", "n3", "n4", "n5", "n6",
      "z7", "z8", "z9",
      "r1", "r2", "r3", "r4", "r5", "r6", "r7",
      "t1", "t2"
    )
  ),
  
  "Mathematics student background" = list(
    resp.type = "msg",
    round = c("m1", "m2", "m3", "m4")
  ),
  
  "Physics student background" = list(
    resp.type = "psg",
    round = c("m1", "m2", "m3", "m4")
  ),
  
  "Student achievement items" = list(
    resp.type = c("asa", "bsa", "isa", "jsa"),
    round = c(
      "c2", "c3", "c4", "c5", "c6",
      "e1", "e2", "e3", "e4", "e5", "e6",
      "l1", "l2", "l3", "l4", "l5", "l6",
      "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9",
      "n1", "n2", "n3", "n4", "n5", "n6",
      "z7", "z8", "z9",
      "r1", "r2", "r3", "r4", "r5", "r6", "r7"
    )
  ),
  
  "Student home" = list(
    resp.type = "ash",
    round = c(
      "b1",
      "e1", "e2", "e3", "e4", "e5", "e6",
      "l1", "l2", "l3", "l4", "l5", "l6",
      "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9",
      "n1", "n2", "n3", "n4", "n5", "n6",
      "z7", "z8", "z9",
      "r1", "r2", "r3", "r4", "r5", "r6", "r7"
    )
  ),
  
  "European student module" = list(
    resp.type = "ise",
    round = c("c2", "c3", "c4", "c5", "c6")
  ),
  
  "Latin American student module" = list(
    resp.type = "isl",
    round = c("c2", "c3", "c4", "c5", "c6")
  ),
  
  "Asian student module" = list(
    resp.type = "iss",
    round = c("c2", "c3", "c4", "c5", "c6")
  ),
  
  "Teacher background" = list(
    resp.type = c("atg", "btg", "ctg", "ptg", "itg", "bt_"),
    round = c(
      "b1",
      "c2", "c3", "c4", "c5", "c6",
      "e1", "e2", "e3", "e4", "e5", "e6",
      "f2",
      "i1", "i2", "i3", "i4", "i5", "i6",
      "v1",
      "l1", "l2", "l3", "l4", "l5", "l6",
      "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9",
      "n1", "n2", "n3", "n4", "n5", "n6",
      "z7", "z8", "z9",
      "r1", "r2", "r3", "r4", "r5", "r6", "r7",
      "t1", "t2", "t3", "t4", "t5", "t6"
    )
  ),
  
  "Mathematics teacher background" = list(
    resp.type = c("btm", "mtg"),
    round = c(
      "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9",
      "z7", "z8", "z9",
      "s1"
    )
  ),
  
  "Physics teacher background" = list(
    resp.type = "ptg",
    round = c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9")
  ),
  
  "Science teacher background" = list(
    resp.type = "bts",
    round = c(
      "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9",
      "z7", "z8", "z9",
      "s1"
    )
  ),
  
  "School background" = list(
    resp.type = c("acg", "bcg", "ccg", "pcg", "icg", "bc_"),
    round = c(
      "f2",
      "c2", "c3", "c4", "c5", "c6",
      "i1", "i2", "i3", "i4", "i5", "i6",
      "v1",
      "e1", "e2", "e3", "e4", "e5", "e6",
      "l1", "l2", "l3", "l4", "l5", "l6",
      "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9",
      "z7", "z8", "z9",
      "r1", "r2", "r3", "r4", "r5", "r6", "r7",
      "s1",
      "t1", "t2", "t3", "t4", "t5", "t6"
    )
  ),
  
  "Mathematics school background" = list(
    resp.type = "mcg",
    round = c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9")
  ),
  
  "Physics school background" = list(
    resp.type = "pcg",
    round = c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9")
  ),
  
  "Leader background data" = list(
    resp.type = c("alg", "blg"),
    round = c("s1", "s2", "s3", "s4")
  ),
  
  "Staff background data" = list(
    resp.type = c("asg", "bsg"),
    round = c("s1", "s2", "s3", "s4")
  ),
  
  "Institutional program background" = list(
    resp.type = "dig",
    round = "t1"
  ),
  
  "Educator background" = list(
    resp.type = "deg",
    round = "t1"
  ),
  
  "Future primary teacher background" = list(
    resp.type = "dpg",
    round = "t1"
  ),
  
  "Future lower-secondary teacher background" = list(
    resp.type = "dsg",
    round = "t1"
  )
  
)





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
  "std.ach.sch.bckg.tch.bck"                      = "Student achievement<br/>School background<br/>Teacher background",
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
  "std.home"                                      = "Student home background"
)





extract.IEA.study.and.cycle <- function(list.abbrev, file.string) {
  
  tmp <- lapply(X = list.abbrev, FUN = function(i) {
    
    study.name <- file.string[1][file.string[1] %in% i[["first.chars"]]]
    
    study.cycle <- file.string[2][file.string[2] %in% i[names(i) != "first.chars"]]
    
    c(study.name, study.cycle)
    
  })
  
  tmp <- Filter(function(i) {length(i) > 1}, tmp)
  
  study.ID <- names(tmp)
  
  cycle.ID <- names(unlist(list.abbrev[[study.ID]])[unlist(list.abbrev[[study.ID]]) == tmp[[names(tmp)]][2]])
  
  list(study.ID, cycle.ID)
}





extract.PISA.2015.plus.study.and.cycle <- function(list.abbrev, file.string) {
  
  tmp <- Filter(length, lapply(list.abbrev, function(i) {
    Filter(isTRUE, lapply(X = i, FUN = function(j) {
      all(grepl(pattern = file.string, x = j, ignore.case = TRUE) == TRUE)
    }))
  }))
  
  cycle.ID <- sapply(X = tmp, FUN = names)
  
  if(length(cycle.ID) == 1) {
    cycle.ID <- cycle.ID
  } else {
    cycle.ID <- "Unknown"
  }
  
  if(length(cycle.ID) == 1 && cycle.ID %in% unlist(sapply(list.abbrev, names))) {
    study.ID <- "PISA"
  } else {
    study.ID <- "Unknown"
  }
  
  list(study.ID, cycle.ID)
}




extract.PISA.pre.2015.study.and.cycle <- function(list.abbrev, file.string) {
  
  tmp <- unlist(Filter(length, lapply(list.abbrev, function(i) {
    Filter(length, lapply(X = i, FUN = function(j) {
      grep(pattern = file.string, x = j, ignore.case = TRUE, value = TRUE)
    }))
  })))
  
  study.ID <- unique(substr(x = unlist(tmp), start = 1, stop = 4))
  cycle.ID <- unique(substr(x = unlist(tmp), start = 5, stop = 8))
  
  if(length(cycle.ID) == 1) {
    cycle.ID <- cycle.ID
  } else {
    cycle.ID <- "Unknown"
  }
  
  if(length(cycle.ID) == 1 && cycle.ID %in% unlist(sapply(list.abbrev, names))) {
    study.ID <- "PISA"
  } else {
    study.ID <- "Unknown"
  }
  
  list(study.ID, cycle.ID)
}



extract.PISA.for.Development.study.and.cycle <- function(list.abbrev, file.string) {
  
  tmp <- Filter(length, lapply(list.abbrev, function(i) {
    Filter(isTRUE, lapply(X = i, FUN = function(j) {
      all(grepl(pattern = file.string, x = j, ignore.case = TRUE) == TRUE)
    }))
  }))
  
  cycle.ID <- sapply(X = tmp, FUN = names)
  
  if(length(cycle.ID) == 1) {
    cycle.ID <- cycle.ID
  } else {
    cycle.ID <- "Unknown"
  }
  
  if(length(cycle.ID) == 1 && cycle.ID %in% unlist(sapply(list.abbrev, names))) {
    study.ID <- "PISA for Development"
  } else {
    study.ID <- "Unknown"
  }
  
  list(study.ID, cycle.ID)
}




all.available.PVs <- c("ASMPV", "ASSPV", "ASMMAT", "ASMWHO", "ASMFAP", "ASMGEM", "ASMDAP", "ASSSCI", "ASSEAS", "ASSLIS", "ASSPHS", "ASMALG", "ASMFNS", "ASMGEO", "ASMMEA", "ASSPHY", "ASMAPP", "ASMKNO", "ASMREA", "ASMDAT", "ASMNUM", "ASSEAR", "ASSLIF", "ASSKNO", "ASSAPP", "ASSREA", "BSMMAT", "BSSSCI", "BSMALG", "BSMDAP", "BSMFNS", "BSMGEO", "BSMMEA", "BSSCHE", "BSSEAS", "BSSLIS", "BSSPHY", "BSSERI", "BSSNOS", "BSMNBM", "BSSNBM", "BSMAPP", "BSMKNO", "BSMREA", "BSMDAT", "BSMNUM", "BSSEAR", "BSSBIO", "BSSKNO", "BSSAPP", "BSSREA", "PSPPHY", "PSPELE", "PSPMEC", "PSPWAV", "PSPAPP", "PSPKNO", "PSPREA", "MSMMAT", "MSMALG", "MSMCAL", "MSMGEO", "MSMKNO", "MSMAPP", "MSMREA", "ASRREA", "ASRINF", "ASRLIT", "ASRIIE", "ASRRSI", "ASEREA", "ASERSI", "ASEIIE", "ASRDOC", "ASREXP", "ASRNAR", "PV[[:digit:]]+CIV", "PV[[:digit:]]+CIL", "PV[[:digit:]]+CT", "PV[[:digit:]]+MATH", "PV[[:digit:]]+READ", "PV[[:digit:]]+SCIE", "PV[[:digit:]]+PROB", "PV[[:digit:]]+INTR", "PV[[:digit:]]+SUPP", "PV[[:digit:]]+EPS", "PV[[:digit:]]+ISI", "PV[[:digit:]]+USE", "PV[[:digit:]]+MACC", "PV[[:digit:]]+MACQ", "PV[[:digit:]]+MACS", "PV[[:digit:]]+MACU", "PV[[:digit:]]+MAPE", "PV[[:digit:]]+MAPF", "PV[[:digit:]]+MAPI", "PV[[:digit:]]+SCEP", "PV[[:digit:]]+SCED", "PV[[:digit:]]+SCID", "PV[[:digit:]]+SKCO", "PV[[:digit:]]+SKPE", "PV[[:digit:]]+SSPH", "PV[[:digit:]]+SSLI", "PV[[:digit:]]+SSES", "PV[[:digit:]]+GLCM", "PV[[:digit:]]+RCLI", "PV[[:digit:]]+RCUN", "PV[[:digit:]]+RCER", "PV[[:digit:]]+RTSN", "PV[[:digit:]]+RTML")




collapse.loaded.file.PV.names <- function(PV.vector, vars.object) {
  
  if(length(PV.vector) > 0) {
    tmp <- vars.object[get(colnames(vars.object)[1]) %in% PV.vector, ]
  } else {
    tmp <- NULL
  }
  
  if(!is.null(tmp) && length(grep(pattern = "[[:digit:]]+$", x = tmp[ , get(colnames(tmp)[1])])) > 0) {
    tmp[ , colnames(tmp)[1] := gsub(pattern = "[[:digit:]]+$", replacement = "", x = get(colnames(tmp)[1]))]
  } else if(!is.null(tmp) && length(grep(pattern = "[[:digit:]]+$", x = tmp[ , get(colnames(tmp)[1])])) == 0) {
    tmp[ , colnames(tmp)[1] := gsub(pattern = "[[:digit:]]+", replacement = "#", x = get(colnames(tmp)[1]))]
  }
  
  
  if(!is.null(tmp) && nrow(tmp) > 0) {
    tmp <- split(x = tmp, by = "Variables")
    lapply(X = tmp, FUN = function(i) {
      i[ , colnames(i)[2] := paste0("1 to ", nrow(i), " PV: ", get(colnames(i)[2]))]
    })
    tmp <- rbindlist(l = tmp)
    tmp <- unique(x = tmp, by = colnames(tmp)[1])
    return(tmp)
  }
}





define.default.weight <- function(study, loaded.names.and.labels, respondent.type) {
  
  
  study.type <- names(Filter(isTRUE, sapply(X = design.weight.variables[c("IEA.JK2.studies", "IEA.BRR.studies", "OECD.BRR.studies")], FUN = function(i) {
    study %in% i
  })))
  
  resp.type <- names(Filter(isTRUE, sapply(X = design.weight.variables[c("IEA.JK2.dflt.std.bckg.types", "IEA.JK2.dflt.sch.bckg.types", "IEA.JK2.dflt.tch.bckg.types", "IEA.BRR.dflt.inst.bckg.types", "IEA.BRR.dflt.prim.tch.bckg.types", "IEA.BRR.dflt.low_sec.tch.bckg.types", "IEA.BRR.dflt.educ.bckg.types", "OECD.BRR.dflt.std.bckg", "OECD.BRR.dflt.sch.bckg", "OECD.BRR.dflt.tch.bckg", "OECD.BRR.dflt.lead.bckg", "OECD.BRR.dflt.staff.bckg")], FUN = function(i) {
    respondent.type %in% i
  })))
  
  org.and.design.resp.type <- str_extract(string = study.type, pattern = "^[[:alpha:]]+\\.[[:alpha:]]+[[:digit:]]*")
  
  resp.type <- grep(pattern = org.and.design.resp.type, x = resp.type, value = TRUE)
  
  if(length(resp.type) == 0) {
    tmp.wgt <- NULL
  } else if(study.type == "IEA.JK2.studies" && resp.type == "IEA.JK2.dflt.std.bckg.types") {
    tmp.wgt <- grep(pattern = paste(design.weight.variables[["IEA.JK2.dflt.std.bckg.wgts"]], collapse = "|"), x = loaded.names.and.labels[ , Variables], value = TRUE)
  } else if(study.type == "IEA.JK2.studies" && resp.type == "IEA.JK2.dflt.sch.bckg.types") {
    tmp.wgt <- grep(pattern = paste(design.weight.variables[["IEA.JK2.dflt.sch.bckg.wgts"]], collapse = "|"), x = loaded.names.and.labels[ , Variables], value = TRUE)
  } else if(study.type == "IEA.JK2.studies" && resp.type == "IEA.JK2.dflt.tch.bckg.types") {
    tmp.wgt <- grep(pattern = paste(design.weight.variables[["IEA.JK2.dflt.tch.bckg.wgts"]], collapse = "|"), x = loaded.names.and.labels[ , Variables], value = TRUE)
  } else if(study.type == "IEA.BRR.studies" && resp.type == "IEA.BRR.dflt.inst.bckg.types") {
    tmp.wgt <- grep(pattern = paste(design.weight.variables[["IEA.BRR.dflt.inst.bckg.wgts"]], collapse = "|"), x = loaded.names.and.labels[ , Variables], value = TRUE)
  } else if(study.type == "IEA.BRR.studies" && resp.type == "IEA.BRR.dflt.prim.tch.bckg.types") {
    tmp.wgt <- grep(pattern = paste(design.weight.variables[["IEA.BRR.dflt.prim.tch.bckg.wgts"]], collapse = "|"), x = loaded.names.and.labels[ , Variables], value = TRUE)
  } else if(study.type == "IEA.BRR.studies" && resp.type == "IEA.BRR.dflt.low_sec.tch.bckg.types") {
    tmp.wgt <- grep(pattern = paste(design.weight.variables[["IEA.BRR.dflt.low_sec.tch.bckg.wgts"]], collapse = "|"), x = loaded.names.and.labels[ , Variables], value = TRUE)
  } else if(study.type == "IEA.BRR.studies" && resp.type == "IEA.BRR.dflt.educ.bckg.types") {
    tmp.wgt <- grep(pattern = paste(design.weight.variables[["IEA.BRR.dflt.educ.bckg.wgts"]], collapse = "|"), x = loaded.names.and.labels[ , Variables], value = TRUE)
  } else if(study.type == "OECD.BRR.studies" && resp.type == "OECD.BRR.dflt.std.bckg") {
    tmp.wgt <- grep(pattern = paste(design.weight.variables[["OECD.BRR.dflt.std.bckg.wgts"]], collapse = "|"), x = loaded.names.and.labels[ , Variables], value = TRUE)
  } else if(study.type == "OECD.BRR.studies" && resp.type == "OECD.BRR.dflt.sch.bckg") {
    tmp.wgt <- grep(pattern = paste(design.weight.variables[["OECD.BRR.dflt.sch.bckg.wgts"]], collapse = "|"), x = loaded.names.and.labels[ , Variables], value = TRUE)
  } else if(study.type == "OECD.BRR.studies" && resp.type == "OECD.BRR.dflt.tch.bckg") {
    tmp.wgt <- grep(pattern = paste(design.weight.variables[["OECD.BRR.dflt.tch.bckg.wgts"]], collapse = "|"), x = loaded.names.and.labels[ , Variables], value = TRUE)
  } else if(study.type == "OECD.BRR.studies" && resp.type == "OECD.BRR.dflt.lead.bckg") {
    tmp.wgt <- grep(pattern = paste(design.weight.variables[["OECD.BRR.dflt.lead.bckg.wgts"]], collapse = "|"), x = loaded.names.and.labels[ , Variables], value = TRUE)
  } else if(study.type == "OECD.BRR.studies" && resp.type == "OECD.BRR.dflt.staff.bckg") {
    tmp.wgt <- grep(pattern = paste(design.weight.variables[["OECD.BRR.dflt.staff.bckg.wgts"]], collapse = "|"), x = loaded.names.and.labels[ , Variables], value = TRUE)
  }
  
}




all.studies.available.weights <- c("SCHWGT", "TOTWGT", "SENWGT", "TOTWGTCH", "SENWGTCH", "HOUSEWGT", "TOTWGTC", "TOTWGTS", "SENWGTS", "TOTWGTT", "SENWGTT", "SENWGTC", "STOTWGTU", "HOUWGT", "TCHWGT", "MTOTWGT", "STOTWGT", "CNTRWGT", "STAFFWGT", "INSWGTE", "FINWGTE", "INSWGTI", "FINWGTI", "INSWGTP", "FINWGTP", "INSWGTS", "FINWGTS", "MATWGT", "SCIWGT", "PHYWGT", "REAWGT", "WNRSCHBW", "SCWEIGHT", "W_FSCHWT", "SENWGT_SCQ", "W_SCHGRNRABWT", "W_SCHGRNRABWT", "SENWGT_PAQ", "W_FSTUWT", "SPFWT0")

  
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  
  
  
  Sys.sleep(2)
  hide(id = "loading-content", anim = TRUE, animType = "fade", time = 2)
  show("app-content")
  observeEvent(input$home, {
    js$scrolltop()
  })
  observeEvent(input$dataMenu, {
    js$scrolltop()
  })
  observeEvent(input$analysisMenu, {
    js$scrolltop()
  })
  observeEvent(input$help, {
    js$scrolltop()
  })
  observeEvent(input$exit, {
    js$scrolltop()
  })
  output$welcomeToRALSA <- renderText("Welcome to RALSA")
  output$welcomeText <- renderText({HTML('The R Analyzer for Large-Scale Assessments (RALSA) is an R package for preparation and analysis of data from large-scale assessments and surveys which use complex sampling and assessment design. Currently, RALSA supports a number of studies with different design and a number of analysis types (see below). Both of these will increase in future.<br/>
RALSA is a free and open source software licensed under GPL v2.0.<br/><br/>
Use the menu on the left to:<br/><br/>
<ul><li>Prepare data for analysis</li></ul>
<ul><ul><li>Convert data (SPSS, or text in case of PISA prior 2015)</li></ul></ul>
<ul><ul><li>Merge study data files from different countries and/or respondents</li></ul></ul>
<ul><ul><li>View variable properties (name, class, variable label, response categories/unique values, user-defined missing values)</li></ul></ul>
<ul><ul><li>Data diagnostic tables (quick weighted or unweighted frequencies and descriptives for inspecting the data and elaborate hypotheses)</li></ul></ul>
<ul><ul><li>Recode variables</li></ul></ul>
<ul><ul><li>Select PISA countries for analysis</li></ul></ul>
<ul><li>Perform analyses (more analysis types will be added in future)</li></ul>
<ul><ul><li>Percentages of respondents in certain groups and averages (means, medians or modes) on variables of interest, per group</li></ul></ul>
<ul><ul><li>Percentiles of continuous variables within groups of respondents</li></ul></ul>
<ul><ul><li>Percentages of respondents reaching or surpassing benchmarks of achievement</li></ul></ul>
<ul><ul><li>Crosstabulations with Rao-Scott first- and second-order chi-square adjustments</li></ul></ul>
<ul><ul><li>Correlations (Pearson or Spearman)</li></ul></ul>
<ul><ul><li>Linear regression</li></ul></ul>
<ul><ul><li>Binary logistic regression</li></ul></ul>
<br/>The percentages and means, percentiles and benchmarks functions are also capable producing graphs for the estimates.<br>
<br/>All data preparation and analysis functions automatically recognize the study design and apply the appropriate techniques to handle the complex sampling assessment design issues, while giving freedom to tweak the analysis (e.g. change the default weight, apply the \"shortcut\" method in TIMSS and PIRLS and so on).<br><br>
Currently, RALSA can work with data for all cycles of the following studies (more will be added in future):<br/><br/>
<ul><li>CivED</li></ul>
<ul><li>ICCS</li></ul>
<ul><li>ICILS</li></ul>
<ul><li>RLII</li></ul>
<ul><li>PIRLS (including PIRLS Literacy and ePIRLS)</li></ul>
<ul><li>TIMSS (including TIMSS Numeracy and eTIMSS PSI)</li></ul>
<ul><li>TiPi (TIMSS and PIRLS joint study)</li></ul>
<ul><li>TIMSS Advanced</li></ul>
<ul><li>SITES</li></ul>
<ul><li>TEDS-M</li></ul>
<ul><li>PISA</li></ul>
<ul><li>PISA for Development</li></ul>
<ul><li>TALIS</li></ul>
<ul><li>TALIS Starting Strong Survey (a.k.a. TALIS 3S)</li></ul>
<ul><li>REDS</li></ul>
<br>For questions, feature requests and bug reports, please write to <a href="mailto:ralsa@ineri.org">ralsa@ineri.org</a>.<br/><br/><br/><br/><br/><br/>')})
  available.volumes <- getVolumes()()
  hide("convertMissToNA")
  hide("convertChooseOutDir")
  hide("consoleConvertData")
  output$h1ConvertData <- renderText("Convert data")
  output$convertIntro <- renderText({HTML("Select and convert data from SPSS or text files (in case of PISA prior its 2015 cycle) into LSA \".RData\" data files.")})
  shinyDirChoose(input, "convertChooseSrcDir", roots = available.volumes, filetype = list(sav = "sav", txt = "txt", sps = "sps"), updateFreq = 1000)
  full.file.list <- reactiveValues(all.files = NULL, SAV.files = NULL, TXT.files = NULL, SPS.files = NULL, convertSyntax = NULL)
  observeEvent(eventExpr = input$convertChooseSrcDir, {
    full.file.list$all.files <- NULL
    full.file.list$SAV.files <- NULL
    full.file.list$TXT.files <- NULL
    full.file.list$SPS.files <- NULL
    full.file.list$convertSyntax <- NULL
    full.file.list$all.files <- list.files(path = parseDirPath(available.volumes, input$convertChooseSrcDir), full.names = FALSE, pattern = "\\.sav$|\\.SAV$|\\.txt$|\\.sps$", recursive = FALSE, ignore.case = FALSE)
    full.file.list$SAV.files <- grep(pattern = "\\.sav$|\\.SAV$", x = full.file.list$all.files, value = TRUE, ignore.case = TRUE)
    full.file.list$TXT.files <- grep(pattern = "\\.txt$", x = full.file.list$all.files, value = TRUE, ignore.case = TRUE)
    full.file.list$SPS.files <- grep(pattern = "\\.sps$", x = full.file.list$all.files, value = TRUE, ignore.case = TRUE)
    full.file.list$SAV.files.lengths <- unname(sapply(X = full.file.list$SAV.files, FUN = nchar))
    full.file.list$TXT.files.lengths <- unname(sapply(X = full.file.list$TXT.files, FUN = nchar))
    full.file.list$SPS.files.unique <- unique(unname(sapply(X = full.file.list$SPS.files, FUN = function(i) {
      substr(x = i, start = 1, stop = 8)
    })))
    output$convertSrcPathDisplay <- renderText({parseDirPath(available.volumes, input$convertChooseSrcDir)})
  })
  observe({
    if(length(parseDirPath(available.volumes, input$convertChooseSrcDir)) > 0 && length(full.file.list$SAV.files.lengths) == 0 && length(full.file.list$TXT.files.lengths) == 0 && length(full.file.list$SPS.files.unique) == 0) {
      showNotification(ui = HTML("The selected folder does not<br/>contain any of the expected file types:<br/>*.sav or *.txt along with *.sps."), type = "error")
    }
    if(length(parseDirPath(available.volumes, input$convertChooseSrcDir)) > 0 && length(full.file.list$TXT.files.lengths) > 0 && length(full.file.list$SPS.files.unique) == 0) {
      showNotification(ui = HTML("The selected folder contains<br/>*.txt data files, but no control<br/>*.sps files"), type = "error")
    }
    if(length(parseDirPath(available.volumes, input$convertChooseSrcDir)) > 0 && length(full.file.list$TXT.files.lengths) == 0 && length(full.file.list$SPS.files.unique) > 0) {
      showNotification(ui = HTML("The selected folder contains<br/>control *.sps files, but no *.txt<br/>data files."), type = "error")
    }
    if(length(full.file.list$SAV.files.lengths) != 0 & length(unique(full.file.list$SAV.files.lengths)) > 1) {
      showNotification(ui = HTML("The selected folder contains<br/>SPSS files with varying name length.<br/>The folder must contain files from one<br/>study and its cycle. Please check the<br>folder content."), duration = 5, type = "error")
    } else if(length(full.file.list$SAV.files.lengths) != 0 & length(unique(full.file.list$SAV.files.lengths)) == 1) {
      if(unique(full.file.list$SAV.files.lengths) == 12) {
        output$convertPISA2015PlusStudyName <- renderUI({NULL})
        output$convertPISA2015PlusStudyCycle <- renderUI({NULL})
        output$convertAvailablePISA2015PlusFilesText <- renderUI({NULL})
        output$convertPISA2015PlusFiles <- renderDT({NULL})
        output$convertPISApre2015StudyName <- renderUI({NULL})
        output$convertPISApre2015StudyCycle <- renderUI({NULL})
        output$convertAvailablePISApre2015FilesText <- renderUI({NULL})
        output$convertPISApre2015Files <- renderDT({NULL})
        output$convertPISADev2019PlusStudyName <- renderUI({NULL})
        output$convertPISADev2019PlusStudyCycle <- renderUI({NULL})
        output$convertAvailablePISADev2019PlusFilesText <- renderUI({NULL})
        output$convertPISADev2019PlusFiles <- renderDT({NULL})
        first.char <- unique(substr(x = full.file.list$SAV.files, start = 1, stop = 1))
        last.two.chars <- unique(substr(x = full.file.list$SAV.files, start = 7, stop = 12))
        last.two.chars <- gsub(pattern = "\\.sav$", replacement = "", x = last.two.chars)
        if(any(c(length(first.char), length(last.two.chars)) > 1)) {
          showNotification(ui = "The folder must contain SPSS files from just one study, cycle or population. Files for multiple studies, cycles and/or populations were found. Please check the folder content.", duration = 5, type = "error")
        } else {
          first.SAV.file <- tolower(c(substr(x = full.file.list$SAV.files[1], start = 1, stop = 3), substr(x = full.file.list$SAV.files[1], start = 7, stop = 8)))
          convert.study.and.cycle <- extract.IEA.study.and.cycle(list.abbrev = studies.and.cycles, file.string = first.SAV.file)
          output$convertIEAStudyName <- renderText({
            if(length(full.file.list$all.files) > 0) {
              HTML(paste0('Study: ', convert.study.and.cycle[[1]]))
            } else {
              return(NULL)
            }
          })
          output$convertIEAStudyCycle <- renderText({
            if(length(full.file.list$all.files) > 0) {
              HTML(paste0('Cycle: ', convert.study.and.cycle[[2]]))
            } else {
              return(NULL)
            }
          })
          output$convertAvailableIEACntsText <- renderText({
            if(length(full.file.list$all.files) > 0) {
              HTML('Use the panels below to select the countries for which the ', convert.study.and.cycle[[1]], ' ', convert.study.and.cycle[[2]], ' data shall be converted from SPSS to LSA ".RData" data sets.')
            } else {
              return(NULL)
            }
          })
          convert.initial.country.list <- country.ISO.and.names[ISOs %in% grep(pattern = paste(substr(x = full.file.list$SAV.files, start = 4, stop = 6), collapse = "|"), x = ISOs, ignore.case = TRUE, value = TRUE), ]
          convert.initial.country.list[ , order_col := 1:nrow(convert.initial.country.list)]
          convert.selected.IEA.files <- data.table(ISOs = as.character(), Names = as.character(), order_col = numeric())
          convertAllCountries <- reactiveValues(
            convertAvailCntIEAFiles = convert.initial.country.list, convertSelectionIEA = convert.selected.IEA.files
          )
          output$convertArrowIEARight <- renderUI({
            if(length(full.file.list$all.files) > 0) {
              actionButton(inputId = "convertArrowIEARight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
            } else {
              return(NULL)
            }
          })
          output$convertDblArrowIEARight <- renderUI({
            if(length(full.file.list$all.files) > 0) {
              actionButton(inputId = "convertDblArrowIEARight", label = NULL, icon("angle-double-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
            } else {
              return(NULL)
            }
          })
          output$convertArrowIEALeft <- renderUI({
            if(length(full.file.list$all.files) > 0) {
              actionButton(inputId = "convertArrowIEALeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
            } else {
              return(NULL)
            }
          })
          output$convertDblArrowIEALeft <- renderUI({
            if(length(full.file.list$all.files) > 0) {
              actionButton(inputId = "convertDblArrowIEALeft", label = NULL, icon("angle-double-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
            } else {
              return(NULL)
            }
          })
          observeEvent(input$convertArrowIEARight, {
            req(input$convertAvailCntIEAFiles_rows_selected)
            convertAllCountries$convertSelectionIEA <- rbind(isolate(convertAllCountries$convertSelectionIEA), convertAllCountries$convertAvailCntIEAFiles[input$convertAvailCntIEAFiles_rows_selected, , drop = FALSE])
            convertAllCountries$convertSelectionIEA <- convertAllCountries$convertSelectionIEA[complete.cases(convertAllCountries$convertSelectionIEA), , drop = FALSE]
            convertAllCountries$convertAvailCntIEAFiles <- isolate(convertAllCountries$convertAvailCntIEAFiles[-input$convertAvailCntIEAFiles_rows_selected, , drop = FALSE])
          })
          observeEvent(input$convertDblArrowIEARight, {
            convertAllCountries$convertSelectionIEA <- rbind(convertAllCountries$convertSelectionIEA, isolate(convertAllCountries$convertAvailCntIEAFiles))
            convertAllCountries$convertAvailCntIEAFiles <- convert.selected.IEA.files
          })
          observeEvent(input$convertArrowIEALeft, {
            req(input$convertSelectionIEA_rows_selected)
            convertAllCountries$convertAvailCntIEAFiles <- rbind(isolate(convertAllCountries$convertAvailCntIEAFiles), convertAllCountries$convertSelectionIEA[input$convertSelectionIEA_rows_selected, , drop = FALSE])
            convertAllCountries$convertAvailCntIEAFiles <- convertAllCountries$convertAvailCntIEAFiles[complete.cases(convertAllCountries$convertAvailCntIEAFiles), , drop = FALSE]
            convertAllCountries$convertSelectionIEA <- isolate(convertAllCountries$convertSelectionIEA[-input$convertSelectionIEA_rows_selected, , drop = FALSE])
          })
          observeEvent(input$convertDblArrowIEALeft, {
            convertAllCountries$convertAvailCntIEAFiles <- rbind(convertAllCountries$convertAvailCntIEAFiles, isolate(convertAllCountries$convertSelectionIEA))
            convertAllCountries$convertSelectionIEA <- convert.selected.IEA.files
          })
          output$convertAvailCntIEAFiles <- renderDT({
            if(length(full.file.list$all.files) > 0) {
              setkeyv(x = convertAllCountries$convertAvailCntIEAFiles, cols = "order_col")
            } else {
              return(NULL)
            }
          },
          caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available countries"),
          rownames = FALSE,
          filter = "top",
          colnames = c("ISO codes", "Country names", "sortingcol"),
          class = "cell-border stripe;compact cell-border;",
          extensions = list("Scroller"),
          options = list(
            language = list(zeroRecords = "All countries have been selected"),
            initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
            dom = "ti",
            searchHighlight = FALSE,
            searchDelay = 100,
            ordering = FALSE,
            pageLength = 5000,
            autoWidth = TRUE,
            columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
            rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}")
            ,
            deferRender = TRUE, scrollY = 400, scroller = TRUE
          ))
          output$convertSelectionIEA <- renderDT({
            if(length(full.file.list$all.files) > 0) {
              convertAllCountries$convertSelectionIEA
            } else {
              return(NULL)
            }
          },
          caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Selected countries"),
          rownames = FALSE,
          filter = "top",
          colnames = c("ISO codes", "Country names", "sortingcol"),
          class = "cell-border stripe;compact cell-border;",
          extensions = list("Scroller"),
          options = list(
            language = list(zeroRecords = "No countries have been selected"),
            initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
            dom = "ti",
            searchHighlight = FALSE,
            searchDelay = 100,
            ordering = FALSE,
            pageLength = 5000,
            autoWidth = TRUE,
            columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
            rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
            deferRender = TRUE, scrollY = 400, scroller = TRUE
          ))
        }
      } else if(unique(full.file.list$SAV.files.lengths) > 12) {
        output$convertAvailCntIEAFiles <- renderDT({NULL})
        output$convertSelectionIEA <- renderDT({NULL})
        output$convertArrowIEARight <- renderUI({NULL})
        output$convertDblArrowIEARight <- renderUI({NULL})
        output$convertArrowIEALeft <- renderUI({NULL})
        output$convertDblArrowIEALeft <- renderUI({NULL})
        output$convertIEAStudyName <- renderUI({NULL})
        output$convertIEAStudyCycle <- renderUI({NULL})
        output$convertAvailableIEACntsText <- renderUI({NULL})
        output$convertPISApre2015StudyName <- renderUI({NULL})
        output$convertPISApre2015StudyCycle <- renderUI({NULL})
        output$convertAvailablePISApre2015FilesText <- renderUI({NULL})
        output$convertPISApre2015Files <- renderDT({NULL})
        if(length(unique(tolower(substr(x = full.file.list$SAV.files, start = 1, stop = 4)))) > 1) {
          showNotification(ui = 'More than one PISA (post-2012) SPSS<br/>files were found. The conversion could<br/>still run, but it is recommended to<br/>have files from just one cycle.', duration = NULL, type = "warning")
        } else {
          convertPISASAVFiles <- reactiveValues(convertAvailPISASAVFiles = full.file.list$SAV.files)
          first.SAV.file <- tolower(substr(x = convertPISASAVFiles$convertAvailPISASAVFiles[[1]], start = 1, stop = 4))
          if(first.SAV.file %in% c("cy6_", "cy07")) {
            output$convertPISADev2019PlusStudyName <- renderUI({NULL})
            output$convertPISADev2019PlusStudyCycle <- renderUI({NULL})
            output$convertAvailablePISADev2019PlusFilesText <- renderUI({NULL})
            output$convertPISADev2019PlusFiles <- renderDT({NULL})
            convert.study.and.cycle <- extract.PISA.2015.plus.study.and.cycle(list.abbrev = PISA.data.files, file.string = first.SAV.file)
            output$convertPISA2015PlusStudyName <- renderText({
              if(length(full.file.list$all.files) > 0) {
                HTML(paste0('Study: ', convert.study.and.cycle[[1]]))
              } else {
                return(NULL)
              }
            })
            output$convertPISA2015PlusStudyCycle <- renderText({
              if(length(full.file.list$all.files) > 0) {
                HTML(paste0('Cycle: ', convert.study.and.cycle[[2]]))
              } else {
                return(NULL)
              }
            })
            output$convertAvailablePISA2015PlusFilesText <- renderText({
              if(length(full.file.list$all.files) > 0) {
                HTML('The table below shows the SPSS PISA ', convert.study.and.cycle[[2]], ' data files available in the source folder which will be converted to LSA ".RData" data sets.')
              } else {
                return(NULL)
              }
            })
            output$convertPISA2015PlusFiles <- renderDT({
              if(length(full.file.list$all.files) > 0) {
                data.table(convertPISASAVFiles$convertAvailPISASAVFiles)
              } else {return(NULL)}
            },
            caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available files"),
            rownames = FALSE,
            filter = "top",
            colnames = "Data files",
            class = "cell-border stripe;compact cell-border;",
            extensions = list("Scroller"),
            options = list(
              initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
              dom = "ti",
              searchHighlight = FALSE,
              searchDelay = 100,
              ordering = FALSE,
              pageLength = 5000,
              autoWidth = TRUE,
              columnDefs = list(list(width = '300px', targets = 0)),
              rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
              deferRender = TRUE, scrollY = 400, scroller = TRUE
            ))
          } else if(first.SAV.file %in% c("cy1m")) {
            output$convertPISA2015PlusStudyName <- renderUI({NULL})
            output$convertPISA2015PlusStudyCycle <- renderUI({NULL})
            output$convertAvailablePISA2015PlusFilesText <- renderUI({NULL})
            output$convertPISA2015PlusFiles <- renderDT({NULL})
            convert.study.and.cycle <- extract.PISA.for.Development.study.and.cycle(list.abbrev = PISA.for.Development.data.files, file.string = first.SAV.file)
            output$convertPISADev2019PlusStudyName <- renderText({
              if(length(full.file.list$all.files) > 0) {
                HTML(paste0('Study: ', convert.study.and.cycle[[1]]))
              } else {
                return(NULL)
              }
            })
            output$convertPISADev2019PlusStudyCycle <- renderText({
              if(length(full.file.list$all.files) > 0) {
                HTML(paste0('Cycle: ', convert.study.and.cycle[[2]]))
              } else {
                return(NULL)
              }
            })
            output$convertAvailablePISADev2019PlusFilesText <- renderText({
              if(length(full.file.list$all.files) > 0) {
                HTML('The table below shows the SPSS PISA for Development ', convert.study.and.cycle[[2]], ' data files available in the source folder which will be converted to LSA ".RData" data sets.')
              } else {
                return(NULL)
              }
            })
            output$convertPISADev2019PlusFiles <- renderDT({
              if(length(full.file.list$all.files) > 0) {
                data.table(convertPISASAVFiles$convertAvailPISASAVFiles)
              } else {return(NULL)}
            },
            caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available files"),
            rownames = FALSE,
            filter = "top",
            colnames = "Data files",
            class = "cell-border stripe;compact cell-border;",
            extensions = list("Scroller"),
            options = list(
              initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
              dom = "ti",
              searchHighlight = FALSE,
              searchDelay = 100,
              ordering = FALSE,
              pageLength = 5000,
              autoWidth = TRUE,
              columnDefs = list(list(width = '300px', targets = 0)),
              rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
              deferRender = TRUE, scrollY = 400, scroller = TRUE
            ))
          }
        }
      }
    } else if(length(full.file.list$TXT.files) != 0 & length(full.file.list$SPS.files) != 0) {
      output$convertAvailCntIEAFiles <- renderDT({NULL})
      output$convertSelectionIEA <- renderDT({NULL})
      output$convertArrowIEARight <- renderUI({NULL})
      output$convertDblArrowIEARight <- renderUI({NULL})
      output$convertArrowIEALeft <- renderUI({NULL})
      output$convertDblArrowIEALeft <- renderUI({NULL})
      output$convertIEAStudyName <- renderUI({NULL})
      output$convertIEAStudyCycle <- renderUI({NULL})
      output$convertAvailableIEACntsText <- renderUI({NULL})
      output$convertPISA2015PlusStudyName <- renderUI({NULL})
      output$convertPISA2015PlusStudyCycle <- renderUI({NULL})
      output$convertAvailablePISA2015PlusFilesText <- renderUI({})
      output$convertPISA2015PlusFiles <- renderDT({NULL})
      if(length(full.file.list$TXT.files) != length(full.file.list$SPS.files)) {
        showNotification(ui = HTML("Some TXT (data) and/or<br/>SPS (control) files do<br/>not have a matching file.<br/>Please check the folder content."), type = "warning")
      }
      if(length(full.file.list$SPS.files.unique) > 1) {
        showNotification(ui = HTML("The folder contains SPS (control)<br/>files for more than one study.<br/>Please check the folder content."), type = "warning")
      }
      convertPISASPSFiles <- reactiveValues(convertAvailPISASPSFiles = full.file.list$SPS.files)
      convertPISATXTFiles <- reactiveValues(convertAvailPISATXTFiles = full.file.list$TXT.files)
      first.SPS.file <- tolower(full.file.list$SPS.files.unique[1])
      convert.study.and.cycle <- extract.PISA.pre.2015.study.and.cycle(list.abbrev = PISA.data.files, file.string = first.SPS.file)
      output$convertPISApre2015StudyName <- renderText({
        if(length(full.file.list$TXT.files) > 0) {
          HTML(paste0('Study: ', convert.study.and.cycle[[1]]))
        } else {
          return(NULL)
        }
      })
      output$convertPISApre2015StudyCycle <- renderText({
        if(length(full.file.list$TXT.files) > 0) {
          HTML(paste0('Cycle: ', convert.study.and.cycle[[2]]))
        } else {
          return(NULL)
        }
      })
      output$convertAvailablePISApre2015FilesText <- renderText({
        if(length(full.file.list$TXT.files) > 0) {
          HTML('The table below shows the PISA TXT data files and SPS syntaxes available in the source folder which will be converted to LSA ".RData" data sets.')
        } else {
          return(NULL)
        }
      })
      default.data.files <- data.table(PISA.data.files[["PISA.pre2015.TXT.files"]][[convert.study.and.cycle[[2]]]], PISA.data.files[["PISA.pre2015.TXT.files"]][[convert.study.and.cycle[[2]]]], 1:length(unlist(PISA.data.files[["PISA.pre2015.TXT.files"]][[convert.study.and.cycle[[2]]]])))
      default.syntaxes <- data.table(PISA.data.files[["PISA.pre2015.SPS.files"]][[convert.study.and.cycle[[2]]]], PISA.data.files[["PISA.pre2015.SPS.files"]][[convert.study.and.cycle[[2]]]], 1:length(PISA.data.files[["PISA.pre2015.SPS.files"]][[convert.study.and.cycle[[2]]]]))
      setkeyv(x = default.data.files, cols = "V1")
      setkeyv(x = default.syntaxes, cols = "V1")
      found.data.files <- data.table(convertPISATXTFiles$convertAvailPISATXTFiles, convertPISATXTFiles$convertAvailPISATXTFiles)
      found.syntaxes <- data.table(convertPISASPSFiles$convertAvailPISASPSFiles, convertPISASPSFiles$convertAvailPISASPSFiles)
      setkeyv(x = found.data.files, cols = "V1")
      setkeyv(x = found.syntaxes, cols = "V1")
      final.data.files <- merge(x = default.data.files, y = found.data.files, all = TRUE)
      final.syntaxes <- merge(x = default.syntaxes, y = found.syntaxes, all = TRUE)
      setkeyv(x = final.data.files, cols = "V3")
      setkeyv(x = final.syntaxes, cols = "V3")
      final.data.and.syntaxes <- data.table(cbind(final.data.files[ , V2.y], final.syntaxes[ , V2.y]))
      final.data.and.syntaxes <- final.data.and.syntaxes[!Reduce(`&`, lapply(final.data.and.syntaxes, is.na))]
      final.data.and.syntaxes[ , V1 := lapply(.SD, function(i) {
        ifelse(test = is.na(i), yes = "* Text data not available *", no = i)
      }), .SDcols = "V1"]
      final.data.and.syntaxes[ , V2 := lapply(.SD, function(i) {
        ifelse(test = is.na(i), yes = "* Syntax file not available *", no = i)
      }), .SDcols = "V2"]
      convertPISATXTFiles <- reactiveValues(convertAvailPISATXTFiles = final.data.and.syntaxes)
      output$convertPISApre2015Files <- renderDT({
        if(length(full.file.list$TXT.files) > 0) {
          final.data.and.syntaxes
        } else {
          return(NULL)
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available files"),
      rownames = FALSE,
      filter = "top",
      colnames = c("TXT data files", "SPSS syntaxes"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 400, scroller = TRUE
      ))
    }
    observe({
      if(exists("convertAllCountries")) {
        if(nrow(convertAllCountries$convertSelectionIEA) > 0) {
          show("convertMissToNA")
          show("convertChooseOutDir")
          show("convertOutPathDisplay")
        } else if(nrow(convertAllCountries$convertSelectionIEA) == 0) {
          hide("convertMissToNA")
          hide("convertChooseOutDir")
          hide("convertOutPathDisplay")
        }
      } else if(exists("final.data.and.syntaxes")) {
        if(nrow(final.data.and.syntaxes) > 0) {
          show("convertMissToNA")
          show("convertChooseOutDir")
          show("convertOutPathDisplay")
        } else if(nrow(final.data.and.syntaxes) == 0) {
          hide("convertMissToNA")
          hide("convertChooseOutDir")
          hide("convertOutPathDisplay")
        }
      } else if(exists("convertPISASAVFiles")) {
        if(length(convertPISASAVFiles$convertAvailPISASAVFiles) > 0) {
          show("convertMissToNA")
          show("convertChooseOutDir")
          show("convertOutPathDisplay")
        } else if(length(convertPISASAVFiles$convertAvailPISASAVFiles) == 0) {
          hide("convertMissToNA")
          hide("convertChooseOutDir")
          hide("convertOutPathDisplay")
        }
      } else if(!exists("convertAllCountries") && !exists("final.data.and.syntaxes") && !exists("convertPISASAVFiles")) {
        hide("convertMissToNA")
        hide("convertChooseOutDir")
        hide("convertOutPathDisplay")
      }
    })
    syntaxConvertData <- reactive({
      if(length(parseSavePath(available.volumes, input$convertChooseOutDir)$datapath) > 0) {
        full.file.list$convertSyntax <- paste0(
          'lsa.convert.data(inp.folder = "',
          parseSavePath(available.volumes, input$convertChooseSrcDir)$datapath,
          if(exists("convertAllCountries") && nrow(convertAllCountries$convertAvailCntIEAFiles) == 0 && nrow(convertAllCountries$convertSelectionIEA) > 1) {
            '"'
          } else if(exists("convertAllCountries") && nrow(convertAllCountries$convertAvailCntIEAFiles) != 0 && nrow(convertAllCountries$convertSelectionIEA) == 1) {
            paste0('", ISO = "', paste(unique(grep(pattern = paste(convertAllCountries$convertSelectionIEA[["ISOs"]], collapse = "|"), x = substr(x = full.file.list$SAV.files, start = 4, stop = 6), ignore.case = TRUE, value = TRUE)), collapse = '", "'), '"')
          } else if (exists("convertAllCountries") && nrow(convertAllCountries$convertAvailCntIEAFiles) != 0 && nrow(convertAllCountries$convertSelectionIEA) > 1) {
            paste0('", ISO = c("', paste(unique(grep(pattern = paste(convertAllCountries$convertSelectionIEA[["ISOs"]], collapse = "|"), x = substr(x = full.file.list$SAV.files, start = 4, stop = 6), ignore.case = TRUE, value = TRUE)), collapse = '", "'), '")')
          } else if(!exists("convertAllCountries")) {
            '"'
          },
          if(exists("convertPISATXTFiles") && length(convertPISATXTFiles$convertAvailPISATXTFiles) && length(parseSavePath(available.volumes, input$convertChooseOutDir)$datapath) > 0) {
            ', PISApre15 = TRUE'
          },
          if(input$convertMissToNA == 1) {
            ", missing.to.NA = TRUE"
          },
          if(exists("convertAllCountries") && nrow(convertAllCountries$convertSelectionIEA) > 0 || !exists("convertAllCountries")) {
            paste0(', out.folder = "', parseSavePath(available.volumes, input$convertChooseOutDir)$datapath, '")')
          }
        )
      }
    })
    shinyDirChoose(input, "convertChooseOutDir", roots = available.volumes, filetype = list(RData = "RData"), updateFreq = 1000)
    output$convertOutPathDisplay <- renderText({
      if(length(full.file.list$all.files) > 0) {
        parseDirPath(available.volumes, input$convertChooseOutDir)
      } else {
        return(NULL)
      }
    })
    observe({
      if(length(parseSavePath(available.volumes, input$convertChooseSrcDir)$datapath) > 0 && length(parseSavePath(available.volumes, input$convertChooseOutDir)$datapath) > 0 && identical(as.character(parseSavePath(available.volumes, input$convertChooseSrcDir)$datapath), as.character(parseSavePath(available.volumes, input$convertChooseOutDir)$datapath))) {
        hide("convertExecBtnHead")
        hide("execConvertData")
        hide("consoleConvertData")
        showNotification(ui = HTML("The converted data files are to be saved<br/>in the same folder where the source<br/>files are located.<b><br/><br/>Please choose a different folder<br/> to save the converted data.<br/>Until then further operations will not<br/>be permitted.</b>"), type = "error", duration = NULL)
      } else {
        show("convertExecBtnHead")
        show("execConvertData")
        show("consoleConvertData")
      }
    })
    study.type.loaded <- reactiveValues(IEA = NULL, PISA.2015.plus = NULL, PISA.pre.2015 = NULL)
    observe({
      study.type.loaded$IEA <- exists("convertAllCountries") == TRUE && nrow(convertAllCountries$convertSelectionIEA) > 0 && length(parseSavePath(available.volumes, input$convertChooseOutDir)$datapath) > 0
      study.type.loaded$PISA.2015.plus <- exists("convertPISASAVFiles") && length(convertPISASAVFiles$convertAvailPISASAVFiles) && length(parseSavePath(available.volumes, input$convertChooseOutDir)$datapath) > 0
      study.type.loaded$PISA.pre.2015 <- exists("convertPISATXTFiles") && length(convertPISATXTFiles$convertAvailPISATXTFiles) && length(parseSavePath(available.volumes, input$convertChooseOutDir)$datapath) > 0
    })
    output$convertSyntaxHead <- renderText({
      if(study.type.loaded$IEA == TRUE ||
         study.type.loaded$PISA.2015.plus == TRUE ||
         study.type.loaded$PISA.pre.2015 == TRUE) {
        HTML("Syntax")
      } else {
        return(NULL)
      }
    })
    output$convertSyntax <- renderText({
      if(study.type.loaded$IEA == TRUE ||
         study.type.loaded$PISA.2015.plus == TRUE ||
         study.type.loaded$PISA.pre.2015 == TRUE) {
        syntaxConvertData()
      } else {
        return(NULL)
      }
    })
    output$convertExecBtnHead <- renderText({
      if(study.type.loaded$IEA == TRUE ||
         study.type.loaded$PISA.2015.plus == TRUE ||
         study.type.loaded$PISA.pre.2015 == TRUE) {
        HTML("Press the button below to execute the syntax")
      } else {
        return(NULL)
      }
    })
    output$execConvertData <- renderUI({
      if(study.type.loaded$IEA == TRUE ||
         study.type.loaded$PISA.2015.plus == TRUE ||
         study.type.loaded$PISA.pre.2015 == TRUE) {
        actionButton(inputId = "execConvertData", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
      } else {
        return(NULL)
      }
    })
    observe({
      if(length(full.file.list$all.files) == 0 || exists("convertAllCountries") && nrow(convertAllCountries$convertSelectionIEA) == 0) {
        hide("consoleConvertData")
      } else if(length(full.file.list$all.files) > 0 || exists("convertAllCountries") && nrow(convertAllCountries$convertSelectionIEA) > 0) {
        show("consoleConvertData")
      }
    })
  })
  observeEvent(input$execConvertData, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consoleConvertData", "")
      expr = eval(parse(text = full.file.list$convertSyntax))
    },
    message = function(i) {
      shinyjs::html(id = "consoleConvertData", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consoleConvertData", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
    showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
  }, ignoreInit = TRUE)
  hide("mergeChooseOutFile")
  output$h1MergeData <- renderText("Merge data")
  output$mergeIntro <- renderText({HTML("Select and merge large-scale assessment .RData files. This works with all supported studies, except for PISA.")})
  shinyDirChoose(input, "mergeChooseSrcDir", roots = available.volumes, filetype = list(RData = "RData"), updateFreq = 1000)
  full.file.list.merge <- reactiveValues(all.files = NULL, RData.files = NULL, mergeSyntax = NULL)
  observeEvent(eventExpr = input$mergeChooseSrcDir, {
    full.file.list.merge$RData.files <- NULL
    full.file.list.merge$all.files <- list.files(path = parseDirPath(available.volumes, input$mergeChooseSrcDir), full.names = FALSE, pattern = "\\.RData$", recursive = FALSE, ignore.case = FALSE)
    full.file.list.merge$RData.files <- grep(pattern = "\\.RData$", x = full.file.list.merge$all.files, value = TRUE, ignore.case = TRUE)
    full.file.list.merge$RData.files.lengths <- unname(sapply(X = full.file.list.merge$RData.files, FUN = nchar))
    output$mergeSrcPathDisplay <- renderText({parseDirPath(available.volumes, input$mergeChooseSrcDir)})
  }, ignoreInit = TRUE)
  observe({
    if(length(parseDirPath(available.volumes, input$mergeChooseSrcDir)) > 0 & length(full.file.list.merge$RData.files.lengths) == 0 || length(parseDirPath(available.volumes, input$mergeChooseSrcDir)) > 0 & !any(unique(substr(x = full.file.list.merge$RData.files, start = 1, stop = 3)) %in% unname(unlist(sapply(respondents.and.cycles, function(i) {
      i["resp.type"]
    }))))) {
      showNotification(ui = HTML("The selected folder does not<br/>contain any files of the expected<br/>type (*.RData) or none from the<br/>expected studies and respondents."), type = "error")
    }
    if(length(full.file.list.merge$RData.files.lengths) != 0 & length(unique(full.file.list.merge$RData.files.lengths)) > 1) {
      showNotification(ui = HTML("The selected folder contains<br/>.RData files with varying name length.<br/>The folder must contain files from one<br/>study and its cycle. Please check the<br>folder content."), duration = 5, type = "error")
    } else if(length(full.file.list.merge$RData.files.lengths) != 0 & length(unique(full.file.list.merge$RData.files.lengths)) == 1) {
      if(unique(full.file.list.merge$RData.files.lengths) == 14) {
        first.char <- unique(substr(x = full.file.list.merge$RData.files, start = 1, stop = 1))
        last.two.chars <- unique(substr(x = full.file.list.merge$RData.files, start = 7, stop = 12))
        last.two.chars <- gsub(pattern = "\\.RData$", replacement = "", x = last.two.chars)
        if(any(c(length(first.char), length(last.two.chars)) > 1)) {
          showNotification(ui = HTML("The folder must contain .RData<br/>files from just one study, cycle<br/>and population. Files for multiple<br/>studies, cycles and/or populations were<br/>found. Please check the folder content."), duration = 5, type = "error")
        } else {
          first.RData.file <- tolower(c(substr(x = full.file.list.merge$RData.files[1], start = 1, stop = 3), substr(x = full.file.list.merge$RData.files[1], start = 7, stop = 8)))
          merge.study.and.cycle <- extract.IEA.study.and.cycle(list.abbrev = studies.and.cycles, file.string = first.RData.file)
          output$mergeIEAStudyName <- renderText({
            if(length(full.file.list.merge$RData.files) > 0) {
              HTML(paste0('Study: ', merge.study.and.cycle[[1]]))
            } else {
              return(NULL)
            }
          })
          output$mergeIEAStudyCycle <- renderText({
            if(length(full.file.list.merge$RData.files) > 0) {
              HTML(paste0('Cycle: ', merge.study.and.cycle[[2]]))
            } else {
              return(NULL)
            }
          })
          output$mergeAvailableIEACntsText <- renderText({
            if(length(full.file.list.merge$RData.files) > 0) {
              HTML('Use the panels below to select the countries for which the ', merge.study.and.cycle[[1]], ' ', merge.study.and.cycle[[2]], ' data shall be merged together.')
            } else {
              return(NULL)
            }
          })
          merge.initial.country.list <- country.ISO.and.names[ISOs %in% grep(pattern = paste(substr(x = full.file.list.merge$RData.files, start = 4, stop = 6), collapse = "|"), x = ISOs, ignore.case = TRUE, value = TRUE), ]
          merge.initial.country.list[ , order_col := 1:nrow(merge.initial.country.list)]
          merge.selected.IEA.files <- data.table(ISOs = as.character(), Names = as.character(), order_col = numeric())
          mergeAllCountries <- reactiveValues(
            mergeAvailCntIEAFiles = merge.initial.country.list, mergeSelectionIEA = merge.selected.IEA.files
          )
          output$mergeArrowIEARight <- renderUI({
            if(length(full.file.list.merge$RData.files) > 0) {
              actionButton(inputId = "mergeArrowIEARight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
            } else {
              return(NULL)
            }
          })
          output$mergeDblArrowIEARight <- renderUI({
            if(length(full.file.list.merge$RData.files) > 0) {
              actionButton(inputId = "mergeDblArrowIEARight", label = NULL, icon("angle-double-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
            } else {
              return(NULL)
            }
          })
          output$mergeArrowIEALeft <- renderUI({
            if(length(full.file.list.merge$RData.files) > 0) {
              actionButton(inputId = "mergeArrowIEALeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
            } else {
              return(NULL)
            }
          })
          output$mergeDblArrowIEALeft <- renderUI({
            if(length(full.file.list.merge$RData.files) > 0) {
              actionButton(inputId = "mergeDblArrowIEALeft", label = NULL, icon("angle-double-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
            } else {
              return(NULL)
            }
          })
          observeEvent(input$mergeArrowIEARight, {
            req(input$mergeAvailCntIEAFiles_rows_selected)
            mergeAllCountries$mergeSelectionIEA <- rbind(isolate(mergeAllCountries$mergeSelectionIEA), mergeAllCountries$mergeAvailCntIEAFiles[input$mergeAvailCntIEAFiles_rows_selected, , drop = FALSE])
            mergeAllCountries$mergeSelectionIEA <- mergeAllCountries$mergeSelectionIEA[complete.cases(mergeAllCountries$mergeSelectionIEA), , drop = FALSE]
            mergeAllCountries$mergeAvailCntIEAFiles <- isolate(mergeAllCountries$mergeAvailCntIEAFiles[-input$mergeAvailCntIEAFiles_rows_selected, , drop = FALSE])
          })
          observeEvent(input$mergeDblArrowIEARight, {
            mergeAllCountries$mergeSelectionIEA <- rbind(mergeAllCountries$mergeSelectionIEA, isolate(mergeAllCountries$mergeAvailCntIEAFiles))
            mergeAllCountries$mergeAvailCntIEAFiles <- merge.selected.IEA.files
          })
          observeEvent(input$mergeArrowIEALeft, {
            req(input$mergeSelectionIEA_rows_selected)
            mergeAllCountries$mergeAvailCntIEAFiles <- rbind(isolate(mergeAllCountries$mergeAvailCntIEAFiles), mergeAllCountries$mergeSelectionIEA[input$mergeSelectionIEA_rows_selected, , drop = FALSE])
            mergeAllCountries$mergeAvailCntIEAFiles <- mergeAllCountries$mergeAvailCntIEAFiles[complete.cases(mergeAllCountries$mergeAvailCntIEAFiles), , drop = FALSE]
            mergeAllCountries$mergeSelectionIEA <- isolate(mergeAllCountries$mergeSelectionIEA[-input$mergeSelectionIEA_rows_selected, , drop = FALSE])
          })
          observeEvent(input$mergeDblArrowIEALeft, {
            mergeAllCountries$mergeAvailCntIEAFiles <- rbind(mergeAllCountries$mergeAvailCntIEAFiles, isolate(mergeAllCountries$mergeSelectionIEA))
            mergeAllCountries$mergeSelectionIEA <- merge.selected.IEA.files
          })
          output$mergeAvailCntIEAFiles <- renderDT({
            if(length(full.file.list.merge$RData.files) > 0) {
              setkeyv(x = mergeAllCountries$mergeAvailCntIEAFiles, cols = "order_col")
            } else {
              return(NULL)
            }
          },
          caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available countries"),
          rownames = FALSE,
          filter = "top",
          colnames = c("ISO codes", "Country names", "sortingcol"),
          class = "cell-border stripe;compact cell-border;",
          extensions = list("Scroller"),
          options = list(
            language = list(zeroRecords = "All countries have been selected"),
            initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
            dom = "ti",
            searchHighlight = FALSE,
            searchDelay = 100,
            ordering = FALSE,
            pageLength = 5000,
            autoWidth = TRUE,
            columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
            rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
            deferRender = TRUE, scrollY = 400, scroller = TRUE
          ))
          output$mergeSelectionIEA <- renderDT({
            if(length(full.file.list.merge$RData.files) > 0) {
              mergeAllCountries$mergeSelectionIEA
            } else {
              return(NULL)
            }
          },
          caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Selected countries"),
          rownames = FALSE,
          filter = "top",
          colnames = c("ISO codes", "Country names", "sortingcol"),
          class = "cell-border stripe;compact cell-border;",
          extensions = list("Scroller"),
          options = list(
            language = list(zeroRecords = "No countries have been selected"),
            initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
            dom = "ti",
            searchHighlight = FALSE,
            searchDelay = 100,
            ordering = FALSE,
            pageLength = 5000,
            autoWidth = TRUE,
            columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
            rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
            deferRender = TRUE, scrollY = 400, scroller = TRUE
          ))
          file.abbrev <- unique(substr(x = full.file.list.merge$RData.files, start = 1, stop = 3))
          cycle.abbrev <- unique(substr(x = full.file.list.merge$RData.files, start = 7, stop = 8))
          merge.unique.abbrevs <- Map(f = c, file.abbrev, cycle.abbrev)
          merge.unique.abbrevs <- lapply(merge.unique.abbrevs, function(i) {
            paste(i, collapse = "")
          })
          file.variables <- lapply(X = file.abbrev, FUN = function(i) {
            tmp.data <- get(load(file.path(parseDirPath(available.volumes, input$mergeChooseSrcDir), grep(pattern = paste0("^", i), x = full.file.list.merge$RData.files, ignore.case = TRUE, value = TRUE)[1])))
            tmp.data <- data.table(
              Variables = names(tmp.data),
              Variable_Labels = sapply(X = tmp.data, FUN = function(j) {
                if(is.null(attr(x = j, which = "variable.label"))) {
                  return(NA_character_)
                } else {
                  attr(x = j, which = "variable.label")
                }
              }),
              order_col = 1:length(names(tmp.data)))
          })
          names(file.variables) <- file.abbrev
          file.variables <- Map(f = cbind, file.variables, lapply(X = file.abbrev, FUN = function(i) {data.table(type = toupper(i))}))
          collapsed.respondents.and.cycles <- lapply(respondents.and.cycles, function(i) {
            unlist(sapply(i[[1]], function(j) {
              paste0(j, i[[2]])
            }, simplify = FALSE), use.names = FALSE)
          })
          merge.available.respondents <- Filter(length, sapply(collapsed.respondents.and.cycles, function(i) {
            grep(pattern = paste(i, collapse = "|"), x = unlist(merge.unique.abbrevs), ignore.case = TRUE, value = TRUE)
          }))
          merge.checkboxes.names <- sort(paste0("(", toupper(sapply(X = merge.available.respondents, FUN = names)), ") ", names(merge.available.respondents)))
          output$mergeAvailRespText <- renderText({
            if(length(full.file.list.merge$RData.files) > 0 && nrow(mergeAllCountries$mergeSelectionIEA) > 0) {
              HTML('The following respondent types were found in the source folder. Use the checkboxes below to select different respondents to merge their data.')
            } else {
              return(NULL)
            }
          })
          output$mergeAvailRespCheckboxes <- renderUI({
            if(length(full.file.list.merge$RData.files) > 0 && nrow(mergeAllCountries$mergeSelectionIEA) > 0) {
              checkboxGroupInput(inputId = "mergeAvailRespCheckboxes", label = NULL, choices = merge.checkboxes.names, width = "400px")
            } else {
              return(NULL)
            }
          })
          output$warnNoSuchCombination <- renderPrint({
            if(length(input$mergeAvailRespCheckboxes) == 0 || any(tolower(substr(x = input$mergeAvailRespCheckboxes, start = 2, stop = 4)) %in% file.abbrev == FALSE)) {
              return(HTML(""))
            } else if(length(input$mergeAvailRespCheckboxes) == 1 && all(tolower(substr(x = input$mergeAvailRespCheckboxes, start = 2, stop = 4)) %in% file.abbrev) == TRUE) {
              if(any(unlist(lapply(X = merge.combinations[[merge.study.and.cycle[[1]]]], FUN = function(i) {
                identical(sort(tolower(substr(x = input$mergeAvailRespCheckboxes, start = 2, stop = 4))), sort(i[1:(length(i) - 1)]))
              }))) == TRUE) {
                HTML("")
              } else {
                HTML("The selected file type cannot be merged on its own, but in combination with other file types. Either add another file type or reconsider the selection.")
              }
            } else if(length(input$mergeAvailRespCheckboxes) > 1) {
              if(any(unlist(lapply(X = merge.combinations[[merge.study.and.cycle[[1]]]], FUN = function(i) {
                identical(sort(tolower(substr(x = input$mergeAvailRespCheckboxes, start = 2, stop = 4))), sort(i[1:(length(i) - 1)]))
              }))) == TRUE) {
                HTML("")
              } else {
                HTML("Merging variables from the the selected file types combination is not supported. Reconsider the file types selection.")
              }
            }
          })
          observe({
            if(length(input$mergeAvailRespCheckboxes) > 0) {
              if(all(unlist(lapply(X = merge.combinations[[merge.study.and.cycle[[1]]]], FUN = function(i) {
                identical(sort(tolower(substr(x = input$mergeAvailRespCheckboxes, start = 2, stop = 4))), sort(i[1:(length(i) - 1)]))
              })) == FALSE)) {
                hide("mergeAvailVarsText")
                hide("mergeAllAvailableVars")
                hide("mergeArrowSelVarsRight")
                hide("mergeDblArrowSelVarsRight")
                hide("mergeArrowSelVarsLeft")
                hide("mergeDblArrowSelVarsLeft")
                hide("mergeVarsSelection")
              } else {
                show("mergeAvailVarsText")
                show("mergeAllAvailableVars")
                show("mergeArrowSelVarsRight")
                show("mergeDblArrowSelVarsRight")
                show("mergeArrowSelVarsLeft")
                show("mergeDblArrowSelVarsLeft")
                show("mergeVarsSelection")
              }
            }
          })
          merge.initial.available.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric(), type = as.character())
          merge.initial.selected.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric(), type = as.character())
          mergeAllVars <- reactiveValues(mergeAvailVars = merge.initial.available.vars, mergeSelectedVars = merge.initial.selected.vars)
          observeEvent(eventExpr = input$mergeAvailRespCheckboxes, {
            mergeAllVars$mergeAvailVars <- unique(x = rbindlist(file.variables[tolower(substr(x = input$mergeAvailRespCheckboxes, start = 2, stop = 4))]), by = "Variables")
            if(is.null(input$mergeAvailRespCheckboxes)) {
              mergeAllVars$mergeAvailVars <- merge.initial.available.vars
              mergeAllVars$mergeSelectedVars <- merge.initial.selected.vars
            }
            observe({
              if(nrow(mergeAllVars$mergeSelectedVars) > 0) {
                mergeAllVars$mergeAvailVars <- mergeAllVars$mergeAvailVars[!mergeAllVars$mergeSelectedVars, on = .(Variables, Variable_Labels, type)]
              }
            })
            if(nrow(mergeAllVars$mergeSelectedVars) > 0) {
              vars.selected.box.unchecked <- setdiff(unique(mergeAllVars$mergeSelectedVars[ , type]), unique(mergeAllVars$mergeAvailVars[ , type]))
            }
            if(exists("vars.selected.box.unchecked") && length(vars.selected.box.unchecked) > 0) {
              mergeAllVars$mergeSelectedVars <- mergeAllVars$mergeSelectedVars[!type %in% vars.selected.box.unchecked, ]
            }
          }, ignoreNULL = FALSE)
          observe({
            if(length(full.file.list.merge$RData.files) == 0 || nrow(mergeAllCountries$mergeSelectionIEA) == 0) {
              mergeAllVars$mergeAvailVars <- merge.initial.available.vars
              mergeAllVars$mergeSelectedVars <- merge.initial.selected.vars
            }
          })
          merge.render.vars.condition <- reactiveVal()
          observe({
            if(nrow(mergeAllCountries$mergeSelectionIEA) == 0 && nrow(mergeAllVars$mergeAvailVars) == 0) {
              merge.render.vars.condition(FALSE)
            } else if(nrow(mergeAllCountries$mergeSelectionIEA) > 0 && nrow(mergeAllVars$mergeAvailVars) > 0 ||
                      nrow(mergeAllCountries$mergeSelectionIEA) > 0 && nrow(mergeAllVars$mergeSelectedVars) > 0) {
              merge.render.vars.condition(TRUE)
            }
          })
          output$mergeArrowSelVarsRight <- renderUI({
            if(length(full.file.list.merge$RData.files) == 0 || merge.render.vars.condition() == FALSE && length(input$mergeAvailRespCheckboxes) == 0) {
              return(NULL)
            } else if(merge.render.vars.condition() == TRUE) {
              if(length(full.file.list.merge$RData.files) == 0 || nrow(mergeAllVars$mergeAvailVars) == 0 && length(input$mergeAvailRespCheckboxes) == 0) {
                return(NULL)
              } else {
                actionButton(inputId = "mergeArrowSelVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
              }
            }
          })
          output$mergeDblArrowSelVarsRight <- renderUI({
            if(length(full.file.list.merge$RData.files) == 0 || merge.render.vars.condition() == FALSE && length(input$mergeAvailRespCheckboxes) == 0) {
              return(NULL)
            } else if(merge.render.vars.condition() == TRUE) {
              if(length(full.file.list.merge$RData.files) == 0 || nrow(mergeAllVars$mergeAvailVars) == 0 && length(input$mergeAvailRespCheckboxes) == 0) {
                return(NULL)
              } else {
                actionButton(inputId = "mergeDblArrowSelVarsRight", label = NULL, icon("angle-double-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
              }
            }
          })
          output$mergeArrowSelVarsLeft <- renderUI({
            if(length(full.file.list.merge$RData.files) == 0 || merge.render.vars.condition() == FALSE && length(input$mergeAvailRespCheckboxes) == 0) {
              return(NULL)
            } else if(merge.render.vars.condition() == TRUE) {
              if(length(full.file.list.merge$RData.files) == 0 || nrow(mergeAllVars$mergeAvailVars) == 0 && length(input$mergeAvailRespCheckboxes) == 0) {
                return(NULL)
              } else {
                actionButton(inputId = "mergeArrowSelVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
              }
            }
          })
          output$mergeDblArrowSelVarsLeft <- renderUI({
            if(length(full.file.list.merge$RData.files) == 0 || merge.render.vars.condition() == FALSE && length(input$mergeAvailRespCheckboxes) == 0) {
              return(NULL)
            } else if(merge.render.vars.condition() == TRUE) {
              if(length(full.file.list.merge$RData.files) == 0 || nrow(mergeAllVars$mergeAvailVars) == 0 && length(input$mergeAvailRespCheckboxes) == 0) {
                return(NULL)
              } else {
                actionButton(inputId = "mergeDblArrowSelVarsLeft", label = NULL, icon("angle-double-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
              }
            }
          })
          output$mergeAvailVarsText <- renderText({
            if(length(full.file.list.merge$RData.files) == 0 | nrow(mergeAllCountries$mergeSelectionIEA) == 0 & nrow(mergeAllVars$mergeAvailVars) == 0) {
              return(NULL)
            } else if(length(full.file.list.merge$RData.files) > 0 && nrow(mergeAllCountries$mergeSelectionIEA) > 0 && nrow(mergeAllVars$mergeAvailVars) > 0 ||
                      nrow(mergeAllCountries$mergeSelectionIEA) > 0 && nrow(mergeAllVars$mergeSelectedVars) > 0) {
              HTML('Use the panels below to select variables from the different respondents selected from above.<br/>Note: Design variables (PVs, weights, weight adjustment variables, etc.) will not be displayed, but will be added to the merged file automatically.')
            }
          })
          output$mergeAllAvailableVars <- renderDT({
            if(length(full.file.list.merge$RData.files) == 0 || merge.render.vars.condition() == FALSE && length(input$mergeAvailRespCheckboxes) == 0) {
              return(NULL)
            } else if(length(full.file.list.merge$RData.files) > 0 && merge.render.vars.condition() == TRUE) {
              if(nrow(mergeAllVars$mergeAvailVars) == 0 && length(input$mergeAvailRespCheckboxes) == 0) {
                return(NULL)
              } else {
                all.design.ID.IT.cols <- c(grep(pattern = "^ID|^IT[[:alpha:]]+[[:alnum:]]+|FAC|ADJ|^IL|TCER", x = mergeAllVars$mergeAvailVars[ , Variables], ignore.case = TRUE, value = TRUE), grep(pattern = paste(unique(unlist(x = studies.all.design.variables, recursive = TRUE, use.names = FALSE)), collapse = "|"), x = mergeAllVars$mergeAvailVars[ , Variables], ignore.case = TRUE, value = TRUE))
                mergeAllVars$mergeAvailVars <- mergeAllVars$mergeAvailVars[!Variables %in% all.design.ID.IT.cols, ]
                setkeyv(x = mergeAllVars$mergeAvailVars, cols = c("type", "order_col"))
              }
            }
          },
          caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available variables"),
          rownames = FALSE,
          filter = "top",
          colnames = c("Names", "Labels", "sortingcol", "Respondent"),
          class = "cell-border stripe;compact cell-border;",
          extensions = list("Scroller"),
          options = list(
            language = list(zeroRecords = "All variables have been selected"),
            initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
            dom = "ti",
            searchHighlight = FALSE,
            searchDelay = 100,
            ordering = FALSE,
            pageLength = 5000,
            autoWidth = TRUE,
            columnDefs = list(list(width = '75px', targets = 0), list(width = '75px', targets = 3), list(visible = FALSE, targets = 2), list(className = 'dt-center', targets = 3)),
            rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
            deferRender = TRUE, scrollY = 400, scroller = TRUE
          ))
          output$mergeVarsSelection <- renderDT({
            if(length(full.file.list.merge$RData.files) == 0 || merge.render.vars.condition() == FALSE && length(input$mergeAvailRespCheckboxes) == 0) {
              return(NULL)
            } else if(length(full.file.list.merge$RData.files) > 0 && merge.render.vars.condition() == TRUE) {
              if(nrow(mergeAllVars$mergeAvailVars) == 0 && length(input$mergeAvailRespCheckboxes) == 0) {
                return(NULL)
              } else {
                setkeyv(x = mergeAllVars$mergeSelectedVars, cols = c("type", "order_col"))
              }
            }
          },
          caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Selected variables"),
          rownames = FALSE,
          filter = "top",
          colnames = c("Names", "Labels", "sortingcol", "Respondent"),
          class = "cell-border stripe;compact cell-border;",
          extensions = list("Scroller"),
          options = list(
            language = list(zeroRecords = "No variables have been selected"),
            initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
            dom = "ti",
            searchHighlight = FALSE,
            searchDelay = 100,
            ordering = FALSE,
            pageLength = 5000,
            autoWidth = TRUE,
            columnDefs = list(list(width = '75px', targets = 0), list(width = '75px', targets = 3), list(visible = FALSE, targets = 2), list(className = 'dt-center', targets = 3)),
            rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
            deferRender = TRUE, scrollY = 400, scroller = TRUE
          ))
          observeEvent(input$mergeArrowSelVarsRight, {
            req(input$mergeAllAvailableVars_rows_selected)
            mergeAllVars$mergeSelectedVars <- rbind(isolate(mergeAllVars$mergeSelectedVars), mergeAllVars$mergeAvailVars[input$mergeAllAvailableVars_rows_selected, , drop = FALSE])
            mergeAllVars$mergeSelectedVars <- mergeAllVars$mergeSelectedVars[complete.cases(mergeAllVars$mergeSelectedVars[ , "Variables"]), , drop = FALSE]
            mergeAllVars$mergeAvailVars <- isolate(mergeAllVars$mergeAvailVars[-input$mergeAllAvailableVars_rows_selected, , drop = FALSE])
          })
          observeEvent(input$mergeDblArrowSelVarsRight, {
            mergeAllVars$mergeSelectedVars <- rbind(mergeAllVars$mergeSelectedVars, isolate(mergeAllVars$mergeAvailVars))
            mergeAllVars$mergeAvailVars <- merge.initial.available.vars
          })
          observeEvent(input$mergeArrowSelVarsLeft, {
            req(input$mergeVarsSelection_rows_selected)
            mergeAllVars$mergeAvailVars <- rbind(isolate(mergeAllVars$mergeAvailVars),        mergeAllVars$mergeSelectedVars[input$mergeVarsSelection_rows_selected, , drop = FALSE])
            mergeAllVars$mergeAvailVars <- mergeAllVars$mergeAvailVars[complete.cases(mergeAllVars$mergeAvailVars[ , "Variables"]), , drop = FALSE]
            mergeAllVars$mergeSelectedVars <- isolate(mergeAllVars$mergeSelectedVars[-input$mergeVarsSelection_rows_selected, , drop = FALSE])
          })
          observeEvent(input$mergeDblArrowSelVarsLeft, {
            mergeAllVars$mergeAvailVars <- rbind(mergeAllVars$mergeAvailVars, isolate(mergeAllVars$mergeSelectedVars))
            mergeAllVars$mergeSelectedVars <- merge.initial.selected.vars
          })
          shinyFileSave(input, "mergeChooseOutFile", filetype = list(RData = "RData"), roots = available.volumes, updateFreq = 100000)
          observe({
            if(length(full.file.list.merge$RData.files) > 0 && nrow(mergeAllVars$mergeSelectedVars) > 0) {
              show("mergeChooseOutFile")
            } else if(length(full.file.list.merge$RData.files) == 0 || nrow(mergeAllVars$mergeSelectedVars) == 0) {
              hide("mergeChooseOutFile")
            }
          })
          observe({
            if(length(parseSavePath(available.volumes, input$mergeChooseSrcDir)$datapath) > 0 && length(parseSavePath(available.volumes, input$mergeChooseOutFile)$datapath) > 0 && identical(as.character(parseSavePath(available.volumes, input$mergeChooseSrcDir)$datapath), dirname(parseSavePath(available.volumes, input$mergeChooseOutFile)$datapath))) {
              hide("mergeExecBtnHead")
              hide("execMergeData")
              showNotification(ui = HTML("The merged file is to be saved<br/>in the same folder where the source<br/>files are located.<b><br/><br/>Please choose a different folder<br/> to save the merged file.<br/>Until then further operations will not<br/>be permitted.</b>"), type = "error", duration = NULL)
            } else {
              show("mergeExecBtnHead")
              show("execMergeData")
            }
          })
          syntaxMergeData <- reactive({
            if(exists("mergeAllVars") && nrow(mergeAllVars$mergeSelectedVars) > 0) {
              tmp.vars <- split(x = mergeAllVars$mergeSelectedVars[ , c("Variables", "type")], by = "type")
              resp.w.all.vars.selected <- setdiff(unique(mergeAllVars$mergeSelectedVars[ , type]), unique(mergeAllVars$mergeAvailVars[ , type]))
              other.vars.selected <- setdiff(unique(mergeAllVars$mergeSelectedVars[ , type]), resp.w.all.vars.selected)
              merge.all.selected <- if(length(resp.w.all.vars.selected) > 0) {
                paste(paste0(tolower(resp.w.all.vars.selected), " = NULL"), sep = '', collapse = ", ")
              } else {
                NULL
              }
              merge.user.selected <- if(length(other.vars.selected) > 0) {
                paste(sapply(tmp.vars[other.vars.selected], function(i) {
                  if(length(i[ , Variables]) > 1) {
                    paste0(tolower(i[ , type[[1]]]), ' = c("', paste(i[ , Variables], collapse = '", "'), '")')
                  } else if(length(i[ , Variables]) == 1) {
                    paste0(tolower(i[ , type[[1]]]), ' = "', paste(i[ , Variables], collapse = '", "'), '"')
                  }
                }), collapse = ', ')
              } else {
                NULL
              }
              merge.sel.vars <- paste0('file.types = list(', paste(c(merge.all.selected, merge.user.selected), collapse = ', '))
            }
            if(length(parseSavePath(available.volumes, input$mergeChooseOutFile)$datapath) > 0 && exists("merge.sel.vars")) {
              full.file.list.merge$mergeSyntax <- paste0(
                'lsa.merge.data(inp.folder = "',
                parseSavePath(available.volumes, input$mergeChooseSrcDir)$datapath,
                '", ',
                merge.sel.vars,
                if(exists("mergeAllCountries") && nrow(mergeAllCountries$mergeSelectionIEA) != 0 && length(mergeAllCountries$mergeSelectionIEA[["ISOs"]]) == 1 && nrow(mergeAllCountries$mergeAvailCntIEAFiles) > 0) {
                  paste0('), ISO = "', tolower(mergeAllCountries$mergeSelectionIEA[["ISOs"]]), '"')
                } else if(exists("mergeAllCountries") && nrow(mergeAllCountries$mergeSelectionIEA) != 0 && length(mergeAllCountries$mergeSelectionIEA[["ISOs"]]) > 1 && nrow(mergeAllCountries$mergeAvailCntIEAFiles) > 0) {
                  paste0('), ISO = c("', paste(tolower(mergeAllCountries$mergeSelectionIEA[["ISOs"]]), collapse = '", "'), '")')
                } else if(exists("mergeAllCountries") && nrow(mergeAllCountries$mergeSelectionIEA) != 0 && length(mergeAllCountries$mergeSelectionIEA[["ISOs"]]) > 1 && nrow(mergeAllCountries$mergeAvailCntIEAFiles) == 0) {
                  ")"
                } else if(exists("mergeAllCountries") && nrow(mergeAllCountries$mergeAvailCntIEAFiles) == 0) {
                  ""
                },
                paste0(', out.file = "', parseSavePath(available.volumes, input$mergeChooseOutFile)$datapath, '")')
              )
            }
          })
          output$mergeOutPathDisplay <- renderText({parseSavePath(available.volumes, input$mergeChooseOutFile)$datapath})
          output$mergeSyntaxHead <- renderText({
            if(length(full.file.list.merge$RData.files) > 0 && nrow(mergeAllVars$mergeSelectedVars) > 0 && length(parseSavePath(available.volumes, input$mergeChooseOutFile)$datapath) > 0) {
              HTML("Syntax")
            } else {
              return(NULL)
            }
          })
          output$mergeSyntax <- renderText({
            if(length(full.file.list.merge$RData.files) > 0 &merge.render.vars.condition() == TRUE) {
              syntaxMergeData()
            } else {
              return(NULL)
            }
          })
          output$mergeExecBtnHead <- renderText({
            if(length(full.file.list.merge$RData.files) > 0 & nrow(mergeAllVars$mergeSelectedVars) > 0 && length(parseSavePath(available.volumes, input$mergeChooseOutFile)$datapath) > 0) {
              HTML("Press the button below to execute the syntax")
            } else {
              return(NULL)
            }
          })
          output$execMergeData <- renderUI({
            if(length(full.file.list.merge$RData.files) > 0 && nrow(mergeAllVars$mergeSelectedVars) > 0 && length(parseSavePath(available.volumes, input$mergeChooseOutFile)$datapath) > 0) {
              actionButton(inputId = "execMergeData", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
            } else {
              return(NULL)
            }
          })
          observe({
            if(nrow(mergeAllVars$mergeSelectedVars) == 0) {
              hide("consoleMergeData")
            } else {
              show("consoleMergeData")
            }
          })
        }
      }
    }
  })
  observeEvent(input$execMergeData, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consoleMergeData", "")
      expr = eval(parse(text = full.file.list.merge$mergeSyntax))
    },
    message = function(i) {
      shinyjs::html(id = "consoleMergeData", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consoleMergeData", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
    showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
  }, ignoreInit = TRUE)
  output$h1VarProperties <- renderText("Variable dictionaries")
  output$varPropsIntro <- renderText({HTML("Select large-scale assessment .RData file to load and display its variables.")})
  file.var.properties <- reactiveValues(loaded = NULL, is.lsa.data = NULL, resp.type = NULL, study = NULL, cycle = NULL, var.props.syntax = NULL)
  hide("varPropsSaveOutput")
  hide("varPropsOpenOutput")
  hide("varPropsChooseOutFile")
  shinyFileChoose(input, "varPropsChooseSrcFile", roots = available.volumes, filetype = list(RData = "RData"))
  observeEvent(eventExpr = input$varPropsChooseSrcFile, {
    file.var.properties$loaded <- NULL
    file.var.properties$is.lsa.data <- FALSE
    file.var.properties$resp.type <- NULL
    file.var.properties$study <- NULL
    file.var.properties$cycle <- NULL
    if(length(parseFilePaths(available.volumes, input$varPropsChooseSrcFile)$datapath) > 0 && file.size(parseFilePaths(available.volumes, input$varPropsChooseSrcFile)$datapath) > 104857600) {
      showNotification(ui = HTML('The size of the selected<br/>file is rather large. Please<br/>wait until the file is loaded.'), type = "message")
    }
    if(length(parseFilePaths(available.volumes, input$varPropsChooseSrcFile)$datapath) > 0) {
      file.var.properties$loaded <- get(load(parseFilePaths(available.volumes, input$varPropsChooseSrcFile)$datapath))
      if("lsa.data" %in% class(file.var.properties$loaded)) {
        file.var.properties$is.lsa.data <- TRUE
      } else {
        file.var.properties$is.lsa.data <- FALSE
      }
      file.var.properties$study <- attr(x = file.var.properties$loaded, which = "study")
      file.var.properties$cycle <- attr(x = file.var.properties$loaded, which = "cycle")
      file.var.properties$resp.type <- attr(x = file.var.properties$loaded, which = "file.type")
      file.var.properties$loaded <- data.table(Variables = names(file.var.properties$loaded), Variable_Labels = sapply(X = file.var.properties$loaded, FUN = function(j) {
        if(is.null(attr(x = j, which = "variable.label"))) {
          return(NA_character_)
        } else {
          attr(x = j, which = "variable.label")
        }
      }),
      order_col = 1:ncol(file.var.properties$loaded))
    }
    output$varPropsSrcPathDisplay <- renderText({parseFilePaths(available.volumes, input$varPropsChooseSrcFile)$datapath})
  }, ignoreInit = TRUE)
  observe({
    if(!is.null(file.var.properties$loaded) && file.var.properties$is.lsa.data == FALSE) {
      output$varPropsStudyName <- renderText({NULL})
      output$varPropsStudyCycle <- renderText({NULL})
      output$varPropsRespHead <- renderText({NULL})
      output$varPropsRespAvailable <- renderText({NULL})
      showNotification(ui = HTML('The data is not of class "lsa.data".<br/>Please check the file content.'), type = "error")
    } else if (!is.null(file.var.properties$loaded) && file.var.properties$is.lsa.data == TRUE) {
      output$varPropsStudyName <- renderText({
        if(!is.null(file.var.properties$loaded)) {
          HTML(paste0('Study: ', file.var.properties$study))
        } else {
          return(NULL)
        }
      })
      output$varPropsStudyCycle <- renderText({
        if(!is.null(file.var.properties$loaded)) {
          HTML(paste0('Cycle: ', file.var.properties$cycle))
        } else {
          return(NULL)
        }
      })
      output$varPropsRespHead <- renderText({
        if(!is.null(file.var.properties$loaded)) {
          HTML('<u>The file contains data from the following respondents:</u>')
        } else {
          return(NULL)
        }
      })
      output$varPropsRespAvailable <- renderText({
        if(is.null(file.var.properties$resp.type)) {
          return(NULL)
        } else {
          HTML(file.merged.respondents[[file.var.properties$resp.type]])
        }
      })
      output$varPropsExplText <- renderText({
        if(is.null(file.var.properties$resp.type)) {
          return(NULL)
        } else {
          HTML('Use the panels below to select the variables for which the dictionaries shall be produced.')
        }
      })
      var.props.initial.available.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      var.props.initial.selected.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      varPropsAllVars <- reactiveValues(varPropsAvailVars = var.props.initial.available.vars, varPropsSelectedVars = var.props.initial.selected.vars)
      observe({
        if(!is.null(file.var.properties$loaded)) {
          varPropsAllVars$varPropsAvailVars <- file.var.properties$loaded
        }
      })
      output$varPropsArrowSelVarsRight <- renderUI({
        if(is.null(file.var.properties$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "varPropsArrowSelVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$varPropsDblArrowSelVarsRight <- renderUI({
        if(is.null(file.var.properties$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "varPropsDblArrowSelVarsRight", label = NULL, icon("angle-double-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$varPropsArrowSelVarsLeft <- renderUI({
        if(is.null(file.var.properties$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "varPropsArrowSelVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$varPropsDblArrowSelVarsLeft <- renderUI({
        if(is.null(file.var.properties$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "varPropsDblArrowSelVarsLeft", label = NULL, icon("angle-double-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$varPropsAllAvailableVars <- renderDT({
        if(is.null(file.var.properties$resp.type)) {
          return(NULL)
        } else {
          setkeyv(x = varPropsAllVars$varPropsAvailVars, cols = "order_col")
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available variables"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "All variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 400, scroller = TRUE
      ))
      output$varPropsVarsSelection <- renderDT({
        if(is.null(file.var.properties$resp.type)) {
          varPropsAllVars$varPropsSelectedVars <- var.props.initial.selected.vars
          return(NULL)
        } else {
          setkeyv(x = varPropsAllVars$varPropsSelectedVars, cols = "order_col")
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Selected variables"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 400, scroller = TRUE
      ))
      observeEvent(input$varPropsArrowSelVarsRight, {
        req(input$varPropsAllAvailableVars_rows_selected)
        varPropsAllVars$varPropsSelectedVars <- rbind(isolate(varPropsAllVars$varPropsSelectedVars), varPropsAllVars$varPropsAvailVars[input$varPropsAllAvailableVars_rows_selected, , drop = FALSE])
        varPropsAllVars$varPropsSelectedVars <- varPropsAllVars$varPropsSelectedVars[complete.cases(varPropsAllVars$varPropsSelectedVars[ , "Variables"]), , drop = FALSE]
        varPropsAllVars$varPropsAvailVars <- isolate(varPropsAllVars$varPropsAvailVars[-input$varPropsAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$varPropsDblArrowSelVarsRight, {
        varPropsAllVars$varPropsSelectedVars <- rbind(varPropsAllVars$varPropsSelectedVars, isolate(varPropsAllVars$varPropsAvailVars))
        varPropsAllVars$varPropsAvailVars <- var.props.initial.available.vars
      })
      observeEvent(input$varPropsArrowSelVarsLeft, {
        req(input$varPropsVarsSelection_rows_selected)
        varPropsAllVars$varPropsAvailVars <- rbind(isolate(varPropsAllVars$varPropsAvailVars),        varPropsAllVars$varPropsSelectedVars[input$varPropsVarsSelection_rows_selected, , drop = FALSE])
        varPropsAllVars$varPropsAvailVars <- varPropsAllVars$varPropsAvailVars[complete.cases(varPropsAllVars$varPropsAvailVars[ , "Variables"]), , drop = FALSE]
        varPropsAllVars$varPropsSelectedVars <- isolate(varPropsAllVars$varPropsSelectedVars[-input$varPropsVarsSelection_rows_selected, , drop = FALSE])
      })
      observeEvent(input$varPropsDblArrowSelVarsLeft, {
        varPropsAllVars$varPropsAvailVars <- rbind(varPropsAllVars$varPropsAvailVars, isolate(varPropsAllVars$varPropsSelectedVars))
        varPropsAllVars$varPropsSelectedVars <- var.props.initial.selected.vars
      })
      observe({
        if(nrow(varPropsAllVars$varPropsAvailVars) == 0) {
          hide("varPropsExplText")
        } else if(nrow(varPropsAllVars$varPropsAvailVars) > 0) {
          show("varPropsExplText")
        }
        if(nrow(varPropsAllVars$varPropsSelectedVars) > 0) {
          show("varPropsSaveOutput")
        } else if(nrow(varPropsAllVars$varPropsSelectedVars) == 0) {
          updateCheckboxInput(session, inputId = "varPropsSaveOutput", value = FALSE)
          hide("varPropsSaveOutput")
          hide("consoleVarProps")
        }
        if(input$varPropsSaveOutput == 1) {
          show("varPropsOpenOutput")
        } else if(input$varPropsSaveOutput == 0) {
          updateCheckboxInput(session, inputId = "varPropsOpenOutput", value = FALSE)
          hide("varPropsOpenOutput")
        }
        if(nrow(varPropsAllVars$varPropsSelectedVars) > 0 && input$varPropsSaveOutput == 1) {
          show("varPropsChooseOutFile")
          show("varPropsOutPathDisplay")
        } else if(nrow(varPropsAllVars$varPropsSelectedVars) == 0 || input$varPropsSaveOutput == 0) {
          hide("varPropsChooseOutFile")
          hide("varPropsOutPathDisplay")
        }
        if(input$varPropsSaveOutput == 1 & length(parseSavePath(available.volumes, input$varPropsChooseOutFile)$datapath) == 1) {
          show("varPropsSyntaxHead")
          show("varPropsSyntax")
          show("varPropsExecBtnHead")
          show("execVarProps")
        } else if(input$varPropsSaveOutput == 1 & length(parseSavePath(available.volumes, input$varPropsChooseOutFile)$datapath) == 0) {
          hide("varPropsSyntaxHead")
          hide("varPropsSyntax")
          hide("varPropsExecBtnHead")
          hide("execVarProps")
        }
      })
      observeEvent(input$execVarProps, {
        show("consoleVarProps")
      })
      shinyFileSave(input, "varPropsChooseOutFile", filetype = list(txt = "txt"), roots = available.volumes, updateFreq = 100000)
      output$varPropsOutPathDisplay <- renderText({parseSavePath(available.volumes, input$varPropsChooseOutFile)$datapath})
      syntaxVarProps <- reactive({
        file.var.properties$var.props.syntax <- paste0('lsa.vars.dict(data.file = "', parseFilePaths(available.volumes, input$varPropsChooseSrcFile)$datapath,
                                                       if(nrow(varPropsAllVars$varPropsAvailVars) == 0) {
                                                         '"'
                                                       } else {
                                                         if(nrow(varPropsAllVars$varPropsSelectedVars) > 1) {
                                                           paste0('", var.names = c("', paste(varPropsAllVars$varPropsSelectedVars[ , Variables], collapse = '", "'), '")')
                                                         } else {
                                                           paste0('", var.names = "', varPropsAllVars$varPropsSelectedVars[ , Variables], '"')
                                                         }
                                                       },
                                                       if(input$varPropsSaveOutput == FALSE & length(parseSavePath(available.volumes, input$varPropsChooseOutFile)$datapath) == 0) {
                                                         NULL
                                                       } else if(input$varPropsSaveOutput == TRUE && length(parseSavePath(available.volumes, input$varPropsChooseOutFile)$datapath) > 0) {
                                                         paste0(', out.file = "', parseSavePath(available.volumes, input$varPropsChooseOutFile)$datapath, '"')
                                                       },
                                                       if(input$varPropsOpenOutput == 1) {
                                                         paste0(', open.out.file = TRUE)')
                                                       } else {
                                                         ')'
                                                       }
        )
      })
      output$varPropsSyntaxHead <- renderText({
        if(nrow(varPropsAllVars$varPropsSelectedVars) > 0) {
          HTML("Syntax")
        } else {
          return(NULL)
        }
      })
      output$varPropsSyntax <- renderText({
        if(nrow(varPropsAllVars$varPropsSelectedVars) > 0) {
          syntaxVarProps()
        } else {
          return(NULL)
        }
      })
      output$varPropsExecBtnHead <- renderText({
        if(nrow(varPropsAllVars$varPropsSelectedVars) > 0) {
          HTML("Press the button below to execute the syntax")
        } else {
          return(NULL)
        }
      })
      output$execVarProps <- renderUI({
        if(nrow(varPropsAllVars$varPropsSelectedVars) > 0) {
          actionButton(inputId = "execVarProps", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
    }
  })
  observeEvent(input$execVarProps, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consoleVarProps", "")
      expr = eval(parse(text = file.var.properties$var.props.syntax))
    },
    message = function(i) {
      shinyjs::html(id = "consoleVarProps", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consoleVarProps", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
    showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
  }, ignoreInit = TRUE)
  output$h1DataDiag <- renderText("Data diagnostics")
  hide("dataDiagChooseOutFile")
  output$dataDiagIntro <- renderText({HTML("Select large-scale assessment .RData file to load.")})
  file.data.diag <- reactiveValues(loaded = NULL, is.lsa.data = NULL, resp.type = NULL, study = NULL, cycle = NULL, country.ID = NULL, default.weight = NULL, data.diag.syntax = NULL)
  shinyFileChoose(input, "dataDiagChooseSrcFile", roots = available.volumes, filetype = list(RData = "RData"))
  observeEvent(eventExpr = input$dataDiagChooseSrcFile, {
    file.data.diag$loaded <- NULL
    file.data.diag$study <- NULL
    file.data.diag$cycle <- NULL
    file.data.diag$resp.type <- NULL
    file.data.diag$default.weight <- NULL
    if(length(parseFilePaths(available.volumes, input$dataDiagChooseSrcFile)$datapath) > 0 && file.size(parseFilePaths(available.volumes, input$dataDiagChooseSrcFile)$datapath) > 104857600) {
      showNotification(ui = HTML('The size of the selected<br/>file is rather large. Please<br/>wait until the file is loaded.'), type = "message")
    }
    if(length(parseFilePaths(available.volumes, input$dataDiagChooseSrcFile)$datapath) > 0) {
      file.data.diag$loaded <- get(load(parseFilePaths(available.volumes, input$dataDiagChooseSrcFile)$datapath))
      if("lsa.data" %in% class(file.data.diag$loaded)) {
        file.data.diag$is.lsa.data <- TRUE
      } else {
        file.data.diag$is.lsa.data <- FALSE
      }
      file.data.diag$study <- attr(x = file.data.diag$loaded, which = "study")
      file.data.diag$cycle <- attr(x = file.data.diag$loaded, which = "cycle")
      file.data.diag$resp.type <- attr(x = file.data.diag$loaded, which = "file.type")
      file.data.diag$loaded <- data.table(Variables = names(file.data.diag$loaded), Variable_Labels = sapply(X = file.data.diag$loaded, FUN = function(j) {
        if(is.null(attr(x = j, which = "variable.label"))) {
          return(NA_character_)
        } else {
          attr(x = j, which = "variable.label")
        }
      }),
      order_col = 1:ncol(file.data.diag$loaded))
      if(!is.null(file.data.diag$study)) {
        file.data.diag$default.weight <- define.default.weight(study = file.data.diag$study, loaded.names.and.labels = file.data.diag$loaded, respondent.type = file.data.diag$resp.type)
      }
      file.data.diag$country.ID <- NULL
      if("IDCNTRY" %in% file.data.diag$loaded[ , Variables]) {
        file.data.diag$country.ID <- "IDCNTRY"
      } else {
        file.data.diag$country.ID <- "CNT"
      }
    }
    output$dataDiagSrcPathDisplay <- renderText({parseFilePaths(available.volumes, input$dataDiagChooseSrcFile)$datapath})
  }, ignoreInit = TRUE)
  observe({
    if(!is.null(file.data.diag$loaded) && file.data.diag$is.lsa.data == FALSE) {
      showNotification(ui = HTML('The data is not of class "lsa.data".<br/>Please check the file content.'), type = "error")
    } else if (!is.null(file.data.diag$loaded) && file.data.diag$is.lsa.data == TRUE) {
      output$dataDiagStudyName <- renderText({
        if(is.null(file.data.diag$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Study: ', file.data.diag$study))
        }
      })
      output$dataDiagStudyCycle <- renderText({
        if(is.null(file.data.diag$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Cycle: ', file.data.diag$cycle))
        }
      })
      output$dataDiagRespHead <- renderText({
        if(is.null(file.data.diag$resp.type)) {
          return(NULL)
        } else {
          HTML('<u>The file contains data from the following respondents:</u>')
        }
      })
      output$dataDiagRespAvailable <- renderText({
        if(is.null(file.data.diag$resp.type)) {
          return(NULL)
        } else {
          HTML(file.merged.respondents[[file.data.diag$resp.type]])
        }
      })
      output$dataDiagNoWeights <- renderText({
        if(!is.null(file.data.diag$loaded) && is.null(file.data.diag$default.weight) || !is.null(file.data.diag$loaded) && length(file.data.diag$default.weight) == 0) {
          HTML('Error: The loaded file does not contain any recognizable default weight variable. Such files cannot be analyzed on their own and must be merged with other respondents in advance.')
        } else {
          return(NULL)
        }
      })
      output$dataDiagVariablesExplText <- renderText({
        if(is.null(file.data.diag$resp.type)) {
          return(NULL)
        } else {
          HTML('Use the panels below to select the variables to produce data diagnostic tables for variables within groups specified by splitting variables.')
        }
      })
      data.diag.initial.available.vars <- file.data.diag$loaded[!Variables %in% c(file.data.diag$default.weight, file.data.diag$country.ID), ]
      data.diag.initial.selected.split.vars <- file.data.diag$loaded[Variables == file.data.diag$country.ID, ]
      data.diag.initial.selected.anal.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      data.diag.initial.selected.weight.var <- file.data.diag$loaded[Variables %in% file.data.diag$default.weight, ]
      dataDiagAllVars <- reactiveValues(dataDiagAvailVars = data.diag.initial.available.vars, dataDiagSelectedSplitVars = data.diag.initial.selected.split.vars, dataDiagSelectedAnalVars = data.diag.initial.selected.anal.vars, dataDiagSelectedWeightVar = data.diag.initial.selected.weight.var)
      output$dataDiagArrowSelSplitVarsRight <- renderUI({
        if(is.null(file.data.diag$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "dataDiagArrowSelSplitVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$dataDiagArrowSelSplitVarsLeft <- renderUI({
        if(is.null(file.data.diag$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "dataDiagArrowSelSplitVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$dataDiagArrowSelAnalVarsRight <- renderUI({
        if(is.null(file.data.diag$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "dataDiagArrowSelAnalVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$dataDiagArrowSelAnalVarsLeft <- renderUI({
        if(is.null(file.data.diag$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "dataDiagArrowSelAnalVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$dataDiagArrowSelWeightVarsRight <- renderUI({
        if(is.null(file.data.diag$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "dataDiagArrowSelWeightVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$dataDiagArrowSelWeightVarsLeft <- renderUI({
        if(is.null(file.data.diag$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "dataDiagArrowSelWeightVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      observeEvent(input$dataDiagArrowSelSplitVarsRight, {
        req(input$dataDiagAllAvailableVars_rows_selected)
        dataDiagAllVars$dataDiagSelectedSplitVars <- rbind(isolate(dataDiagAllVars$dataDiagSelectedSplitVars), dataDiagAllVars$dataDiagAvailVars[input$dataDiagAllAvailableVars_rows_selected, , drop = FALSE])
        dataDiagAllVars$dataDiagSelectedSplitVars <- dataDiagAllVars$dataDiagSelectedSplitVars[complete.cases(dataDiagAllVars$dataDiagSelectedSplitVars[ , "Variables"]), , drop = FALSE]
        dataDiagAllVars$dataDiagAvailVars <- isolate(dataDiagAllVars$dataDiagAvailVars[-input$dataDiagAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$dataDiagArrowSelSplitVarsLeft, {
        req(input$dataDiagSplitVars_rows_selected)
        dataDiagAllVars$dataDiagAvailVars <- rbind(isolate(dataDiagAllVars$dataDiagAvailVars),        dataDiagAllVars$dataDiagSelectedSplitVars[input$dataDiagSplitVars_rows_selected, , drop = FALSE])
        dataDiagAllVars$dataDiagAvailVars <- dataDiagAllVars$dataDiagAvailVars[complete.cases(dataDiagAllVars$dataDiagAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(dataDiagAllVars$dataDiagSelectedSplitVars) > 0) {
          dataDiagAllVars$dataDiagSelectedSplitVars <- isolate(dataDiagAllVars$dataDiagSelectedSplitVars[-input$dataDiagSplitVars_rows_selected, , drop = FALSE])
        }
        if(!file.data.diag$country.ID %in% dataDiagAllVars$dataDiagSelectedSplitVars[ , Variables]) {
          showNotification(ui = HTML("Country ID <b>must always be</b> the first splitting variable!"), type = "error")
        }
        dataDiagAllVars$dataDiagSelectedSplitVars <- rbindlist(l = list(dataDiagAllVars$dataDiagSelectedSplitVars, dataDiagAllVars$dataDiagAvailVars[Variables == file.data.diag$country.ID, ]))
        dataDiagAllVars$dataDiagAvailVars <- dataDiagAllVars$dataDiagAvailVars[Variables != file.data.diag$country.ID, ]
      })
      observeEvent(input$dataDiagArrowSelAnalVarsRight, {
        req(input$dataDiagAllAvailableVars_rows_selected)
        dataDiagAllVars$dataDiagSelectedAnalVars <- rbind(isolate(dataDiagAllVars$dataDiagSelectedAnalVars), dataDiagAllVars$dataDiagAvailVars[input$dataDiagAllAvailableVars_rows_selected, , drop = FALSE])
        dataDiagAllVars$dataDiagSelectedAnalVars <- dataDiagAllVars$dataDiagSelectedAnalVars[complete.cases(dataDiagAllVars$dataDiagSelectedAnalVars[ , "Variables"]), , drop = FALSE]
        dataDiagAllVars$dataDiagAvailVars <- isolate(dataDiagAllVars$dataDiagAvailVars[-input$dataDiagAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$dataDiagArrowSelAnalVarsLeft, {
        req(input$dataDiagAnalVars_rows_selected)
        dataDiagAllVars$dataDiagAvailVars <- rbind(isolate(dataDiagAllVars$dataDiagAvailVars),        dataDiagAllVars$dataDiagSelectedAnalVars[input$dataDiagAnalVars_rows_selected, , drop = FALSE])
        dataDiagAllVars$dataDiagAvailVars <- dataDiagAllVars$dataDiagAvailVars[complete.cases(dataDiagAllVars$dataDiagAvailVars[ , "Variables"]), , drop = FALSE]
        dataDiagAllVars$dataDiagSelectedAnalVars <- isolate(dataDiagAllVars$dataDiagSelectedAnalVars[-input$dataDiagAnalVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$dataDiagArrowSelWeightVarsRight, {
        req(input$dataDiagAllAvailableVars_rows_selected)
        dataDiagAllVars$dataDiagSelectedWeightVar <- rbind(isolate(dataDiagAllVars$dataDiagSelectedWeightVar), dataDiagAllVars$dataDiagAvailVars[input$dataDiagAllAvailableVars_rows_selected, , drop = FALSE])
        dataDiagAllVars$dataDiagSelectedWeightVar <- dataDiagAllVars$dataDiagSelectedWeightVar[complete.cases(dataDiagAllVars$dataDiagSelectedWeightVar[ , "Variables"]), , drop = FALSE]
        dataDiagAllVars$dataDiagAvailVars <- isolate(dataDiagAllVars$dataDiagAvailVars[-input$dataDiagAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$dataDiagArrowSelWeightVarsLeft, {
        req(input$dataDiagWeightVar_rows_selected)
        dataDiagAllVars$dataDiagAvailVars <- rbind(isolate(dataDiagAllVars$dataDiagAvailVars),        dataDiagAllVars$dataDiagSelectedWeightVar[input$dataDiagWeightVar_rows_selected, , drop = FALSE])
        dataDiagAllVars$dataDiagAvailVars <- dataDiagAllVars$dataDiagAvailVars[complete.cases(dataDiagAllVars$dataDiagAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(dataDiagAllVars$dataDiagSelectedWeightVar) > 0) {
          dataDiagAllVars$dataDiagSelectedWeightVar <- isolate(dataDiagAllVars$dataDiagSelectedWeightVar[-input$dataDiagWeightVar_rows_selected, , drop = FALSE])
        }
      })
      output$dataDiagAllAvailableVars <- renderDT({
        setkeyv(x = dataDiagAllVars$dataDiagAvailVars, cols = "order_col")
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available variables"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables available"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 517, scroller = TRUE
      ))
      output$dataDiagSplitVars <- renderDT({
        dataDiagAllVars$dataDiagSelectedSplitVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Split variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$dataDiagInclMiss <- renderUI({
        if(nrow(dataDiagAllVars$dataDiagSelectedSplitVars) == 0) {
          return(NULL)
        } else {
          checkboxInput(inputId = "dataDiagInclMiss", label = "Compute statistics for the missing values of the split variables", value = FALSE, width = "400px")
        }
      })
      output$dataDiagAnalVars <- renderDT({
        dataDiagAllVars$dataDiagSelectedAnalVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Analysis variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$dataDiagWeightVar <- renderDT({
        dataDiagAllVars$dataDiagSelectedWeightVar
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Weight variable"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = paste0("No weight selected, the tables will contain unweighted statistics")),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$dataDiagWgtsNotWgts <- renderText({
        if(any(dataDiagAllVars$dataDiagSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE)) {
          HTML('Warning: The variable in "Weight variable" is not a weight. Please check the added variable.')
        } else {
          return(NULL)
        }
      })
      observe({
        if(nrow(dataDiagAllVars$dataDiagSelectedWeightVar) > 1) {
          showNotification(ui = HTML("Only one weight variable can be selected!"), type = "error")
          dataDiagAllVars$dataDiagAvailVars <- rbind(isolate(dataDiagAllVars$dataDiagAvailVars),        dataDiagAllVars$dataDiagSelectedWeightVar[nrow(dataDiagAllVars$dataDiagSelectedWeightVar), , drop = FALSE])
          dataDiagAllVars$dataDiagAvailVars <- dataDiagAllVars$dataDiagAvailVars[complete.cases(dataDiagAllVars$dataDiagAvailVars[ , "Variables"]), , drop = FALSE]
          dataDiagAllVars$dataDiagSelectedWeightVar <- isolate(dataDiagAllVars$dataDiagSelectedWeightVar[-2, , drop = FALSE])
        }
      })
      output$dataDiagContFreq <- renderUI({
        checkboxInput(inputId = "dataDiagContFreq", label = "Compute frequencies for continuous variables", value = FALSE, width = "450px")
      })
      shinyFileSave(input, "dataDiagChooseOutFile", filetype = list(xlsx = "xlsx"), roots = available.volumes, updateFreq = 100000)
      output$dataDiagOpenOutput <- renderUI({
        if(length(parseSavePath(available.volumes, input$dataDiagChooseOutFile)$datapath) > 0) {
          checkboxInput(inputId = "dataDiagOpenOutput", label = "Open the output when done", value = TRUE, width = "250px")
        }
      })
      syntaxDataDiag <- reactive({
        file.data.diag$data.diag.syntax <- paste0(
          'lsa.data.diag(data.file = "', parseFilePaths(available.volumes, input$dataDiagChooseSrcFile)$datapath, '", ',
          if(length(dataDiagAllVars$dataDiagSelectedSplitVars[ , Variables]) == 1) {
            paste0('split.vars = "', dataDiagAllVars$dataDiagSelectedSplitVars[ , Variables], '"')
          } else if(length(dataDiagAllVars$dataDiagSelectedSplitVars[ , Variables]) > 1) {
            paste0('split.vars = c("', paste(dataDiagAllVars$dataDiagSelectedSplitVars[ , Variables], collapse = '", "'), '")')
          },
          if(nrow(dataDiagAllVars$dataDiagSelectedAnalVars) == 1) {
            paste0(', variables = "', dataDiagAllVars$dataDiagSelectedAnalVars[ , Variables], '"')
          } else if(nrow(dataDiagAllVars$dataDiagSelectedAnalVars) > 1) {
            paste0(', variables = c("', paste(dataDiagAllVars$dataDiagSelectedAnalVars[ , Variables], collapse = '", "'), '")')
          },
          if(nrow(dataDiagAllVars$dataDiagSelectedWeightVar) == 1 && dataDiagAllVars$dataDiagSelectedWeightVar[ , Variables] == file.data.diag$default.weight) {
            NULL
          } else if(nrow(dataDiagAllVars$dataDiagSelectedWeightVar) == 1 && dataDiagAllVars$dataDiagSelectedWeightVar[ , Variables] %in% all.studies.available.weights && dataDiagAllVars$dataDiagSelectedWeightVar[ , Variables] != file.data.diag$default.weight) {
            paste0(', weight.var = "', dataDiagAllVars$dataDiagSelectedWeightVar[ , Variables], '"')
          } else if(nrow(dataDiagAllVars$dataDiagSelectedWeightVar) == 0) {
            paste0(', weight.var = "none"')
          },
          if(!is.null(input$dataDiagContFreq) && input$dataDiagContFreq == TRUE) {
            ", cont.freq = TRUE"
          },
          if(!is.null(input$dataDiagInclMiss) && input$dataDiagInclMiss == TRUE) {
            ", include.missing = TRUE"
          },
          paste0(', output.file = "', parseSavePath(available.volumes, input$dataDiagChooseOutFile)$datapath, '"'),
          if(!is.null(input$dataDiagOpenOutput) && input$dataDiagOpenOutput == FALSE) {
            ', open.output = FALSE'
          } else if(!is.null(input$dataDiagOpenOutput) && input$dataDiagOpenOutput == TRUE) {
            NULL
          },
          ')'
        )
      })
      output$dataDiagSyntaxHead <- renderText({
        if(length(parseSavePath(available.volumes, input$dataDiagChooseOutFile)$datapath) == 1) {
          HTML("Syntax")
        } else {
          return(NULL)
        }
      })
      output$dataDiagSyntax <- renderText({
        if(length(parseSavePath(available.volumes, input$dataDiagChooseOutFile)$datapath) == 1) {
          syntaxDataDiag()
        } else {
          return(NULL)
        }
      })
      output$dataDiagExecBtnHead <- renderText({
        if(length(parseSavePath(available.volumes, input$dataDiagChooseOutFile)$datapath) == 1) {
          HTML("Press the button below to execute the syntax")
        } else {
          return(NULL)
        }
      })
      output$execDataDiag <- renderUI({
        if(length(parseSavePath(available.volumes, input$dataDiagChooseOutFile)$datapath) == 1) {
          actionButton(inputId = "execDataDiag", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      observe({
        if(is.null(file.data.diag$loaded) || nrow(dataDiagAllVars$dataDiagSelectedAnalVars) < 1 || any(dataDiagAllVars$dataDiagSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE) || is.null(file.data.diag$default.weight) || length(file.data.diag$default.weight) == 0) {
          hide("dataDiagContFreq")
          hide("dataDiagChooseOutFile")
          hide("dataDiagOpenOutput")
          hide("dataDiagSyntaxHead")
          hide("dataDiagSyntax")
          hide("dataDiagExecBtnHead")
          hide("execDataDiag")
          hide("consoleDataDiag")
        } else if (!is.null(file.data.diag$loaded) || nrow(dataDiagAllVars$dataDiagSelectedAnalVars) > 0 || any(dataDiagAllVars$dataDiagSelectedWeightVar[ , Variables] %in% all.studies.available.weights == TRUE) || !is.null(file.data.diag$default.weight) || length(file.data.diag$default.weight) != 0) {
          show("dataDiagContFreq")
          show("dataDiagChooseOutFile")
          show("dataDiagOpenOutput")
          show("dataDiagSyntaxHead")
          show("dataDiagSyntax")
          show("dataDiagExecBtnHead")
          show("execDataDiag")
          show("consoleDataDiag")
        }
        if(is.null(file.data.diag$default.weight) || length(file.data.diag$default.weight) == 0) {
          hide("dataDiagVariablesExplText")
          hide("dataDiagAllAvailableVars")
          hide("dataDiagArrowSelSplitVarsRight")
          hide("dataDiagArrowSelSplitVarsLeft")
          hide("dataDiagSplitVars")
          hide("dataDiagInclMiss")
          hide("dataDiagArrowSelAnalVarsRight")
          hide("dataDiagArrowSelAnalVarsLeft")
          hide("dataDiagAnalVars")
          hide("dataDiagArrowSelWeightVarsRight")
          hide("dataDiagArrowSelWeightVarsLeft")
          hide("dataDiagWeightVar")
        } else if(!is.null(file.data.diag$default.weight) || length(file.data.diag$default.weight) != 0) {
          show("dataDiagVariablesExplText")
          show("dataDiagAllAvailableVars")
          show("dataDiagArrowSelSplitVarsRight")
          show("dataDiagArrowSelSplitVarsLeft")
          show("dataDiagSplitVars")
          show("dataDiagInclMiss")
          show("dataDiagArrowSelAnalVarsRight")
          show("dataDiagArrowSelAnalVarsLeft")
          show("dataDiagAnalVars")
          show("dataDiagArrowSelWeightVarsRight")
          show("dataDiagArrowSelWeightVarsLeft")
          show("dataDiagWeightVar")
        }
      })
    }
  })
  observeEvent(input$execDataDiag, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consoleDataDiag", "")
      tryCatch({
        expr = eval(parse(text = file.data.diag$data.diag.syntax))
        showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
      }, error = function(e) {
        message("", e)
        showNotification(ui = HTML(paste0("Something went wrong. Possible reasons: <br/>", paste(gsub(pattern = "\\n|Error: ", replacement = "<br/>", x = e)))), type = "error", duration = NULL)
      })
    },
    message = function(i) {
      shinyjs::html(id = "consoleDataDiag", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consoleDataDiag", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
  }, ignoreInit = TRUE)
  hide("recodeInNewVars")
  hide("recodeNewMissings")
  hide("recodeChooseOutFile")
  output$h1RecodeVars <- renderText("Recode variables")
  output$recodeIntro <- renderText({HTML("Select large-scale assessment .RData file to load.")})
  file.var.recode <- reactiveValues(loaded = NULL, is.lsa.data = NULL, resp.type = NULL, study = NULL, cycle = NULL, var.levels = NULL, var.num.values = NULL, var.char.values = NULL, var.missings = NULL, var.classes = NULL, recode.syntax = NULL)
  shinyFileChoose(input, "recodeChooseSrcFile", roots = available.volumes, filetype = list(RData = "RData"))
  observeEvent(eventExpr = input$recodeChooseSrcFile, {
    file.var.recode$loaded <- NULL
    file.var.recode$is.lsa.data <- FALSE
    file.var.recode$resp.type <- NULL
    file.var.recode$study <- NULL
    file.var.recode$cycle <- NULL
    file.var.recode$var.classes <- NULL
    if(length(parseFilePaths(available.volumes, input$recodeChooseSrcFile)$datapath) > 0 && file.size(parseFilePaths(available.volumes, input$recodeChooseSrcFile)$datapath) > 104857600) {
      showNotification(ui = HTML('The size of the selected<br/>file is rather large. Please<br/>wait until the file is loaded.'), type = "message")
    }
    session$sendCustomMessage("unbindDT", "recodeSchemeFAC")
    session$sendCustomMessage("unbindDT", "recodeSchemeNUM")
    session$sendCustomMessage("unbindDT", "recodeSchemeCHAR")
    session$sendCustomMessage("unbindDT", "recodeNewVarLabels")
    session$sendCustomMessage("unbindDT", "recodeNewVarNames")
    if(length(parseFilePaths(available.volumes, input$recodeChooseSrcFile)$datapath) > 0) {
      file.var.recode$loaded <- get(load(parseFilePaths(available.volumes, input$recodeChooseSrcFile)$datapath))
      file.var.recode$var.levels <- Filter(Negate(is.null), lapply(X = file.var.recode$loaded, FUN = function(i) {
        if(is.null(attr(x = i, which = "levels"))) {
          NULL
        } else {
          attr(x = i, which = "levels")
        }
      }))
      file.var.recode$var.classes <- lapply(X = file.var.recode$loaded, FUN = class)
      file.var.recode$var.num.values <- Filter(Negate(is.null), lapply(X = file.var.recode$loaded, FUN = function(i) {
        if(!is.numeric(i)) {
          NULL
        } else {
          sort(unique(i[!is.na(i)]))
        }
      }))
      file.var.recode$var.char.values <- Filter(Negate(is.null), lapply(X = file.var.recode$loaded, FUN = function(i) {
        if(!is.character(i)) {
          NULL
        } else {
          unique(i[!is.na(i)])
        }
      }))
      file.var.recode$missings <- Filter(Negate(is.null), lapply(X = file.var.recode$loaded, FUN = function(i) {
        if(is.null(attr(x = i, which = "missings"))) {
          NULL
        } else {
          if(is.null(names(i))) {
            attr(x = i, which = "missings")
          } else {
            tmp.names.miss <- names(attr(x = i, which = "missings"))
            tmp.miss <- attr(x = i, which = "missings")
            names(tmp.miss) <- tmp.names.miss
          }
        }
      }))
      if("lsa.data" %in% class(file.var.recode$loaded)) {
        file.var.recode$is.lsa.data <- TRUE
      } else {
        file.var.recode$is.lsa.data <- FALSE
      }
      file.var.recode$study <- attr(x = file.var.recode$loaded, which = "study")
      file.var.recode$cycle <- attr(x = file.var.recode$loaded, which = "cycle")
      file.var.recode$resp.type <- attr(x = file.var.recode$loaded, which = "file.type")
      file.var.recode$loaded <- data.table(Variables = names(file.var.recode$loaded), Variable_Labels = sapply(X = file.var.recode$loaded, FUN = function(j) {
        if(is.null(attr(x = j, which = "variable.label"))) {
          return(NA_character_)
        } else {
          attr(x = j, which = "variable.label")
        }
      }),
      order_col = 1:ncol(file.var.recode$loaded))
    }
    output$recodeSrcPathDisplay <- renderText({parseFilePaths(available.volumes, input$recodeChooseSrcFile)$datapath})
  }, ignoreInit = TRUE)
  observe({
    if(!is.null(file.var.recode$loaded) && file.var.recode$is.lsa.data == FALSE) {
      showNotification(ui = HTML('The data is not of class "lsa.data".<br/>Please check the file content.'), type = "error")
    } else if (!is.null(file.var.recode$loaded) && file.var.recode$is.lsa.data == TRUE) {
      output$recodeStudyName <- renderText({
        if(is.null(file.var.recode$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Study: ', file.var.recode$study))
        }
      })
      output$recodeStudyCycle <- renderText({
        if(is.null(file.var.recode$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Cycle: ', file.var.recode$cycle))
        }
      })
      output$recodeRespHead <- renderText({
        if(is.null(file.var.recode$resp.type)) {
          return(NULL)
        } else {
          HTML('<u>The file contains data from the following respondents:</u>')
        }
      })
      output$recodeRespAvailable <- renderText({
        if(is.null(file.var.recode$resp.type)) {
          return(NULL)
        } else {
          HTML(file.merged.respondents[[file.var.recode$resp.type]])
        }
      })
      output$recodeVariablesExplText <- renderText({
        if(is.null(file.var.recode$resp.type)) {
          return(NULL)
        } else {
          HTML('Use the panels below to select the variables which shall be recoded.<br/><b>Note: The selected variables <u>must</u> have the same structure - same class, number of levels (if they are factors), same user-defined missing values (if any).</b><br/>Running the "Variable dictionaties" module in advance can be helpful to identify the structure of the variables of interest.')
        }
      })
      recode.initial.available.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      recode.initial.selected.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      recodeAllVars <- reactiveValues(recodeAvailVars = recode.initial.available.vars, recodeSelectedVars = recode.initial.selected.vars)
      observe({
        if(!is.null(file.var.recode$loaded)) {
          recodeAllVars$recodeAvailVars <- file.var.recode$loaded
        }
      })
      output$recodeArrowSelVarsRight <- renderUI({
        if(is.null(file.var.recode$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "recodeArrowSelVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$recodeArrowSelVarsLeft <- renderUI({
        if(is.null(file.var.recode$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "recodeArrowSelVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$recodeAllAvailableVars <- renderDT({
        if(is.null(file.var.recode$resp.type)) {
          return(NULL)
        } else {
          setkeyv(x = recodeAllVars$recodeAvailVars, cols = "order_col")
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available variables"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "All variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 400, scroller = TRUE
      ))
      output$recodeVarsSelection <- renderDT({
        if(is.null(file.var.recode$resp.type)) {
          return(NULL)
        } else {
          setkeyv(x = recodeAllVars$recodeSelectedVars, cols = "order_col")
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Selected variables"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 400, scroller = TRUE
      ))
      observeEvent(input$recodeArrowSelVarsRight, {
        req(input$recodeAllAvailableVars_rows_selected)
        recodeAllVars$recodeSelectedVars <- rbind(isolate(recodeAllVars$recodeSelectedVars), recodeAllVars$recodeAvailVars[input$recodeAllAvailableVars_rows_selected, , drop = FALSE])
        recodeAllVars$recodeSelectedVars <- recodeAllVars$recodeSelectedVars[complete.cases(recodeAllVars$recodeSelectedVars[ , "Variables"]), , drop = FALSE]
        recodeAllVars$recodeAvailVars <- isolate(recodeAllVars$recodeAvailVars[-input$recodeAllAvailableVars_rows_selected, , drop = FALSE])
        session$sendCustomMessage("unbindDT", "recodeSchemeFAC")
        session$sendCustomMessage("unbindDT", "recodeSchemeNUM")
        session$sendCustomMessage("unbindDT", "recodeSchemeCHAR")
        session$sendCustomMessage("unbindDT", "recodeNewVarLabels")
        session$sendCustomMessage("unbindDT", "recodeNewVarNames")
      })
      observeEvent(input$recodeArrowSelVarsLeft, {
        req(input$recodeVarsSelection_rows_selected)
        recodeAllVars$recodeAvailVars <- rbind(isolate(recodeAllVars$recodeAvailVars), recodeAllVars$recodeSelectedVars[input$recodeVarsSelection_rows_selected, , drop = FALSE])
        recodeAllVars$recodeAvailVars <- recodeAllVars$recodeAvailVars[complete.cases(recodeAllVars$recodeAvailVars[ , "Variables"]), , drop = FALSE]
        recodeAllVars$recodeSelectedVars <- isolate(recodeAllVars$recodeSelectedVars[-input$recodeVarsSelection_rows_selected, , drop = FALSE])
        session$sendCustomMessage("unbindDT", "recodeSchemeFAC")
        session$sendCustomMessage("unbindDT", "recodeSchemeNUM")
        session$sendCustomMessage("unbindDT", "recodeSchemeCHAR")
        session$sendCustomMessage("unbindDT", "recodeNewVarLabels")
        session$sendCustomMessage("unbindDT", "recodeNewVarNames")
      })
      output$recodeMissingsWarn <- renderText({
        if(new.values.and.labels.mismatch$diff.missings == TRUE) {
          if(all(paste0(new.recoding.labels.FAC$labels, new.recoding.labels.NUM$labels, new.recoding.labels.CHAR$labels) == "")) {
            HTML('Warning: The new missing values do not match any of the the newly entered values. Please check the new values.')
          } else {
            HTML('Warning: The new missing values do not match any of the newly entered value labels. Please check the new value labels.')
          }
        }
      })
      recode.sel.vars.mismatch <- reactiveValues(value = NULL)
      observe({
        recode.sel.vars.classes <- unique(unlist(file.var.recode$var.classes[recodeAllVars$recodeSelectedVars[ , Variables]]))
        recode.FAC.unique <- length(unique(file.var.recode$var.levels[recodeAllVars$recodeSelectedVars[ , Variables]]))
        recode.NUM.unique <- length(unique(file.var.recode$var.num.values[recodeAllVars$recodeSelectedVars[ , Variables]]))
        recode.CHAR.unique <- length(unique(file.var.recode$var.char.values[recodeAllVars$recodeSelectedVars[ , Variables]]))
        if(length(recode.sel.vars.classes) > 1) {
          recode.sel.vars.mismatch$value <- FALSE
        } else if(length(recode.sel.vars.classes) == 1 && recode.sel.vars.classes == "factor") {
          if(recode.FAC.unique == 1) {
            recode.sel.vars.mismatch$value <- TRUE
          } else if(recode.FAC.unique > 1) {
            recode.sel.vars.mismatch$value <- FALSE
          } else if(recode.FAC.unique == 0) {
            recode.sel.vars.mismatch$value <- NULL
          }
        } else if(length(recode.sel.vars.classes) == 1 && recode.sel.vars.classes == "numeric") {
          if(recode.NUM.unique == 1) {
            recode.sel.vars.mismatch$value <- TRUE
          } else if(recode.NUM.unique > 1) {
            recode.sel.vars.mismatch$value <- FALSE
          } else if(recode.NUM.unique == 0) {
            recode.sel.vars.mismatch$value <- NULL
          }
        } else if(length(recode.sel.vars.classes) == 1 && recode.sel.vars.classes == "character") {
          if(recode.CHAR.unique == 1) {
            recode.sel.vars.mismatch$value <- TRUE
          } else if(recode.CHAR.unique > 1) {
            recode.sel.vars.mismatch$value <- FALSE
          } else if(recode.CHAR.unique == 0) {
            recode.sel.vars.mismatch$value <- NULL
          }
        }
      })
      output$recodeSchemeExpl <- renderText({
        if(nrow(recodeAllVars$recodeSelectedVars) > 0 && recode.sel.vars.mismatch$value == TRUE) {
          if(!is.null(file.var.recode$loaded) && unlist(unique(file.var.recode$var.classes[recodeAllVars$recodeSelectedVars[ , Variables]])) == "factor") {
            HTML('The selected variables are <b>factor</b> variables. Use the table below to define the recoding scheme.<br/><b>Notes:</b><br/>1. If no new value is defined for a corresponding old value, it will be set to &ltNA>.<br/>2. If no new labels are defined, the recoded variables will be set to numeric, otherwise they will remain as factors.<br/>3. If new labels are defined, their number <b>must be</b> the same as the number of new values: <ul><li>Against each of the "New levels" where a value is defined, values for the "New labels" <b>must</b> be defined as well.</li><li>If more than one of the "Old levels" have the same "New levels" defined, their "New labels" <b>must</b> be the same as well.</li></ul>')
          } else if(!is.null(file.var.recode$loaded) && unlist(unique(file.var.recode$var.classes[recodeAllVars$recodeSelectedVars[ , Variables]])) == "numeric") {
            HTML('The selected variables are <b>numeric</b> variables. Use the table below to define the recoding scheme.<br/><b>Notes:</b><br/>1. If no new value is defined for a corresponding old value, it will be set to &ltNA>.<br/>2. If no new labels are defined, the recoded variables will remain as numeric.<br/>3. If new labels are defined in the last column, the recoded variables will be converted to factors and the labels will be defined to their levels. In this case, the number of new labels <b>must be</b> the same as the number of new values: <ul><li>Against each of the "New values" where a value is defined, values for the "New labels" <b>must be</b> defined as well.</li><li>If more than one of the "Old values" have the same "New values" defined, their "New labels" <b>must be</b> the same as well.</li></ul>')
          } else if(!is.null(file.var.recode$loaded) && unlist(unique(file.var.recode$var.classes[recodeAllVars$recodeSelectedVars[ , Variables]])) == "character") {
            HTML('The selected variables are <b>character</b> variables. Use the table below to define the recoding scheme.<br/><b>Notes:</b><br/>1. If no new value is defined for a corresponding old value, it will be set to &ltNA>.<br/>2. If no new labels are defined in the last column, the recoded variables will remain as character.<br/>3. If new labels are defined in the last column, the recoded variables will be converted to factors and the new labels will be defined to their levels. In this case, the number of new labels <b>must be</b> the same as the number of new values: <ul><li>Against each of the "New values" where a value is defined, values for the "New labels" <b>must</b> be defined as well.</li><li>If more than one of the "Old values" have the same "New values" defined, their "New labels" <b>must</b> be the same as well.</li></ul>')
          }
        } else if(!is.null(file.var.recode$loaded) && nrow(recodeAllVars$recodeSelectedVars) > 0 && recode.sel.vars.mismatch$value == FALSE) {
          return(NULL)
        }
      })
      output$recodeSchemeWarn <- renderText({
        if(nrow(recodeAllVars$recodeSelectedVars) > 0 && recode.sel.vars.mismatch$value == FALSE) {
          HTML('The selected variables do <u>not</u> have the same structure - class, number of levels/unique values, and/or user-defied missing values. Check the selected variables\' properties using the "Variable dictionaries" module.')
        } else if(nrow(recodeAllVars$recodeSelectedVars) > 0 && recode.sel.vars.mismatch$value == TRUE) {
          return(NULL)
        }
      })
      generate.recode.new.inputs <- function(obj, input.type, pix.width) {
        unlist(lapply(X = seq_along(obj), FUN = function(i) {
          i <- paste0(textInput(inputId = paste0(input.type, i), label = NULL, value = NULL, width = pix.width))
        }))
      }
      gather.recode.new.inputs.data <- function(id, len) {
        unlist(lapply(seq_len(len), function(i) {
          input[[paste0(id, i)]]
        }))
      }
      initial.recode.new.values.FAC <- reactiveValues(values = NULL)
      entered.new.values.FAC <- reactiveValues(values = NULL)
      new.recoding.values.FAC <- reactiveValues(values = NULL)
      new.recoding.labels.FAC <- reactiveValues(labels = NULL)
      observe({
        initial.recode.new.values.FAC$values <- gsub(pattern = "<", replacement = "&lt;", x = unlist(unique(file.var.recode$var.levels[recodeAllVars$recodeSelectedVars[ , Variables]])))
        entered.new.values.FAC$values <- cbind(
          V1 = data.table(initial.recode.new.values.FAC$values),
          V2 = data.table(1:length(initial.recode.new.values.FAC$values)),
          V3 = data.table(rep(x = as.character(icon(name = "arrow-right")), times = length(initial.recode.new.values.FAC$values))),
          V4 = data.table(generate.recode.new.inputs(obj = unlist(unique(file.var.recode$var.levels[recodeAllVars$recodeSelectedVars[ , Variables]])), input.type = "numinp", pix.width = "100%")),
          V5 = data.table(generate.recode.new.inputs(obj = unlist(unique(file.var.recode$var.levels[recodeAllVars$recodeSelectedVars[ , Variables]])), input.type = "labinp", pix.width = "100%"))
        )
        new.recoding.values.FAC$values <- gather.recode.new.inputs.data(id = "numinp", len = length(unlist(unique(file.var.recode$var.levels[recodeAllVars$recodeSelectedVars[ , Variables]]))))
        new.recoding.labels.FAC$labels <- gather.recode.new.inputs.data(id = "labinp", len = length(unlist(unique(file.var.recode$var.levels[recodeAllVars$recodeSelectedVars[ , Variables]]))))
      })
      initial.recode.new.values.NUM <- reactiveValues(values = NULL)
      entered.new.values.NUM <- reactiveValues(values = NULL)
      new.recoding.values.NUM <- reactiveValues(values = NULL)
      new.recoding.labels.NUM <- reactiveValues(labels = NULL)
      observe({
        initial.recode.new.values.NUM$values <- gsub(pattern = "<", replacement = "&lt;", x = unlist(unique(file.var.recode$var.num.values[recodeAllVars$recodeSelectedVars[ , Variables]])))
        entered.new.values.NUM$values <- cbind(
          V1 = data.table(initial.recode.new.values.NUM$values),
          V2 = data.table(rep(x = as.character(icon(name = "arrow-right")), times = length(initial.recode.new.values.NUM$values))),
          V3 = data.table(generate.recode.new.inputs(obj = unlist(unique(file.var.recode$var.num.values[recodeAllVars$recodeSelectedVars[ , Variables]])), input.type = "numinp", pix.width = "100%")),
          V4 = data.table(generate.recode.new.inputs(obj = unlist(unique(file.var.recode$var.num.values[recodeAllVars$recodeSelectedVars[ , Variables]])), input.type = "labinp", pix.width = "100%"))
        )
        new.recoding.values.NUM$values <- gather.recode.new.inputs.data(id = "numinp", len = length(unlist(unique(file.var.recode$var.num.values[recodeAllVars$recodeSelectedVars[ , Variables]]))))
        new.recoding.labels.NUM$labels <- gather.recode.new.inputs.data(id = "labinp", len = length(unlist(unique(file.var.recode$var.num.values[recodeAllVars$recodeSelectedVars[ , Variables]]))))
      })
      initial.recode.new.values.CHAR <- reactiveValues(values = NULL)
      entered.new.values.CHAR <- reactiveValues(values = NULL)
      new.recoding.values.CHAR <- reactiveValues(values = NULL)
      new.recoding.labels.CHAR <- reactiveValues(labels = NULL)
      observe({
        initial.recode.new.values.CHAR$values <- gsub(pattern = "<", replacement = "&lt;", x = unlist(unique(file.var.recode$var.char.values[recodeAllVars$recodeSelectedVars[ , Variables]])))
        entered.new.values.CHAR$values <- cbind(
          V1 = data.table(initial.recode.new.values.CHAR$values),
          V2 = data.table(rep(x = as.character(icon(name = "arrow-right")), times = length(initial.recode.new.values.CHAR$values))),
          V3 = data.table(generate.recode.new.inputs(obj = unlist(unique(file.var.recode$var.char.values[recodeAllVars$recodeSelectedVars[ , Variables]])), input.type = "charimp", pix.width = "100%")),
          V4 = data.table(generate.recode.new.inputs(obj = unlist(unique(file.var.recode$var.char.values[recodeAllVars$recodeSelectedVars[ , Variables]])), input.type = "labinp", pix.width = "100%"))
        )
        new.recoding.values.CHAR$values <- gather.recode.new.inputs.data(id = "charimp", len = length(unlist(unique(file.var.recode$var.char.values[recodeAllVars$recodeSelectedVars[ , Variables]]))))
        new.recoding.labels.CHAR$labels <- gather.recode.new.inputs.data(id = "labinp", len = length(unlist(unique(file.var.recode$var.char.values[recodeAllVars$recodeSelectedVars[ , Variables]]))))
      })
      output$recodeWarnSchemeIncomplete <- renderText({
        if(!is.null(file.var.recode$loaded)) {
          if(!all(new.recoding.values.FAC$values == "") & any(new.recoding.values.FAC$values == "") || !all(new.recoding.values.NUM$values == "") & any(new.recoding.values.NUM$values == "") || !all(new.recoding.values.CHAR$values == "") & any(new.recoding.values.CHAR$values == "")) {
            HTML("Warning: Not all old values have new corresponding ones. These values will be set to &ltNA>. Revise the recoding scheme unless this is desired.")
          }
        }
      })
      output$warningNotNumeric <- renderText({
        if(nrow(recodeAllVars$recodeSelectedVars) > 0 && recode.sel.vars.mismatch$value == TRUE && length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.FAC$values, invert = TRUE)) > 0) {
          HTML('Warning: The input fields for the new categories (column "New levels") in the table below accept only numbers and no other characters. Please check the entered values.')
        } else if(nrow(recodeAllVars$recodeSelectedVars) > 0 && recode.sel.vars.mismatch$value == TRUE && length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.NUM$values, invert = TRUE)) > 0) {
          HTML('Warning: The input fields for the new values (column "New values") in the table below accept only numbers and no other characters. Please check the entered values.')
        } else {
          return(NULL)
        }
      })
      output$warningDiffNumValuesLabels <- renderText({
        if(new.values.and.labels.mismatch$diff.count == TRUE) {
          HTML('Warning: The number of new values and their labels differ. Please check the entered values and labels.')
        } else {
          return(NULL)
        }
      })
      output$warningUniqueLabelsValues <- renderText({
        if(new.values.and.labels.mismatch$value == TRUE) {
          HTML('Warning: More than one unique new label has been defined for the same unique new value. Please check the entered values and labels.')
        } else {
          return(NULL)
        }
      })
      output$warningUniqueValuesLabels <- renderText({
        if(new.values.and.labels.mismatch$label == TRUE) {
          HTML('Warning: More than one unique new value has been defined for the same unique new label. Please check the entered values and labels.')
        } else {
          return(NULL)
        }
      })
      output$recodeSchemeFAC <- renderDT({
        if(!is.null(file.var.recode$loaded) && !is.null(unlist(unique(file.var.recode$var.levels[recodeAllVars$recodeSelectedVars[ , Variables]]))) && recode.sel.vars.mismatch$value == TRUE) {
          entered.new.values.FAC$values
        } else {
          return(NULL)
        }
      },
      rownames = FALSE,
      colnames = c("Old variable factor labels", "Old levels", as.character(icon(name = "arrow-right")), "New levels", "New labels"),
      class = "cell-border stripe;compact cell-border;",
      selection = "none",
      escape = FALSE,
      options = list(
        pageLength = 5000,
        dom = 'BRrt',
        autoWidth = TRUE,
        columnDefs = list(list(width = '50px', targets = 1:3), list(width = "330px", targets = 4), list(className = 'dt-center', targets = 1:4)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      ))
      output$recodeSchemeNUM <- renderDT({
        if(!is.null(file.var.recode$loaded) && !is.null(unlist(unique(file.var.recode$var.num.values[recodeAllVars$recodeSelectedVars[ , Variables]]))) && recode.sel.vars.mismatch$value == TRUE) {
          entered.new.values.NUM$values
        } else {
          return(NULL)
        }
      },
      rownames = FALSE,
      colnames = c("Old values", as.character(icon(name = "arrow-right")), "New values", "New labels"),
      class = "cell-border stripe;compact cell-border;",
      selection="none",
      escape = FALSE,
      options = list(
        pageLength = 5000,
        dom = 'BRrt',
        autoWidth = TRUE,
        columnDefs = list(list(width = '50px', targets = 1:2), list(width = "400px", targets = 3), list(className = 'dt-center', targets = 1:3)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      ))
      output$recodeSchemeCHAR <- renderDT({
        if(!is.null(file.var.recode$loaded) && !is.null(unlist(unique(file.var.recode$var.char.values[recodeAllVars$recodeSelectedVars[ , Variables]]))) && recode.sel.vars.mismatch$value == TRUE) {
          entered.new.values.CHAR$values
        } else {
          return(NULL)
        }
      },
      rownames = FALSE,
      colnames = c("Old values", as.character(icon(name = "arrow-right")), "New values", "New labels"),
      class = "cell-border stripe;compact cell-border;",
      selection="none",
      escape = FALSE,
      options = list(
        pageLength = 5000,
        dom = 'BRrt',
        autoWidth = TRUE,
        columnDefs = list(list(width = '50px', targets = 1), list(width = "400px", targets = c(0, 2, 3)), list(className = 'dt-center', targets = 1:3)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      ))
      observe({
        if(nrow(recodeAllVars$recodeSelectedVars) == 0) {
          session$sendCustomMessage("unbindDT", "recodeSchemeFAC")
          session$sendCustomMessage("unbindDT", "recodeSchemeNUM")
          session$sendCustomMessage("unbindDT", "recodeSchemeCHAR")
          session$sendCustomMessage("unbindDT", "recodeNewVarLabels")
          session$sendCustomMessage("unbindDT", "recodeNewVarNames")
        }
      })
      initial.new.var.names <- reactiveValues(names = NULL)
      entered.new.var.names <- reactiveValues(names = NULL)
      new.recoded.var.names <- reactiveValues(names = NULL)
      observe({
        entered.new.var.names$names <- data.table(
          V1 = recodeAllVars$recodeSelectedVars[ , Variables],
          V2 = rep(x = as.character(icon(name = "arrow-right")), times = length(recodeAllVars$recodeSelectedVars[ , Variables])),
          V3 = generate.recode.new.inputs(obj = recodeAllVars$recodeSelectedVars[ , Variables], input.type = "newvarnames", pix.width = "100%")
        )
        new.recoded.var.names$names <- gather.recode.new.inputs.data(id = "newvarnames", len = length(recodeAllVars$recodeSelectedVars[ , Variables]))
        if(nrow(recodeAllVars$recodeSelectedVars) == 0) {
          updateCheckboxInput(session, inputId = "recodeInNewVars", label = "Recode into new variables", value = TRUE)
        }
      })
      observeEvent(input$recodeInNewVars, {
        session$sendCustomMessage("unbindDT", "recodeNewVarNames")
        session$sendCustomMessage("unbindDT", "recodeNewVarLabels")
      })
      output$recodeOverwriteWarn <- renderText({
        if(!is.null(file.var.recode$loaded) && input$recodeInNewVars == FALSE) {
          HTML('The recoding will overwrite the existing variables. To change this, check the box from above.')
        } else {
          return(NULL)
        }
      })
      output$recodeNewVarNames <- renderDT({
        if(input$recodeInNewVars == TRUE && !is.null(unlist(unique(file.var.recode$var.levels[recodeAllVars$recodeSelectedVars[ , Variables]]))) | !is.null(unlist(unique(file.var.recode$var.num.values[recodeAllVars$recodeSelectedVars[ , Variables]]))) | !is.null(unlist(unique(file.var.recode$var.char.values[recodeAllVars$recodeSelectedVars[ , Variables]]))) && recode.sel.vars.mismatch$value == TRUE) {
          entered.new.var.names$names
        } else {
          return(NULL)
        }
      },
      rownames = FALSE,
      colnames = c("Old variable names", as.character(icon(name = "arrow-right")), "New variable names"),
      class = "cell-border stripe;compact cell-border;",
      selection="none",
      escape = FALSE,
      options = list(
        pageLength = 5000,
        dom = 'BRrt',
        autoWidth = TRUE,
        columnDefs = list(list(width = '190px', targets = 0), list(width = '50px', targets = 1), list(width = '175px', targets = 2), list(className = 'dt-center', targets = 1:2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      ))
      initial.new.var.labels <- reactiveValues(labels = NULL)
      entered.new.var.labels <- reactiveValues(labels = NULL)
      new.recoded.var.labels <- reactiveValues(labels = NULL)
      observe({
        entered.new.var.labels$labels <- data.table(
          V1 = recodeAllVars$recodeSelectedVars[ , Variables],
          V2 = generate.recode.new.inputs(obj = recodeAllVars$recodeSelectedVars[ , Variables], input.type = "newvarlabels", pix.width = "100%")
        )
        new.recoded.var.labels$labels <- gather.recode.new.inputs.data(id = "newvarlabels", len = length(recodeAllVars$recodeSelectedVars[ , Variables]))
      })
      output$recodeVarLabExpl <- renderText({
        if(input$recodeInNewVars == TRUE && !is.null(unlist(unique(file.var.recode$var.levels[recodeAllVars$recodeSelectedVars[ , Variables]]))) | !is.null(unlist(unique(file.var.recode$var.num.values[recodeAllVars$recodeSelectedVars[ , Variables]]))) | !is.null(unlist(unique(file.var.recode$var.char.values[recodeAllVars$recodeSelectedVars[ , Variables]]))) && recode.sel.vars.mismatch$value == TRUE) {
          HTML('Optional: Variable labels for the new recoded variables can be defined below.')
        } else {
          return(NULL)
        }
      })
      output$recodeNewVarLabels <- renderDT({
        if(input$recodeInNewVars == TRUE && !is.null(unlist(unique(file.var.recode$var.levels[recodeAllVars$recodeSelectedVars[ , Variables]]))) | !is.null(unlist(unique(file.var.recode$var.num.values[recodeAllVars$recodeSelectedVars[ , Variables]]))) | !is.null(unlist(unique(file.var.recode$var.char.values[recodeAllVars$recodeSelectedVars[ , Variables]]))) && recode.sel.vars.mismatch$value == TRUE) {
          entered.new.var.labels$labels
        } else {
          return(NULL)
        }
      },
      rownames = FALSE,
      colnames = c("New variable names", "New variable labels"),
      class = "cell-border stripe;compact cell-border;",
      selection="none",
      escape = FALSE,
      options = list(
        pageLength = 5000,
        dom = 'BRrt',
        autoWidth = TRUE,
        columnDefs = list(list(width = '190px', targets = 0), list(className = 'dt-center', targets = 1)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      ))
      new.values.and.labels.mismatch <- reactiveValues(value = FALSE, label = FALSE, diff.count = FALSE, diff.missings = FALSE)
      observe({
        any.recode.values.NULL <- all(is.null(new.recoding.values.FAC$values), is.null(new.recoding.values.NUM$values), is.null(new.recoding.values.CHAR$values))
        any.recode.values.empty <- any(c(new.recoding.values.FAC$values, new.recoding.values.NUM$values, new.recoding.values.CHAR$values) == "")
        if(!is.null(new.recoding.values.FAC$values)) {
          new.values.and.labels <- suppressWarnings(na.omit(unique(data.table(V1 = as.numeric(new.recoding.values.FAC$values), V2 = as.character(new.recoding.labels.FAC$labels)))))
        } else if(!is.null(new.recoding.values.NUM$values)) {
          new.values.and.labels <- suppressWarnings(na.omit(unique(data.table(V1 = as.numeric(new.recoding.values.NUM$values), V2 = as.character(new.recoding.labels.NUM$labels)))))
        } else if(!is.null(new.recoding.values.CHAR$values)) {
          new.values.and.labels <- na.omit(unique(data.table(V1 = new.recoding.values.CHAR$values, V2 = new.recoding.labels.CHAR$labels[1:length(new.recoding.values.CHAR$values)])))
          new.values.and.labels <- new.values.and.labels[V1 != ""]
        } else {
          new.values.and.labels <- data.table(V1 = numeric(), V2 = character())
        }
        if(!is.null(new.recoding.values.FAC$values)) {
          if(length(new.recoding.values.FAC$values[new.recoding.values.FAC$values != ""]) > 0 && length(new.recoding.labels.FAC$labels[new.recoding.labels.FAC$labels != ""]) == 0 || length(new.recoding.values.FAC$values[new.recoding.values.FAC$values != ""]) == length(new.recoding.labels.FAC$labels[new.recoding.labels.FAC$labels != ""])) {
            new.values.and.labels.mismatch$diff.count <- FALSE
          } else if(!is.null(new.recoding.values.FAC$values) && length(new.recoding.values.FAC$values[new.recoding.values.FAC$values != ""]) > 0 && length(new.recoding.labels.FAC$labels[new.recoding.labels.FAC$labels != ""]) > 0 && length(new.recoding.values.FAC$values[new.recoding.values.FAC$values != ""]) != length(new.recoding.labels.FAC$labels[new.recoding.labels.FAC$labels != ""])) {
            new.values.and.labels.mismatch$diff.count <- TRUE
          }
        } else if(!is.null(new.recoding.values.NUM$values)) {
          if(length(new.recoding.values.NUM$values[new.recoding.values.NUM$values != ""]) > 0 && length(new.recoding.labels.NUM$labels[new.recoding.labels.NUM$labels != ""]) == 0 || length(new.recoding.values.NUM$values[new.recoding.values.NUM$values != ""]) == length(new.recoding.labels.NUM$labels[new.recoding.labels.NUM$labels != ""])) {
            new.values.and.labels.mismatch$diff.count <- FALSE
          } else if(!is.null(new.recoding.values.NUM$values) && length(new.recoding.values.NUM$values[new.recoding.values.NUM$values != ""]) > 0 && length(new.recoding.labels.NUM$labels[new.recoding.labels.NUM$labels != ""]) > 0 && length(new.recoding.values.NUM$values[new.recoding.values.NUM$values != ""]) != length(new.recoding.labels.NUM$labels[new.recoding.labels.NUM$labels != ""])) {
            new.values.and.labels.mismatch$diff.count <- TRUE
          }
        } else if(!is.null(new.recoding.values.CHAR$values)) {
          if(length(new.recoding.values.CHAR$values[new.recoding.values.CHAR$values != ""]) > 0 && length(new.recoding.labels.CHAR$labels[new.recoding.labels.CHAR$labels != ""]) == 0 || length(new.recoding.values.CHAR$values[new.recoding.values.CHAR$values != ""]) == length(new.recoding.labels.CHAR$labels[new.recoding.labels.CHAR$labels != ""])) {
            new.values.and.labels.mismatch$diff.count <- FALSE
          } else if(!is.null(new.recoding.values.CHAR$values) && length(new.recoding.values.CHAR$values[new.recoding.values.CHAR$values != ""]) > 0 && length(new.recoding.labels.CHAR$labels[new.recoding.labels.CHAR$labels != ""]) > 0 && length(new.recoding.values.CHAR$values[new.recoding.values.CHAR$values != ""]) != length(new.recoding.labels.CHAR$labels[new.recoding.labels.CHAR$labels != ""])) {
            new.values.and.labels.mismatch$diff.count <- TRUE
          }
        }
        if(nrow(new.values.and.labels) > 0 && all(new.values.and.labels[ , V2] != "")) {
          if(all(new.values.and.labels[ , V2] != "") && any(duplicated(new.values.and.labels[ , V1]) == TRUE)) {
            new.values.and.labels.mismatch$value <- TRUE
          } else if(all(new.values.and.labels[ , V2] != "") && any(duplicated(new.values.and.labels[ , V1]) == FALSE)) {
            new.values.and.labels.mismatch$value <- FALSE
          }
          if(any(duplicated(new.values.and.labels[ , V1]) == FALSE) && any(duplicated(new.values.and.labels[ , V2]) == TRUE)) {
            new.values.and.labels.mismatch$label <- TRUE
          } else if(any(duplicated(new.values.and.labels[ , V1]) == FALSE) && any(duplicated(new.values.and.labels[ , V2]) == FALSE)) {
            new.values.and.labels.mismatch$label <- FALSE
          }
        }
        if(is.null(file.var.recode$loaded) || nrow(recodeAllVars$recodeSelectedVars) == 0 || all(new.recoding.values.FAC$values == "") & all(new.recoding.values.NUM$values == "") & all(new.recoding.values.CHAR$values == "") || length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.FAC$values, invert = TRUE)) > 0 || length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.NUM$values, invert = TRUE)) > 0 || new.values.and.labels.mismatch$diff.count == TRUE || new.values.and.labels.mismatch$value == TRUE || new.values.and.labels.mismatch$label == TRUE) {
          hide("recodeNewMissings")
          hide("recodeMissingsWarn")
        } else if(nrow(recodeAllVars$recodeSelectedVars) > 0 || all(new.recoding.values.FAC$values != "") & all(new.recoding.values.NUM$values != "") & all(new.recoding.values.CHAR$values != "") || length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.FAC$values, invert = TRUE)) == 0 || length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.NUM$values, invert = TRUE)) == 0 || new.values.and.labels.mismatch$diff.count == FALSE || new.values.and.labels.mismatch$value == FALSE || new.values.and.labels.mismatch$label == FALSE) {
          show("recodeNewMissings")
          show("recodeMissingsWarn")
        }
        if(!is.null(new.recoding.labels.FAC$labels)) {
          if(any(new.recoding.labels.FAC$labels != "")) {
            if(input$recodeNewMissings != "" && any(!gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]) %in% new.recoding.labels.FAC$labels)) {
              new.values.and.labels.mismatch$diff.missings <- TRUE
            } else if(input$recodeNewMissings == "" || all(gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]) %in% new.recoding.labels.FAC$labels)) {
              new.values.and.labels.mismatch$diff.missings <- FALSE
            }
          } else {
            if(input$recodeNewMissings != "" && any(!gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]) %in% new.recoding.values.FAC$values)) {
              new.values.and.labels.mismatch$diff.missings <- TRUE
            } else if(input$recodeNewMissings == "" || all(gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]) %in% new.recoding.values.FAC$values)) {
              new.values.and.labels.mismatch$diff.missings <- FALSE
            }
          }
        } else if(!is.null(new.recoding.labels.NUM$labels)) {
          if(any(new.recoding.labels.NUM$labels != "")) {
            if(input$recodeNewMissings != "" && any(!gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]) %in% new.recoding.labels.NUM$labels)) {
              new.values.and.labels.mismatch$diff.missings <- TRUE
            } else if(input$recodeNewMissings == "" || all(gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]) %in% new.recoding.labels.NUM$labels)) {
              new.values.and.labels.mismatch$diff.missings <- FALSE
            }
          } else {
            if(input$recodeNewMissings != "" && any(!gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]) %in% new.recoding.values.NUM$values)) {
              new.values.and.labels.mismatch$diff.missings <- TRUE
            } else if(input$recodeNewMissings == "" || all(gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]) %in% new.recoding.values.NUM$values)) {
              new.values.and.labels.mismatch$diff.missings <- FALSE
            }
          }
        } else if(!is.null(new.recoding.labels.CHAR$labels)) {
          if(any(new.recoding.labels.CHAR$labels != "")) {
            if(input$recodeNewMissings != "" && any(!gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]) %in% new.recoding.labels.CHAR$labels)) {
              new.values.and.labels.mismatch$diff.missings <- TRUE
            } else if(input$recodeNewMissings == "" || all(gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]) %in% new.recoding.labels.CHAR$labels)) {
              new.values.and.labels.mismatch$diff.missings <- FALSE
            }
          } else {
            if(input$recodeNewMissings != "" && any(!gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]) %in% new.recoding.values.CHAR$values)) {
              new.values.and.labels.mismatch$diff.missings <- TRUE
            } else if(input$recodeNewMissings == "" || all(gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]) %in% new.recoding.values.CHAR$values)) {
              new.values.and.labels.mismatch$diff.missings <- FALSE
            }
          }
        }
        if(is.null(file.var.recode$loaded) || new.values.and.labels.mismatch$diff.missings == TRUE || nrow(recodeAllVars$recodeSelectedVars) == 0 || all(new.recoding.values.FAC$values == "") & all(new.recoding.values.NUM$values == "") & all(new.recoding.values.CHAR$values == "") || length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.FAC$values, invert = TRUE)) > 0 || length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.NUM$values, invert = TRUE)) > 0 || new.values.and.labels.mismatch$diff.count == TRUE || new.values.and.labels.mismatch$value == TRUE || new.values.and.labels.mismatch$label == TRUE) {
          hide("recodeInNewVars")
          hide("recodeOverwriteWarn")
          hide("recodeNewVarNames")
          hide("recodeVarLabExpl")
          hide("recodeNewVarLabels")
          hide("recodeSyntaxHead")
          hide("recodeSyntax")
          hide("recodeExecBtnHead")
          hide("execRecode")
        } else if(!is.null(file.var.recode$loaded) || new.values.and.labels.mismatch$diff.missings == FALSE || nrow(recodeAllVars$recodeSelectedVars) > 0 || all(new.recoding.values.FAC$values != "") & all(new.recoding.values.NUM$values != "") & all(new.recoding.values.CHAR$values != "") || length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.FAC$values, invert = TRUE)) == 0 || length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.NUM$values, invert = TRUE)) == 0 || new.values.and.labels.mismatch$diff.count == FALSE || new.values.and.labels.mismatch$value == FALSE || new.values.and.labels.mismatch$label == FALSE) {
          show("recodeInNewVars")
          show("recodeOverwriteWarn")
          show("recodeNewVarNames")
          show("recodeVarLabExpl")
          show("recodeNewVarLabels")
          show("recodeSyntaxHead")
          show("recodeSyntax")
          show("recodeExecBtnHead")
          show("execRecode")
        }
        if(is.null(file.var.recode$loaded) || new.values.and.labels.mismatch$diff.missings == TRUE || nrow(recodeAllVars$recodeSelectedVars) == 0 || all(new.recoding.values.FAC$values == "") & all(new.recoding.values.NUM$values == "") & all(new.recoding.values.CHAR$values == "") || length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.FAC$values, invert = TRUE)) > 0 || length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.NUM$values, invert = TRUE)) > 0 || new.values.and.labels.mismatch$diff.count == TRUE || new.values.and.labels.mismatch$value == TRUE || new.values.and.labels.mismatch$label == TRUE || input$recodeInNewVars == TRUE & any(new.recoded.var.names$names == "")) {
          hide("recodeChooseOutFile")
        } else if(!is.null(file.var.recode$loaded) || new.values.and.labels.mismatch$diff.missings == FALSE || nrow(recodeAllVars$recodeSelectedVars) > 0 || all(new.recoding.values.FAC$values != "") & all(new.recoding.values.NUM$values != "") & all(new.recoding.values.CHAR$values != "") || length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.FAC$values, invert = TRUE)) == 0 || length(grep(pattern = "^[[:digit:]]+$|^$", x = new.recoding.values.NUM$values, invert = TRUE)) == 0 || new.values.and.labels.mismatch$diff.count == FALSE || new.values.and.labels.mismatch$value == FALSE || new.values.and.labels.mismatch$label == FALSE || input$recodeInNewVars == FALSE) {
          show("recodeChooseOutFile")
        }
      })
      shinyFileSave(input, "recodeChooseOutFile", filetype = list(RData = "RData"), roots = available.volumes, updateFreq = 100000)
      syntaxRecodeVars <- reactive({
        file.var.recode$recode.syntax <- paste0(
          'lsa.recode.vars(data.file = "', parseFilePaths(available.volumes, input$recodeChooseSrcFile)$datapath,
          if(nrow(recodeAllVars$recodeSelectedVars) == 1) {
            paste0(paste0('", src.variables = "', recodeAllVars$recodeSelectedVars[ , Variables]), '"')
          } else if(nrow(recodeAllVars$recodeSelectedVars) > 1) {
            paste0(paste0('", src.variables = c("', paste(recodeAllVars$recodeSelectedVars[ , Variables], collapse = '", "')), '")')
          },
          if(all(new.recoded.var.names$names != "") && length(new.recoded.var.names$names) > 1 && input$recodeInNewVars == 1) {
            paste0(', new.variables = c("', paste(new.recoded.var.names$names, collapse = '", "'), '")')
          } else if(all(new.recoded.var.names$names != "") && length(new.recoded.var.names$names) == 1 && input$recodeInNewVars == 1) {
            paste0(', new.variables = "', new.recoded.var.names$names, '"')
          },
          if(!is.null(new.recoding.values.FAC$values) && length(initial.recode.new.values.FAC$values[new.recoding.values.FAC$values != ""]) > 0) {
            paste0(', old.new = "', paste(paste0(1:length(initial.recode.new.values.FAC$values[new.recoding.values.FAC$values != ""]), "="), new.recoding.values.FAC$values[new.recoding.values.FAC$values != ""], sep = "", collapse = ";"), '"')
          } else if(!is.null(new.recoding.values.NUM$values) && length(initial.recode.new.values.NUM$values[new.recoding.values.NUM$values != ""]) > 0) {
            paste0(', old.new = "', paste(paste0(1:length(initial.recode.new.values.NUM$values[new.recoding.values.NUM$values != ""]), "="), new.recoding.values.NUM$values[new.recoding.values.NUM$values != ""], sep = "", collapse = ";"), '"')
          } else if(!is.null(new.recoding.values.CHAR$values) && length(initial.recode.new.values.CHAR$values[new.recoding.values.CHAR$values != ""]) > 0) {
            paste0(', old.new = "', paste(paste0("'", gsub(pattern = "\\&lt;", replacement = "<", x = initial.recode.new.values.CHAR$values)[new.recoding.values.CHAR$values != ""], "'", "="), paste0("'", new.recoding.values.CHAR$values[new.recoding.values.CHAR$values != ""], "'"), sep = "", collapse = ";"), '"')
          },
          if(!is.null(new.recoding.values.FAC$values) && length(initial.recode.new.values.FAC$values[new.recoding.values.FAC$values != ""]) > 0 && length(grep(pattern = "^$", x = new.recoding.labels.FAC$labels, invert = TRUE)) == length(grep(pattern = "^$", x = new.recoding.values.FAC$values, invert = TRUE))) {
            FAC.new.labels <- data.table(V1 = unique(new.recoding.values.FAC$values[new.recoding.values.FAC$values != ""]), V2 = unique(new.recoding.labels.FAC$labels[new.recoding.labels.FAC$labels != ""]), key = "V1")
            paste0(', new.labels = c("', paste(FAC.new.labels[ , V2], collapse = '", "'), '")')
          } else if(!is.null(new.recoding.values.NUM$values) && length(initial.recode.new.values.NUM$values[new.recoding.values.NUM$values != ""]) > 0 && length(grep(pattern = "^$", x = new.recoding.labels.NUM$labels, invert = TRUE)) == length(grep(pattern = "^$", x = new.recoding.values.NUM$values, invert = TRUE))) {
            NUM.new.labels <- data.table(V1 = unique(new.recoding.values.NUM$values[new.recoding.values.NUM$values != ""]), V2 = unique(new.recoding.labels.NUM$labels[new.recoding.labels.NUM$labels != ""]), key = "V1")
            paste0(', new.labels = c("', paste(NUM.new.labels[ , V2], collapse = '", "'), '")')
          } else if(!is.null(new.recoding.values.CHAR$values) && length(initial.recode.new.values.CHAR$values[new.recoding.values.CHAR$values != ""]) > 0 && length(grep(pattern = "^$", x = new.recoding.labels.CHAR$labels, invert = TRUE)) == length(grep(pattern = "^$", x = new.recoding.values.CHAR$values, invert = TRUE))) {
            CHAR.new.labels <- data.table(V1 = unique(new.recoding.values.CHAR$values[new.recoding.values.CHAR$values != ""]), V2 = unique(new.recoding.labels.CHAR$labels[new.recoding.labels.CHAR$labels != ""]), key = "V1")
            paste0(', new.labels = c("', paste(CHAR.new.labels[ , V2], collapse = '", "'), '")')
          },
          if(input$recodeNewMissings != "") {
            paste0(', missings.attr = list("', paste(gsub(pattern = '"+', replacement = '', x = strsplit(x = input$recodeNewMissings, split = '"*,[[:space:]]*"*')[[1]]), collapse = '", "'), '")')
          },
          if(!is.null(new.recoded.var.labels$labels) && new.recoded.var.labels$labels != "" && input$recodeInNewVars == 1) {
            if(length(new.recoded.var.labels$labels) == 1) {
              paste0(', variable.labels = "', paste(new.recoded.var.labels$labels, collapse = '", "'), '"')
            } else if(length(new.recoded.var.labels$labels) > 1) {
              paste0(', variable.labels = c("', paste(new.recoded.var.labels$labels, collapse = '", "'), '")')
            }
          },
          if(length(parseSavePath(available.volumes, input$recodeChooseOutFile)$datapath) == 1) {
            paste0(', out.file = "', parseSavePath(available.volumes, input$recodeChooseOutFile)$datapath, '")')
          }
        )
      })
      output$recodeSyntaxHead <- renderText({
        if(!is.null(file.var.recode$loaded) && length(parseSavePath(available.volumes, input$recodeChooseOutFile)$datapath) == 1) {
          HTML("Syntax")
        } else {
          return(NULL)
        }
      })
      output$recodeSyntax <- renderText({
        if(!is.null(file.var.recode$loaded) && length(parseSavePath(available.volumes, input$recodeChooseOutFile)$datapath) == 1) {
          syntaxRecodeVars()
        } else {
          return(NULL)
        }
      })
      output$recodeExecBtnHead <- renderText({
        if(!is.null(file.var.recode$loaded) && length(parseSavePath(available.volumes, input$recodeChooseOutFile)$datapath) == 1) {
          HTML("Press the button below to execute the syntax")
        } else {
          return(NULL)
        }
      })
      output$execRecode <- renderUI({
        if(!is.null(file.var.recode$loaded) && length(parseSavePath(available.volumes, input$recodeChooseOutFile)$datapath) == 1) {
          actionButton(inputId = "execRecode", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
    }
  })
  observeEvent(input$execRecode, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consoleRecode", "")
      tryCatch({
        expr = eval(parse(text = file.var.recode$recode.syntax))
        showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
      }, error = function(e) {
        message("", e)
        showNotification(ui = HTML(paste0("Something went wrong. Possible reasons: <br/>", paste(gsub(pattern = "\\n|Error: ", replacement = "<br/>", x = e)))), type = "error", duration = NULL)
      })
    },
    message = function(i) {
      shinyjs::html(id = "consoleRecode", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consoleRecode", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
  }, ignoreInit = TRUE)
  output$h1selectPISACountries <- renderText("Select PISA countries' data")
  hide("selectPISACountriesChooseOutFile")
  output$selectPISACountriesIntro <- renderText({HTML("Select  PISA .RData file to load.")})
  file.select.PISA <- reactiveValues(loaded = NULL, is.lsa.data = NULL, resp.type = NULL, study = NULL, cycle = NULL, country.ID = NULL, default.weight = NULL, select.PISA.syntax = NULL)
  shinyFileChoose(input, "selectPISACountriesChooseSrcFile", roots = available.volumes, filetype = list(RData = "RData"))
  observeEvent(eventExpr = input$selectPISACountriesChooseSrcFile, {
    file.select.PISA$loaded <- NULL
    file.select.PISA$study <- NULL
    file.select.PISA$cycle <- NULL
    file.select.PISA$resp.type <- NULL
    if(length(parseFilePaths(available.volumes, input$selectPISACountriesChooseSrcFile)$datapath) > 0 && file.size(parseFilePaths(available.volumes, input$selectPISACountriesChooseSrcFile)$datapath) > 104857600) {
      showNotification(ui = HTML('The size of the selected<br/>file is rather large. Please<br/>wait until the file is loaded.'), type = "message")
    }
    if(length(parseFilePaths(available.volumes, input$selectPISACountriesChooseSrcFile)$datapath) > 0) {
      file.select.PISA$loaded <- get(load(parseFilePaths(available.volumes, input$selectPISACountriesChooseSrcFile)$datapath))
      if(!"lsa.data" %in% class(file.select.PISA$loaded)) {
        file.select.PISA$loaded <- NULL
        showNotification(ui = HTML('The loaded data file<br/>does not contain "lsa.data".<br/>All operations stop<br/>here.<br/>Check your input.'), type = "error")
      } else if(attr(x = file.select.PISA$loaded, which = "study") != "PISA") {
        file.select.PISA$loaded <- NULL
        showNotification(ui = HTML('The loaded data file does<br/>not contain PISA data.<br/>All operations stop here.<br/>Check your input.'), type = "error")
      } else {
        if("lsa.data" %in% class(file.select.PISA$loaded)) {
          file.select.PISA$is.lsa.data <- TRUE
        } else {
          file.select.PISA$is.lsa.data <- FALSE
        }
        file.select.PISA$study <- attr(x = file.select.PISA$loaded, which = "study")
        file.select.PISA$cycle <- attr(x = file.select.PISA$loaded, which = "cycle")
        file.select.PISA$resp.type <- attr(x = file.select.PISA$loaded, which = "file.type")
        file.select.PISA$loaded <- data.table(CNT = sort(levels(file.select.PISA$loaded[ , CNT])), order_col = 1:length(levels(file.select.PISA$loaded[ , CNT])))
      }
    }
    output$selectPISACountriesSrcPathDisplay <- renderText({parseFilePaths(available.volumes, input$selectPISACountriesChooseSrcFile)$datapath})
  }, ignoreInit = TRUE)
  observe({
    if(!is.null(file.select.PISA$loaded) && file.select.PISA$is.lsa.data == FALSE) {
      showNotification(ui = HTML('The data is not of class "lsa.data".<br/>Please check the file content.'), type = "error")
    } else if (!is.null(file.select.PISA$loaded) && file.select.PISA$is.lsa.data == TRUE) {
      output$selectPISACountriesStudyName <- renderText({
        if(is.null(file.select.PISA$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Study: ', file.select.PISA$study))
        }
      })
      output$selectPISACountriesStudyCycle <- renderText({
        if(is.null(file.select.PISA$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Cycle: ', file.select.PISA$cycle))
        }
      })
      output$selectPISACountriesRespHead <- renderText({
        if(is.null(file.select.PISA$resp.type)) {
          return(NULL)
        } else {
          HTML('<u>The file contains data from the following respondents:</u>')
        }
      })
      output$selectPISACountriesRespAvailable <- renderText({
        if(is.null(file.select.PISA$resp.type)) {
          return(NULL)
        } else {
          HTML(file.merged.respondents[[file.select.PISA$resp.type]])
        }
      })
      output$selectPISACountriesExplText <- renderText({
        if(is.null(file.select.PISA$resp.type)) {
          return(NULL)
        } else {
          HTML('Use the panels below to select the countries whose data shall be kept in the new file.')
        }
      })
      select.PISA.initial.available.countries <- file.select.PISA$loaded
      select.PISA.selected.countries <- data.table(CNT = as.character(), order_col = as.numeric())
      selectPISAAllCnt <- reactiveValues(selectPISAAvailCnt = select.PISA.initial.available.countries, selectPISASelCnt = select.PISA.selected.countries)
      output$selectPISACountriesArrowSelCntRight <- renderUI({
        if(is.null(file.select.PISA$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "selectPISACountriesArrowSelCntRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$selectPISACountriesArrowSelCntLeft <- renderUI({
        if(is.null(file.select.PISA$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "selectPISACountriesArrowSelCntLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      observeEvent(input$selectPISACountriesArrowSelCntRight, {
        req(input$selectPISACountriesAvailableCountries_rows_selected)
        selectPISAAllCnt$selectPISASelCnt <- rbind(isolate(selectPISAAllCnt$selectPISASelCnt), selectPISAAllCnt$selectPISAAvailCnt[input$selectPISACountriesAvailableCountries_rows_selected, , drop = FALSE])
        selectPISAAllCnt$selectPISASelCnt <- selectPISAAllCnt$selectPISASelCnt[complete.cases(selectPISAAllCnt$selectPISASelCnt), , drop = FALSE]
        selectPISAAllCnt$selectPISAAvailCnt <- isolate(selectPISAAllCnt$selectPISAAvailCnt[-input$selectPISACountriesAvailableCountries_rows_selected, , drop = FALSE])
      })
      observeEvent(input$selectPISACountriesArrowSelCntLeft, {
        req(input$selectPISASelectedCountries_rows_selected)
        selectPISAAllCnt$selectPISAAvailCnt <- rbind(isolate(selectPISAAllCnt$selectPISAAvailCnt),        selectPISAAllCnt$selectPISASelCnt[input$selectPISASelectedCountries_rows_selected, , drop = FALSE])
        selectPISAAllCnt$selectPISAAvailCnt <- selectPISAAllCnt$selectPISAAvailCnt[complete.cases(selectPISAAllCnt$selectPISAAvailCnt), , drop = FALSE]
        selectPISAAllCnt$selectPISASelCnt <- isolate(selectPISAAllCnt$selectPISASelCnt[-input$selectPISASelectedCountries_rows_selected, , drop = FALSE])
      })
      output$selectPISACountriesAvailableCountries <- renderDT({
        setkeyv(x = selectPISAAllCnt$selectPISAAvailCnt, cols = "order_col")
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available countries"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No countries available"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 1)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 350, scroller = TRUE
      ))
      output$selectPISASelectedCountries <- renderDT({
        setkeyv(x = selectPISAAllCnt$selectPISASelCnt, cols = "order_col")
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Selected countries"),
      rownames = FALSE,
      colnames = c("Names", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No countries have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 1)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 405, scroller = TRUE
      ))
      shinyFileSave(input, "selectPISACountriesChooseOutFile", filetype = list(RData = "RData"), roots = available.volumes, updateFreq = 100000)
      observe({
        if(is.null(file.select.PISA$loaded) || length(parseSavePath(available.volumes, input$selectPISACountriesChooseSrcFile)$datapath) > 0 && nrow(selectPISAAllCnt$selectPISASelCnt) == 0) {
          hide("selectPISACountriesChooseOutFile")
        } else {
          show("selectPISACountriesChooseOutFile")
        }
        if(is.null(file.select.PISA$loaded)) {
          hide("h1selectPISACountries")
          hide("selectPISACountriesStudyName")
          hide("selectPISACountriesStudyCycle")
          hide("selectPISACountriesRespHead")
          hide("selectPISACountriesRespAvailable")
          hide("selectPISACountriesExplText")
          hide("selectPISACountriesAvailableCountries")
          hide("selectPISACountriesArrowSelCntRight")
          hide("selectPISACountriesArrowSelCntLeft")
          hide("selectPISASelectedCountries")
          hide("selectPISACountriesSyntaxHead")
          hide("selectPISACountriesSyntax")
          hide("selectPISACountriesExecBtnHead")
          hide("execSelectPISACountries")
        } else {
          show("h1selectPISACountries")
          show("selectPISACountriesStudyName")
          show("selectPISACountriesStudyCycle")
          show("selectPISACountriesRespHead")
          show("selectPISACountriesRespAvailable")
          show("selectPISACountriesExplText")
          show("selectPISACountriesAvailableCountries")
          show("selectPISACountriesArrowSelCntRight")
          show("selectPISACountriesArrowSelCntLeft")
          show("selectPISASelectedCountries")
          show("selectPISACountriesSyntaxHead")
          show("selectPISACountriesSyntax")
          show("selectPISACountriesExecBtnHead")
          show("execSelectPISACountries")
        }
      })
      syntaxSelectPISA <- reactive({
        file.select.PISA$select.PISA.syntax <- paste0(
          'lsa.select.countries.PISA(data.file = "', parseFilePaths(available.volumes, input$selectPISACountriesChooseSrcFile)$datapath, '", ',
          if(nrow(selectPISAAllCnt$selectPISASelCnt) > 0) {
            if(nrow(selectPISAAllCnt$selectPISASelCnt) == 1) {
              paste0('cnt.names = "', paste(selectPISAAllCnt$selectPISASelCnt[ , CNT], collapse = '", "'), '"')
            } else if(nrow(selectPISAAllCnt$selectPISASelCnt) > 1) {
              paste0('cnt.names = c("', paste(selectPISAAllCnt$selectPISASelCnt[ , CNT], collapse = '", "'), '")')
            }
          },
          paste0(', output.file = "', parseSavePath(available.volumes, input$selectPISACountriesChooseOutFile)$datapath, '"'),
          ')'
        )
      })
      output$selectPISACountriesSyntaxHead <- renderText({
        if(length(parseSavePath(available.volumes, input$selectPISACountriesChooseOutFile)$datapath) == 1) {
          HTML("Syntax")
        } else {
          return(NULL)
        }
      })
      output$selectPISACountriesSyntax <- renderText({
        if(length(parseSavePath(available.volumes, input$selectPISACountriesChooseOutFile)$datapath) == 1 && nrow(rbindlist(l = list(selectPISAAllCnt$selectPISASelCnt))) >= 1) {
          syntaxSelectPISA()
        } else {
          return(NULL)
        }
      })
      output$selectPISACountriesExecBtnHead <- renderText({
        if(length(parseSavePath(available.volumes, input$selectPISACountriesChooseOutFile)$datapath) == 1) {
          HTML("Press the button below to execute the syntax")
        } else {
          return(NULL)
        }
      })
      output$execSelectPISACountries <- renderUI({
        if(length(parseSavePath(available.volumes, input$selectPISACountriesChooseOutFile)$datapath) == 1) {
          actionButton(inputId = "execSelectPISACountries", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
    }
  })
  observeEvent(input$execSelectPISACountries, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consoleSelectPISACountries", "")
      tryCatch({
        expr = eval(parse(text = file.select.PISA$select.PISA.syntax))
        showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
      }, error = function(e) {
        message("", e)
        showNotification(ui = HTML(paste0("Something went wrong. Possible reasons: <br/>", paste(gsub(pattern = "\\n|Error: ", replacement = "<br/>", x = e)))), type = "error", duration = NULL)
      })
    },
    message = function(i) {
      shinyjs::html(id = "consoleSelectPISACountries", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consoleSelectPISACountries", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
  }, ignoreInit = TRUE)
  output$h1PctsMeans <- renderText("Percentages and means")
  hide("pctsMeansChooseOutFile")
  output$pctsMeansIntro <- renderText({HTML("Select large-scale assessment .RData file to load.")})
  file.pct.means <- reactiveValues(loaded = NULL, is.lsa.data = NULL, resp.type = NULL, study = NULL, cycle = NULL, country.ID = NULL, PV.sets = NULL, default.weight = NULL, pct.means.syntax = NULL)
  shinyFileChoose(input, "pctsMeansChooseSrcFile", roots = available.volumes, filetype = list(RData = "RData"))
  observeEvent(eventExpr = input$pctsMeansChooseSrcFile, {
    file.pct.means$loaded <- NULL
    file.pct.means$study <- NULL
    file.pct.means$cycle <- NULL
    file.pct.means$resp.type <- NULL
    file.pct.means$PV.sets <- NULL
    file.pct.means$default.weight <- NULL
    if(length(parseFilePaths(available.volumes, input$pctsMeansChooseSrcFile)$datapath) > 0 && file.size(parseFilePaths(available.volumes, input$pctsMeansChooseSrcFile)$datapath) > 104857600) {
      showNotification(ui = HTML('The size of the selected<br/>file is rather large. Please<br/>wait until the file is loaded.'), type = "message")
    }
    if(length(parseFilePaths(available.volumes, input$pctsMeansChooseSrcFile)$datapath) > 0) {
      file.pct.means$loaded <- get(load(parseFilePaths(available.volumes, input$pctsMeansChooseSrcFile)$datapath))
      if("lsa.data" %in% class(file.pct.means$loaded)) {
        file.pct.means$is.lsa.data <- TRUE
      } else {
        file.pct.means$is.lsa.data <- FALSE
      }
      file.pct.means$study <- attr(x = file.pct.means$loaded, which = "study")
      file.pct.means$cycle <- attr(x = file.pct.means$loaded, which = "cycle")
      file.pct.means$resp.type <- attr(x = file.pct.means$loaded, which = "file.type")
      file.pct.means$loaded <- data.table(Variables = names(file.pct.means$loaded), Variable_Labels = sapply(X = file.pct.means$loaded, FUN = function(j) {
        if(is.null(attr(x = j, which = "variable.label"))) {
          return(NA_character_)
        } else {
          attr(x = j, which = "variable.label")
        }
      }),
      order_col = 1:ncol(file.pct.means$loaded))
      file.pct.means$PV.sets <- NULL
      tmp.PV.names <- grep(pattern = paste(all.available.PVs, collapse = "|"), x = file.pct.means$loaded[ , Variables], value = TRUE)
      if(length(tmp.PV.names) > 0) {
        collapsed.PVs <- collapse.loaded.file.PV.names(PV.vector = tmp.PV.names, vars.object = file.pct.means$loaded)
        file.pct.means$loaded <- file.pct.means$loaded[!Variables %in% tmp.PV.names]
        file.pct.means$loaded <- rbindlist(l = list(file.pct.means$loaded, collapsed.PVs))
        setkeyv(x = file.pct.means$loaded, cols = "order_col")
        file.pct.means$PV.sets <- collapsed.PVs[ , Variables]
      }
      if(!is.null(file.pct.means$study)) {
        file.pct.means$default.weight <- define.default.weight(study = file.pct.means$study, loaded.names.and.labels = file.pct.means$loaded, respondent.type = file.pct.means$resp.type)
      }
      file.pct.means$country.ID <- NULL
      if("IDCNTRY" %in% file.pct.means$loaded[ , Variables]) {
        file.pct.means$country.ID <- "IDCNTRY"
      } else {
        file.pct.means$country.ID <- "CNT"
      }
    }
    output$pctsMeansSrcPathDisplay <- renderText({parseFilePaths(available.volumes, input$pctsMeansChooseSrcFile)$datapath})
  }, ignoreInit = TRUE)
  observe({
    if(!is.null(file.pct.means$loaded) && file.pct.means$is.lsa.data == FALSE) {
      showNotification(ui = HTML('The data is not of class "lsa.data".<br/>Please check the file content.'), type = "error")
    } else if (!is.null(file.pct.means$loaded) && file.pct.means$is.lsa.data == TRUE) {
      output$pctsMeansStudyName <- renderText({
        if(is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Study: ', file.pct.means$study))
        }
      })
      output$pctsMeansStudyCycle <- renderText({
        if(is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Cycle: ', file.pct.means$cycle))
        }
      })
      output$pctsMeansRespHead <- renderText({
        if(is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else {
          HTML('<u>The file contains data from the following respondents:</u>')
        }
      })
      output$pctsMeansRespAvailable <- renderText({
        if(is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else {
          HTML(file.merged.respondents[[file.pct.means$resp.type]])
        }
      })
      output$pctsMeansNoWeights <- renderText({
        if(!is.null(file.pct.means$loaded) && is.null(file.pct.means$default.weight) || !is.null(file.pct.means$loaded) && length(file.pct.means$default.weight) == 0) {
          HTML('Error: The loaded file does not contain any recognizable default weight variable. Such files cannot be analyzed on their own and must be merged with other respondents in advance.')
        } else {
          return(NULL)
        }
      })
      output$pctsMeansVariablesExplText <- renderText({
        if(is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else {
          HTML('Use the panels below to select the variables to compute percentages within groups specified by splitting variables and means of continuous variables for these groups.')
        }
      })
      pcts.means.initial.available.vars <- file.pct.means$loaded[!Variables %in% c(file.pct.means$default.weight, file.pct.means$country.ID), ]
      pcts.means.initial.selected.split.vars <- file.pct.means$loaded[Variables == file.pct.means$country.ID, ]
      pcts.means.initial.selected.bckg.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      pcts.means.initial.selected.PV.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      pcts.means.initial.selected.weight.var <- file.pct.means$loaded[Variables %in% file.pct.means$default.weight, ]
      pctsMeansAllVars <- reactiveValues(pctsMeansAvailVars = pcts.means.initial.available.vars, pctsMeansSelectedSplitVars = pcts.means.initial.selected.split.vars, pctsMeansSelectedBckgVars = pcts.means.initial.selected.bckg.vars, pctsMeansSelectedPVVars = pcts.means.initial.selected.PV.vars, pctsMeansSelectedWeightVar = pcts.means.initial.selected.weight.var)
      output$pctsMeansArrowSelSplitVarsRight <- renderUI({
        if(is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "pctsMeansArrowSelSplitVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$pctsMeansArrowSelSplitVarsLeft <- renderUI({
        if(is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "pctsMeansArrowSelSplitVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$pctsMeansArrowSelBckgVarsRight <- renderUI({
        if(is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "pctsMeansArrowSelBckgVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$pctsMeansArrowSelBckgVarsLeft <- renderUI({
        if(is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "pctsMeansArrowSelBckgVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$pctsMeansArrowSelPVsRight <- renderUI({
        if(is.null(file.pct.means$resp.type) || is.null(file.pct.means$PV.sets)) {
          return(NULL)
        } else {
          actionButton(inputId = "pctsMeansArrowSelPVsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$pctsMeansArrowSelPVsLeft <- renderUI({
        if(is.null(file.pct.means$resp.type) || is.null(file.pct.means$PV.sets)) {
          return(NULL)
        } else {
          actionButton(inputId = "pctsMeansArrowSelPVsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$pctsMeansArrowSelPVsRightDisbld <- renderUI({
        if(is.null(file.pct.means$resp.type) || is.null(file.pct.means$PV.sets)) {
          actionButton(inputId = "pctsMeansArrowSelPVsRightDisbld", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      output$pctsMeansArrowSelPVsLeftDisbld <- renderUI({
        if(is.null(file.pct.means$resp.type) || is.null(file.pct.means$PV.sets)) {
          actionButton(inputId = "pctsMeansArrowSelPVsLeftDisbld", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      output$pctsMeansArrowSelWeightVarsRight <- renderUI({
        if(is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "pctsMeansArrowSelWeightVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$pctsMeansArrowSelWeightVarsLeft <- renderUI({
        if(is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "pctsMeansArrowSelWeightVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      observeEvent(input$pctsMeansArrowSelSplitVarsRight, {
        req(input$pctsMeansAllAvailableVars_rows_selected)
        pctsMeansAllVars$pctsMeansSelectedSplitVars <- rbind(isolate(pctsMeansAllVars$pctsMeansSelectedSplitVars), pctsMeansAllVars$pctsMeansAvailVars[input$pctsMeansAllAvailableVars_rows_selected, , drop = FALSE])
        pctsMeansAllVars$pctsMeansSelectedSplitVars <- pctsMeansAllVars$pctsMeansSelectedSplitVars[complete.cases(pctsMeansAllVars$pctsMeansSelectedSplitVars[ , "Variables"]), , drop = FALSE]
        pctsMeansAllVars$pctsMeansAvailVars <- isolate(pctsMeansAllVars$pctsMeansAvailVars[-input$pctsMeansAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$pctsMeansArrowSelSplitVarsLeft, {
        req(input$pctsMeansSplitVars_rows_selected)
        pctsMeansAllVars$pctsMeansAvailVars <- rbind(isolate(pctsMeansAllVars$pctsMeansAvailVars),        pctsMeansAllVars$pctsMeansSelectedSplitVars[input$pctsMeansSplitVars_rows_selected, , drop = FALSE])
        pctsMeansAllVars$pctsMeansAvailVars <- pctsMeansAllVars$pctsMeansAvailVars[complete.cases(pctsMeansAllVars$pctsMeansAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(pctsMeansAllVars$pctsMeansSelectedSplitVars) > 0) {
          pctsMeansAllVars$pctsMeansSelectedSplitVars <- isolate(pctsMeansAllVars$pctsMeansSelectedSplitVars[-input$pctsMeansSplitVars_rows_selected, , drop = FALSE])
        }
        if(!file.pct.means$country.ID %in% pctsMeansAllVars$pctsMeansSelectedSplitVars[ , Variables]) {
          showNotification(ui = HTML("Country ID <b>must always be</b> the first splitting variable!"), type = "error")
        }
        pctsMeansAllVars$pctsMeansSelectedSplitVars <- rbindlist(l = list(pctsMeansAllVars$pctsMeansSelectedSplitVars, pctsMeansAllVars$pctsMeansAvailVars[Variables == file.pct.means$country.ID, ]))
        pctsMeansAllVars$pctsMeansAvailVars <- pctsMeansAllVars$pctsMeansAvailVars[Variables != file.pct.means$country.ID, ]
      })
      observeEvent(input$pctsMeansArrowSelBckgVarsRight, {
        req(input$pctsMeansAllAvailableVars_rows_selected)
        pctsMeansAllVars$pctsMeansSelectedBckgVars <- rbind(isolate(pctsMeansAllVars$pctsMeansSelectedBckgVars), pctsMeansAllVars$pctsMeansAvailVars[input$pctsMeansAllAvailableVars_rows_selected, , drop = FALSE])
        pctsMeansAllVars$pctsMeansSelectedBckgVars <- pctsMeansAllVars$pctsMeansSelectedBckgVars[complete.cases(pctsMeansAllVars$pctsMeansSelectedBckgVars[ , "Variables"]), , drop = FALSE]
        pctsMeansAllVars$pctsMeansAvailVars <- isolate(pctsMeansAllVars$pctsMeansAvailVars[-input$pctsMeansAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$pctsMeansArrowSelBckgVarsLeft, {
        req(input$pctsMeansBckgVars_rows_selected)
        pctsMeansAllVars$pctsMeansAvailVars <- rbind(isolate(pctsMeansAllVars$pctsMeansAvailVars),        pctsMeansAllVars$pctsMeansSelectedBckgVars[input$pctsMeansBckgVars_rows_selected, , drop = FALSE])
        pctsMeansAllVars$pctsMeansAvailVars <- pctsMeansAllVars$pctsMeansAvailVars[complete.cases(pctsMeansAllVars$pctsMeansAvailVars[ , "Variables"]), , drop = FALSE]
        pctsMeansAllVars$pctsMeansSelectedBckgVars <- isolate(pctsMeansAllVars$pctsMeansSelectedBckgVars[-input$pctsMeansBckgVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$pctsMeansArrowSelPVsRight, {
        req(input$pctsMeansAllAvailableVars_rows_selected)
        pctsMeansAllVars$pctsMeansSelectedPVVars <- rbind(isolate(pctsMeansAllVars$pctsMeansSelectedPVVars), pctsMeansAllVars$pctsMeansAvailVars[input$pctsMeansAllAvailableVars_rows_selected, , drop = FALSE])
        pctsMeansAllVars$pctsMeansSelectedPVVars <- pctsMeansAllVars$pctsMeansSelectedPVVars[complete.cases(pctsMeansAllVars$pctsMeansSelectedPVVars[ , "Variables"]), , drop = FALSE]
        pctsMeansAllVars$pctsMeansAvailVars <- isolate(pctsMeansAllVars$pctsMeansAvailVars[-input$pctsMeansAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$pctsMeansArrowSelPVsLeft, {
        req(input$pctsMeansPVVars_rows_selected)
        pctsMeansAllVars$pctsMeansAvailVars <- rbind(isolate(pctsMeansAllVars$pctsMeansAvailVars),        pctsMeansAllVars$pctsMeansSelectedPVVars[input$pctsMeansPVVars_rows_selected, , drop = FALSE])
        pctsMeansAllVars$pctsMeansAvailVars <- pctsMeansAllVars$pctsMeansAvailVars[complete.cases(pctsMeansAllVars$pctsMeansAvailVars[ , "Variables"]), , drop = FALSE]
        pctsMeansAllVars$pctsMeansSelectedPVVars <- isolate(pctsMeansAllVars$pctsMeansSelectedPVVars[-input$pctsMeansPVVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$pctsMeansArrowSelWeightVarsRight, {
        req(input$pctsMeansAllAvailableVars_rows_selected)
        pctsMeansAllVars$pctsMeansSelectedWeightVar <- rbind(isolate(pctsMeansAllVars$pctsMeansSelectedWeightVar), pctsMeansAllVars$pctsMeansAvailVars[input$pctsMeansAllAvailableVars_rows_selected, , drop = FALSE])
        pctsMeansAllVars$pctsMeansSelectedWeightVar <- pctsMeansAllVars$pctsMeansSelectedWeightVar[complete.cases(pctsMeansAllVars$pctsMeansSelectedWeightVar[ , "Variables"]), , drop = FALSE]
        pctsMeansAllVars$pctsMeansAvailVars <- isolate(pctsMeansAllVars$pctsMeansAvailVars[-input$pctsMeansAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$pctsMeansArrowSelWeightVarsLeft, {
        req(input$pctsMeansWeightVar_rows_selected)
        pctsMeansAllVars$pctsMeansAvailVars <- rbind(isolate(pctsMeansAllVars$pctsMeansAvailVars),        pctsMeansAllVars$pctsMeansSelectedWeightVar[input$pctsMeansWeightVar_rows_selected, , drop = FALSE])
        pctsMeansAllVars$pctsMeansAvailVars <- pctsMeansAllVars$pctsMeansAvailVars[complete.cases(pctsMeansAllVars$pctsMeansAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(pctsMeansAllVars$pctsMeansSelectedWeightVar) > 0) {
          pctsMeansAllVars$pctsMeansSelectedWeightVar <- isolate(pctsMeansAllVars$pctsMeansSelectedWeightVar[-input$pctsMeansWeightVar_rows_selected, , drop = FALSE])
        }
      })
      output$pctsMeansAllAvailableVars <- renderDT({
        setkeyv(x = pctsMeansAllVars$pctsMeansAvailVars, cols = "order_col")
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available variables"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables available"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 766, scroller = TRUE
      ))
      output$pctsMeansSplitVars <- renderDT({
        pctsMeansAllVars$pctsMeansSelectedSplitVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Split variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$pctsMeansInclMiss <- renderUI({
        if(!is.null(pctsMeansAllVars$pctsMeansSelectedSplitVars) && nrow(pctsMeansAllVars$pctsMeansSelectedSplitVars) == 0) {
          return(NULL)
        } else {
          checkboxInput(inputId = "pctsMeansInclMiss", label = "Compute statistics for the missing values of the split variables", value = FALSE, width = "400px")
        }
      })
      output$pctsMeansBckgVars <- renderDT({
        pctsMeansAllVars$pctsMeansSelectedBckgVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Background continuous variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$pctsMeansPVVars <- renderDT({
        if(is.null(file.pct.means$PV.sets)) {
          return(NULL)
        } else {
          pctsMeansAllVars$pctsMeansSelectedPVVars
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$pctsMeansPVVarsDisbld <- renderDT({
        if(is.null(file.pct.means$PV.sets)) {
          data.table(V1 = as.character(), V2 = as.character())
        } else {
          return(NULL)
        }
      },
      select = "none",
      caption = htmltools::tags$caption(style = "color: #c6c6c6; font-weight: bold;", "Plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels"),
      options = list(
        language = list(zeroRecords = "No variables can be selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#c6c6c6', 'color': '#ffffff'});", "}"),
        dom = "ti",
        ordering = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0)),
        headerCallback = JS("function(thead, data, start, end, display){$('th', thead).css('border-bottom', 'none');}"),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$pctsMeansWeightVar <- renderDT({
        pctsMeansAllVars$pctsMeansSelectedWeightVar
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Weight variable"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = paste0("No weight selected, default (", file.pct.means$default.weight, ") will be used")),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$pctsMeansPVsNotPVs <- renderText({
        if(!is.null(pctsMeansAllVars$pctsMeansSelectedPVVars) && any(pctsMeansAllVars$pctsMeansSelectedPVVars[ , Variables] %in% file.pct.means$PV.sets == FALSE)) {
          HTML('Warning: One or more of the selected variables in "Plausible values" is not a set of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$pctsMeansSplitArePVs <- renderText({
        if(!is.null(pctsMeansAllVars$pctsMeansSelectedSplitVars) && any(pctsMeansAllVars$pctsMeansSelectedSplitVars[ , Variables] %in% file.pct.means$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Split variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$pctsMeansBckgArePVs <- renderText({
        if(!is.null(pctsMeansAllVars$pctsMeansSelectedBckgVars) && any(pctsMeansAllVars$pctsMeansSelectedBckgVars[ , Variables] %in% file.pct.means$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Background continuous variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$pctsMeansWgtsNotWgts <- renderText({
        if(any(pctsMeansAllVars$pctsMeansSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE)) {
          HTML('Warning: The variable in "Weight variable" is not a weight. Please check the added variable.')
        } else {
          return(NULL)
        }
      })
      output$pctsMeansWarnMoreVars <- renderText({
        if(!is.null(pctsMeansAllVars$pctsMeansSelectedBckgVars) && nrow(pctsMeansAllVars$pctsMeansSelectedBckgVars) > 1) {
          HTML('<b>Note:</b> Averages for more than one background variable can be computed at the same time. However, the estimates will slightly differ compared to computing means one by one because the cases with the missing values on each "bckg.avg.vars" are removed in advance and the more variables are provided, the more cases are likely to be removed.')
        } else if(!is.null(pctsMeansAllVars$pctsMeansSelectedBckgVars) && nrow(pctsMeansAllVars$pctsMeansSelectedBckgVars) > 0 && nrow(pctsMeansAllVars$pctsMeansSelectedPVVars) > 0 && all(pctsMeansAllVars$pctsMeansSelectedPVVars[ , Variables] %in% file.pct.means$PV.sets) == TRUE) {
          HTML('<b>Note:</b> Averages for both background variables and PVs can be computed at the same time. However, the estimates will slightly differ compared to computing means one by one because the cases with the missing values on each "bckg.avg.vars" are removed in advance and the more variables are provided, the more cases are likely to be removed.')
        }
      })
      observe({
        if(nrow(pctsMeansAllVars$pctsMeansSelectedWeightVar) > 1) {
          showNotification(ui = HTML("Only one weight variable can be selected!"), type = "error")
          pctsMeansAllVars$pctsMeansAvailVars <- rbind(isolate(pctsMeansAllVars$pctsMeansAvailVars),        pctsMeansAllVars$pctsMeansSelectedWeightVar[nrow(pctsMeansAllVars$pctsMeansSelectedWeightVar), , drop = FALSE])
          pctsMeansAllVars$pctsMeansAvailVars <- pctsMeansAllVars$pctsMeansAvailVars[complete.cases(pctsMeansAllVars$pctsMeansAvailVars[ , "Variables"]), , drop = FALSE]
          pctsMeansAllVars$pctsMeansSelectedWeightVar <- isolate(pctsMeansAllVars$pctsMeansSelectedWeightVar[-2, , drop = FALSE])
        }
      })
      observeEvent(eventExpr = input$pctsMeansChooseSrcFile, {
        pctsMeansAllVars$pctsMeansSelectedSplitVars <- NULL
        pctsMeansAllVars$pctsMeansSelectedPVVars <- NULL
        pctsMeansAllVars$pctsMeansSelectedBckgVars <- NULL
      }, ignoreInit = TRUE)
      output$pctsMeansShortcut <- renderUI({
        if(!is.null(file.pct.means$loaded) && file.pct.means$study %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "eTIMSS PSI", "preTIMSS", "TIMSS Advanced", "TiPi")) {
          checkboxInput(inputId = "pctsMeansShortcut", label = "Use shortcut method for computing SE", value = FALSE, width = "350px")
        }
      })
      output$pctsMeansGraphs <- renderUI({
        if(!is.null(file.pct.means$loaded)) {
          checkboxInput(inputId = "pctsMeansGraphs", label = "Produce graphs", value = FALSE, width = "350px")
        }
      })
      output$centralTendencyType <- renderUI({
        if(is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else {
          radioButtons(inputId = "centralTendencyType", label = "Measure of central tendency", choices = c("Mean", "Median", "Mode"), width = "200px", selected = "Mean")
        }
      })
      output$centralTendencyTypeExpl <- renderText({
        if(is.null(file.pct.means$PV.sets) && is.null(file.pct.means$resp.type)) {
          return(NULL)
        } else if(!is.null(file.pct.means$PV.sets) && !is.null(file.pct.means$resp.type) && !is.null(input$centralTendencyType) && input$centralTendencyType == "Mean") {
          HTML('<br/><br/>Computes the mean (arithmetic average) of continuous variables.')
        } else if(!is.null(file.pct.means$PV.sets) && !is.null(file.pct.means$resp.type) && !is.null(input$centralTendencyType) && input$centralTendencyType == "Median") {
          HTML('<br/><br/>Computes the median of continuous or ordinal variables.')
        } else if(!is.null(file.pct.means$PV.sets) && !is.null(file.pct.means$resp.type) && !is.null(input$centralTendencyType) && input$centralTendencyType == "Mode") {
          HTML('<br/><br/>Computes the mode of continuous or ordinal variables.')
        }
      })
      shinyFileSave(input, "pctsMeansChooseOutFile", filetype = list(xlsx = "xlsx"), roots = available.volumes, updateFreq = 100000)
      output$pctsMeansOpenOutput <- renderUI({
        if(length(parseSavePath(available.volumes, input$pctsMeansChooseOutFile)$datapath) > 0) {
          checkboxInput(inputId = "pctsMeansOpenOutput", label = "Open the output when done", value = TRUE, width = "250px")
        }
      })
      syntaxPctsMeans <- reactive({
        file.pct.means$pct.means.syntax <- paste0(
          'lsa.pcts.means(data.file = "', parseFilePaths(available.volumes, input$pctsMeansChooseSrcFile)$datapath, '", ',
          if(length(pctsMeansAllVars$pctsMeansSelectedSplitVars[ , Variables]) == 1) {
            paste0('split.vars = "', pctsMeansAllVars$pctsMeansSelectedSplitVars[ , Variables], '"')
          } else if(length(pctsMeansAllVars$pctsMeansSelectedSplitVars[ , Variables]) > 1) {
            paste0('split.vars = c("', paste(pctsMeansAllVars$pctsMeansSelectedSplitVars[ , Variables], collapse = '", "'), '")')
          },
          if(nrow(pctsMeansAllVars$pctsMeansSelectedBckgVars) == 1) {
            paste0(', bckg.avg.vars = "', pctsMeansAllVars$pctsMeansSelectedBckgVars[ , Variables], '"')
          } else if(nrow(pctsMeansAllVars$pctsMeansSelectedBckgVars) > 1) {
            paste0(', bckg.avg.vars = c("', paste(pctsMeansAllVars$pctsMeansSelectedBckgVars[ , Variables], collapse = '", "'), '")')
          },
          if(nrow(pctsMeansAllVars$pctsMeansSelectedPVVars) == 1) {
            paste0(', PV.root.avg = "', pctsMeansAllVars$pctsMeansSelectedPVVars[ , Variables], '"')
          } else if(nrow(pctsMeansAllVars$pctsMeansSelectedPVVars) > 1) {
            paste0(', PV.root.avg = c("', paste(pctsMeansAllVars$pctsMeansSelectedPVVars[ , Variables], collapse = '", "'), '")')
          },
          if(!is.null(input$centralTendencyType) && input$centralTendencyType == "Mean") {
            NULL
          } else if(!is.null(input$centralTendencyType) && input$centralTendencyType == "Median") {
            ', central.tendency = "median"'
          } else if(!is.null(input$centralTendencyType) && input$centralTendencyType == "Mode") {
            ', central.tendency = "mode"'
          },
          if(nrow(pctsMeansAllVars$pctsMeansSelectedWeightVar) == 1 && pctsMeansAllVars$pctsMeansSelectedWeightVar[ , Variables] == file.pct.means$default.weight) {
            NULL
          } else if(nrow(pctsMeansAllVars$pctsMeansSelectedWeightVar) == 1 && pctsMeansAllVars$pctsMeansSelectedWeightVar[ , Variables] %in% all.studies.available.weights && pctsMeansAllVars$pctsMeansSelectedWeightVar[ , Variables] != file.pct.means$default.weight) {
            paste0(', weight.var = "', pctsMeansAllVars$pctsMeansSelectedWeightVar[ , Variables], '"')
          } else if(nrow(pctsMeansAllVars$pctsMeansSelectedWeightVar) == 0) {
            NULL
          },
          if(!is.null(input$pctsMeansInclMiss) && input$pctsMeansInclMiss == TRUE) {
            ", include.missing = TRUE"
          },
          if(!is.null(input$pctsMeansShortcut) && input$pctsMeansShortcut == TRUE) {
            ", shortcut = TRUE"
          },
          if(!is.null(input$pctsMeansGraphs) && input$pctsMeansGraphs == TRUE) {
            ", graphs = TRUE"
          },
          paste0(', output.file = "', parseSavePath(available.volumes, input$pctsMeansChooseOutFile)$datapath, '"'),
          if(!is.null(input$pctsMeansOpenOutput) && input$pctsMeansOpenOutput == FALSE) {
            ', open.output = FALSE'
          } else if(!is.null(input$pctsMeansOpenOutput) && input$pctsMeansOpenOutput == TRUE) {
            NULL
          },
          ')'
        )
      })
      output$pctsMeansSyntaxHead <- renderText({
        if(length(parseSavePath(available.volumes, input$pctsMeansChooseOutFile)$datapath) == 1) {
          HTML("Syntax")
        } else {
          return(NULL)
        }
      })
      output$pctsMeansSyntax <- renderText({
        if(length(parseSavePath(available.volumes, input$pctsMeansChooseOutFile)$datapath) == 1 && nrow(rbindlist(l = list(pctsMeansAllVars$pctsMeansSelectedSplitVars, pctsMeansAllVars$pctsMeansSelectedPVVars, pctsMeansAllVars$pctsMeansSelectedBckgVars))) >= 1) {
          syntaxPctsMeans()
        } else {
          return(NULL)
        }
      })
      output$pctsMeansExecBtnHead <- renderText({
        if(length(parseSavePath(available.volumes, input$pctsMeansChooseOutFile)$datapath) == 1) {
          HTML("Press the button below to execute the syntax")
        } else {
          return(NULL)
        }
      })
      output$execPctsMeans <- renderUI({
        if(length(parseSavePath(available.volumes, input$pctsMeansChooseOutFile)$datapath) == 1) {
          actionButton(inputId = "execPctsMeans", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      observe({
        if(is.null(pctsMeansAllVars$pctsMeansSelectedPVVars) ||
           is.null(file.pct.means$loaded) ||
           any(pctsMeansAllVars$pctsMeansSelectedPVVars[ , Variables] %in% file.pct.means$PV.sets == FALSE) ||
           any(pctsMeansAllVars$pctsMeansSelectedSplitVars[ , Variables] %in% file.pct.means$PV.sets == TRUE) ||
           any(pctsMeansAllVars$pctsMeansSelectedBckgVars[ , Variables] %in% file.pct.means$PV.sets == TRUE) ||
           any(pctsMeansAllVars$pctsMeansSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE) ||
           is.null(file.pct.means$default.weight) ||
           length(file.pct.means$default.weight) == 0) {
          hide("pctsMeansShortcut")
          hide("pctsMeansChooseOutFile")
          hide("pctsMeansOpenOutput")
          hide("pctsMeansSyntaxHead")
          hide("pctsMeansSyntax")
          hide("pctsMeansExecBtnHead")
          hide("execPctsMeans")
          hide("consolePctsMeans")
        } else if (!is.null(file.pct.means$loaded) ||
                   all(pctsMeansAllVars$pctsMeansSelectedPVVars[ , Variables] %in% file.pct.means$PV.sets == TRUE) ||
                   all(pctsMeansAllVars$pctsMeansSelectedSplitVars[ , Variables] %in% file.pct.means$PV.sets == FALSE) ||
                   all(pctsMeansAllVars$pctsMeansSelectedBckgVars[ , Variables] %in% file.pct.means$PV.sets == FALSE) ||
                   all(pctsMeansAllVars$pctsMeansSelectedWeightVar[ , Variables] %in% all.studies.available.weights == TRUE) ||
                   !is.null(file.pct.means$default.weight) ||
                   length(file.pct.means$default.weight) != 0) {
          show("pctsMeansShortcut")
          show("pctsMeansChooseOutFile")
          show("pctsMeansOpenOutput")
          show("pctsMeansSyntaxHead")
          show("pctsMeansSyntax")
          show("pctsMeansExecBtnHead")
          show("execPctsMeans")
          show("consolePctsMeans")
        }
        if(is.null(file.pct.means$default.weight) || length(file.pct.means$default.weight) == 0) {
          hide("pctsMeansVariablesExplText")
          hide("pctsMeansAllAvailableVars")
          hide("pctsMeansArrowSelSplitVarsRight")
          hide("pctsMeansArrowSelSplitVarsLeft")
          hide("pctsMeansSplitVars")
          hide("pctsMeansInclMiss")
          hide("pctsMeansArrowSelBckgVarsRight")
          hide("pctsMeansArrowSelBckgVarsLeft")
          hide("pctsMeansBckgVars")
          hide("pctsMeansArrowSelPVsRight")
          hide("pctsMeansArrowSelPVsLeft")
          hide("pctsMeansArrowSelPVsRightDisbld")
          hide("pctsMeansArrowSelPVsLeftDisbld")
          hide("pctsMeansPVVarsDisbld")
          hide("pctsMeansPVVars")
          hide("pctsMeansArrowSelWeightVarsRight")
          hide("pctsMeansArrowSelWeightVarsLeft")
          hide("pctsMeansWeightVar")
        } else if(!is.null(file.pct.means$default.weight) || length(file.pct.means$default.weight) != 0) {
          show("pctsMeansVariablesExplText")
          show("pctsMeansAllAvailableVars")
          show("pctsMeansArrowSelSplitVarsRight")
          show("pctsMeansArrowSelSplitVarsLeft")
          show("pctsMeansSplitVars")
          show("pctsMeansInclMiss")
          show("pctsMeansArrowSelBckgVarsRight")
          show("pctsMeansArrowSelBckgVarsLeft")
          show("pctsMeansBckgVars")
          show("pctsMeansArrowSelPVsRight")
          show("pctsMeansArrowSelPVsLeft")
          show("pctsMeansArrowSelPVsRightDisbld")
          show("pctsMeansArrowSelPVsLeftDisbld")
          show("pctsMeansPVVarsDisbld")
          show("pctsMeansPVVars")
          show("pctsMeansArrowSelWeightVarsRight")
          show("pctsMeansArrowSelWeightVarsLeft")
          show("pctsMeansWeightVar")
        }
        if(nrow(rbindlist(l = list(pctsMeansAllVars$pctsMeansSelectedPVVars, pctsMeansAllVars$pctsMeansSelectedBckgVars), fill = TRUE)) == 0 ||
           is.null(file.pct.means$loaded) ||
           any(pctsMeansAllVars$pctsMeansSelectedPVVars[ , Variables] %in% file.pct.means$PV.sets == FALSE) ||
           any(pctsMeansAllVars$pctsMeansSelectedSplitVars[ , Variables] %in% file.pct.means$PV.sets == TRUE) ||
           any(pctsMeansAllVars$pctsMeansSelectedBckgVars[ , Variables] %in% file.pct.means$PV.sets == TRUE) ||
           any(pctsMeansAllVars$pctsMeansSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE) ||
           is.null(file.pct.means$default.weight) ||
           length(file.pct.means$default.weight) == 0) {
          hide("centralTendencyType")
          hide("centralTendencyTypeExpl")
        } else if(nrow(rbindlist(l = list(pctsMeansAllVars$pctsMeansSelectedPVVars, pctsMeansAllVars$pctsMeansSelectedBckgVars), fill = TRUE)) > 0 ||
                  all(pctsMeansAllVars$pctsMeansSelectedPVVars[ , Variables] %in% file.pct.means$PV.sets == TRUE) ||
                  all(pctsMeansAllVars$pctsMeansSelectedSplitVars[ , Variables] %in% file.pct.means$PV.sets == FALSE) ||
                  all(pctsMeansAllVars$pctsMeansSelectedBckgVars[ , Variables] %in% file.pct.means$PV.sets == FALSE) ||
                  all(pctsMeansAllVars$pctsMeansSelectedWeightVar[ , Variables] %in% all.studies.available.weights == TRUE) ||
                  !is.null(file.pct.means$default.weight) ||
                  length(file.pct.means$default.weight) != 0
        ) {
          show("centralTendencyType")
          show("centralTendencyTypeExpl")
        }
      })
    }
  })
  observeEvent(input$execPctsMeans, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consolePctsMeans", "")
      tryCatch({
        expr = eval(parse(text = file.pct.means$pct.means.syntax))
        showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
      }, error = function(e) {
        message("", e)
        showNotification(ui = HTML(paste0("Something went wrong. Possible reasons: <br/>", paste(gsub(pattern = "\\n|Error: ", replacement = "<br/>", x = e)))), type = "error", duration = NULL)
      })
    },
    message = function(i) {
      shinyjs::html(id = "consolePctsMeans", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consolePctsMeans", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
  }, ignoreInit = TRUE)
  output$h1Prctls <- renderText("Percentiles")
  hide("prctlsChooseOutFile")
  output$prctlsIntro <- renderText({HTML("Select large-scale assessment .RData file to load.")})
  file.prctls <- reactiveValues(loaded = NULL, is.lsa.data = NULL, vars.classes = NULL, resp.type = NULL, study = NULL, cycle = NULL, country.ID = NULL, PV.sets = NULL, default.weight = NULL, prctls.values = "5 25 50 75 95", prctls.syntax = NULL)
  shinyFileChoose(input, "prctlsChooseSrcFile", roots = available.volumes, filetype = list(RData = "RData"))
  observeEvent(eventExpr = input$prctlsChooseSrcFile, {
    file.prctls$loaded <- NULL
    file.prctls$vars.classes <- NULL
    file.prctls$study <- NULL
    file.prctls$cycle <- NULL
    file.prctls$resp.type <- NULL
    file.prctls$PV.sets <- NULL
    file.prctls$default.weight <- NULL
    if(length(parseFilePaths(available.volumes, input$prctlsChooseSrcFile)$datapath) > 0 && file.size(parseFilePaths(available.volumes, input$prctlsChooseSrcFile)$datapath) > 104857600) {
      showNotification(ui = HTML('The size of the selected<br/>file is rather large. Please<br/>wait until the file is loaded.'), type = "message")
    }
    if(length(parseFilePaths(available.volumes, input$prctlsChooseSrcFile)$datapath) > 0) {
      file.prctls$loaded <- get(load(parseFilePaths(available.volumes, input$prctlsChooseSrcFile)$datapath))
      if("lsa.data" %in% class(file.prctls$loaded)) {
        file.prctls$is.lsa.data <- TRUE
      } else {
        file.prctls$is.lsa.data <- FALSE
      }
      file.prctls$study <- attr(x = file.prctls$loaded, which = "study")
      file.prctls$cycle <- attr(x = file.prctls$loaded, which = "cycle")
      file.prctls$resp.type <- attr(x = file.prctls$loaded, which = "file.type")
      file.prctls$vars.classes <- lapply(X = file.prctls$loaded, FUN = class)
      file.prctls$loaded <- data.table(Variables = names(file.prctls$loaded), Variable_Labels = sapply(X = file.prctls$loaded, FUN = function(j) {
        if(is.null(attr(x = j, which = "variable.label"))) {
          return(NA_character_)
        } else {
          attr(x = j, which = "variable.label")
        }
      }),
      order_col = 1:ncol(file.prctls$loaded))
      tmp.PV.names <- grep(pattern = paste(all.available.PVs, collapse = "|"), x = file.prctls$loaded[ , Variables], value = TRUE)
      if(length(tmp.PV.names) > 0) {
        collapsed.PVs <- collapse.loaded.file.PV.names(PV.vector = tmp.PV.names, vars.object = file.prctls$loaded)
        file.prctls$loaded <- file.prctls$loaded[!Variables %in% tmp.PV.names]
        file.prctls$loaded <- rbindlist(l = list(file.prctls$loaded, collapsed.PVs))
        setkeyv(x = file.prctls$loaded, cols = "order_col")
        file.prctls$PV.sets <- collapsed.PVs[ , Variables]
      }
      if(!is.null(file.prctls$study)) {
        file.prctls$default.weight <- define.default.weight(study = file.prctls$study, loaded.names.and.labels = file.prctls$loaded, respondent.type = file.prctls$resp.type)
      }
      file.prctls$country.ID <- NULL
      if("IDCNTRY" %in% file.prctls$loaded[ , Variables]) {
        file.prctls$country.ID <- "IDCNTRY"
      } else {
        file.prctls$country.ID <- "CNT"
      }
    }
    output$prctlsSrcPathDisplay <- renderText({parseFilePaths(available.volumes, input$prctlsChooseSrcFile)$datapath})
  }, ignoreInit = TRUE)
  observe({
    if(!is.null(file.prctls$loaded) && file.prctls$is.lsa.data == FALSE) {
      showNotification(ui = HTML('The data is not of class "lsa.data".<br/>Please check the file content.'), type = "error")
    } else if (!is.null(file.prctls$loaded) && file.prctls$is.lsa.data == TRUE) {
      output$prctlsStudyName <- renderText({
        if(is.null(file.prctls$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Study: ', file.prctls$study))
        }
      })
      output$prctlsStudyCycle <- renderText({
        if(is.null(file.prctls$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Cycle: ', file.prctls$cycle))
        }
      })
      output$prctlsRespHead <- renderText({
        if(is.null(file.prctls$resp.type)) {
          return(NULL)
        } else {
          HTML('<u>The file contains data from the following respondents:</u>')
        }
      })
      output$prctlsRespAvailable <- renderText({
        if(is.null(file.prctls$resp.type)) {
          return(NULL)
        } else {
          HTML(file.merged.respondents[[file.prctls$resp.type]])
        }
      })
      output$prctlsNoWeights <- renderText({
        if(!is.null(file.prctls$loaded) && is.null(file.prctls$default.weight) || !is.null(file.prctls$loaded) && length(file.prctls$default.weight) == 0) {
          HTML('Error: The loaded file does not contain any recognizable default weight variable. Such files cannot be analyzed on their own and must be merged with other respondents in advance.')
        } else {
          return(NULL)
        }
      })
      output$prctlsVariablesExplText <- renderText({
        if(is.null(file.prctls$resp.type)) {
          return(NULL)
        } else {
          HTML('Use the panels below to select the variables to compute percentiles of continuous variables within groups specified by splitting variables and precentages of respondents within these groups.')
        }
      })
      prctls.initial.available.vars <- file.prctls$loaded[!Variables %in% c(file.prctls$default.weight, file.prctls$country.ID), ]
      prctls.initial.selected.split.vars <- file.prctls$loaded[Variables == file.prctls$country.ID, ]
      prctls.initial.selected.bckg.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      prctls.initial.selected.PV.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      prctls.initial.selected.weight.var <- file.prctls$loaded[Variables %in% file.prctls$default.weight, ]
      prctlsAllVars <- reactiveValues(prctlsAvailVars = prctls.initial.available.vars, prctlsSelectedSplitVars = prctls.initial.selected.split.vars, prctlsSelectedBckgVars = prctls.initial.selected.bckg.vars, prctlsSelectedPVVars = prctls.initial.selected.PV.vars, prctlsSelectedWeightVar = prctls.initial.selected.weight.var)
      output$prctlsArrowSelSplitVarsRight <- renderUI({
        if(is.null(file.prctls$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "prctlsArrowSelSplitVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$prctlsArrowSelSplitVarsLeft <- renderUI({
        if(is.null(file.prctls$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "prctlsArrowSelSplitVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$prctlsArrowSelBckgVarsRight <- renderUI({
        if(is.null(file.prctls$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "prctlsArrowSelBckgVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$prctlsArrowSelBckgVarsLeft <- renderUI({
        if(is.null(file.prctls$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "prctlsArrowSelBckgVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$prctlsArrowSelPVsRight <- renderUI({
        if(is.null(file.prctls$resp.type) || is.null(file.prctls$PV.sets)) {
          return(NULL)
        } else {
          actionButton(inputId = "prctlsArrowSelPVsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$prctlsArrowSelPVsLeft <- renderUI({
        if(is.null(file.prctls$resp.type) || is.null(file.prctls$PV.sets)) {
          return(NULL)
        } else {
          actionButton(inputId = "prctlsArrowSelPVsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$prctlsArrowSelPVsRightDisbld <- renderUI({
        if(is.null(file.prctls$resp.type) || is.null(file.prctls$PV.sets)) {
          actionButton(inputId = "prctlsArrowSelPVsRightDisbld", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      output$prctlsArrowSelPVsLeftDisbld <- renderUI({
        if(is.null(file.prctls$resp.type) || is.null(file.prctls$PV.sets)) {
          actionButton(inputId = "prctlsArrowSelPVsLeftDisbld", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      output$prctlsArrowSelWeightVarsRight <- renderUI({
        if(is.null(file.prctls$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "prctlsArrowSelWeightVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$prctlsArrowSelWeightVarsLeft <- renderUI({
        if(is.null(file.prctls$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "prctlsArrowSelWeightVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      observeEvent(input$prctlsArrowSelSplitVarsRight, {
        req(input$prctlsAllAvailableVars_rows_selected)
        prctlsAllVars$prctlsSelectedSplitVars <- rbind(isolate(prctlsAllVars$prctlsSelectedSplitVars), prctlsAllVars$prctlsAvailVars[input$prctlsAllAvailableVars_rows_selected, , drop = FALSE])
        prctlsAllVars$prctlsSelectedSplitVars <- prctlsAllVars$prctlsSelectedSplitVars[complete.cases(prctlsAllVars$prctlsSelectedSplitVars[ , "Variables"]), , drop = FALSE]
        prctlsAllVars$prctlsAvailVars <- isolate(prctlsAllVars$prctlsAvailVars[-input$prctlsAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$prctlsArrowSelSplitVarsLeft, {
        req(input$prctlsSplitVars_rows_selected)
        prctlsAllVars$prctlsAvailVars <- rbind(isolate(prctlsAllVars$prctlsAvailVars),        prctlsAllVars$prctlsSelectedSplitVars[input$prctlsSplitVars_rows_selected, , drop = FALSE])
        prctlsAllVars$prctlsAvailVars <- prctlsAllVars$prctlsAvailVars[complete.cases(prctlsAllVars$prctlsAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(prctlsAllVars$prctlsSelectedSplitVars) > 0) {
          prctlsAllVars$prctlsSelectedSplitVars <- isolate(prctlsAllVars$prctlsSelectedSplitVars[-input$prctlsSplitVars_rows_selected, , drop = FALSE])
        }
        if(!file.prctls$country.ID %in% prctlsAllVars$prctlsSelectedSplitVars[ , Variables]) {
          showNotification(ui = HTML("Country ID <b>must always be</b> the first splitting variable!"), type = "error")
        }
        prctlsAllVars$prctlsSelectedSplitVars <- rbindlist(l = list(prctlsAllVars$prctlsSelectedSplitVars, prctlsAllVars$prctlsAvailVars[Variables == file.prctls$country.ID, ]))
        prctlsAllVars$prctlsAvailVars <- prctlsAllVars$prctlsAvailVars[Variables != file.prctls$country.ID, ]
      })
      observeEvent(input$prctlsArrowSelBckgVarsRight, {
        req(input$prctlsAllAvailableVars_rows_selected)
        prctlsAllVars$prctlsSelectedBckgVars <- rbind(isolate(prctlsAllVars$prctlsSelectedBckgVars), prctlsAllVars$prctlsAvailVars[input$prctlsAllAvailableVars_rows_selected, , drop = FALSE])
        prctlsAllVars$prctlsSelectedBckgVars <- prctlsAllVars$prctlsSelectedBckgVars[complete.cases(prctlsAllVars$prctlsSelectedBckgVars[ , "Variables"]), , drop = FALSE]
        prctlsAllVars$prctlsAvailVars <- isolate(prctlsAllVars$prctlsAvailVars[-input$prctlsAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$prctlsArrowSelBckgVarsLeft, {
        req(input$prctlsBckgVars_rows_selected)
        prctlsAllVars$prctlsAvailVars <- rbind(isolate(prctlsAllVars$prctlsAvailVars),        prctlsAllVars$prctlsSelectedBckgVars[input$prctlsBckgVars_rows_selected, , drop = FALSE])
        prctlsAllVars$prctlsAvailVars <- prctlsAllVars$prctlsAvailVars[complete.cases(prctlsAllVars$prctlsAvailVars[ , "Variables"]), , drop = FALSE]
        prctlsAllVars$prctlsSelectedBckgVars <- isolate(prctlsAllVars$prctlsSelectedBckgVars[-input$prctlsBckgVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$prctlsArrowSelPVsRight, {
        req(input$prctlsAllAvailableVars_rows_selected)
        prctlsAllVars$prctlsSelectedPVVars <- rbind(isolate(prctlsAllVars$prctlsSelectedPVVars), prctlsAllVars$prctlsAvailVars[input$prctlsAllAvailableVars_rows_selected, , drop = FALSE])
        prctlsAllVars$prctlsSelectedPVVars <- prctlsAllVars$prctlsSelectedPVVars[complete.cases(prctlsAllVars$prctlsSelectedPVVars[ , "Variables"]), , drop = FALSE]
        if(nrow(prctlsAllVars$prctlsSelectedPVVars)) {
          prctlsAllVars$prctlsAvailVars <- isolate(prctlsAllVars$prctlsAvailVars[-input$prctlsAllAvailableVars_rows_selected, , drop = FALSE])
        }
      })
      observeEvent(input$prctlsArrowSelPVsLeft, {
        req(input$prctlsPVVars_rows_selected)
        prctlsAllVars$prctlsAvailVars <- rbind(isolate(prctlsAllVars$prctlsAvailVars),        prctlsAllVars$prctlsSelectedPVVars[input$prctlsPVVars_rows_selected, , drop = FALSE])
        prctlsAllVars$prctlsAvailVars <- prctlsAllVars$prctlsAvailVars[complete.cases(prctlsAllVars$prctlsAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(prctlsAllVars$prctlsSelectedPVVars)) {
          prctlsAllVars$prctlsSelectedPVVars <- isolate(prctlsAllVars$prctlsSelectedPVVars[-input$prctlsPVVars_rows_selected, , drop = FALSE])
        }
      })
      observeEvent(input$prctlsArrowSelWeightVarsRight, {
        req(input$prctlsAllAvailableVars_rows_selected)
        prctlsAllVars$prctlsSelectedWeightVar <- rbind(isolate(prctlsAllVars$prctlsSelectedWeightVar), prctlsAllVars$prctlsAvailVars[input$prctlsAllAvailableVars_rows_selected, , drop = FALSE])
        prctlsAllVars$prctlsSelectedWeightVar <- prctlsAllVars$prctlsSelectedWeightVar[complete.cases(prctlsAllVars$prctlsSelectedWeightVar[ , "Variables"]), , drop = FALSE]
        prctlsAllVars$prctlsAvailVars <- isolate(prctlsAllVars$prctlsAvailVars[-input$prctlsAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$prctlsArrowSelWeightVarsLeft, {
        req(input$prctlsWeightVar_rows_selected)
        prctlsAllVars$prctlsAvailVars <- rbind(isolate(prctlsAllVars$prctlsAvailVars),        prctlsAllVars$prctlsSelectedWeightVar[input$prctlsWeightVar_rows_selected, , drop = FALSE])
        prctlsAllVars$prctlsAvailVars <- prctlsAllVars$prctlsAvailVars[complete.cases(prctlsAllVars$prctlsAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(prctlsAllVars$prctlsSelectedWeightVar) > 0) {
          prctlsAllVars$prctlsSelectedWeightVar <- isolate(prctlsAllVars$prctlsSelectedWeightVar[-input$prctlsWeightVar_rows_selected, , drop = FALSE])
        }
      })
      output$prctlsAllAvailableVars <- renderDT({
        setkeyv(x = prctlsAllVars$prctlsAvailVars, cols = "order_col")
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available variables"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables available"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 766, scroller = TRUE
      ))
      output$prctlsSplitVars <- renderDT({
        prctlsAllVars$prctlsSelectedSplitVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Split variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$prctlsInclMiss <- renderUI({
        if(nrow(prctlsAllVars$prctlsSelectedSplitVars) == 0) {
          return(NULL)
        } else {
          checkboxInput(inputId = "prctlsInclMiss", label = "Compute statistics for the missing values of the split variables", value = FALSE, width = "400px")
        }
      })
      output$prctlsBckgVars <- renderDT({
        prctlsAllVars$prctlsSelectedBckgVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Background continuous variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$prctlsPVVars <- renderDT({
        if(is.null(file.prctls$PV.sets)) {
          return(NULL)
        } else {
          prctlsAllVars$prctlsSelectedPVVars
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$prctlsPVVarsDisbld <- renderDT({
        if(is.null(file.prctls$PV.sets)) {
          data.table(V1 = as.character(), V2 = as.character())
        } else {
          return(NULL)
        }
      },
      select = "none",
      caption = htmltools::tags$caption(style = "color: #c6c6c6; font-weight: bold;", "Plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels"),
      options = list(
        language = list(zeroRecords = "No variables can be selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#c6c6c6', 'color': '#ffffff'});", "}"),
        dom = "ti",
        ordering = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0)),
        headerCallback = JS("function(thead, data, start, end, display){$('th', thead).css('border-bottom', 'none');}"),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$prctlsWeightVar <- renderDT({
        prctlsAllVars$prctlsSelectedWeightVar
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Weight variable"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = paste0("No weight selected, default (", file.prctls$default.weight, ") will be used")),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$prctlsPVsNotPVs <- renderText({
        if(!is.null(prctlsAllVars$prctlsSelectedPVVars) && any(prctlsAllVars$prctlsSelectedPVVars[ , Variables] %in% file.prctls$PV.sets == FALSE)) {
          HTML('Warning: One or more of the selected variables in "Plausible values" is not a set of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$prctlsSplitArePVs <- renderText({
        if(any(prctlsAllVars$prctlsSelectedSplitVars[ , Variables] %in% file.prctls$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Split variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$prctlsBckgArePVs <- renderText({
        if(!is.null(prctlsAllVars$prctlsSelectedPVVars) && any(prctlsAllVars$prctlsSelectedBckgVars[ , Variables] %in% file.prctls$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Background continuous variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$prctlsBckgNotCnt <- renderText({
        if(!is.null(prctlsAllVars$prctlsSelectedPVVars) && any(unlist(file.prctls$vars.classes[prctlsAllVars$prctlsSelectedBckgVars[ , Variables]]) != "numeric")) {
          HTML('Warning: One or more of the selected variables in "Background continuous variables" is not continuous. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$prctlsWgtsNotWgts <- renderText({
        if(any(prctlsAllVars$prctlsSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE)) {
          HTML('Warning: The variable in "Weight variable" is not a weight. Please check the added variable.')
        } else {
          return(NULL)
        }
      })
      output$prctlsWarnMoreVars <- renderText({
        if(!is.null(prctlsAllVars$prctlsSelectedPVVars) && nrow(prctlsAllVars$prctlsSelectedBckgVars) > 1) {
          HTML('<b>Note:</b> Percentiles for more than one background variable can be computed at the same time. However, the estimates will slightly differ compared to computing percentiles one by one because the cases with the missing values on each "bckg.prctls.vars" are removed in advance and the more variables are provided, the more cases are likely to be removed.')
        } else if(!is.null(prctlsAllVars$prctlsSelectedPVVars) && nrow(prctlsAllVars$prctlsSelectedBckgVars) > 0 && nrow(prctlsAllVars$prctlsSelectedPVVars) > 0 && all(prctlsAllVars$prctlsSelectedPVVars[ , Variables] %in% file.prctls$PV.sets) == TRUE) {
          HTML('<b>Note:</b> Percentiles for both background variables and PVs can be computed at the same time. However, the estimates will slightly differ compared to computing percentiles one by one because the cases with the missing values on each "bckg.prctls.vars" are removed in advance and the more variables are provided, the more cases are likely to be removed.')
        }
      })
      observe({
        if(nrow(prctlsAllVars$prctlsSelectedWeightVar) > 1) {
          showNotification(ui = HTML("Only one weight variable can be selected!"), type = "error")
          prctlsAllVars$prctlsAvailVars <- rbind(isolate(prctlsAllVars$prctlsAvailVars),        prctlsAllVars$prctlsSelectedWeightVar[nrow(prctlsAllVars$prctlsSelectedWeightVar), , drop = FALSE])
          prctlsAllVars$prctlsAvailVars <- prctlsAllVars$prctlsAvailVars[complete.cases(prctlsAllVars$prctlsAvailVars[ , "Variables"]), , drop = FALSE]
          prctlsAllVars$prctlsSelectedWeightVar <- isolate(prctlsAllVars$prctlsSelectedWeightVar[-2, , drop = FALSE])
        }
      })
      observeEvent(eventExpr = input$prctlsChooseSrcFile, {
        prctlsAllVars$prctlsSelectedPVVars <- NULL
        prctlsAllVars$prctlsSelectedBckgVars <- NULL
      }, ignoreInit = TRUE)
      output$prctlsValuesExpl <- renderText({
        if(nrow(prctlsAllVars$prctlsAvailVars) == 0) {
          return(NULL)
        } else {
          HTML('In the field below, add/change the percentiles that will be calculated from the distribution of values for the selected variables.<br/>The values <b>must</b> be whole numbers, divided by spaces.')
        }
      })
      output$prctlsValues <- renderUI({
        if(nrow(prctlsAllVars$prctlsAvailVars) == 0) {
          return(NULL)
        } else {
          textInput(inputId = "prctlsValues", label = "Percentiles", value = file.prctls$prctls.values, width = "350px")
        }
      })
      output$prctlsValuesReset <- renderUI({
        if(nrow(prctlsAllVars$prctlsAvailVars) == 0) {
          return(NULL)
        } else {
          actionButton(inputId = "prctlsValuesReset", label = "Reset", icon = icon("undo-alt"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      prctls.not.numbers <- reactiveVal()
      observe({
        if(!is.null(input$prctlsValues) && grepl(pattern = "[[:alpha:]]|[[:punct:]]", x = input$prctlsValues) == TRUE) {
          prctls.not.numbers(TRUE)
        } else if(!is.null(input$prctlsValues) && grepl(pattern = "[[:alpha:]]|[[:punct:]]", x = input$prctlsValues) == FALSE) {
          prctls.not.numbers(FALSE)
        }
      })
      output$prctlsNotNum <- renderText({
        if(!is.null(input$prctlsValues) && prctls.not.numbers() == TRUE) {
          HTML('<br/>Warning: The values passed to the field on the left can contain <u>only</u> numbers and spaces. Please check the input.')
        } else {
          return(NULL)
        }
      })
      prctls.out.of.bounds <- reactiveVal()
      observe({
        suppressWarnings(
          if(!is.null(input$prctlsValues) && any(na.omit(as.numeric(unlist(str_split(string = input$prctlsValues, pattern = "[[:space:]]+")))) > 99)) {
            prctls.out.of.bounds(TRUE)
          } else if(!is.null(input$prctlsValues) && any(na.omit(as.numeric(unlist(str_split(string = input$prctlsValues, pattern = "[[:space:]]+")))) < 1)) {
            prctls.out.of.bounds(TRUE)
          } else {
            prctls.out.of.bounds(FALSE)
          }
        )
      })
      output$prctlsNotInRange <- renderText({
        if(!is.null(input$prctlsValues) && prctls.out.of.bounds() == TRUE) {
          HTML('<br/>Warning: The values passed to the field on the left can be <u>only</u> between 1 and 99. Please check the input.')
        } else {
          return(NULL)
        }
      })
      observeEvent(input$prctlsValuesReset, {
        shinyjs::reset("prctlsValues")
      })
      output$prctlsShortcut <- renderUI({
        if(!is.null(file.prctls$loaded) && file.prctls$study %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "eTIMSS PSI", "preTIMSS", "TIMSS Advanced", "TiPi")) {
          checkboxInput(inputId = "prctlsShortcut", label = "Use shortcut method for computing SE", value = FALSE, width = "350px")
        }
      })
      output$prctlsGraphs <- renderUI({
        if(!is.null(file.prctls$loaded)) {
          checkboxInput(inputId = "prctlsGraphs", label = "Produce graphs", value = FALSE, width = "350px")
        }
      })
      shinyFileSave(input, "prctlsChooseOutFile", filetype = list(xlsx = "xlsx"), roots = available.volumes, updateFreq = 100000)
      output$prctlsOpenOutput <- renderUI({
        if(length(parseSavePath(available.volumes, input$prctlsChooseOutFile)$datapath) > 0) {
          checkboxInput(inputId = "prctlsOpenOutput", label = "Open the output when done", value = TRUE, width = "250px")
        }
      })
      syntaxPrctls <- reactive({
        tmp.prctls.values <- unlist(str_split(string = trimws(input$prctlsValues), pattern = "[[:space:]]+"))
        file.prctls$prctls.syntax <- paste0(
          paste0('lsa.prctls(data.file = "', parseFilePaths(available.volumes, input$prctlsChooseSrcFile)$datapath, '"'),
          if(nrow(prctlsAllVars$prctlsSelectedSplitVars) == 1) {
            paste0(', split.vars = "', prctlsAllVars$prctlsSelectedSplitVars[ , Variables], '"')
          } else if(nrow(prctlsAllVars$prctlsSelectedSplitVars) > 1) {
            paste0(', split.vars = c("', paste(prctlsAllVars$prctlsSelectedSplitVars[ , Variables], collapse = '", "'), '")')
          },
          if(nrow(prctlsAllVars$prctlsSelectedBckgVars) == 0) {
            NULL
          } else if(nrow(prctlsAllVars$prctlsSelectedBckgVars) == 1) {
            paste0(', bckg.prctls.vars = "', prctlsAllVars$prctlsSelectedBckgVars[ , Variables], '"')
          } else if(nrow(prctlsAllVars$prctlsSelectedBckgVars) > 1) {
            paste0(', bckg.prctls.vars = c("', paste(prctlsAllVars$prctlsSelectedBckgVars[ , Variables], collapse = '", "'), '")')
          },
          if(nrow(prctlsAllVars$prctlsSelectedPVVars) == 0) {
            NULL
          } else if(nrow(prctlsAllVars$prctlsSelectedPVVars) == 1) {
            paste0(', PV.root.prctls = "', prctlsAllVars$prctlsSelectedPVVars[ , Variables], '"')
          } else if(nrow(prctlsAllVars$prctlsSelectedPVVars) > 1) {
            paste0(', PV.root.prctls = c("', paste(prctlsAllVars$prctlsSelectedPVVars[ , Variables], collapse = '", "'), '")')
          },
          if(length(tmp.prctls.values) == 1) {
            paste0(', prctls = ', gsub(pattern = "[[:space:]]+", replacement = "", x = tmp.prctls.values))
          } else if(length(tmp.prctls.values) > 1) {
            paste0(', prctls = c(', paste(tmp.prctls.values, collapse = ', '), ')')
          } else {
            NULL
          },
          if(nrow(prctlsAllVars$prctlsSelectedWeightVar) == 0) {
            NULL
          } else if(nrow(prctlsAllVars$prctlsSelectedWeightVar) == 1 && prctlsAllVars$prctlsSelectedWeightVar[ , Variables] == file.prctls$default.weight) {
            NULL
          } else if(nrow(prctlsAllVars$prctlsSelectedWeightVar) == 1 && prctlsAllVars$prctlsSelectedWeightVar[ , Variables] != file.prctls$default.weight) {
            paste0(', weight.var = "', prctlsAllVars$prctlsSelectedWeightVar[ , Variables], '"')
          },
          if(is.null(input$prctlsInclMiss) || input$prctlsInclMiss == FALSE) {
            NULL
          } else if(!is.null(input$prctlsInclMiss) || input$prctlsInclMiss == TRUE) {
            ', include.missing = TRUE'
          },
          if(is.null(input$prctlsShortcut) || input$prctlsShortcut == FALSE) {
            NULL
          } else if(!is.null(input$prctlsShortcut) || input$prctlsShortcut == TRUE) {
            ', shortcut = TRUE'
          },
          if(is.null(input$prctlsGraphs) || input$prctlsGraphs == FALSE) {
            NULL
          } else if(!is.null(input$prctlsGraphs) || input$prctlsGraphs == TRUE) {
            ', graphs = TRUE'
          },
          paste0(', output.file = "', parseSavePath(available.volumes, input$prctlsChooseOutFile)$datapath, '"'),
          if(is.null(input$prctlsOpenOutput) || input$prctlsOpenOutput == FALSE) {
            NULL
          } else if(!is.null(input$prctlsOpenOutput) || input$prctlsOpenOutput == TRUE) {
            ', open.output = TRUE'
          },
          ')'
        )
      })
      output$prctlsSyntaxHead <- renderText({
        if(length(parseSavePath(available.volumes, input$prctlsChooseOutFile)$datapath) == 1) {
          HTML("Syntax")
        } else {
          return(NULL)
        }
      })
      output$prctlsSyntax <- renderText({
        if(length(parseSavePath(available.volumes, input$prctlsChooseOutFile)$datapath) == 1 && nrow(rbindlist(l = list(prctlsAllVars$prctlsSelectedPVVars, prctlsAllVars$prctlsSelectedBckgVars))) >= 1) {
          syntaxPrctls()
        } else {
          return(NULL)
        }
      })
      output$prctlsExecBtnHead <- renderText({
        if(length(parseSavePath(available.volumes, input$prctlsChooseOutFile)$datapath) == 1) {
          HTML("Press the button below to execute the syntax")
        } else {
          return(NULL)
        }
      })
      output$execPrctls <- renderUI({
        if(length(parseSavePath(available.volumes, input$prctlsChooseOutFile)$datapath) == 1) {
          actionButton(inputId = "execPrctls", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      observe({
        if(is.null(file.prctls$loaded) || nrow(rbindlist(l = list(prctlsAllVars$prctlsSelectedBckgVars, prctlsAllVars$prctlsSelectedPVVars))) == 0 || any(prctlsAllVars$prctlsSelectedPVVars[ , Variables] %in% file.prctls$PV.sets == FALSE) || any(prctlsAllVars$prctlsSelectedSplitVars[ , Variables] %in% file.prctls$PV.sets == TRUE) || any(prctlsAllVars$prctlsSelectedBckgVars[ , Variables] %in% file.prctls$PV.sets == TRUE) || is.null(file.prctls$default.weight) || any(prctlsAllVars$prctlsSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE) || any(unlist(file.prctls$vars.classes[prctlsAllVars$prctlsSelectedBckgVars[ , Variables]]) != "numeric")) {
          hide("prctlsValuesExpl")
          hide("prctlsValues")
          hide("prctlsValuesReset")
          hide("prctlsWarnMoreVars")
        } else if(!is.null(file.prctls$loaded) || nrow(rbindlist(l = list(prctlsAllVars$prctlsSelectedBckgVars, prctlsAllVars$prctlsSelectedPVVars))) > 0 || any(prctlsAllVars$prctlsSelectedPVVars[ , Variables] %in% file.prctls$PV.sets == TRUE) || any(prctlsAllVars$prctlsSelectedSplitVars[ , Variables] %in% file.prctls$PV.sets == FALSE) || any(prctlsAllVars$prctlsSelectedBckgVars[ , Variables] %in% file.prctls$PV.sets == FALSE) || !is.null(file.prctls$default.weight) || any(prctlsAllVars$prctlsSelectedWeightVar[ , Variables] %in% all.studies.available.weights == TRUE) || all(unlist(file.prctls$vars.classes[prctlsAllVars$prctlsSelectedBckgVars[ , Variables]]) != "numeric")) {
          show("prctlsValuesExpl")
          show("prctlsValues")
          show("prctlsValuesReset")
          show("prctlsWarnMoreVars")
        }
        if(is.null(file.prctls$loaded) || is.null(prctlsAllVars$prctlsSelectedPVVars) || any(prctlsAllVars$prctlsSelectedPVVars[ , Variables] %in% file.prctls$PV.sets == FALSE) || is.null(prctlsAllVars$prctlsSelectedSplitVars) || any(prctlsAllVars$prctlsSelectedSplitVars[ , Variables] %in% file.prctls$PV.sets == TRUE) || any(prctlsAllVars$prctlsSelectedBckgVars[ , Variables] %in% file.prctls$PV.sets == TRUE) || is.null(prctlsAllVars$prctlsSelectedWeightVar) || any(prctlsAllVars$prctlsSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE) || nrow(rbindlist(l = list(prctlsAllVars$prctlsSelectedBckgVars, prctlsAllVars$prctlsSelectedPVVars))) == 0 || input$prctlsValues == "" || prctls.not.numbers() == TRUE || prctls.out.of.bounds() == TRUE || any(unlist(file.prctls$vars.classes[prctlsAllVars$prctlsSelectedBckgVars[ , Variables]]) != "numeric")) {
          hide("prctlsShortcut")
          hide("prctlsGraphs")
          hide("prctlsChooseOutFile")
          hide("prctlsOpenOutput")
          hide("prctlsSyntaxHead")
          hide("prctlsSyntax")
          hide("prctlsExecBtnHead")
          hide("execPrctls")
          hide("consolePrctls")
        } else if (!is.null(file.prctls$loaded) || any(prctlsAllVars$prctlsSelectedPVVars[ , Variables] %in% file.prctls$PV.sets == TRUE) || any(prctlsAllVars$prctlsSelectedSplitVars[ , Variables] %in% file.prctls$PV.sets == FALSE) || any(prctlsAllVars$prctlsSelectedBckgVars[ , Variables] %in% file.prctls$PV.sets == FALSE) || any(prctlsAllVars$prctlsSelectedWeightVar[ , Variables] %in% all.studies.available.weights == TRUE) || nrow(rbindlist(l = list(prctlsAllVars$prctlsSelectedBckgVars, prctlsAllVars$prctlsSelectedPVVars))) > 0 || input$prctlsValues != "" || prctls.not.numbers() == FALSE || prctls.out.of.bounds() == FALSE || all(unlist(file.prctls$vars.classes[prctlsAllVars$prctlsSelectedBckgVars[ , Variables]]) != "numeric")) {
          show("prctlsShortcut")
          show("prctlsGraphs")
          show("prctlsChooseOutFile")
          show("prctlsOpenOutput")
          show("prctlsSyntaxHead")
          show("prctlsSyntax")
          show("prctlsExecBtnHead")
          show("execPrctls")
          show("consolePrctls")
        }
        if(is.null(file.prctls$default.weight) || length(file.prctls$default.weight) == 0) {
          hide("prctlsVariablesExplText")
          hide("prctlsAllAvailableVars")
          hide("prctlsArrowSelSplitVarsRight")
          hide("prctlsArrowSelSplitVarsLeft")
          hide("prctlsSplitVars")
          hide("prctlsInclMiss")
          hide("prctlsArrowSelBckgVarsRight")
          hide("prctlsArrowSelBckgVarsLeft")
          hide("prctlsBckgVars")
          hide("prctlsArrowSelPVsRight")
          hide("prctlsArrowSelPVsLeft")
          hide("prctlsArrowSelPVsRightDisbld")
          hide("prctlsArrowSelPVsLeftDisbld")
          hide("prctlsPVVarsDisbld")
          hide("prctlsPVVars")
          hide("prctlsArrowSelWeightVarsRight")
          hide("prctlsArrowSelWeightVarsLeft")
          hide("prctlsWeightVar")
        } else if(!is.null(file.prctls$default.weight) || length(file.prctls$default.weight) != 0) {
          show("prctlsVariablesExplText")
          show("prctlsAllAvailableVars")
          show("prctlsArrowSelSplitVarsRight")
          show("prctlsArrowSelSplitVarsLeft")
          show("prctlsSplitVars")
          show("prctlsInclMiss")
          show("prctlsArrowSelBckgVarsRight")
          show("prctlsArrowSelBckgVarsLeft")
          show("prctlsBckgVars")
          show("prctlsArrowSelPVsRight")
          show("prctlsArrowSelPVsLeft")
          show("prctlsArrowSelPVsRightDisbld")
          show("prctlsArrowSelPVsLeftDisbld")
          show("prctlsPVVarsDisbld")
          show("prctlsPVVars")
          show("prctlsArrowSelWeightVarsRight")
          show("prctlsArrowSelWeightVarsLeft")
          show("prctlsWeightVar")
        }
      })
    }
  })
  observeEvent(input$execPrctls, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consolePrctls", "")
      tryCatch({
        expr = eval(parse(text = file.prctls$prctls.syntax))
        showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
      }, error = function(e) {
        message("", e)
        showNotification(ui = HTML(paste0("Something went wrong. Possible reasons: <br/>", paste(gsub(pattern = "\\n|Error: ", replacement = "<br/>", x = e)))), type = "error", duration = NULL)
      })
    },
    message = function(i) {
      shinyjs::html(id = "consolePrctls", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consolePrctls", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
  }, ignoreInit = TRUE)
  output$h1Bench <- renderText("Benchmarks")
  hide("benchChooseOutFile")
  output$benchIntro <- renderText({HTML("Select large-scale assessment .RData file to load.")})
  file.bench <- reactiveValues(loaded = NULL, is.lsa.data = NULL, resp.type = NULL, study = NULL, cycle = NULL, country.ID = NULL, PV.sets = NULL, default.weight = NULL, bench.values = NULL, bench.syntax = NULL)
  shinyFileChoose(input, "benchChooseSrcFile", roots = available.volumes, filetype = list(RData = "RData"))
  observeEvent(eventExpr = input$benchChooseSrcFile, {
    file.bench$loaded <- NULL
    file.bench$study <- NULL
    file.bench$cycle <- NULL
    file.bench$resp.type <- NULL
    file.bench$PV.sets <- NULL
    file.bench$bench.values <- NULL
    if(length(parseFilePaths(available.volumes, input$benchChooseSrcFile)$datapath) > 0 && file.size(parseFilePaths(available.volumes, input$benchChooseSrcFile)$datapath) > 104857600) {
      showNotification(ui = HTML('The size of the selected<br/>file is rather large. Please<br/>wait until the file is loaded.'), type = "message")
    }
    if(length(parseFilePaths(available.volumes, input$benchChooseSrcFile)$datapath) > 0) {
      file.bench$loaded <- get(load(parseFilePaths(available.volumes, input$benchChooseSrcFile)$datapath))
      if("lsa.data" %in% class(file.bench$loaded)) {
        file.bench$is.lsa.data <- TRUE
      } else {
        file.bench$is.lsa.data <- FALSE
      }
      file.bench$study <- attr(x = file.bench$loaded, which = "study")
      file.bench$cycle <- attr(x = file.bench$loaded, which = "cycle")
      file.bench$resp.type <- attr(x = file.bench$loaded, which = "file.type")
      file.bench$loaded <- data.table(Variables = names(file.bench$loaded), Variable_Labels = sapply(X = file.bench$loaded, FUN = function(j) {
        if(is.null(attr(x = j, which = "variable.label"))) {
          return(NA_character_)
        } else {
          attr(x = j, which = "variable.label")
        }
      }),
      order_col = 1:ncol(file.bench$loaded))
      file.bench$PV.sets <- NULL
      tmp.PV.names <- grep(pattern = paste(all.available.PVs, collapse = "|"), x = file.bench$loaded[ , Variables], value = TRUE)
      if(length(tmp.PV.names) > 0) {
        collapsed.PVs <- collapse.loaded.file.PV.names(PV.vector = tmp.PV.names, vars.object = file.bench$loaded)
        file.bench$loaded <- file.bench$loaded[!Variables %in% tmp.PV.names]
        file.bench$loaded <- rbindlist(l = list(file.bench$loaded, collapsed.PVs))
        setkeyv(x = file.bench$loaded, cols = "order_col")
        file.bench$PV.sets <- collapsed.PVs[ , Variables]
      }
      if(!is.null(file.bench$study)) {
        file.bench$default.weight <- define.default.weight(study = file.bench$study, loaded.names.and.labels = file.bench$loaded, respondent.type = file.bench$resp.type)
      }
      file.bench$country.ID <- NULL
      if("IDCNTRY" %in% file.bench$loaded[ , Variables]) {
        file.bench$country.ID <- "IDCNTRY"
      } else {
        file.bench$country.ID <- "CNT"
      }
    }
    output$benchSrcPathDisplay <- renderText({parseFilePaths(available.volumes, input$benchChooseSrcFile)$datapath})
  }, ignoreInit = TRUE)
  observe({
    if(!is.null(file.bench$loaded) && file.bench$is.lsa.data == FALSE) {
      showNotification(ui = HTML('The data is not of class "lsa.data".<br/>Please check the file content.'), type = "error")
    } else if (!is.null(file.bench$loaded) && file.bench$is.lsa.data == TRUE) {
      output$benchStudyName <- renderText({
        if(is.null(file.bench$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Study: ', file.bench$study))
        }
      })
      output$benchStudyCycle <- renderText({
        if(is.null(file.bench$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Cycle: ', file.bench$cycle))
        }
      })
      output$benchRespHead <- renderText({
        if(is.null(file.bench$resp.type)) {
          return(NULL)
        } else {
          HTML('<u>The file contains data from the following respondents:</u>')
        }
      })
      output$benchRespAvailable <- renderText({
        if(is.null(file.bench$resp.type)) {
          return(NULL)
        } else {
          HTML(file.merged.respondents[[file.bench$resp.type]])
        }
      })
      output$benchNoWeights <- renderText({
        if(!is.null(file.bench$loaded) && is.null(file.bench$default.weight) || length(file.bench$default.weight) == 0) {
          HTML('Error: The loaded file does not contain any recognizable default weight variable. Such files cannot be analyzed on their own and must be merged with other respondents in advance.')
        } else {
          return(NULL)
        }
      })
      output$noPVsInFile <- renderText({
        if(!is.null(file.bench$loaded) && is.null(file.bench$PV.sets)) {
          HTML('Error: The loaded file does not contain any recognizable set of plausible values, so no percentage of respondents at or above certain benchmarks can be computed. Please check the respondent types available in the loaded file from above.')
        } else {
          return(NULL)
        }
      })
      output$benchType <- renderUI({
        if(is.null(file.bench$PV.sets) || is.null(file.bench$resp.type)) {
          return(NULL)
        } else {
          radioButtons(inputId = "benchType", label = "Select benchmark type", choices = c("Discrete", "Cumulative"), width = "200px", selected = "Discrete")
        }
      })
      output$benchTypeExpl <- renderText({
        if(is.null(file.bench$PV.sets) && is.null(file.bench$resp.type)) {
          return(NULL)
        } else if(!is.null(file.bench$PV.sets) && !is.null(file.bench$resp.type) && !is.null(input$benchType) && input$benchType == "Discrete") {
          HTML('<br/>Computes the percentages of respondents (population estimate) within the boundaries specified by the benchmark values.<br/>Note: If background analysis variable is added, its average for each group will be computed as well.')
        } else if(!is.null(file.bench$PV.sets) && !is.null(file.bench$resp.type) && !is.null(input$benchType) && input$benchType == "Cumulative") {
          HTML('<br/>Computes the percentages of respondents (population estimate) at or above the benchmark values.<br/>Note: With this benchmarks type no analysis variable can be selected.')
        }
      })
      output$benchVariablesExplText <- renderText({
        if(is.null(file.bench$PV.sets) || is.null(file.bench$resp.type)) {
          return(NULL)
        } else {
          HTML('Use the panels below to select the variables to compute percentages of respondents (population estimate) reaching or surpassing specified benchmarks within groups specified by splitting variables.')
        }
      })
      bench.initial.available.vars <- file.bench$loaded[!Variables %in% c(file.bench$default.weight, file.bench$country.ID), ]
      bench.initial.selected.split.vars <- file.bench$loaded[Variables == file.bench$country.ID, ]
      bench.initial.selected.bckg.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      bench.initial.selected.PV.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      bench.initial.selected.weight.var <- file.bench$loaded[Variables %in% file.bench$default.weight, ]
      benchAllVars <- reactiveValues(benchAvailVars = bench.initial.available.vars, benchSelectedSplitVars = bench.initial.selected.split.vars, benchSelectedBckgVars = bench.initial.selected.bckg.vars, benchSelectedPVVars = bench.initial.selected.PV.vars, benchSelectedWeightVar = bench.initial.selected.weight.var)
      output$benchArrowSelSplitVarsRight <- renderUI({
        if(is.null(file.bench$PV.sets) || is.null(file.bench$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "benchArrowSelSplitVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$benchArrowSelSplitVarsLeft <- renderUI({
        if(is.null(file.bench$PV.sets)|| is.null(file.bench$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "benchArrowSelSplitVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$benchArrowSelBckgVarsRight <- renderUI({
        if(is.null(file.bench$PV.sets) || is.null(file.bench$resp.type) || is.null(input$benchType) || input$benchType == "Cumulative") {
          return(NULL)
        } else if(!is.null(file.bench$PV.sets) && !is.null(file.bench$resp.type) && input$benchType == "Discrete") {
          actionButton(inputId = "benchArrowSelBckgVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$benchArrowSelBckgVarsLeft <- renderUI({
        if(is.null(file.bench$PV.sets) || is.null(file.bench$resp.type) || is.null(input$benchType) || input$benchType == "Cumulative") {
          return(NULL)
        } else if(!is.null(file.bench$PV.sets) && !is.null(file.bench$resp.type) && input$benchType == "Discrete") {
          actionButton(inputId = "benchArrowSelBckgVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$benchArrowSelBckgVarsRightDisbld <- renderUI({
        if(is.null(file.bench$resp.type) || is.null(input$benchType) || input$benchType == "Discrete") {
          return(NULL)
        } else if(!is.null(file.bench$resp.type) && input$benchType == "Cumulative") {
          actionButton(inputId = "benchArrowSelBckgVarsRightDisbld", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        }
      })
      output$benchArrowSelBckgVarsLeftDisbld <- renderUI({
        if(is.null(file.bench$resp.type) || is.null(input$benchType) || input$benchType == "Discrete") {
          return(NULL)
        } else if(!is.null(file.bench$resp.type) & !is.null(input$benchType) & input$benchType == "Cumulative") {
          actionButton(inputId = "benchArrowSelBckgVarsLeftDisbld", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        }
      })
      output$benchArrowSelPVsRight <- renderUI({
        if(is.null(file.bench$resp.type) || is.null(file.bench$PV.sets)) {
          return(NULL)
        } else {
          actionButton(inputId = "benchArrowSelPVsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$benchArrowSelPVsLeft <- renderUI({
        if(is.null(file.bench$resp.type) || is.null(file.bench$PV.sets)) {
          return(NULL)
        } else {
          actionButton(inputId = "benchArrowSelPVsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$benchArrowSelWeightVarsRight <- renderUI({
        if(is.null(file.bench$PV.sets) || is.null(file.bench$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "benchArrowSelWeightVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$benchArrowSelWeightVarsLeft <- renderUI({
        if(is.null(file.bench$PV.sets)|| is.null(file.bench$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "benchArrowSelWeightVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      observeEvent(input$benchArrowSelSplitVarsRight, {
        req(input$benchAllAvailableVars_rows_selected)
        benchAllVars$benchSelectedSplitVars <- rbind(isolate(benchAllVars$benchSelectedSplitVars), benchAllVars$benchAvailVars[input$benchAllAvailableVars_rows_selected, , drop = FALSE])
        benchAllVars$benchSelectedSplitVars <- benchAllVars$benchSelectedSplitVars[complete.cases(benchAllVars$benchSelectedSplitVars[ , "Variables"]), , drop = FALSE]
        benchAllVars$benchAvailVars <- isolate(benchAllVars$benchAvailVars[-input$benchAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$benchArrowSelSplitVarsLeft, {
        req(input$benchSplitVars_rows_selected)
        benchAllVars$benchAvailVars <- rbind(isolate(benchAllVars$benchAvailVars),        benchAllVars$benchSelectedSplitVars[input$benchSplitVars_rows_selected, , drop = FALSE])
        benchAllVars$benchAvailVars <- benchAllVars$benchAvailVars[complete.cases(benchAllVars$benchAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(benchAllVars$benchSelectedSplitVars) > 0) {
          benchAllVars$benchSelectedSplitVars <- isolate(benchAllVars$benchSelectedSplitVars[-input$benchSplitVars_rows_selected, , drop = FALSE])
        }
        if(!file.bench$country.ID %in% benchAllVars$benchSelectedSplitVars[ , Variables]) {
          showNotification(ui = HTML("Country ID <b>must always be</b> the first splitting variable!"), type = "error")
        }
        benchAllVars$benchSelectedSplitVars <- rbindlist(l = list(benchAllVars$benchSelectedSplitVars, benchAllVars$benchAvailVars[Variables == file.bench$country.ID, ]))
        benchAllVars$benchAvailVars <- benchAllVars$benchAvailVars[Variables != file.bench$country.ID, ]
      })
      observeEvent(input$benchArrowSelBckgVarsRight, {
        req(input$benchAllAvailableVars_rows_selected)
        benchAllVars$benchSelectedBckgVars <- rbind(isolate(benchAllVars$benchSelectedBckgVars), benchAllVars$benchAvailVars[input$benchAllAvailableVars_rows_selected, , drop = FALSE])
        benchAllVars$benchSelectedBckgVars <- benchAllVars$benchSelectedBckgVars[complete.cases(benchAllVars$benchSelectedBckgVars[ , "Variables"]), , drop = FALSE]
        benchAllVars$benchAvailVars <- isolate(benchAllVars$benchAvailVars[-input$benchAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$benchArrowSelBckgVarsLeft, {
        req(input$benchBckgVars_rows_selected)
        benchAllVars$benchAvailVars <- rbind(isolate(benchAllVars$benchAvailVars),        benchAllVars$benchSelectedBckgVars[input$benchBckgVars_rows_selected, , drop = FALSE])
        benchAllVars$benchAvailVars <- benchAllVars$benchAvailVars[complete.cases(benchAllVars$benchAvailVars[ , "Variables"]), , drop = FALSE]
        benchAllVars$benchSelectedBckgVars <- isolate(benchAllVars$benchSelectedBckgVars[-input$benchBckgVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$benchArrowSelPVsRight, {
        req(input$benchAllAvailableVars_rows_selected)
        benchAllVars$benchSelectedPVVars <- rbind(isolate(benchAllVars$benchSelectedPVVars), benchAllVars$benchAvailVars[input$benchAllAvailableVars_rows_selected, , drop = FALSE])
        benchAllVars$benchSelectedPVVars <- benchAllVars$benchSelectedPVVars[complete.cases(benchAllVars$benchSelectedPVVars[ , "Variables"]), , drop = FALSE]
        if(nrow(benchAllVars$benchSelectedPVVars) > 0) {
          benchAllVars$benchAvailVars <- isolate(benchAllVars$benchAvailVars[-input$benchAllAvailableVars_rows_selected, , drop = FALSE])
        }
      })
      observeEvent(input$benchArrowSelPVsLeft, {
        req(input$benchPVVars_rows_selected)
        benchAllVars$benchAvailVars <- rbind(isolate(benchAllVars$benchAvailVars),        benchAllVars$benchSelectedPVVars[input$benchPVVars_rows_selected, , drop = FALSE])
        benchAllVars$benchAvailVars <- benchAllVars$benchAvailVars[complete.cases(benchAllVars$benchAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(benchAllVars$benchSelectedPVVars) > 0) {
          benchAllVars$benchSelectedPVVars <- isolate(benchAllVars$benchSelectedPVVars[-input$benchPVVars_rows_selected, , drop = FALSE])
        }
      })
      observeEvent(input$benchArrowSelWeightVarsRight, {
        req(input$benchAllAvailableVars_rows_selected)
        benchAllVars$benchSelectedWeightVar <- rbind(isolate(benchAllVars$benchSelectedWeightVar), benchAllVars$benchAvailVars[input$benchAllAvailableVars_rows_selected, , drop = FALSE])
        benchAllVars$benchSelectedWeightVar <- benchAllVars$benchSelectedWeightVar[complete.cases(benchAllVars$benchSelectedWeightVar[ , "Variables"]), , drop = FALSE]
        benchAllVars$benchAvailVars <- isolate(benchAllVars$benchAvailVars[-input$benchAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$benchArrowSelWeightVarsLeft, {
        req(input$benchWeightVar_rows_selected)
        benchAllVars$benchAvailVars <- rbind(isolate(benchAllVars$benchAvailVars),        benchAllVars$benchSelectedWeightVar[input$benchWeightVar_rows_selected, , drop = FALSE])
        benchAllVars$benchAvailVars <- benchAllVars$benchAvailVars[complete.cases(benchAllVars$benchAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(benchAllVars$benchSelectedWeightVar) > 0) {
          benchAllVars$benchSelectedWeightVar <- isolate(benchAllVars$benchSelectedWeightVar[-input$benchWeightVar_rows_selected, , drop = FALSE])
        }
      })
      output$benchAllAvailableVars <- renderDT({
        if(!is.null(file.bench$PV.sets)) {
          setkeyv(x = benchAllVars$benchAvailVars, cols = "order_col")
        } else {
          return(NULL)
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available variables"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables available"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 766, scroller = TRUE
      ))
      output$benchSplitVars <- renderDT({
        if(!is.null(file.bench$PV.sets)) {
          benchAllVars$benchSelectedSplitVars
        } else {
          return(NULL)
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Split variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$benchInclMiss <- renderUI({
        if(is.null(file.bench$PV.sets) || nrow(benchAllVars$benchSelectedSplitVars) == 0) {
          return(NULL)
        } else {
          checkboxInput(inputId = "benchInclMiss", label = "Compute statistics for the missing values of the split variables", value = FALSE, width = "400px")
        }
      })
      output$benchBckgVars <- renderDT({
        if(!is.null(file.bench$PV.sets) && !is.null(input$benchType) && input$benchType == "Discrete") {
          benchAllVars$benchSelectedBckgVars
        } else {
          return(NULL)
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Analysis (background continuous) variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$benchBckgVarsDisbld <- renderDT({
        if(!is.null(input$benchType) && input$benchType == "Cumulative") {
          data.table(V1 = as.character(), V2 = as.character())
        } else {
          return(NULL)
        }
      },
      select = "none",
      caption = htmltools::tags$caption(style = "color: #c6c6c6; font-weight: bold;", "Analysis (background continuous) variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels"),
      options = list(
        language = list(zeroRecords = "No variables can be selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#c6c6c6', 'color': '#ffffff'});", "}"),
        dom = "ti",
        ordering = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0)),
        headerCallback = JS("function(thead, data, start, end, display){$('th', thead).css('border-bottom', 'none');}"),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$benchPVVars <- renderDT({
        if(is.null(file.bench$PV.sets)) {
          return(NULL)
        } else {
          benchAllVars$benchSelectedPVVars
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$benchWeightVar <- renderDT({
        if(!is.null(file.bench$PV.sets)) {
          benchAllVars$benchSelectedWeightVar
        } else {
          return(NULL)
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Weight variable"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = paste0("No weight selected, default (", file.bench$default.weight, ") will be used")),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$benchPVsNotPVs <- renderText({
        if(!is.null(benchAllVars$benchSelectedPVVars) && any(benchAllVars$benchSelectedPVVars[ , Variables] %in% file.bench$PV.sets == FALSE)) {
          HTML('Warning: One or more of the selected variables in "Plausible values" is not a set of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$benchSplitArePVs <- renderText({
        if(any(benchAllVars$benchSelectedSplitVars[ , Variables] %in% file.bench$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Split variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$benchBckgArePVs <- renderText({
        if(!is.null(benchAllVars$benchSelectedBckgVars) && any(benchAllVars$benchSelectedBckgVars[ , Variables] %in% file.bench$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Analysis (background continuous) variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$benchWgtsNotWgts <- renderText({
        if(any(benchAllVars$benchSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE)) {
          HTML('Warning: The variable in "Weight variable" is not a weight. Please check the added variable.')
        } else {
          return(NULL)
        }
      })
      observe({
        if(!is.null(benchAllVars$benchSelectedBckgVars) && nrow(benchAllVars$benchSelectedBckgVars) > 1) {
          showNotification(ui = HTML("Only one background variable can be selected!"), type = "error")
          benchAllVars$benchAvailVars <- rbind(isolate(benchAllVars$benchAvailVars), benchAllVars$benchSelectedBckgVars[nrow(benchAllVars$benchSelectedBckgVars), , drop = FALSE])
          benchAllVars$benchAvailVars <- benchAllVars$benchAvailVars[complete.cases(benchAllVars$benchAvailVars[ , "Variables"]), , drop = FALSE]
          benchAllVars$benchSelectedBckgVars <- isolate(benchAllVars$benchSelectedBckgVars[-2, , drop = FALSE])
        }
      })
      observe({
        if(!is.null(benchAllVars$benchSelectedPVVars) && nrow(benchAllVars$benchSelectedPVVars) > 1) {
          showNotification(ui = HTML("Only one set of PVs can be selected!"), type = "error")
          benchAllVars$benchAvailVars <- rbind(isolate(benchAllVars$benchAvailVars), benchAllVars$benchSelectedPVVars[nrow(benchAllVars$benchSelectedPVVars), , drop = FALSE])
          benchAllVars$benchAvailVars <- benchAllVars$benchAvailVars[complete.cases(benchAllVars$benchAvailVars[ , "Variables"]), , drop = FALSE]
          benchAllVars$benchSelectedPVVars <- isolate(benchAllVars$benchSelectedPVVars[-2, , drop = FALSE])
        }
      })
      observe({
        if(nrow(benchAllVars$benchSelectedWeightVar) > 1) {
          showNotification(ui = HTML("Only one weight variable can be selected!"), type = "error")
          benchAllVars$benchAvailVars <- rbind(isolate(benchAllVars$benchAvailVars), benchAllVars$benchSelectedWeightVar[nrow(benchAllVars$benchSelectedWeightVar), , drop = FALSE])
          benchAllVars$benchAvailVars <- benchAllVars$benchAvailVars[complete.cases(benchAllVars$benchAvailVars[ , "Variables"]), , drop = FALSE]
          benchAllVars$benchSelectedWeightVar <- isolate(benchAllVars$benchSelectedWeightVar[-2, , drop = FALSE])
        }
      })
      observeEvent(eventExpr = input$benchChooseSrcFile, {
        benchAllVars$benchSelectedPVVars <- NULL
        benchAllVars$benchSelectedBckgVars <- NULL
      }, ignoreInit = TRUE)
      observe({
        if(!is.null(benchAllVars$benchSelectedPVVars) && nrow(benchAllVars$benchSelectedPVVars) > 0) {
          if(intersect(file.bench$study, names(default.benchmarks)) == "ICCS") {
            tmp.benchmarks <- default.benchmarks[["ICCS"]]
            bench.vals <- tmp.benchmarks[[intersect(file.bench$cycle, names(tmp.benchmarks))]]
          } else if(intersect(file.bench$study, names(default.benchmarks)) == "ICILS") {
            bench.vals <- default.benchmarks[["ICILS"]]
          } else if(intersect(file.bench$study, names(default.benchmarks)) == "PIRLS") {
            bench.vals <- default.benchmarks[["PIRLS"]]
          } else if(intersect(file.bench$study, names(default.benchmarks)) == "prePIRLS") {
            bench.vals <- default.benchmarks[["prePIRLS"]]
          } else if(intersect(file.bench$study, names(default.benchmarks)) == "ePIRLS") {
            bench.vals <- default.benchmarks[["ePIRLS"]]
          } else if(intersect(file.bench$study, names(default.benchmarks)) == "TIMSS") {
            bench.vals <- default.benchmarks[["TIMSS"]]
          } else if(intersect(file.bench$study, names(default.benchmarks)) == "preTIMSS") {
            bench.vals <- default.benchmarks[["preTIMSS"]]
          } else if(intersect(file.bench$study, names(default.benchmarks)) == "eTIMSS PSI") {
            bench.vals <- default.benchmarks[["eTIMSS PSI"]]
          } else if(intersect(file.bench$study, names(default.benchmarks)) == "TIMSS Advanced") {
            bench.vals <- default.benchmarks[["TIMSS Advanced"]]
          } else if(intersect(file.bench$study, names(default.benchmarks)) == "TiPi") {
            bench.vals <- default.benchmarks[["TiPi"]]
          } else if(intersect(file.bench$study, names(default.benchmarks)) == "PISA") {
            tmp.PV.root.name <- benchAllVars$benchSelectedPVVars[ , Variables]
            tmp.benchmarks <- default.benchmarks[["PISA"]]
            tmp.benchmarks.PVs <- grep(pattern = "root.PVs$", x = names(tmp.benchmarks), value = TRUE)
            tmp.benchmarks.PVs <- names(unlist(sapply(X = tmp.benchmarks.PVs, FUN = function(i) {
              intersect(tmp.benchmarks[[i]], tmp.PV.root.name)
            })))
            tmp.benchmarks.PVs <- gsub(pattern = ".root.PVs", replacement = "", x = tmp.benchmarks.PVs)
            tmp.benchmarks <- default.benchmarks[["PISA"]][[tmp.benchmarks.PVs]]
            bench.vals <- tmp.benchmarks[[as.character(file.bench$cycle)]]
          } else if(intersect(file.bench$study, names(default.benchmarks)) == "PISA for Development") {
            tmp.PV.root.name <- benchAllVars$benchSelectedPVVars[ , Variables]
            tmp.benchmarks <- default.benchmarks[["PISA for Development"]]
            tmp.benchmarks.PVs <- grep(pattern = "root.PVs$", x = names(tmp.benchmarks), value = TRUE)
            tmp.benchmarks.PVs <- names(unlist(sapply(X = tmp.benchmarks.PVs, FUN = function(i) {
              intersect(tmp.benchmarks[[i]], tmp.PV.root.name)
            })))
            tmp.benchmarks.PVs <- gsub(pattern = ".root.PVs", replacement = "", x = tmp.benchmarks.PVs)
            tmp.benchmarks <- default.benchmarks[["PISA for Development"]][[tmp.benchmarks.PVs]]
            bench.vals <- tmp.benchmarks[[as.character(file.bench$cycle)]]
          }
          file.bench$bench.values <- bench.vals
        }
      })
      output$benchValuesExpl <- renderText({
        if(nrow(benchAllVars$benchAvailVars) == 0) {
          return(NULL)
        } else {
          HTML('In the field below, add/change the benchmark cut-points for which percentahes of respondents (population estimate), reaching or surpassing, will be calculated for the selected PV set.<br/>The values can be whole numbers or decimal numbers (use period as decimal separator), divided by spaces.')
        }
      })
      output$benchValues <- renderUI({
        if(nrow(benchAllVars$benchAvailVars) == 0) {
          return(NULL)
        } else {
          textInput(inputId = "benchValues", label = "Achievement benchmarks", value = paste(file.bench$bench.values, collapse = " "), width = "350px")
        }
      })
      output$benchValuesReset <- renderUI({
        if(nrow(benchAllVars$benchAvailVars) == 0) {
          return(NULL)
        } else {
          actionButton(inputId = "benchValuesReset", label = "Reset", icon = icon("undo-alt"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      bench.not.numbers <- reactiveVal()
      observe({
        if(!is.null(input$benchValues) && grepl(pattern = "[[:alpha:]]|(?=[^.[:^punct:]])", x = input$benchValues, perl = TRUE) == TRUE) {
          bench.not.numbers(TRUE)
        } else if(!is.null(input$benchValues) && grepl(pattern = "[[:alpha:]]|(?=[^.[:^punct:]])", x = input$benchValues, perl = TRUE) == FALSE) {
          bench.not.numbers(FALSE)
        }
      })
      output$benchNotNum <- renderText({
        if(!is.null(input$benchValues) && bench.not.numbers() == TRUE) {
          HTML('<br/>Warning: The values passed to the field on the left can contain <u>only</u> numbers and spaces. Please check the input.')
        } else {
          return(NULL)
        }
      })
      bench.out.of.bounds <- reactiveVal()
      observe({
        suppressWarnings(
          if(!is.null(input$benchValues) && any(na.omit(as.numeric(unlist(str_split(string = input$benchValues, pattern = "[[:space:]]+")))) < 1)) {
            bench.out.of.bounds(TRUE)
          } else {
            bench.out.of.bounds(FALSE)
          }
        )
      })
      output$benchNotInRange <- renderText({
        if(!is.null(input$benchValues) && bench.out.of.bounds() == TRUE) {
          HTML('<br/>Warning: The values passed to the field on the left <u>must</u> be greater than 1. Please check the input.')
        } else {
          return(NULL)
        }
      })
      observeEvent(input$benchValuesReset, {
        shinyjs::reset("benchValues")
      })
      output$benchComputeWithin <- renderUI({
        if(is.null(file.bench$resp.type)) {
          return(NULL)
        } else {
          checkboxInput(inputId = "benchComputeWithin", label = "Compute percentages within benchmarks", width = "350px", value = FALSE)
        }
      })
      output$benchComputeWithinExpl <- renderText({
        if(is.null(file.bench$resp.type)) {
          return(NULL)
        } else if(!is.null(file.bench$resp.type) && !is.null(input$benchType) && input$benchComputeWithin == FALSE) {
          HTML('Compute the percentages of respondents reaching or surpassing each of the cut-off scores defined by the benchmark values.')
        } else if(!is.null(file.bench$resp.type) && !is.null(input$benchType) && input$benchComputeWithin == TRUE) {
          HTML('Compute the percentages of respondents with given characteristic at each of the performance levels.')
        }
      })
      output$benchShortcut <- renderUI({
        if(!is.null(file.bench$loaded) && file.bench$study %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "eTIMSS PSI", "preTIMSS", "TIMSS Advanced", "TiPi")) {
          checkboxInput(inputId = "benchShortcut", label = "Use shortcut method for computing SE", value = FALSE, width = "350px")
        }
      })
      output$benchGraphs <- renderUI({
        if(!is.null(file.bench$loaded)) {
          checkboxInput(inputId = "benchGraphs", label = "Produce graphs", value = FALSE, width = "350px")
        }
      })
      shinyFileSave(input, "benchChooseOutFile", filetype = list(xlsx = "xlsx"), roots = available.volumes, updateFreq = 100000)
      output$benchOpenOutput <- renderUI({
        if(length(parseSavePath(available.volumes, input$benchChooseOutFile)$datapath) > 0) {
          checkboxInput(inputId = "benchOpenOutput", label = "Open the output when done", value = TRUE, width = "250px")
        }
      })
      syntaxBench <- reactive({
        tmp.bench.values <- unlist(str_split(string = trimws(input$benchValues), pattern = "[[:space:]]+"))
        file.bench$bench.syntax <- paste0(
          paste0('lsa.bench(data.file = "', parseFilePaths(available.volumes, input$benchChooseSrcFile)$datapath, '"'),
          if(nrow(benchAllVars$benchSelectedSplitVars) == 1) {
            paste0(', split.vars = "', benchAllVars$benchSelectedSplitVars[ , Variables], '"')
          } else if(nrow(benchAllVars$benchSelectedSplitVars) > 1) {
            paste0(', split.vars = c("', paste(benchAllVars$benchSelectedSplitVars[ , Variables], collapse = '", "'), '")')
          },
          if(nrow(benchAllVars$benchSelectedPVVars) == 0) {
            NULL
          } else if(nrow(benchAllVars$benchSelectedPVVars) == 1) {
            paste0(', PV.root.bench = "', benchAllVars$benchSelectedPVVars[ , Variables], '"')
          } else if(nrow(benchAllVars$benchSelectedPVVars) > 1) {
            paste0(', PV.root.bench = c("', paste(benchAllVars$benchSelectedPVVars[ , Variables], collapse = '", "'), '")')
          },
          if(length(tmp.bench.values) == 1) {
            paste0(', bench.vals = ', gsub(pattern = "[[:space:]]+", replacement = "", x = tmp.bench.values))
          } else if(length(tmp.bench.values) > 1) {
            paste0(', bench.vals = c(', paste(tmp.bench.values, collapse = ', '), ')')
          } else {
            NULL
          },
          if(input$benchType == "Discrete") {
            NULL
          } else if(input$benchType == "Cumulative") {
            paste0(', bench.type = "cumulative"')
          },
          if(input$benchComputeWithin == FALSE) {
            NULL
          } else if(input$benchComputeWithin == TRUE) {
            ' , pcts.within = TRUE'
          },
          if(nrow(benchAllVars$benchSelectedBckgVars) == 0) {
            NULL
          } else if(nrow(benchAllVars$benchSelectedBckgVars) == 1) {
            paste0(', bckg.var = "', benchAllVars$benchSelectedBckgVars[ , Variables], '"')
          } else if(nrow(benchAllVars$benchSelectedBckgVars) > 1) {
            paste0(', bckg.var = c("', paste(benchAllVars$benchSelectedBckgVars[ , Variables], collapse = '", "'), '")')
          },
          if(nrow(benchAllVars$benchSelectedWeightVar) == 0) {
            NULL
          } else if(nrow(benchAllVars$benchSelectedWeightVar) == 1 && benchAllVars$benchSelectedWeightVar[ , Variables] == file.bench$default.weight) {
            NULL
          } else if(nrow(benchAllVars$benchSelectedWeightVar) == 1 && benchAllVars$benchSelectedWeightVar[ , Variables] != file.bench$default.weight) {
            paste0(', weight.var = "', benchAllVars$benchSelectedWeightVar[ , Variables], '"')
          },
          if(is.null(input$benchInclMiss) || input$benchInclMiss == FALSE) {
            NULL
          } else if(!is.null(input$benchInclMiss) || input$benchInclMiss == TRUE) {
            ', include.missing = TRUE'
          },
          if(is.null(input$benchShortcut) || input$benchShortcut == FALSE) {
            NULL
          } else if(!is.null(input$benchShortcut) || input$benchShortcut == TRUE) {
            ', shortcut = TRUE'
          },
          if(is.null(input$benchGraphs) || input$benchGraphs == FALSE) {
            NULL
          } else if(!is.null(input$benchGraphs) || input$benchGraphs == TRUE) {
            ', graphs = TRUE'
          },
          paste0(', output.file = "', parseSavePath(available.volumes, input$benchChooseOutFile)$datapath, '"'),
          if(is.null(input$benchOpenOutput) || input$benchOpenOutput == FALSE) {
            NULL
          } else if(!is.null(input$benchOpenOutput) || input$benchOpenOutput == TRUE) {
            ', open.output = TRUE'
          },
          ')'
        )
      })
      output$benchSyntaxHead <- renderText({
        if(length(parseSavePath(available.volumes, input$benchChooseOutFile)$datapath) == 1) {
          HTML("Syntax")
        } else {
          return(NULL)
        }
      })
      output$benchSyntax <- renderText({
        if(length(parseSavePath(available.volumes, input$benchChooseOutFile)$datapath) == 1 && nrow(rbindlist(l = list(benchAllVars$benchSelectedPVVars, benchAllVars$benchSelectedBckgVars))) >= 1) {
          syntaxBench()
        } else {
          return(NULL)
        }
      })
      output$benchExecBtnHead <- renderText({
        if(length(parseSavePath(available.volumes, input$benchChooseOutFile)$datapath) == 1) {
          HTML("Press the button below to execute the syntax")
        } else {
          return(NULL)
        }
      })
      output$execBench <- renderUI({
        if(length(parseSavePath(available.volumes, input$benchChooseOutFile)$datapath) == 1) {
          actionButton(inputId = "execBench", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      observe({
        if(is.null(file.bench$loaded) || !is.null(benchAllVars$benchSelectedPVVars) && !is.null(benchAllVars$benchSelectedBckgVars)) {
          if(nrow(rbindlist(l = list(benchAllVars$benchSelectedBckgVars, benchAllVars$benchSelectedPVVars))) == 0 || nrow(benchAllVars$benchSelectedPVVars)  == 0 || any(benchAllVars$benchSelectedPVVars[ , Variables] %in% file.bench$PV.sets == FALSE) || any(benchAllVars$benchSelectedSplitVars[ , Variables] %in% file.bench$PV.sets == TRUE) || any(benchAllVars$benchSelectedBckgVars[ , Variables] %in% file.bench$PV.sets == TRUE) || any(benchAllVars$benchSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE)) {
            hide("benchValuesExpl")
            hide("benchValues")
            hide("benchValuesReset")
            hide("benchWarnMoreVars")
          } else if(!is.null(file.bench$loaded) && nrow(rbindlist(l = list(benchAllVars$benchSelectedBckgVars, benchAllVars$benchSelectedPVVars))) > 0 || nrow(benchAllVars$benchSelectedPVVars) > 0 || any(benchAllVars$benchSelectedPVVars[ , Variables] %in% file.bench$PV.sets == TRUE) || any(benchAllVars$benchSelectedSplitVars[ , Variables] %in% file.bench$PV.sets == FALSE) || any(benchAllVars$benchSelectedBckgVars[ , Variables] %in% file.bench$PV.sets == FALSE) || any(benchAllVars$benchSelectedWeightVar[ , Variables] %in% all.studies.available.weights == TRUE)) {
            show("benchValuesExpl")
            show("benchValues")
            show("benchValuesReset")
            show("benchWarnMoreVars")
          }
          if(is.null(file.bench$loaded) || nrow(benchAllVars$benchSelectedPVVars) == 0 || any(benchAllVars$benchSelectedPVVars[ , Variables] %in% file.bench$PV.sets == FALSE) || any(benchAllVars$benchSelectedSplitVars[ , Variables] %in% file.bench$PV.sets == TRUE) || any(benchAllVars$benchSelectedBckgVars[ , Variables] %in% file.bench$PV.sets == TRUE) || any(benchAllVars$benchSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE) || nrow(rbindlist(l = list(benchAllVars$benchSelectedBckgVars, benchAllVars$benchSelectedPVVars))) == 0 || input$benchValues == "" || bench.not.numbers() == TRUE || bench.out.of.bounds() == TRUE) {
            hide("benchComputeWithin")
            hide("benchComputeWithinExpl")
            hide("benchShortcut")
            hide("benchGraphs")
            hide("benchChooseOutFile")
            hide("benchOpenOutput")
            hide("benchSyntaxHead")
            hide("benchSyntax")
            hide("benchExecBtnHead")
            hide("execBench")
          } else if (!is.null(file.bench$loaded) || nrow(benchAllVars$benchSelectedPVVars) > 0 || any(benchAllVars$benchSelectedPVVars[ , Variables] %in% file.bench$PV.sets == TRUE) || any(benchAllVars$benchSelectedSplitVars[ , Variables] %in% file.bench$PV.sets == FALSE) || any(benchAllVars$benchSelectedBckgVars[ , Variables] %in% file.bench$PV.sets == FALSE) || any(benchAllVars$benchSelectedWeightVar[ , Variables] %in% all.studies.available.weights == TRUE) || nrow(rbindlist(l = list(benchAllVars$benchSelectedBckgVars, benchAllVars$benchSelectedPVVars))) > 0 || input$benchValues != "" || bench.not.numbers() == FALSE || bench.out.of.bounds() == FALSE) {
            show("benchComputeWithin")
            show("benchComputeWithinExpl")
            show("benchShortcut")
            show("benchGraphs")
            show("benchChooseOutFile")
            show("benchOpenOutput")
            show("benchSyntaxHead")
            show("benchSyntax")
            show("benchExecBtnHead")
            show("execBench")
          }
        }
        if(is.null(file.bench$default.weight) || length(file.bench$default.weight) == 0) {
          hide("benchType")
          hide("benchTypeExpl")
          hide("benchVariablesExplText")
          hide("benchAllAvailableVars")
          hide("benchArrowSelSplitVarsRight")
          hide("benchArrowSelSplitVarsLeft")
          hide("benchSplitVars")
          hide("benchInclMiss")
          hide("benchArrowSelBckgVarsRight")
          hide("benchArrowSelBckgVarsLeft")
          hide("benchBckgVars")
          hide("benchArrowSelPVsRight")
          hide("benchArrowSelPVsLeft")
          hide("benchArrowSelPVsRightDisbld")
          hide("benchArrowSelPVsLeftDisbld")
          hide("benchPVVarsDisbld")
          hide("benchPVVars")
          hide("benchArrowSelWeightVarsRight")
          hide("benchArrowSelWeightVarsLeft")
          hide("benchWeightVar")
        } else if(!is.null(file.bench$default.weight) || length(file.bench$default.weight) != 0) {
          show("benchType")
          show("benchTypeExpl")
          show("benchVariablesExplText")
          show("benchAllAvailableVars")
          show("benchArrowSelSplitVarsRight")
          show("benchArrowSelSplitVarsLeft")
          show("benchSplitVars")
          show("benchInclMiss")
          show("benchArrowSelBckgVarsRight")
          show("benchArrowSelBckgVarsLeft")
          show("benchBckgVars")
          show("benchArrowSelPVsRight")
          show("benchArrowSelPVsLeft")
          show("benchArrowSelPVsRightDisbld")
          show("benchArrowSelPVsLeftDisbld")
          show("benchPVVarsDisbld")
          show("benchPVVars")
          show("benchArrowSelWeightVarsRight")
          show("benchArrowSelWeightVarsLeft")
          show("benchWeightVar")
        }
      })
    }
  })
  observeEvent(input$execBench, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consoleBench", "")
      tryCatch({
        expr = eval(parse(text = file.bench$bench.syntax))
        showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
      }, error = function(e) {
        message("", e)
        showNotification(ui = HTML(paste0("Something went wrong. Possible reasons: <br/>", paste(gsub(pattern = "\\n|Error: ", replacement = "<br/>", x = e)))), type = "error", duration = NULL)
      })
    },
    message = function(i) {
      shinyjs::html(id = "consoleBench", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consoleBench", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
  }, ignoreInit = TRUE)
  output$h1CrossTabs <- renderText("Crosstabulations")
  hide("crossTabsChooseOutFile")
  output$crossTabsIntro <- renderText({HTML("Select large-scale assessment .RData file to load.")})
  file.crosstabs <- reactiveValues(loaded = NULL, is.lsa.data = NULL, resp.type = NULL, study = NULL, cycle = NULL, country.ID = NULL, PV.sets = NULL, default.weight = NULL, crosstabs.syntax = NULL)
  shinyFileChoose(input, "crossTabsChooseSrcFile", roots = available.volumes, filetype = list(RData = "RData"))
  observeEvent(eventExpr = input$crossTabsChooseSrcFile, {
    file.crosstabs$loaded <- NULL
    file.crosstabs$study <- NULL
    file.crosstabs$cycle <- NULL
    file.crosstabs$resp.type <- NULL
    file.crosstabs$PV.sets <- NULL
    file.crosstabs$default.weight <- NULL
    if(length(parseFilePaths(available.volumes, input$crossTabsChooseSrcFile)$datapath) > 0 && file.size(parseFilePaths(available.volumes, input$crossTabsChooseSrcFile)$datapath) > 104857600) {
      showNotification(ui = HTML('The size of the selected<br/>file is rather large. Please<br/>wait until the file is loaded.'), type = "message")
    }
    if(length(parseFilePaths(available.volumes, input$crossTabsChooseSrcFile)$datapath) > 0) {
      file.crosstabs$loaded <- get(load(parseFilePaths(available.volumes, input$crossTabsChooseSrcFile)$datapath))
      if("lsa.data" %in% class(file.crosstabs$loaded)) {
        file.crosstabs$is.lsa.data <- TRUE
      } else {
        file.crosstabs$is.lsa.data <- FALSE
      }
      file.crosstabs$study <- attr(x = file.crosstabs$loaded, which = "study")
      file.crosstabs$cycle <- attr(x = file.crosstabs$loaded, which = "cycle")
      file.crosstabs$resp.type <- attr(x = file.crosstabs$loaded, which = "file.type")
      file.crosstabs$loaded <- data.table(Variables = names(file.crosstabs$loaded), Variable_Labels = sapply(X = file.crosstabs$loaded, FUN = function(j) {
        if(is.null(attr(x = j, which = "variable.label"))) {
          return(NA_character_)
        } else {
          attr(x = j, which = "variable.label")
        }
      }),
      order_col = 1:ncol(file.crosstabs$loaded))
      file.crosstabs$PV.sets <- NULL
      tmp.PV.names <- grep(pattern = paste(all.available.PVs, collapse = "|"), x = file.crosstabs$loaded[ , Variables], value = TRUE)
      if(length(tmp.PV.names) > 0) {
        collapsed.PVs <- collapse.loaded.file.PV.names(PV.vector = tmp.PV.names, vars.object = file.crosstabs$loaded)
        file.crosstabs$loaded <- file.crosstabs$loaded[!Variables %in% tmp.PV.names]
        file.crosstabs$loaded <- rbindlist(l = list(file.crosstabs$loaded, collapsed.PVs))
        setkeyv(x = file.crosstabs$loaded, cols = "order_col")
        file.crosstabs$PV.sets <- collapsed.PVs[ , Variables]
      }
      if(!is.null(file.crosstabs$study)) {
        file.crosstabs$default.weight <- define.default.weight(study = file.crosstabs$study, loaded.names.and.labels = file.crosstabs$loaded, respondent.type = file.crosstabs$resp.type)
      }
      file.crosstabs$country.ID <- NULL
      if("IDCNTRY" %in% file.crosstabs$loaded[ , Variables]) {
        file.crosstabs$country.ID <- "IDCNTRY"
      } else {
        file.crosstabs$country.ID <- "CNT"
      }
    }
    output$crossTabsSrcPathDisplay <- renderText({parseFilePaths(available.volumes, input$crossTabsChooseSrcFile)$datapath})
  }, ignoreInit = TRUE)
  observe({
    if(!is.null(file.crosstabs$loaded) && file.crosstabs$is.lsa.data == FALSE) {
      showNotification(ui = HTML('The data is not of class "lsa.data".<br/>Please check the file content.'), type = "error")
    } else if (!is.null(file.crosstabs$loaded) && file.crosstabs$is.lsa.data == TRUE) {
      output$crossTabsStudyName <- renderText({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Study: ', file.crosstabs$study))
        }
      })
      output$crossTabsStudyCycle <- renderText({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Cycle: ', file.crosstabs$cycle))
        }
      })
      output$crossTabsRespHead <- renderText({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          HTML('<u>The file contains data from the following respondents:</u>')
        }
      })
      output$crossTabsRespAvailable <- renderText({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          HTML(file.merged.respondents[[file.crosstabs$resp.type]])
        }
      })
      output$crossTabsNoWeights <- renderText({
        if(!is.null(file.crosstabs$loaded) && is.null(file.crosstabs$default.weight) || !is.null(file.crosstabs$loaded) && length(file.crosstabs$default.weight) == 0) {
          HTML('Error: The loaded file does not contain any recognizable default weight variable. Such files cannot be analyzed on their own and must be merged with other respondents in advance.')
        } else {
          return(NULL)
        }
      })
      output$crossTabsVariablesExplText <- renderText({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          HTML('Use the panels below to select the variables to compute crosstabulations for.')
        }
      })
      crosstabs.initial.available.vars <- file.crosstabs$loaded[!Variables %in% c(file.crosstabs$default.weight, file.crosstabs$country.ID), ]
      crosstabs.initial.selected.split.vars <- file.crosstabs$loaded[Variables == file.crosstabs$country.ID, ]
      crosstabs.initial.selected.bckg.row.var <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      crosstabs.initial.selected.bckg.col.var <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      crosstabs.initial.selected.PV.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      crosstabs.initial.selected.weight.var <- file.crosstabs$loaded[Variables %in% file.crosstabs$default.weight, ]
      crossTabsAllVars <- reactiveValues(crossTabsAvailVars = crosstabs.initial.available.vars, crossTabsSelectedSplitVars = crosstabs.initial.selected.split.vars, crossTabsSelectedBckgRowVar = crosstabs.initial.selected.bckg.row.var, crossTabsSelectedBckgColVar = crosstabs.initial.selected.bckg.col.var, crossTabsSelectedPVVars = crosstabs.initial.selected.PV.vars, crossTabsSelectedWeightVar = crosstabs.initial.selected.weight.var)
      output$crossTabsArrowSelSplitVarsRight <- renderUI({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "crossTabsArrowSelSplitVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$crossTabsArrowSelSplitVarsLeft <- renderUI({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "crossTabsArrowSelSplitVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$crossTabsArrowSelBckgRowVarRight <- renderUI({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "crossTabsArrowSelBckgRowVarRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$crossTabsArrowSelBckgRowVarLeft <- renderUI({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "crossTabsArrowSelBckgRowVarLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$crossTabsArrowSelBckgColVarRight <- renderUI({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "crossTabsArrowSelBckgColVarRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$crossTabsArrowSelBckgColVarLeft <- renderUI({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "crossTabsArrowSelBckgColVarLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$crossTabsArrowSelWeightVarRight <- renderUI({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "crossTabsArrowSelWeightVarRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$crossTabsArrowSelWeightVarLeft <- renderUI({
        if(is.null(file.crosstabs$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "crossTabsArrowSelWeightVarLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      observeEvent(input$crossTabsArrowSelSplitVarsRight, {
        req(input$crossTabsAllAvailableVars_rows_selected)
        crossTabsAllVars$crossTabsSelectedSplitVars <- rbind(isolate(crossTabsAllVars$crossTabsSelectedSplitVars), crossTabsAllVars$crossTabsAvailVars[input$crossTabsAllAvailableVars_rows_selected, , drop = FALSE])
        crossTabsAllVars$crossTabsSelectedSplitVars <- crossTabsAllVars$crossTabsSelectedSplitVars[complete.cases(crossTabsAllVars$crossTabsSelectedSplitVars[ , "Variables"]), , drop = FALSE]
        crossTabsAllVars$crossTabsAvailVars <- isolate(crossTabsAllVars$crossTabsAvailVars[-input$crossTabsAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$crossTabsArrowSelSplitVarsLeft, {
        req(input$crossTabsSplitVars_rows_selected)
        crossTabsAllVars$crossTabsAvailVars <- rbind(isolate(crossTabsAllVars$crossTabsAvailVars),        crossTabsAllVars$crossTabsSelectedSplitVars[input$crossTabsSplitVars_rows_selected, , drop = FALSE])
        crossTabsAllVars$crossTabsAvailVars <- crossTabsAllVars$crossTabsAvailVars[complete.cases(crossTabsAllVars$crossTabsAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(crossTabsAllVars$crossTabsSelectedSplitVars) > 0) {
          crossTabsAllVars$crossTabsSelectedSplitVars <- isolate(crossTabsAllVars$crossTabsSelectedSplitVars[-input$crossTabsSplitVars_rows_selected, , drop = FALSE])
        }
        if(!file.crosstabs$country.ID %in% crossTabsAllVars$crossTabsSelectedSplitVars[ , Variables]) {
          showNotification(ui = HTML("Country ID <b>must always be</b> the first splitting variable!"), type = "error")
        }
        crossTabsAllVars$crossTabsSelectedSplitVars <- rbindlist(l = list(crossTabsAllVars$crossTabsSelectedSplitVars, crossTabsAllVars$crossTabsAvailVars[Variables == file.crosstabs$country.ID, ]))
        crossTabsAllVars$crossTabsAvailVars <- crossTabsAllVars$crossTabsAvailVars[Variables != file.crosstabs$country.ID, ]
      })
      observeEvent(input$crossTabsArrowSelBckgRowVarRight, {
        req(input$crossTabsAllAvailableVars_rows_selected)
        crossTabsAllVars$crossTabsSelectedBckgRowVar <- rbind(isolate(crossTabsAllVars$crossTabsSelectedBckgRowVar), crossTabsAllVars$crossTabsAvailVars[input$crossTabsAllAvailableVars_rows_selected, , drop = FALSE])
        crossTabsAllVars$crossTabsSelectedBckgRowVar <- crossTabsAllVars$crossTabsSelectedBckgRowVar[complete.cases(crossTabsAllVars$crossTabsSelectedBckgRowVar[ , "Variables"]), , drop = FALSE]
        crossTabsAllVars$crossTabsAvailVars <- isolate(crossTabsAllVars$crossTabsAvailVars[-input$crossTabsAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$crossTabsArrowSelBckgRowVarLeft, {
        req(input$crossTabsBckgRowVar_rows_selected)
        crossTabsAllVars$crossTabsAvailVars <- rbind(isolate(crossTabsAllVars$crossTabsAvailVars),        crossTabsAllVars$crossTabsSelectedBckgRowVar[input$crossTabsBckgRowVar_rows_selected, , drop = FALSE])
        crossTabsAllVars$crossTabsAvailVars <- crossTabsAllVars$crossTabsAvailVars[complete.cases(crossTabsAllVars$crossTabsAvailVars[ , "Variables"]), , drop = FALSE]
        crossTabsAllVars$crossTabsSelectedBckgRowVar <- isolate(crossTabsAllVars$crossTabsSelectedBckgRowVar[-input$crossTabsBckgRowVar_rows_selected, , drop = FALSE])
      })
      observeEvent(input$crossTabsArrowSelBckgColVarRight, {
        req(input$crossTabsAllAvailableVars_rows_selected)
        crossTabsAllVars$crossTabsSelectedBckgColVar <- rbind(isolate(crossTabsAllVars$crossTabsSelectedBckgColVar), crossTabsAllVars$crossTabsAvailVars[input$crossTabsAllAvailableVars_rows_selected, , drop = FALSE])
        crossTabsAllVars$crossTabsSelectedBckgColVar <- crossTabsAllVars$crossTabsSelectedBckgColVar[complete.cases(crossTabsAllVars$crossTabsSelectedBckgColVar[ , "Variables"]), , drop = FALSE]
        crossTabsAllVars$crossTabsAvailVars <- isolate(crossTabsAllVars$crossTabsAvailVars[-input$crossTabsAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$crossTabsArrowSelBckgColVarLeft, {
        req(input$crossTabsBckgColVar_rows_selected)
        crossTabsAllVars$crossTabsAvailVars <- rbind(isolate(crossTabsAllVars$crossTabsAvailVars),        crossTabsAllVars$crossTabsSelectedBckgColVar[input$crossTabsBckgColVar_rows_selected, , drop = FALSE])
        crossTabsAllVars$crossTabsAvailVars <- crossTabsAllVars$crossTabsAvailVars[complete.cases(crossTabsAllVars$crossTabsAvailVars[ , "Variables"]), , drop = FALSE]
        crossTabsAllVars$crossTabsSelectedBckgColVar <- isolate(crossTabsAllVars$crossTabsSelectedBckgColVar[-input$crossTabsBckgColVar_rows_selected, , drop = FALSE])
      })
      observeEvent(input$crossTabsArrowSelWeightVarRight, {
        req(input$crossTabsAllAvailableVars_rows_selected)
        crossTabsAllVars$crossTabsSelectedWeightVar <- rbind(isolate(crossTabsAllVars$crossTabsSelectedWeightVar), crossTabsAllVars$crossTabsAvailVars[input$crossTabsAllAvailableVars_rows_selected, , drop = FALSE])
        crossTabsAllVars$crossTabsSelectedWeightVar <- crossTabsAllVars$crossTabsSelectedWeightVar[complete.cases(crossTabsAllVars$crossTabsSelectedWeightVar[ , "Variables"]), , drop = FALSE]
        crossTabsAllVars$crossTabsAvailVars <- isolate(crossTabsAllVars$crossTabsAvailVars[-input$crossTabsAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$crossTabsArrowSelWeightVarLeft, {
        req(input$crossTabsWeightVar_rows_selected)
        crossTabsAllVars$crossTabsAvailVars <- rbind(isolate(crossTabsAllVars$crossTabsAvailVars),        crossTabsAllVars$crossTabsSelectedWeightVar[input$crossTabsWeightVar_rows_selected, , drop = FALSE])
        crossTabsAllVars$crossTabsAvailVars <- crossTabsAllVars$crossTabsAvailVars[complete.cases(crossTabsAllVars$crossTabsAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(crossTabsAllVars$crossTabsSelectedWeightVar) > 0) {
          crossTabsAllVars$crossTabsSelectedWeightVar <- isolate(crossTabsAllVars$crossTabsSelectedWeightVar[-input$crossTabsWeightVar_rows_selected, , drop = FALSE])
        }
      })
      output$crossTabsAllAvailableVars <- renderDT({
        setkeyv(x = crossTabsAllVars$crossTabsAvailVars, cols = "order_col")
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available variables"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables available"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 726, scroller = TRUE
      ))
      output$crossTabsSplitVars <- renderDT({
        crossTabsAllVars$crossTabsSelectedSplitVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Split variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$crossTabsInclMiss <- renderUI({
        if(nrow(crossTabsAllVars$crossTabsSelectedSplitVars) == 0) {
          return(NULL)
        } else {
          checkboxInput(inputId = "crossTabsInclMiss", label = "Compute statistics for the missing values of the split variables", value = FALSE, width = "400px")
        }
      })
      output$crossTabsBckgRowVar <- renderDT({
        crossTabsAllVars$crossTabsSelectedBckgRowVar
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Background row variable"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      observe({
        if(!is.null(crossTabsAllVars$crossTabsSelectedBckgRowVar) && nrow(crossTabsAllVars$crossTabsSelectedBckgRowVar) > 1) {
          showNotification(ui = HTML("Only one row variable can be selected!"), type = "error")
          crossTabsAllVars$crossTabsAvailVars <- rbind(isolate(crossTabsAllVars$crossTabsAvailVars),        crossTabsAllVars$crossTabsSelectedBckgRowVar[nrow(crossTabsAllVars$crossTabsSelectedBckgRowVar), , drop = FALSE])
          crossTabsAllVars$crossTabsAvailVars <- crossTabsAllVars$crossTabsAvailVars[complete.cases(crossTabsAllVars$crossTabsAvailVars[ , "Variables"]), , drop = FALSE]
          crossTabsAllVars$crossTabsSelectedBckgRowVar <- isolate(crossTabsAllVars$crossTabsSelectedBckgRowVar[-2, , drop = FALSE])
        }
      })
      output$crossTabsBckgColVar <- renderDT({
        crossTabsAllVars$crossTabsSelectedBckgColVar
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Background column variable"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      observe({
        if(!is.null(crossTabsAllVars$crossTabsSelectedBckgColVar) && nrow(crossTabsAllVars$crossTabsSelectedBckgColVar) > 1) {
          showNotification(ui = HTML("Only one column variable can be selected!"), type = "error")
          crossTabsAllVars$crossTabsAvailVars <- rbind(isolate(crossTabsAllVars$crossTabsAvailVars),        crossTabsAllVars$crossTabsSelectedBckgColVar[nrow(crossTabsAllVars$crossTabsSelectedBckgColVar), , drop = FALSE])
          crossTabsAllVars$crossTabsAvailVars <- crossTabsAllVars$crossTabsAvailVars[complete.cases(crossTabsAllVars$crossTabsAvailVars[ , "Variables"]), , drop = FALSE]
          crossTabsAllVars$crossTabsSelectedBckgColVar <- isolate(crossTabsAllVars$crossTabsSelectedBckgColVar[-2, , drop = FALSE])
        }
      })
      output$crossTabsWeightVar <- renderDT({
        crossTabsAllVars$crossTabsSelectedWeightVar
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Weight variable"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = paste0("No weight selected, default (", file.crosstabs$default.weight, ") will be used")),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$crossTabsSplitArePVs <- renderText({
        if(any(crossTabsAllVars$crossTabsSelectedSplitVars[ , Variables] %in% file.crosstabs$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Split variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$crossTabsBckgRowIsPVs <- renderText({
        if(!is.null(crossTabsAllVars$crossTabsSelectedBckgRowVar) && any(crossTabsAllVars$crossTabsSelectedBckgRowVar[ , Variables] %in% file.crosstabs$PV.sets == TRUE)) {
          HTML('Warning: The selected variable in "Background row variable" is sets of PVs. Please check the added variable.')
        } else {
          return(NULL)
        }
      })
      output$crossTabsBckgColIsPVs <- renderText({
        if(!is.null(crossTabsAllVars$crossTabsSelectedBckgColVar) && any(crossTabsAllVars$crossTabsSelectedBckgColVar[ , Variables] %in% file.crosstabs$PV.sets == TRUE)) {
          HTML('Warning: The selected variable in "Background column variable" is sets of PVs. Please check the added variable.')
        } else {
          return(NULL)
        }
      })
      output$crossTabsWgtsNotWgts <- renderText({
        if(any(crossTabsAllVars$crossTabsSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE)) {
          HTML('Warning: The variable in "Weight variable" is not a weight. Please check the added variable.')
        } else {
          return(NULL)
        }
      })
      observe({
        if(nrow(crossTabsAllVars$crossTabsSelectedWeightVar) > 1) {
          showNotification(ui = HTML("Only one weight variable can be selected!"), type = "error")
          crossTabsAllVars$crossTabsAvailVars <- rbind(isolate(crossTabsAllVars$crossTabsAvailVars),        crossTabsAllVars$crossTabsSelectedWeightVar[nrow(crossTabsAllVars$crossTabsSelectedWeightVar), , drop = FALSE])
          crossTabsAllVars$crossTabsAvailVars <- crossTabsAllVars$crossTabsAvailVars[complete.cases(crossTabsAllVars$crossTabsAvailVars[ , "Variables"]), , drop = FALSE]
          crossTabsAllVars$crossTabsSelectedWeightVar <- isolate(crossTabsAllVars$crossTabsSelectedWeightVar[-2, , drop = FALSE])
        }
      })
      observeEvent(eventExpr = input$crossTabsChooseSrcFile, {
        crossTabsAllVars$crossTabsSelectedPVVars <- NULL
        crossTabsAllVars$crossTabsSelectedBckgRowVar <- NULL
        crossTabsAllVars$crossTabsSelectedBckgColVar <- NULL
      }, ignoreInit = TRUE)
      output$crossTabsExpCnts <- renderUI({
        if(!is.null(file.crosstabs$loaded)) {
          checkboxInput(inputId = "crossTabsExpCnts", label = "Compute the expected counts", value = FALSE, width = "350px")
        }
      })
      output$crossTabsRowPcts <- renderUI({
        if(!is.null(file.crosstabs$loaded)) {
          checkboxInput(inputId = "crossTabsRowPcts", label = "Compute the row percentages", value = FALSE, width = "350px")
        }
      })
      output$crossTabsColPcts <- renderUI({
        if(!is.null(file.crosstabs$loaded)) {
          checkboxInput(inputId = "crossTabsColPcts", label = "Compute the column percentages", value = FALSE, width = "350px")
        }
      })
      output$crossTabsTotPcts <- renderUI({
        if(!is.null(file.crosstabs$loaded)) {
          checkboxInput(inputId = "crossTabsTotPcts", label = "Compute the total percentages", value = FALSE, width = "350px")
        }
      })
      output$crossTabsShortcut <- renderUI({
        if(!is.null(file.crosstabs$loaded) && file.crosstabs$study %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "eTIMSS PSI", "preTIMSS", "TIMSS Advanced", "TiPi")) {
          checkboxInput(inputId = "crossTabsShortcut", label = "Use shortcut method for computing SE", value = FALSE, width = "350px")
        }
      })
      shinyFileSave(input, "crossTabsChooseOutFile", filetype = list(xlsx = "xlsx"), roots = available.volumes, updateFreq = 100000)
      output$crossTabsOpenOutput <- renderUI({
        if(length(parseSavePath(available.volumes, input$crossTabsChooseOutFile)$datapath) > 0) {
          checkboxInput(inputId = "crossTabsOpenOutput", label = "Open the output when done", value = TRUE, width = "250px")
        }
      })
      syntaxCrossTabs <- reactive({
        file.crosstabs$crosstabs.syntax <- paste0(
          'lsa.crosstabs(data.file = "', parseFilePaths(available.volumes, input$crossTabsChooseSrcFile)$datapath, '", ',
          if(length(crossTabsAllVars$crossTabsSelectedSplitVars[ , Variables]) == 1) {
            paste0('split.vars = "', crossTabsAllVars$crossTabsSelectedSplitVars[ , Variables], '"')
          } else if(length(crossTabsAllVars$crossTabsSelectedSplitVars[ , Variables]) > 1) {
            paste0('split.vars = c("', paste(crossTabsAllVars$crossTabsSelectedSplitVars[ , Variables], collapse = '", "'), '")')
          },
          if(nrow(crossTabsAllVars$crossTabsSelectedBckgRowVar) == 1) {
            paste0(', bckg.row.var = "', crossTabsAllVars$crossTabsSelectedBckgRowVar[ , Variables], '"')
          },
          if(nrow(crossTabsAllVars$crossTabsSelectedBckgColVar) == 1) {
            paste0(', bckg.col.var = "', crossTabsAllVars$crossTabsSelectedBckgColVar[ , Variables], '"')
          },
          if(nrow(crossTabsAllVars$crossTabsSelectedWeightVar) == 1 && crossTabsAllVars$crossTabsSelectedWeightVar[ , Variables] == file.crosstabs$default.weight) {
            NULL
          } else if(nrow(crossTabsAllVars$crossTabsSelectedWeightVar) == 1 && crossTabsAllVars$crossTabsSelectedWeightVar[ , Variables] %in% all.studies.available.weights && crossTabsAllVars$crossTabsSelectedWeightVar[ , Variables] != file.crosstabs$default.weight) {
            paste0(', weight.var = "', crossTabsAllVars$crossTabsSelectedWeightVar[ , Variables], '"')
          } else if(nrow(crossTabsAllVars$crossTabsSelectedWeightVar) == 0) {
            NULL
          },
          if(!is.null(input$crossTabsInclMiss) && input$crossTabsInclMiss == TRUE) {
            ", include.missing = TRUE"
          },
          if(!is.null(input$crossTabsExpCnts) && input$crossTabsExpCnts == TRUE) {
            ", expected.cnts = TRUE"
          },
          if(!is.null(input$crossTabsRowPcts) && input$crossTabsRowPcts == TRUE) {
            ", row.pcts = TRUE"
          },
          if(!is.null(input$crossTabsColPcts) && input$crossTabsColPcts == TRUE) {
            ", column.pcts = TRUE"
          },
          if(!is.null(input$crossTabsTotPcts) && input$crossTabsTotPcts == TRUE) {
            ", total.pcts = TRUE"
          },
          if(!is.null(input$crossTabsShortcut) && input$crossTabsShortcut == TRUE) {
            ", shortcut = TRUE"
          },
          paste0(', output.file = "', parseSavePath(available.volumes, input$crossTabsChooseOutFile)$datapath, '"'),
          if(!is.null(input$crossTabsOpenOutput) && input$crossTabsOpenOutput == FALSE) {
            ', open.output = FALSE'
          } else if(!is.null(input$crossTabsOpenOutput) && input$crossTabsOpenOutput == TRUE) {
            NULL
          },
          ')'
        )
      })
      output$crossTabsSyntaxHead <- renderText({
        if(length(parseSavePath(available.volumes, input$crossTabsChooseOutFile)$datapath) == 1) {
          HTML("Syntax")
        } else {
          return(NULL)
        }
      })
      output$crossTabsSyntax <- renderText({
        if(length(parseSavePath(available.volumes, input$crossTabsChooseOutFile)$datapath) == 1 && nrow(rbindlist(l = list(crossTabsAllVars$crossTabsSelectedBckgRowVar, crossTabsAllVars$crossTabsSelectedBckgColVar))) >= 1) {
          syntaxCrossTabs()
        } else {
          return(NULL)
        }
      })
      output$crossTabsExecBtnHead <- renderText({
        if(length(parseSavePath(available.volumes, input$crossTabsChooseOutFile)$datapath) == 1) {
          HTML("Press the button below to execute the syntax")
        } else {
          return(NULL)
        }
      })
      output$execCrossTabs <- renderUI({
        if(length(parseSavePath(available.volumes, input$crossTabsChooseOutFile)$datapath) == 1) {
          actionButton(inputId = "execCrossTabs", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      observe({
        if(is.null(crossTabsAllVars$crossTabsSelectedPVVars) || is.null(file.crosstabs$loaded) || any(crossTabsAllVars$crossTabsSelectedSplitVars[ , Variables] %in% file.crosstabs$PV.sets == TRUE) || nrow(crossTabsAllVars$crossTabsSelectedBckgRowVar) == 0 || nrow(crossTabsAllVars$crossTabsSelectedBckgColVar) == 0 || any(crossTabsAllVars$crossTabsSelectedBckgRowVar[ , Variables] %in% file.crosstabs$PV.sets == TRUE) || any(crossTabsAllVars$crossTabsSelectedBckgColVar[ , Variables] %in% file.crosstabs$PV.sets == TRUE) || any(crossTabsAllVars$crossTabsSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE) || is.null(file.crosstabs$default.weight) || length(file.crosstabs$default.weight) == 0) {
          hide("crossTabsExpCnts")
          hide("crossTabsRowPcts")
          hide("crossTabsColPcts")
          hide("crossTabsTotPcts")
          hide("crossTabsShortcut")
          hide("crossTabsChooseOutFile")
          hide("crossTabsOpenOutput")
          hide("crossTabsSyntaxHead")
          hide("crossTabsSyntax")
          hide("crossTabsExecBtnHead")
          hide("execCrossTabs")
          hide("consoleCrossTabs")
        } else if (!is.null(file.crosstabs$loaded) || any(crossTabsAllVars$crossTabsSelectedPVVars[ , Variables] %in% file.crosstabs$PV.sets == TRUE) || any(crossTabsAllVars$crossTabsSelectedSplitVars[ , Variables] %in% file.crosstabs$PV.sets == FALSE) || any(crossTabsAllVars$crossTabsSelectedBckgRowVar[ , Variables] %in% file.crosstabs$PV.sets == FALSE) || any(crossTabsAllVars$crossTabsSelectedBckgColVar[ , Variables] %in% file.crosstabs$PV.sets == FALSE) || any(crossTabsAllVars$crossTabsSelectedWeightVar[ , Variables] %in% all.studies.available.weights == TRUE) || !is.null(file.crosstabs$default.weight) || length(file.crosstabs$default.weight) != 0) {
          show("crossTabsExpCnts")
          show("crossTabsRowPcts")
          show("crossTabsColPcts")
          show("crossTabsTotPcts")
          show("crossTabsShortcut")
          show("crossTabsChooseOutFile")
          show("crossTabsOpenOutput")
          show("crossTabsSyntaxHead")
          show("crossTabsSyntax")
          show("crossTabsExecBtnHead")
          show("execCrossTabs")
          show("consoleCrossTabs")
        }
        if(is.null(file.crosstabs$default.weight) || length(file.crosstabs$default.weight) == 0) {
          hide("crossTabsVariablesExplText")
          hide("crossTabsAllAvailableVars")
          hide("crossTabsArrowSelSplitVarRight")
          hide("crossTabsArrowSelSplitVarLeft")
          hide("crossTabsSplitVars")
          hide("crossTabsInclMiss")
          hide("crossTabsArrowSelBckgVarRight")
          hide("crossTabsArrowSelBckgVarLeft")
          hide("crossTabsBckgVars")
          hide("crossTabsArrowSelWeightVarRight")
          hide("crossTabsArrowSelWeightVarLeft")
          hide("crossTabsWeightVar")
        } else if(!is.null(file.crosstabs$default.weight) || length(file.crosstabs$default.weight) != 0) {
          show("crossTabsVariablesExplText")
          show("crossTabsAllAvailableVars")
          show("crossTabsArrowSelSplitVarRight")
          show("crossTabsArrowSelSplitVarLeft")
          show("crossTabsSplitVars")
          show("crossTabsInclMiss")
          show("crossTabsArrowSelBckgVarRight")
          show("crossTabsArrowSelBckgVarLeft")
          show("crossTabsBckgVars")
          show("crossTabsArrowSelWeightVarsRight")
          show("crossTabsArrowSelWeightVarLeft")
          show("crossTabsWeightVar")
        }
      })
    }
  })
  observeEvent(input$execCrossTabs, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consoleCrossTabs", "")
      tryCatch({
        expr = eval(parse(text = file.crosstabs$crosstabs.syntax))
        showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
      }, error = function(e) {
        message("", e)
        showNotification(ui = HTML(paste0("Something went wrong. Possible reasons: <br/>", paste(gsub(pattern = "\\n|Error: ", replacement = "<br/>", x = e)))), type = "error", duration = NULL)
      })
    },
    message = function(i) {
      shinyjs::html(id = "consoleCrossTabs", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consoleCrossTabs", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
  }, ignoreInit = TRUE)
  output$h1Corr <- renderText("Correlations")
  hide("corrChooseOutFile")
  output$corrIntro <- renderText({HTML("Select large-scale assessment .RData file to load.")})
  file.corr <- reactiveValues(loaded = NULL, is.lsa.data = NULL, resp.type = NULL, study = NULL, cycle = NULL, country.ID = NULL, PV.sets = NULL, default.weight = NULL, corr.syntax = NULL)
  shinyFileChoose(input, "corrChooseSrcFile", roots = available.volumes, filetype = list(RData = "RData"))
  observeEvent(eventExpr = input$corrChooseSrcFile, {
    file.corr$loaded <- NULL
    file.corr$study <- NULL
    file.corr$cycle <- NULL
    file.corr$resp.type <- NULL
    file.corr$PV.sets <- NULL
    file.corr$default.weight <- NULL
    if(length(parseFilePaths(available.volumes, input$corrChooseSrcFile)$datapath) > 0 && file.size(parseFilePaths(available.volumes, input$corrChooseSrcFile)$datapath) > 104857600) {
      showNotification(ui = HTML('The size of the selected<br/>file is rather large. Please<br/>wait until the file is loaded.'), type = "message")
    }
    if(length(parseFilePaths(available.volumes, input$corrChooseSrcFile)$datapath) > 0) {
      file.corr$loaded <- get(load(parseFilePaths(available.volumes, input$corrChooseSrcFile)$datapath))
      if("lsa.data" %in% class(file.corr$loaded)) {
        file.corr$is.lsa.data <- TRUE
      } else {
        file.corr$is.lsa.data <- FALSE
      }
      file.corr$study <- attr(x = file.corr$loaded, which = "study")
      file.corr$cycle <- attr(x = file.corr$loaded, which = "cycle")
      file.corr$resp.type <- attr(x = file.corr$loaded, which = "file.type")
      file.corr$loaded <- data.table(Variables = names(file.corr$loaded), Variable_Labels = sapply(X = file.corr$loaded, FUN = function(j) {
        if(is.null(attr(x = j, which = "variable.label"))) {
          return(NA_character_)
        } else {
          attr(x = j, which = "variable.label")
        }
      }),
      order_col = 1:ncol(file.corr$loaded))
      file.corr$PV.sets <- NULL
      tmp.PV.names <- grep(pattern = paste(all.available.PVs, collapse = "|"), x = file.corr$loaded[ , Variables], value = TRUE)
      if(length(tmp.PV.names) > 0) {
        collapsed.PVs <- collapse.loaded.file.PV.names(PV.vector = tmp.PV.names, vars.object = file.corr$loaded)
        file.corr$loaded <- file.corr$loaded[!Variables %in% tmp.PV.names]
        file.corr$loaded <- rbindlist(l = list(file.corr$loaded, collapsed.PVs))
        setkeyv(x = file.corr$loaded, cols = "order_col")
        file.corr$PV.sets <- collapsed.PVs[ , Variables]
      }
      if(!is.null(file.corr$study)) {
        file.corr$default.weight <- define.default.weight(study = file.corr$study, loaded.names.and.labels = file.corr$loaded, respondent.type = file.corr$resp.type)
      }
      file.corr$country.ID <- NULL
      if("IDCNTRY" %in% file.corr$loaded[ , Variables]) {
        file.corr$country.ID <- "IDCNTRY"
      } else {
        file.corr$country.ID <- "CNT"
      }
    }
    output$corrSrcPathDisplay <- renderText({parseFilePaths(available.volumes, input$corrChooseSrcFile)$datapath})
  }, ignoreInit = TRUE)
  observe({
    if(!is.null(file.corr$loaded) && file.corr$is.lsa.data == FALSE) {
      showNotification(ui = HTML('The data is not of class "lsa.data".<br/>Please check the file content.'), type = "error")
    } else if (!is.null(file.corr$loaded) && file.corr$is.lsa.data == TRUE) {
      output$corrStudyName <- renderText({
        if(is.null(file.corr$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Study: ', file.corr$study))
        }
      })
      output$corrStudyCycle <- renderText({
        if(is.null(file.corr$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Cycle: ', file.corr$cycle))
        }
      })
      output$corrRespHead <- renderText({
        if(is.null(file.corr$resp.type)) {
          return(NULL)
        } else {
          HTML('<u>The file contains data from the following respondents:</u>')
        }
      })
      output$corrRespAvailable <- renderText({
        if(is.null(file.corr$resp.type)) {
          return(NULL)
        } else {
          HTML(file.merged.respondents[[file.corr$resp.type]])
        }
      })
      output$corrNoWeights <- renderText({
        if(!is.null(file.corr$loaded) && is.null(file.corr$default.weight) || !is.null(file.corr$loaded) && length(file.corr$default.weight) == 0) {
          HTML('Error: The loaded file does not contain any recognizable default weight variable. Such files cannot be analyzed on their own and must be merged with other respondents in advance.')
        } else {
          return(NULL)
        }
      })
      output$corrType <- renderUI({
        if(is.null(file.corr$resp.type)) {
          return(NULL)
        } else {
          radioButtons(inputId = "corrType", label = "Select correlation type", choices = c("Pearson", "Spearman"), width = "200px", selected = "Pearson")
        }
      })
      output$corrTypeExpl <- renderText({
        if(is.null(file.corr$PV.sets) && is.null(file.corr$resp.type)) {
          return(NULL)
        } else if(!is.null(file.corr$PV.sets) && !is.null(file.corr$resp.type) && !is.null(input$corrType) && input$corrType == "Pearson") {
          HTML('<br/><br/>Computes a Pearson product-moment linear correlation coefficient between two continuous variables.')
        } else if(!is.null(file.corr$PV.sets) && !is.null(file.corr$resp.type) && !is.null(input$corrType) && input$corrType == "Spearman") {
          HTML('<br/><br/>Computes a Spearman rank-order monotonic correlation between continuous or ordinal variables.')
        }
      })
      output$corrVariablesExplText <- renderText({
        if(is.null(file.corr$resp.type)) {
          return(NULL)
        } else {
          HTML('Use the panels below to select the variables to compute correlations within groups specified by splitting variables.')
        }
      })
      corr.initial.available.vars <- file.corr$loaded[!Variables %in% c(file.corr$default.weight, file.corr$country.ID), ]
      corr.initial.selected.split.vars <- file.corr$loaded[Variables == file.corr$country.ID, ]
      corr.initial.selected.bckg.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      corr.initial.selected.PV.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      corr.initial.selected.weight.var <- file.corr$loaded[Variables %in% file.corr$default.weight, ]
      corrAllVars <- reactiveValues(corrAvailVars = corr.initial.available.vars, corrSelectedSplitVars = corr.initial.selected.split.vars, corrSelectedBckgVars = corr.initial.selected.bckg.vars, corrSelectedPVVars = corr.initial.selected.PV.vars, corrSelectedWeightVar = corr.initial.selected.weight.var)
      output$corrArrowSelSplitVarsRight <- renderUI({
        if(is.null(file.corr$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "corrArrowSelSplitVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$corrArrowSelSplitVarsLeft <- renderUI({
        if(is.null(file.corr$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "corrArrowSelSplitVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$corrArrowSelBckgVarsRight <- renderUI({
        if(is.null(file.corr$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "corrArrowSelBckgVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$corrArrowSelBckgVarsLeft <- renderUI({
        if(is.null(file.corr$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "corrArrowSelBckgVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$corrArrowSelPVsRight <- renderUI({
        if(is.null(file.corr$resp.type) || is.null(file.corr$PV.sets)) {
          return(NULL)
        } else {
          actionButton(inputId = "corrArrowSelPVsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$corrArrowSelPVsLeft <- renderUI({
        if(is.null(file.corr$resp.type) || is.null(file.corr$PV.sets)) {
          return(NULL)
        } else {
          actionButton(inputId = "corrArrowSelPVsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$corrArrowSelPVsRightDisbld <- renderUI({
        if(is.null(file.corr$resp.type) || is.null(file.corr$PV.sets)) {
          actionButton(inputId = "corrArrowSelPVsRightDisbld", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      output$corrArrowSelPVsLeftDisbld <- renderUI({
        if(is.null(file.corr$resp.type) || is.null(file.corr$PV.sets)) {
          actionButton(inputId = "corrArrowSelPVsLeftDisbld", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      output$corrArrowSelWeightVarsRight <- renderUI({
        if(is.null(file.corr$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "corrArrowSelWeightVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$corrArrowSelWeightVarsLeft <- renderUI({
        if(is.null(file.corr$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "corrArrowSelWeightVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      observeEvent(input$corrArrowSelSplitVarsRight, {
        req(input$corrAllAvailableVars_rows_selected)
        corrAllVars$corrSelectedSplitVars <- rbind(isolate(corrAllVars$corrSelectedSplitVars), corrAllVars$corrAvailVars[input$corrAllAvailableVars_rows_selected, , drop = FALSE])
        corrAllVars$corrSelectedSplitVars <- corrAllVars$corrSelectedSplitVars[complete.cases(corrAllVars$corrSelectedSplitVars[ , "Variables"]), , drop = FALSE]
        corrAllVars$corrAvailVars <- isolate(corrAllVars$corrAvailVars[-input$corrAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$corrArrowSelSplitVarsLeft, {
        req(input$corrSplitVars_rows_selected)
        corrAllVars$corrAvailVars <- rbind(isolate(corrAllVars$corrAvailVars),        corrAllVars$corrSelectedSplitVars[input$corrSplitVars_rows_selected, , drop = FALSE])
        corrAllVars$corrAvailVars <- corrAllVars$corrAvailVars[complete.cases(corrAllVars$corrAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(corrAllVars$corrSelectedSplitVars) > 0) {
          corrAllVars$corrSelectedSplitVars <- isolate(corrAllVars$corrSelectedSplitVars[-input$corrSplitVars_rows_selected, , drop = FALSE])
        }
        if(!file.corr$country.ID %in% corrAllVars$corrSelectedSplitVars[ , Variables]) {
          showNotification(ui = HTML("Country ID <b>must always be</b> the first splitting variable!"), type = "error")
        }
        corrAllVars$corrSelectedSplitVars <- rbindlist(l = list(corrAllVars$corrSelectedSplitVars, corrAllVars$corrAvailVars[Variables == file.corr$country.ID, ]))
        corrAllVars$corrAvailVars <- corrAllVars$corrAvailVars[Variables != file.corr$country.ID, ]
      })
      observeEvent(input$corrArrowSelBckgVarsRight, {
        req(input$corrAllAvailableVars_rows_selected)
        corrAllVars$corrSelectedBckgVars <- rbind(isolate(corrAllVars$corrSelectedBckgVars), corrAllVars$corrAvailVars[input$corrAllAvailableVars_rows_selected, , drop = FALSE])
        corrAllVars$corrSelectedBckgVars <- corrAllVars$corrSelectedBckgVars[complete.cases(corrAllVars$corrSelectedBckgVars[ , "Variables"]), , drop = FALSE]
        corrAllVars$corrAvailVars <- isolate(corrAllVars$corrAvailVars[-input$corrAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$corrArrowSelBckgVarsLeft, {
        req(input$corrBckgVars_rows_selected)
        corrAllVars$corrAvailVars <- rbind(isolate(corrAllVars$corrAvailVars),        corrAllVars$corrSelectedBckgVars[input$corrBckgVars_rows_selected, , drop = FALSE])
        corrAllVars$corrAvailVars <- corrAllVars$corrAvailVars[complete.cases(corrAllVars$corrAvailVars[ , "Variables"]), , drop = FALSE]
        corrAllVars$corrSelectedBckgVars <- isolate(corrAllVars$corrSelectedBckgVars[-input$corrBckgVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$corrArrowSelPVsRight, {
        req(input$corrAllAvailableVars_rows_selected)
        corrAllVars$corrSelectedPVVars <- rbind(isolate(corrAllVars$corrSelectedPVVars), corrAllVars$corrAvailVars[input$corrAllAvailableVars_rows_selected, , drop = FALSE])
        corrAllVars$corrSelectedPVVars <- corrAllVars$corrSelectedPVVars[complete.cases(corrAllVars$corrSelectedPVVars[ , "Variables"]), , drop = FALSE]
        corrAllVars$corrAvailVars <- isolate(corrAllVars$corrAvailVars[-input$corrAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$corrArrowSelPVsLeft, {
        req(input$corrPVVars_rows_selected)
        corrAllVars$corrAvailVars <- rbind(isolate(corrAllVars$corrAvailVars),        corrAllVars$corrSelectedPVVars[input$corrPVVars_rows_selected, , drop = FALSE])
        corrAllVars$corrAvailVars <- corrAllVars$corrAvailVars[complete.cases(corrAllVars$corrAvailVars[ , "Variables"]), , drop = FALSE]
        corrAllVars$corrSelectedPVVars <- isolate(corrAllVars$corrSelectedPVVars[-input$corrPVVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$corrArrowSelWeightVarsRight, {
        req(input$corrAllAvailableVars_rows_selected)
        corrAllVars$corrSelectedWeightVar <- rbind(isolate(corrAllVars$corrSelectedWeightVar), corrAllVars$corrAvailVars[input$corrAllAvailableVars_rows_selected, , drop = FALSE])
        corrAllVars$corrSelectedWeightVar <- corrAllVars$corrSelectedWeightVar[complete.cases(corrAllVars$corrSelectedWeightVar[ , "Variables"]), , drop = FALSE]
        corrAllVars$corrAvailVars <- isolate(corrAllVars$corrAvailVars[-input$corrAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$corrArrowSelWeightVarsLeft, {
        req(input$corrWeightVar_rows_selected)
        corrAllVars$corrAvailVars <- rbind(isolate(corrAllVars$corrAvailVars),        corrAllVars$corrSelectedWeightVar[input$corrWeightVar_rows_selected, , drop = FALSE])
        corrAllVars$corrAvailVars <- corrAllVars$corrAvailVars[complete.cases(corrAllVars$corrAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(corrAllVars$corrSelectedWeightVar) > 0) {
          corrAllVars$corrSelectedWeightVar <- isolate(corrAllVars$corrSelectedWeightVar[-input$corrWeightVar_rows_selected, , drop = FALSE])
        }
      })
      output$corrAllAvailableVars <- renderDT({
        setkeyv(x = corrAllVars$corrAvailVars, cols = "order_col")
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available variables"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables available"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 766, scroller = TRUE
      ))
      output$corrSplitVars <- renderDT({
        corrAllVars$corrSelectedSplitVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Split variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$corrInclMiss <- renderUI({
        if(nrow(corrAllVars$corrSelectedSplitVars) == 0) {
          return(NULL)
        } else {
          checkboxInput(inputId = "corrInclMiss", label = "Compute statistics for the missing values of the split variables", value = FALSE, width = "400px")
        }
      })
      output$corrBckgVars <- renderDT({
        corrAllVars$corrSelectedBckgVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Background variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$corrPVVars <- renderDT({
        if(is.null(file.corr$PV.sets)) {
          return(NULL)
        } else {
          corrAllVars$corrSelectedPVVars
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$corrPVVarsDisbld <- renderDT({
        if(is.null(file.corr$PV.sets)) {
          data.table(V1 = as.character(), V2 = as.character())
        } else {
          return(NULL)
        }
      },
      select = "none",
      caption = htmltools::tags$caption(style = "color: #c6c6c6; font-weight: bold;", "Plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels"),
      options = list(
        language = list(zeroRecords = "No variables can be selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#c6c6c6', 'color': '#ffffff'});", "}"),
        dom = "ti",
        ordering = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0)),
        headerCallback = JS("function(thead, data, start, end, display){$('th', thead).css('border-bottom', 'none');}"),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$corrWeightVar <- renderDT({
        corrAllVars$corrSelectedWeightVar
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Weight variable"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = paste0("No weight selected, default (", file.corr$default.weight, ") will be used")),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$corrPVsNotPVs <- renderText({
        if(!is.null(corrAllVars$corrSelectedPVVars) && any(corrAllVars$corrSelectedPVVars[ , Variables] %in% file.corr$PV.sets == FALSE)) {
          HTML('Warning: One or more of the selected variables in "Plausible values" is not a set of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$corrSplitArePVs <- renderText({
        if(any(corrAllVars$corrSelectedSplitVars[ , Variables] %in% file.corr$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Split variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$corrBckgArePVs <- renderText({
        if(!is.null(corrAllVars$corrSelectedBckgVars) && any(corrAllVars$corrSelectedBckgVars[ , Variables] %in% file.corr$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Background continuous variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$corrWgtsNotWgts <- renderText({
        if(any(corrAllVars$corrSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE)) {
          HTML('Warning: The variable in "Weight variable" is not a weight. Please check the added variable.')
        } else {
          return(NULL)
        }
      })
      output$corrWarnMoreVars <- renderText({
        if(!is.null(corrAllVars$corrSelectedBckgVars) && nrow(corrAllVars$corrSelectedBckgVars) > 2) {
          HTML('<b>Note:</b> Correlations for more than a pair of background variables can be computed at the same time. However, the estimates will slightly differ compared to computing correlations for just a pair of background variables because only listwise deletion is used.')
        } else if(!is.null(corrAllVars$corrSelectedBckgVars) && nrow(corrAllVars$corrSelectedBckgVars) > 0 && nrow(corrAllVars$corrSelectedPVVars) > 1 && all(corrAllVars$corrSelectedPVVars[ , Variables] %in% file.corr$PV.sets) == TRUE) {
          HTML('<b>Note:</b> Correlations between one or more background variables and sets of PVs can be computed at the same time. However, the estimates will slightly differ compared to computing correlations just beween sets of PVs because only listwise deletion method is used.')
        } else if(!is.null(corrAllVars$corrSelectedBckgVars) && nrow(corrAllVars$corrSelectedBckgVars) > 1 && nrow(corrAllVars$corrSelectedPVVars) > 0 && all(corrAllVars$corrSelectedPVVars[ , Variables] %in% file.corr$PV.sets) == TRUE) {
          HTML('<b>Note:</b> Correlations between a set of PVs and more than one background variable can be computed at the same time. However, the estimates will slightly differ compared to computing correlations just beween a set of PVs and a background variable because only listwise deletion method is used.')
        }
      })
      observe({
        if(nrow(corrAllVars$corrSelectedWeightVar) > 1) {
          showNotification(ui = HTML("Only one weight variable can be selected!"), type = "error")
          corrAllVars$corrAvailVars <- rbind(isolate(corrAllVars$corrAvailVars),        corrAllVars$corrSelectedWeightVar[nrow(corrAllVars$corrSelectedWeightVar), , drop = FALSE])
          corrAllVars$corrAvailVars <- corrAllVars$corrAvailVars[complete.cases(corrAllVars$corrAvailVars[ , "Variables"]), , drop = FALSE]
          corrAllVars$corrSelectedWeightVar <- isolate(corrAllVars$corrSelectedWeightVar[-2, , drop = FALSE])
        }
      })
      observeEvent(eventExpr = input$corrChooseSrcFile, {
        corrAllVars$corrSelectedPVVars <- NULL
        corrAllVars$corrSelectedBckgVars <- NULL
      }, ignoreInit = TRUE)
      output$corrShortcut <- renderUI({
        if(!is.null(file.corr$study) && file.corr$study %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "eTIMSS PSI", "preTIMSS", "TIMSS Advanced", "TiPi")) {
          checkboxInput(inputId = "corrShortcut", label = "Use shortcut method for computing SE", value = FALSE, width = "350px")
        } else {
          NULL
        }
      })
      shinyFileSave(input, "corrChooseOutFile", filetype = list(xlsx = "xlsx"), roots = available.volumes, updateFreq = 100000)
      output$corrOpenOutput <- renderUI({
        if(length(parseSavePath(available.volumes, input$corrChooseOutFile)$datapath) > 0) {
          checkboxInput(inputId = "corrOpenOutput", label = "Open the output when done", value = TRUE, width = "250px")
        }
      })
      syntaxCorr <- reactive({
        file.corr$corr.syntax <- paste0(
          'lsa.corr(data.file = "', parseFilePaths(available.volumes, input$corrChooseSrcFile)$datapath, '", ',
          if(length(corrAllVars$corrSelectedSplitVars[ , Variables]) == 1) {
            paste0('split.vars = "', corrAllVars$corrSelectedSplitVars[ , Variables], '"')
          } else if(length(corrAllVars$corrSelectedSplitVars[ , Variables]) > 1) {
            paste0('split.vars = c("', paste(corrAllVars$corrSelectedSplitVars[ , Variables], collapse = '", "'), '")')
          },
          if(nrow(corrAllVars$corrSelectedBckgVars) == 1) {
            paste0(', bckg.corr.vars = "', corrAllVars$corrSelectedBckgVars[ , Variables], '"')
          } else if(nrow(corrAllVars$corrSelectedBckgVars) > 1) {
            paste0(', bckg.corr.vars = c("', paste(corrAllVars$corrSelectedBckgVars[ , Variables], collapse = '", "'), '")')
          },
          if(nrow(corrAllVars$corrSelectedPVVars) == 1) {
            paste0(', PV.root.corr = "', corrAllVars$corrSelectedPVVars[ , Variables], '"')
          } else if(nrow(corrAllVars$corrSelectedPVVars) > 1) {
            paste0(', PV.root.corr = c("', paste(corrAllVars$corrSelectedPVVars[ , Variables], collapse = '", "'), '")')
          },
          if(!is.null(input$corrType) && input$corrType == "Pearson") {
            NULL
          } else if(!is.null(input$corrType) && input$corrType == "Spearman") {
            ', corr.type = "Spearman"'
          },
          if(nrow(corrAllVars$corrSelectedWeightVar) == 1 && corrAllVars$corrSelectedWeightVar[ , Variables] == file.corr$default.weight) {
            NULL
          } else if(nrow(corrAllVars$corrSelectedWeightVar) == 1 && corrAllVars$corrSelectedWeightVar[ , Variables] %in% all.studies.available.weights && corrAllVars$corrSelectedWeightVar[ , Variables] != file.corr$default.weight) {
            paste0(', weight.var = "', corrAllVars$corrSelectedWeightVar[ , Variables], '"')
          } else if(nrow(corrAllVars$corrSelectedWeightVar) == 0) {
            NULL
          },
          if(!is.null(input$corrInclMiss) && input$corrInclMiss == TRUE) {
            ", include.missing = TRUE"
          },
          if(!is.null(input$corrShortcut) && input$corrShortcut == TRUE) {
            ", shortcut = TRUE"
          },
          paste0(', output.file = "', parseSavePath(available.volumes, input$corrChooseOutFile)$datapath, '"'),
          if(!is.null(input$corrOpenOutput) && input$corrOpenOutput == FALSE) {
            ', open.output = FALSE'
          } else if(!is.null(input$corrOpenOutput) && input$corrOpenOutput == TRUE) {
            NULL
          },
          ')'
        )
      })
      output$corrSyntaxHead <- renderText({
        if(length(parseSavePath(available.volumes, input$corrChooseOutFile)$datapath) == 1) {
          HTML("Syntax")
        } else {
          return(NULL)
        }
      })
      output$corrSyntax <- renderText({
        if(length(parseSavePath(available.volumes, input$corrChooseOutFile)$datapath) == 1 && nrow(rbindlist(l = list(corrAllVars$corrSelectedPVVars, corrAllVars$corrSelectedBckgVars))) >= 2) {
          syntaxCorr()
        } else {
          return(NULL)
        }
      })
      output$corrExecBtnHead <- renderText({
        if(length(parseSavePath(available.volumes, input$corrChooseOutFile)$datapath) == 1) {
          HTML("Press the button below to execute the syntax")
        } else {
          return(NULL)
        }
      })
      output$execCorr <- renderUI({
        if(length(parseSavePath(available.volumes, input$corrChooseOutFile)$datapath) == 1) {
          actionButton(inputId = "execCorr", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      observe({
        if(nrow(rbindlist(l = list(corrAllVars$corrSelectedPVVars, corrAllVars$corrSelectedBckgVars))) < 2 || is.null(file.corr$loaded) || is.null(corrAllVars$corrSelectedPVVars) || any(corrAllVars$corrSelectedPVVars[ , Variables] %in% file.corr$PV.sets == FALSE) || any(corrAllVars$corrSelectedSplitVars[ , Variables] %in% file.corr$PV.sets == TRUE) || any(corrAllVars$corrSelectedBckgVars[ , Variables] %in% file.corr$PV.sets == TRUE) || any(corrAllVars$corrSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE) || is.null(file.corr$default.weight) || length(file.corr$default.weight) == 0) {
          hide("corrShortcut")
          hide("corrChooseOutFile")
          hide("corrOpenOutput")
          hide("corrSyntaxHead")
          hide("corrSyntax")
          hide("corrExecBtnHead")
          hide("execCorr")
          hide("consoleCorr")
        } else if (nrow(rbindlist(l = list(corrAllVars$corrSelectedPVVars, corrAllVars$corrSelectedBckgVars))) >= 2 || !is.null(file.corr$loaded) || !is.null(corrAllVars$corrSelectedPVVars) || any(corrAllVars$corrSelectedPVVars[ , Variables] %in% file.corr$PV.sets == TRUE) || any(corrAllVars$corrSelectedSplitVars[ , Variables] %in% file.corr$PV.sets == FALSE) || any(corrAllVars$corrSelectedBckgVars[ , Variables] %in% file.corr$PV.sets == FALSE) || any(corrAllVars$corrSelectedWeightVar[ , Variables] %in% all.studies.available.weights == TRUE) || !is.null(file.corr$default.weight) || length(file.corr$default.weight) != 0) {
          show("corrShortcut")
          show("corrChooseOutFile")
          show("corrOpenOutput")
          show("corrSyntaxHead")
          show("corrSyntax")
          show("corrExecBtnHead")
          show("execCorr")
          show("consoleCorr")
        }
        if(is.null(file.corr$default.weight) || length(file.corr$default.weight) == 0) {
          hide("corrVariablesExplText")
          hide("corrAllAvailableVars")
          hide("corrArrowSelSplitVarsRight")
          hide("corrArrowSelSplitVarsLeft")
          hide("corrSplitVars")
          hide("corrInclMiss")
          hide("corrArrowSelBckgVarsRight")
          hide("corrArrowSelBckgVarsLeft")
          hide("corrBckgVars")
          hide("corrArrowSelPVsRight")
          hide("corrArrowSelPVsLeft")
          hide("corrArrowSelPVsRightDisbld")
          hide("corrArrowSelPVsLeftDisbld")
          hide("corrPVVarsDisbld")
          hide("corrPVVars")
          hide("corrArrowSelWeightVarsRight")
          hide("corrArrowSelWeightVarsLeft")
          hide("corrWeightVar")
        } else if(!is.null(file.corr$default.weight) || length(file.corr$default.weight) != 0) {
          show("corrVariablesExplText")
          show("corrAllAvailableVars")
          show("corrArrowSelSplitVarsRight")
          show("corrArrowSelSplitVarsLeft")
          show("corrSplitVars")
          show("corrInclMiss")
          show("corrArrowSelBckgVarsRight")
          show("corrArrowSelBckgVarsLeft")
          show("corrBckgVars")
          show("corrArrowSelPVsRight")
          show("corrArrowSelPVsLeft")
          show("corrArrowSelPVsRightDisbld")
          show("corrArrowSelPVsLeftDisbld")
          show("corrPVVarsDisbld")
          show("corrPVVars")
          show("corrArrowSelWeightVarsRight")
          show("corrArrowSelWeightVarsLeft")
          show("corrWeightVar")
        }
      })
    }
  })
  observeEvent(input$execCorr, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consoleCorr", "")
      tryCatch({
        expr = eval(parse(text = file.corr$corr.syntax))
        showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
      }, error = function(e) {
        message("", e)
        showNotification(ui = HTML(paste0("Something went wrong. Possible reasons: <br/>", paste(gsub(pattern = "\\n|Error: ", replacement = "<br/>", x = e)))), type = "error", duration = NULL)
      })
    },
    message = function(i) {
      shinyjs::html(id = "consoleCorr", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consoleCorr", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
  }, ignoreInit = TRUE)
  hide("linRegChooseOutFile")
  output$h1LinReg <- renderText("Linear regression")
  output$linRegIntro <- renderText({HTML("Select large-scale assessment .RData file to load.")})
  file.lin.reg <- reactiveValues(loaded = NULL, is.lsa.data = NULL, resp.type = NULL, study = NULL, cycle = NULL, country.ID = NULL, PV.sets = NULL, var.levels = NULL, var.num.values = NULL, var.char.values = NULL, var.missings = NULL, var.unique.values = NULL, default.weight = NULL, lin.reg.syntax = NULL)
  shinyFileChoose(input, "linRegChooseSrcFile", roots = available.volumes, filetype = list(RData = "RData"))
  observeEvent(eventExpr = input$linRegChooseSrcFile, {
    file.lin.reg$loaded <- NULL
    file.lin.reg$is.lsa.data <- FALSE
    file.lin.reg$resp.type <- NULL
    file.lin.reg$study <- NULL
    file.lin.reg$cycle <- NULL
    file.lin.reg$var.classes <- NULL
    file.lin.reg$default.weight <- NULL
    if(length(parseFilePaths(available.volumes, input$linRegChooseSrcFile)$datapath) > 0 && file.size(parseFilePaths(available.volumes, input$linRegChooseSrcFile)$datapath) > 104857600) {
      showNotification(ui = HTML('The size of the selected<br/>file is rather large. Please<br/>wait until the file is loaded.'), type = "message")
    }
    if(length(parseFilePaths(available.volumes, input$linRegChooseSrcFile)$datapath) > 0) {
      file.lin.reg$loaded <- get(load(parseFilePaths(available.volumes, input$linRegChooseSrcFile)$datapath))
      file.lin.reg$var.levels <- Filter(Negate(is.null), lapply(X = file.lin.reg$loaded, FUN = function(i) {
        if(is.null(attr(x = i, which = "levels"))) {
          NULL
        } else {
          attr(x = i, which = "levels")
        }
      }))
      file.lin.reg$var.num.values <- Filter(Negate(is.null), lapply(X = file.lin.reg$loaded, FUN = function(i) {
        if(!is.numeric(i)) {
          NULL
        } else {
          sort(unique(i[!is.na(i)]))
        }
      }))
      file.lin.reg$var.char.values <- Filter(Negate(is.null), lapply(X = file.lin.reg$loaded, FUN = function(i) {
        if(!is.character(i)) {
          NULL
        } else {
          unique(i[!is.na(i)])
        }
      }))
      file.lin.reg$missings <- Filter(Negate(is.null), lapply(X = file.lin.reg$loaded, FUN = function(i) {
        if(is.null(attr(x = i, which = "missings"))) {
          NULL
        } else {
          if(is.null(names(i))) {
            attr(x = i, which = "missings")
          } else {
            tmp.names.miss <- names(attr(x = i, which = "missings"))
            tmp.miss <- attr(x = i, which = "missings")
            names(tmp.miss) <- tmp.names.miss
          }
        }
      }))
      file.lin.reg$var.unique.values <- c(file.lin.reg$var.levels, file.lin.reg$var.num.values, file.lin.reg$var.char.values)
      tmp.names <- names(file.lin.reg$var.unique.values)
      file.lin.reg$var.unique.values <- lapply(names(file.lin.reg$var.unique.values), function(i) {
        setdiff(file.lin.reg$var.unique.values[[i]], file.lin.reg$missings[[i]])
      })
      names(file.lin.reg$var.unique.values) <- tmp.names
      if("lsa.data" %in% class(file.lin.reg$loaded)) {
        file.lin.reg$is.lsa.data <- TRUE
      } else {
        file.lin.reg$is.lsa.data <- FALSE
      }
      file.lin.reg$study <- attr(x = file.lin.reg$loaded, which = "study")
      file.lin.reg$cycle <- attr(x = file.lin.reg$loaded, which = "cycle")
      file.lin.reg$resp.type <- attr(x = file.lin.reg$loaded, which = "file.type")
      file.lin.reg$loaded <- data.table(Variables = names(file.lin.reg$loaded), Variable_Labels = sapply(X = file.lin.reg$loaded, FUN = function(j) {
        if(is.null(attr(x = j, which = "variable.label"))) {
          return(NA_character_)
        } else {
          attr(x = j, which = "variable.label")
        }
      }),
      order_col = 1:ncol(file.lin.reg$loaded))
      file.lin.reg$PV.sets <- NULL
      tmp.PV.names <- grep(pattern = paste(all.available.PVs, collapse = "|"), x = file.lin.reg$loaded[ , Variables], value = TRUE)
      if(length(tmp.PV.names) > 0) {
        collapsed.PVs <- collapse.loaded.file.PV.names(PV.vector = tmp.PV.names, vars.object = file.lin.reg$loaded)
        file.lin.reg$loaded <- file.lin.reg$loaded[!Variables %in% tmp.PV.names]
        file.lin.reg$loaded <- rbindlist(l = list(file.lin.reg$loaded, collapsed.PVs))
        setkeyv(x = file.lin.reg$loaded, cols = "order_col")
        file.lin.reg$PV.sets <- collapsed.PVs[ , Variables]
      }
      if(!is.null(file.lin.reg$study)) {
        file.lin.reg$default.weight <- define.default.weight(study = file.lin.reg$study, loaded.names.and.labels = file.lin.reg$loaded, respondent.type = file.lin.reg$resp.type)
      }
      file.lin.reg$country.ID <- NULL
      if("IDCNTRY" %in% file.lin.reg$loaded[ , Variables]) {
        file.lin.reg$country.ID <- "IDCNTRY"
      } else {
        file.lin.reg$country.ID <- "CNT"
      }
    }
    output$linRegSrcPathDisplay <- renderText({parseFilePaths(available.volumes, input$linRegChooseSrcFile)$datapath})
  }, ignoreInit = TRUE)
  observe({
    if(!is.null(file.lin.reg$loaded) && file.lin.reg$is.lsa.data == FALSE) {
      showNotification(ui = HTML('The data is not of class "lsa.data".<br/>Please check the file content.'), type = "error")
    } else if (!is.null(file.lin.reg$loaded) && file.lin.reg$is.lsa.data == TRUE) {
      output$linRegStudyName <- renderText({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Study: ', file.lin.reg$study))
        }
      })
      output$linRegStudyCycle <- renderText({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Cycle: ', file.lin.reg$cycle))
        }
      })
      output$linRegRespHead <- renderText({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          HTML('<u>The file contains data from the following respondents:</u>')
        }
      })
      output$linRegRespAvailable <- renderText({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          HTML(file.merged.respondents[[file.lin.reg$resp.type]])
        }
      })
      output$linRegNoWeights <- renderText({
        if(!is.null(file.lin.reg$loaded) && is.null(file.lin.reg$default.weight) || !is.null(file.lin.reg$loaded) && length(file.lin.reg$default.weight) == 0) {
          HTML('Error: The loaded file does not contain any recognizable default weight variable. Such files cannot be analyzed on their own and must be merged with other respondents in advance.')
        } else {
          return(NULL)
        }
      })
      output$linRegVariablesExplText <- renderText({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          HTML('Use the panels below to select variables to compute linear regression coefficients within groups specified by splitting variables.')
        }
      })
      lin.reg.initial.available.vars <- file.lin.reg$loaded[!Variables %in% c(file.lin.reg$default.weight, file.lin.reg$country.ID), ]
      lin.reg.initial.selected.split.vars <- file.lin.reg$loaded[Variables == file.lin.reg$country.ID, ]
      lin.reg.initial.selected.indep.cat.bckg.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      lin.reg.initial.selected.indep.cnt.bckg.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      lin.reg.initial.selected.indep.PV.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      lin.reg.initial.selected.dep.bckg.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      lin.reg.initial.selected.dep.PV.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      lin.reg.initial.selected.weight.var <- file.lin.reg$loaded[Variables %in% file.lin.reg$default.weight, ]
      lin.reg.initial.interactions <- data.table(Variable1 = as.character(), Check = as.character(), Variable2 = as.character())
      linRegAllVars <- reactiveValues(linRegAvailVars = lin.reg.initial.available.vars, linRegSelectedSplitVars = lin.reg.initial.selected.split.vars, linRegSelectedIndepCatBckgVars = lin.reg.initial.selected.indep.cat.bckg.vars, linRegSelectedIndepCntBckgVars = lin.reg.initial.selected.indep.cnt.bckg.vars, linRegSelectedIndepPVVars = lin.reg.initial.selected.indep.PV.vars, linRegSelectedDepBckgVars = lin.reg.initial.selected.dep.bckg.vars, linRegSelectedDepPVVars = lin.reg.initial.selected.dep.PV.vars, linRegSelectedWeightVar = lin.reg.initial.selected.weight.var)
      output$linRegArrowSelSplitVarsRight <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "linRegArrowSelSplitVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$linRegArrowSelSplitVarsLeft <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "linRegArrowSelSplitVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$linRegArrowSelIndepCatBckgVarsRight <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "linRegArrowSelIndepCatBckgVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px; margin-top: 9px")
        }
      })
      output$linRegArrowSelIndepCatBckgVarsLeft <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "linRegArrowSelIndepCatBckgVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$linRegArrowSelIndepCntBckgVarsRight <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "linRegArrowSelIndepCntBckgVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$linRegArrowSelIndepCntBckgVarsLeft <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "linRegArrowSelIndepCntBckgVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$linRegArrowSelIndepPVsRight <- renderUI({
        if(is.null(file.lin.reg$resp.type) || is.null(file.lin.reg$PV.sets)) {
          return(NULL)
        } else {
          actionButton(inputId = "linRegArrowSelIndepPVsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$linRegArrowSelIndepPVsLeft <- renderUI({
        if(is.null(file.lin.reg$resp.type) || is.null(file.lin.reg$PV.sets)) {
          return(NULL)
        } else {
          actionButton(inputId = "linRegArrowSelIndepPVsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$linRegArrowSelIndepPVsRightDisbld <- renderUI({
        if(is.null(file.lin.reg$resp.type) || is.null(file.lin.reg$PV.sets)) {
          actionButton(inputId = "linRegArrowSelIndepPVsRightDisbld", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      output$linRegArrowSelIndepPVsLeftDisbld <- renderUI({
        if(is.null(file.lin.reg$resp.type) || is.null(file.lin.reg$PV.sets)) {
          actionButton(inputId = "linRegArrowSelIndepPVsLeftDisbld", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      output$linRegArrowSelDepBckgVarsRight <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else if(!is.null(file.lin.reg$resp.type) && !is.null(input$linRegChooseDepType) && input$linRegChooseDepType == "Background variable") {
          actionButton(inputId = "linRegArrowSelDepBckgVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$linRegArrowSelDepBckgVarsLeft <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else if(!is.null(file.lin.reg$resp.type) && !is.null(input$linRegChooseDepType) && input$linRegChooseDepType == "Background variable") {
          actionButton(inputId = "linRegArrowSelDepBckgVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$linRegArrowSelDepPVsRight <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else if(!is.null(file.lin.reg$resp.type) && !is.null(input$linRegChooseDepType) && input$linRegChooseDepType == "Plausible values" && !is.null(file.lin.reg$PV.sets)) {
          actionButton(inputId = "linRegArrowSelDepPVsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$linRegArrowSelDepPVsLeft <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else if(!is.null(file.lin.reg$resp.type) && !is.null(input$linRegChooseDepType) && input$linRegChooseDepType == "Plausible values" && !is.null(file.lin.reg$PV.sets)) {
          actionButton(inputId = "linRegArrowSelDepPVsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$linRegArrowSelDepPVsRightDisbld <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else if(!is.null(file.lin.reg$resp.type) && !is.null(input$linRegChooseDepType) && input$linRegChooseDepType == "Plausible values" && is.null(file.lin.reg$PV.sets)) {
          actionButton(inputId = "linRegArrowSelDepPVsRightDisbld", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        }
      })
      output$linRegArrowSelDepPVsLeftDisbld <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else if(!is.null(file.lin.reg$resp.type) && !is.null(input$linRegChooseDepType) && input$linRegChooseDepType == "Plausible values" && is.null(file.lin.reg$PV.sets)) {
          actionButton(inputId = "linRegArrowSelDepPVsLeftDisbld", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        }
      })
      output$linRegArrowSelWeightVarsRight <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "linRegArrowSelWeightVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$linRegArrowSelWeightVarsLeft <- renderUI({
        if(is.null(file.lin.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "linRegArrowSelWeightVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      observeEvent(input$linRegArrowSelSplitVarsRight, {
        req(input$linRegAllAvailableVars_rows_selected)
        linRegAllVars$linRegSelectedSplitVars <- rbind(isolate(linRegAllVars$linRegSelectedSplitVars), linRegAllVars$linRegAvailVars[input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegSelectedSplitVars <- linRegAllVars$linRegSelectedSplitVars[complete.cases(linRegAllVars$linRegSelectedSplitVars[ , "Variables"]), , drop = FALSE]
        linRegAllVars$linRegAvailVars <- isolate(linRegAllVars$linRegAvailVars[-input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$linRegArrowSelSplitVarsLeft, {
        req(input$linRegSplitVars_rows_selected)
        linRegAllVars$linRegAvailVars <- rbind(isolate(linRegAllVars$linRegAvailVars),        linRegAllVars$linRegSelectedSplitVars[input$linRegSplitVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[complete.cases(linRegAllVars$linRegAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(linRegAllVars$linRegSelectedSplitVars) > 0) {
          linRegAllVars$linRegSelectedSplitVars <- isolate(linRegAllVars$linRegSelectedSplitVars[-input$linRegSplitVars_rows_selected, , drop = FALSE])
        }
        if(!file.lin.reg$country.ID %in% linRegAllVars$linRegSelectedSplitVars[ , Variables]) {
          showNotification(ui = HTML("Country ID <b>must always be</b> the first splitting variable!"), type = "error")
        }
        linRegAllVars$linRegSelectedSplitVars <- rbindlist(l = list(linRegAllVars$linRegSelectedSplitVars, linRegAllVars$linRegAvailVars[Variables == file.lin.reg$country.ID, ]))
        linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[Variables != file.lin.reg$country.ID, ]
      })
      observeEvent(input$linRegArrowSelIndepCatBckgVarsRight, {
        req(input$linRegAllAvailableVars_rows_selected)
        linRegAllVars$linRegSelectedIndepCatBckgVars <- rbind(isolate(linRegAllVars$linRegSelectedIndepCatBckgVars), linRegAllVars$linRegAvailVars[input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegSelectedIndepCatBckgVars <- linRegAllVars$linRegSelectedIndepCatBckgVars[complete.cases(linRegAllVars$linRegSelectedIndepCatBckgVars[ , "Variables"]), , drop = FALSE]
        linRegAllVars$linRegAvailVars <- isolate(linRegAllVars$linRegAvailVars[-input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
        session$sendCustomMessage("unbindDT", "linRegIndepCatBckgVars")
      })
      observeEvent(input$linRegArrowSelIndepCatBckgVarsLeft, {
        req(input$linRegIndepCatBckgVars_rows_selected)
        linRegAllVars$linRegAvailVars <- rbind(isolate(linRegAllVars$linRegAvailVars),        linRegAllVars$linRegSelectedIndepCatBckgVars[input$linRegIndepCatBckgVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[complete.cases(linRegAllVars$linRegAvailVars[ , "Variables"]), , drop = FALSE]
        linRegAllVars$linRegSelectedIndepCatBckgVars <- isolate(linRegAllVars$linRegSelectedIndepCatBckgVars[-input$linRegIndepCatBckgVars_rows_selected, , drop = FALSE])
        session$sendCustomMessage("unbindDT", "linRegIndepCatBckgVars")
      })
      observeEvent(input$linRegArrowSelIndepCntBckgVarsRight, {
        req(input$linRegAllAvailableVars_rows_selected)
        linRegAllVars$linRegSelectedIndepCntBckgVars <- rbind(isolate(linRegAllVars$linRegSelectedIndepCntBckgVars), linRegAllVars$linRegAvailVars[input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegSelectedIndepCntBckgVars <- linRegAllVars$linRegSelectedIndepCntBckgVars[complete.cases(linRegAllVars$linRegSelectedIndepCntBckgVars[ , "Variables"]), , drop = FALSE]
        linRegAllVars$linRegAvailVars <- isolate(linRegAllVars$linRegAvailVars[-input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$linRegArrowSelIndepCntBckgVarsLeft, {
        req(input$linRegIndepCntBckgVars_rows_selected)
        linRegAllVars$linRegAvailVars <- rbind(isolate(linRegAllVars$linRegAvailVars),        linRegAllVars$linRegSelectedIndepCntBckgVars[input$linRegIndepCntBckgVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[complete.cases(linRegAllVars$linRegAvailVars[ , "Variables"]), , drop = FALSE]
        linRegAllVars$linRegSelectedIndepCntBckgVars <- isolate(linRegAllVars$linRegSelectedIndepCntBckgVars[-input$linRegIndepCntBckgVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$linRegArrowSelIndepPVsRight, {
        req(input$linRegAllAvailableVars_rows_selected)
        linRegAllVars$linRegSelectedIndepPVVars <- rbind(isolate(linRegAllVars$linRegSelectedIndepPVVars), linRegAllVars$linRegAvailVars[input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegSelectedIndepPVVars <- linRegAllVars$linRegSelectedIndepPVVars[complete.cases(linRegAllVars$linRegSelectedIndepPVVars[ , "Variables"]), , drop = FALSE]
        if(nrow(linRegAllVars$linRegSelectedIndepPVVars) > 0) {
          linRegAllVars$linRegAvailVars <- isolate(linRegAllVars$linRegAvailVars[-input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
        }
      })
      observeEvent(input$linRegArrowSelIndepPVsLeft, {
        req(input$linRegIndepPVVars_rows_selected)
        linRegAllVars$linRegAvailVars <- rbind(isolate(linRegAllVars$linRegAvailVars),        linRegAllVars$linRegSelectedIndepPVVars[input$linRegIndepPVVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[complete.cases(linRegAllVars$linRegAvailVars[ , "Variables"]), , drop = FALSE]
        linRegAllVars$linRegSelectedIndepPVVars <- isolate(linRegAllVars$linRegSelectedIndepPVVars[-input$linRegIndepPVVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$linRegArrowSelDepBckgVarsRight, {
        req(input$linRegAllAvailableVars_rows_selected)
        linRegAllVars$linRegSelectedDepBckgVars <- rbind(isolate(linRegAllVars$linRegSelectedDepBckgVars), linRegAllVars$linRegAvailVars[input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegSelectedDepBckgVars <- linRegAllVars$linRegSelectedDepBckgVars[complete.cases(linRegAllVars$linRegSelectedDepBckgVars[ , "Variables"]), , drop = FALSE]
        linRegAllVars$linRegAvailVars <- isolate(linRegAllVars$linRegAvailVars[-input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$linRegArrowSelDepBckgVarsLeft, {
        req(input$linRegDepBckgVars_rows_selected)
        linRegAllVars$linRegAvailVars <- rbind(isolate(linRegAllVars$linRegAvailVars),        linRegAllVars$linRegSelectedDepBckgVars[input$linRegDepBckgVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[complete.cases(linRegAllVars$linRegAvailVars[ , "Variables"]), , drop = FALSE]
        linRegAllVars$linRegSelectedDepBckgVars <- isolate(linRegAllVars$linRegSelectedDepBckgVars[-input$linRegDepBckgVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$linRegArrowSelDepPVsRight, {
        req(input$linRegAllAvailableVars_rows_selected)
        linRegAllVars$linRegSelectedDepPVVars <- rbind(isolate(linRegAllVars$linRegSelectedDepPVVars), linRegAllVars$linRegAvailVars[input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegSelectedDepPVVars <- linRegAllVars$linRegSelectedDepPVVars[complete.cases(linRegAllVars$linRegSelectedDepPVVars[ , "Variables"]), , drop = FALSE]
        if(nrow(linRegAllVars$linRegSelectedDepPVVars) > 0) {
          linRegAllVars$linRegAvailVars <- isolate(linRegAllVars$linRegAvailVars[-input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
        }
      })
      observeEvent(input$linRegArrowSelDepPVsLeft, {
        req(input$linRegDepPVVars_rows_selected)
        linRegAllVars$linRegAvailVars <- rbind(isolate(linRegAllVars$linRegAvailVars),        linRegAllVars$linRegSelectedDepPVVars[input$linRegDepPVVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[complete.cases(linRegAllVars$linRegAvailVars[ , "Variables"]), , drop = FALSE]
        linRegAllVars$linRegSelectedDepPVVars <- isolate(linRegAllVars$linRegSelectedDepPVVars[-input$linRegDepPVVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$linRegArrowSelWeightVarsRight, {
        req(input$linRegAllAvailableVars_rows_selected)
        linRegAllVars$linRegSelectedWeightVar <- rbind(isolate(linRegAllVars$linRegSelectedWeightVar), linRegAllVars$linRegAvailVars[input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
        linRegAllVars$linRegSelectedWeightVar <- linRegAllVars$linRegSelectedWeightVar[complete.cases(linRegAllVars$linRegSelectedWeightVar[ , "Variables"]), , drop = FALSE]
        if(nrow(linRegAllVars$linRegSelectedWeightVar) > 0) {
          linRegAllVars$linRegAvailVars <- isolate(linRegAllVars$linRegAvailVars[-input$linRegAllAvailableVars_rows_selected, , drop = FALSE])
        }
      })
      observeEvent(input$linRegArrowSelWeightVarsLeft, {
        req(input$linRegWeightVar_rows_selected)
        linRegAllVars$linRegAvailVars <- rbind(isolate(linRegAllVars$linRegAvailVars),        linRegAllVars$linRegSelectedWeightVar[input$linRegWeightVar_rows_selected, , drop = FALSE])
        linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[complete.cases(linRegAllVars$linRegAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(linRegAllVars$linRegSelectedWeightVar) > 0) {
          linRegAllVars$linRegSelectedWeightVar <- isolate(linRegAllVars$linRegSelectedWeightVar[-input$linRegWeightVar_rows_selected, , drop = FALSE])
        }
      })
      observeEvent(input$linRegChooseDepType, {
        linRegAllVars$linRegAvailVars <- rbind(isolate(linRegAllVars$linRegAvailVars),        linRegAllVars$linRegSelectedDepBckgVars[input$linRegDepBckgVars_rows_all, , drop = FALSE])
        linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[complete.cases(linRegAllVars$linRegAvailVars[ , "Variables"]), , drop = FALSE]
        if(!is.null(linRegAllVars$linRegSelectedDepBckgVars) && nrow(linRegAllVars$linRegSelectedDepBckgVars) > 0) {
          linRegAllVars$linRegSelectedDepBckgVars <- isolate(linRegAllVars$linRegSelectedDepBckgVars[-input$linRegDepBckgVars_rows_all, , drop = FALSE])
        }
        linRegAllVars$linRegAvailVars <- rbind(isolate(linRegAllVars$linRegAvailVars),        linRegAllVars$linRegSelectedDepPVVars[input$linRegDepPVVars_rows_all, , drop = FALSE])
        linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[complete.cases(linRegAllVars$linRegAvailVars[ , "Variables"]), , drop = FALSE]
        if(!is.null(linRegAllVars$linRegSelectedDepPVVars) && nrow(linRegAllVars$linRegSelectedDepPVVars) > 0) {
          linRegAllVars$linRegSelectedDepPVVars <- isolate(linRegAllVars$linRegSelectedDepPVVars[-input$linRegDepPVVars_rows_all, , drop = FALSE])
        }
      })
      generate.lin.reg.contr.new.inputs <- function(FUN, len, id, ...) {
        inputs <- character(len)
        lapply(seq_len(len), function(i) {
          inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
        })
      }
      generate.lin.reg.refcat.new.inputs <- function(FUN, id, ...) {
        as.character(FUN(id, label = NULL, ...))
      }
      gather.lin.reg.cat.new.inputs.data <- function(id, len) {
        unlist(lapply(seq_len(len), function(i) {
          value = input[[paste0(id, i)]]
          if (is.null(value)) NA else value
        }))
      }
      lin.reg.contrasts <- reactiveValues(values = NULL)
      new.lin.reg.contrasts <- reactiveValues(contrasts = NULL, ref.cats = NULL)
      observe({
        if(nrow(linRegAllVars$linRegSelectedIndepCatBckgVars) > 0) {
          lin.reg.contrasts$values <- cbind(
            V1 = data.table(linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]),
            V2 = data.table(linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variable_Labels]),
            V3 = data.table(sapply(X = file.lin.reg$var.unique.values, FUN = function(i) {
              length(i)
            })[linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]]),
            V4 = if(any(sapply(X = file.lin.reg$var.unique.values[linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]], FUN = is.null)) == FALSE) {
              generate.lin.reg.contr.new.inputs(FUN = selectInput, id = 'linregcontrast', len = nrow(linRegAllVars$linRegSelectedIndepCatBckgVars), choices = c("Dummy", "Deviation", "Simple"), width = "100%")
            } else {
              generate.lin.reg.contr.new.inputs(FUN = selectInput, id = 'linregcontrast', len = nrow(linRegAllVars$linRegSelectedIndepCatBckgVars), choices = "PVs are added, check your input", width = "100%")
            },
            V5 = lapply(seq_along(1:nrow(linRegAllVars$linRegSelectedIndepCatBckgVars)), function(i) {
              if(any(sapply(X = file.lin.reg$var.unique.values[linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]], FUN = is.null)) == FALSE) {
                generate.lin.reg.refcat.new.inputs(FUN = selectInput, id = paste0("linregrefcat", i), choices = file.lin.reg$var.unique.values[linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]][i], width = "100%")
              } else {
                generate.lin.reg.refcat.new.inputs(FUN = selectInput, id = paste0("linregrefcat", i), choices = "PVs are added, check your input", width = "100%")
              }
            }),
            V6 = data.table(linRegAllVars$linRegSelectedIndepCatBckgVars[ , order_col])
          )
          if(nrow(lin.reg.contrasts$values)) {
            new.lin.reg.contrasts$contrasts <- gather.lin.reg.cat.new.inputs.data(id = "linregcontrast", len = nrow(lin.reg.contrasts$values))
            new.lin.reg.contrasts$ref.cats <- gather.lin.reg.cat.new.inputs.data(id = "linregrefcat", len = nrow(lin.reg.contrasts$values))
          }
        }
      })
      output$linRegAllAvailableVars <- renderDT({
        setkeyv(x = linRegAllVars$linRegAvailVars, cols = "order_col")
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available variables"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables available"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 1541, scroller = TRUE
      ))
      output$linRegSplitVars <- renderDT({
        linRegAllVars$linRegSelectedSplitVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Split variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$linRegInclMiss <- renderUI({
        if(nrow(linRegAllVars$linRegSelectedSplitVars) == 0) {
          return(NULL)
        } else {
          checkboxInput(inputId = "linRegInclMiss", label = "Compute statistics for the missing values of the split variables", value = FALSE, width = "400px")
        }
      })
      output$linRegIndepCatCaption <- renderText({
        HTML("Independent background categorical variables")
      })
      output$linRegIndepCatBckgVars <- renderDT({
        if(nrow(linRegAllVars$linRegSelectedIndepCatBckgVars) == 0) {
          data.table(Variables = as.character(), Variable_Labels = as.character(), n.cat = as.character(), contrast = as.character(), ref.cat = as.numeric(), order_col = as.character())
        } else {
          lin.reg.contrasts$values
        }
      },
      rownames = FALSE,
      selection = "single",
      colnames = c("Names", "Labels", "N cat.", "Contrast", "Ref. cat.", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      escape = FALSE,
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(width = '40px', targets = 2:4), list(className = 'dt-center', targets = 2:4), list(visible = FALSE, targets = 5)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        drawCallback = JS('function() {Shiny.bindAll(this.api().table().node());} '),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$linRegIndepCntBckgVars <- renderDT({
        linRegAllVars$linRegSelectedIndepCntBckgVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Independent background continuous variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$linRegIndepPVVars <- renderDT({
        if(is.null(file.lin.reg$PV.sets)) {
          return(NULL)
        } else {
          linRegAllVars$linRegSelectedIndepPVVars
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Independent plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$linRegIndepPVVarsDisbld <- renderDT({
        if(is.null(file.lin.reg$PV.sets)) {
          data.table(V1 = as.character(), V2 = as.character())
        } else {
          return(NULL)
        }
      },
      select = "none",
      caption = htmltools::tags$caption(style = "color: #c6c6c6; font-weight: bold;", "Independent plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels"),
      options = list(
        language = list(zeroRecords = "No variables can be selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#c6c6c6', 'color': '#ffffff'});", "}"),
        dom = "ti",
        ordering = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0)),
        headerCallback = JS("function(thead, data, start, end, display){$('th', thead).css('border-bottom', 'none');}"),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$linRegChooseDepType <- renderUI({
        if(!is.null(file.lin.reg$loaded)) {
          radioButtons(inputId = "linRegChooseDepType", label = "Choose the type of dependent variable", choices = c("Background variable", "Plausible values"), selected = "Background variable", inline = TRUE, width = "500px")
        } else {
          return(NULL)
        }
      })
      output$linRegDepBckgVars <- renderDT({
        if(!is.null(input$linRegChooseDepType) && input$linRegChooseDepType == "Background variable") {
          linRegAllVars$linRegSelectedDepBckgVars
        } else {
          return(NULL)
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Dependent background variable"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$linRegDepPVVars <- renderDT({
        if(!is.null(input$linRegChooseDepType) && input$linRegChooseDepType == "Plausible values" && !is.null(file.lin.reg$PV.sets)) {
          linRegAllVars$linRegSelectedDepPVVars
        } else {
          return(NULL)
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Dependent plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$linRegDepPVVarsDisbld <- renderDT({
        if(is.null(file.lin.reg$PV.sets) && !is.null(input$linRegChooseDepType) && input$linRegChooseDepType == "Plausible values") {
          data.table(V1 = as.character(), V2 = as.character())
        } else {
          return(NULL)
        }
      },
      select = "none",
      caption = htmltools::tags$caption(style = "color: #c6c6c6; font-weight: bold;", "Independent plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels"),
      options = list(
        language = list(zeroRecords = "No variables can be selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#c6c6c6', 'color': '#ffffff'});", "}"),
        dom = "ti",
        ordering = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0)),
        headerCallback = JS("function(thead, data, start, end, display){$('th', thead).css('border-bottom', 'none');}"),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      generate.interaction.checkboxes.lin.reg = function(FUN, len, id, ...) {
        inputs <- character(len)
        lapply(seq_len(len), function(i) {
          inputs[i] <- as.character(FUN(paste0(id, len, i), label = NULL, ...))
        })
      }
      gather.lin.reg.inter.inputs <- function(id, len) {
        sapply(seq_len(len), function(i) {
          value <- input[[paste0(id, len, i)]]
          if(is.null(value)) {
            NA
          } else {
            value
          }
        })
      }
      linRegPossibleInteractions <- reactive({
        if(!is.null(file.lin.reg$resp.type) && nrow(rbindlist(l = list(linRegAllVars$linRegSelectedIndepCatBckgVars, linRegAllVars$linRegSelectedIndepCntBckgVars, linRegAllVars$linRegSelectedIndepPVVars))) > 1) {
          selected.vars <- c(linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables], linRegAllVars$linRegSelectedIndepCntBckgVars[ , Variables], linRegAllVars$linRegSelectedIndepPVVars[ , Variables])
          tmp.interactions <- transpose(as.data.table(combn(x = selected.vars, m = 2)))
          data.table(Variable1 = tmp.interactions[ , V1], Check = generate.interaction.checkboxes.lin.reg(FUN = checkboxInput, len = nrow(tmp.interactions), id = "cbox_", width = "5px"), Variable2 = tmp.interactions[ , V2])
        } else {
          lin.reg.initial.interactions
        }
      })
      output$linRegInteractions <- renderDT({
        linRegPossibleInteractions()
      },
      server = FALSE,
      escape = FALSE,
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Define interactions"),
      rownames = FALSE,
      colnames = c("Variable 1", "", "Variable 2"),
      extensions = list("Scroller"),
      selection="none",
      class = "row-border stripe;compact cell-border;",
      options = list(
        language = list(zeroRecords = "No variables available for interactions"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '20px', targets = 1), list(targets = 1, className = "small" ), list(className = 'dt-center', targets = c(0, 1, 2))),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      linRegSelectedInteractions <- reactive({
        if(nrow(linRegPossibleInteractions()) > 0) {
          linRegPossibleInteractions()[gather.lin.reg.inter.inputs(id = "cbox_", len = nrow(linRegPossibleInteractions())) == TRUE]
        } else {
          lin.reg.initial.interactions
        }
      })
      output$linRegWeightVar <- renderDT({
        linRegAllVars$linRegSelectedWeightVar
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Weight variable"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = paste0("No weight selected, default (", file.lin.reg$default.weight, ") will be used")),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$linRegSplitArePVs <- renderText({
        if(!is.null(linRegAllVars$linRegSelectedSplitVars) && any(linRegAllVars$linRegSelectedSplitVars[ , Variables] %in% file.lin.reg$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Split variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$linRegBckgCatArePVs <- renderText({
        if(!is.null(linRegAllVars$linRegSelectedIndepCatBckgVars) && any(linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables] %in% file.lin.reg$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Independent background <u>categorical</u> variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$linRegBckgCntArePVs <- renderText({
        if(!is.null(linRegAllVars$linRegSelectedIndepCntBckgVars) && any(linRegAllVars$linRegSelectedIndepCntBckgVars[ , Variables] %in% file.lin.reg$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Independent background <u>continuous</u> variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$linRegIndepPVsAreBckg <- renderText({
        if(!is.null(linRegAllVars$linRegSelectedIndepPVVars) && any(linRegAllVars$linRegSelectedIndepPVVars[ , Variables] %in% file.lin.reg$PV.sets == FALSE)) {
          HTML('Warning: One or more of the selected variables in "Independent <u>plausible</u> values" are background variables. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$linRegDepBckgArePVs <- renderText({
        if(!is.null(linRegAllVars$linRegSelectedDepBckgVars) && any(linRegAllVars$linRegSelectedDepBckgVars[ , Variables] %in% file.lin.reg$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Dependent background variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$linRegDepPVsAreBckg <- renderText({
        if(!is.null(linRegAllVars$linRegSelectedDepPVVars) && any(linRegAllVars$linRegSelectedDepPVVars[ , Variables] %in% file.lin.reg$PV.sets == FALSE)) {
          HTML('Warning: One or more of the selected variables in "Dependent plausible values" are background variables. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$linRegWgtsNotWgts <- renderText({
        if(!is.null(linRegAllVars$linRegSelectedWeightVar) && any(linRegAllVars$linRegSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE)) {
          HTML('Warning: The variable in "Weight variable" is not a weight. Please check the added variable.')
        } else {
          return(NULL)
        }
      })
      observe({
        if(nrow(linRegAllVars$linRegSelectedWeightVar) > 1) {
          showNotification(ui = HTML("Only one weight variable can be selected!"), type = "error")
          linRegAllVars$linRegAvailVars <- rbind(isolate(linRegAllVars$linRegAvailVars),        linRegAllVars$linRegSelectedWeightVar[nrow(linRegAllVars$linRegSelectedWeightVar), , drop = FALSE])
          linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[complete.cases(linRegAllVars$linRegAvailVars[ , "Variables"]), , drop = FALSE]
          linRegAllVars$linRegSelectedWeightVar <- isolate(linRegAllVars$linRegSelectedWeightVar[-2, , drop = FALSE])
        }
      })
      observe({
        if(!is.null(linRegAllVars$linRegSelectedDepBckgVars) && nrow(linRegAllVars$linRegSelectedDepBckgVars) > 1) {
          showNotification(ui = HTML("Only one background<br/>variable can be selected<br/>as dependent!"), type = "error")
          linRegAllVars$linRegAvailVars <- rbind(isolate(linRegAllVars$linRegAvailVars),        linRegAllVars$linRegSelectedDepBckgVars[nrow(linRegAllVars$linRegSelectedDepBckgVars), , drop = FALSE])
          linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[complete.cases(linRegAllVars$linRegAvailVars[ , "Variables"]), , drop = FALSE]
          linRegAllVars$linRegSelectedDepBckgVars <- isolate(linRegAllVars$linRegSelectedDepBckgVars[-2, , drop = FALSE])
        }
      })
      observe({
        if(!is.null(linRegAllVars$linRegSelectedDepPVVars) && nrow(linRegAllVars$linRegSelectedDepPVVars) > 1) {
          showNotification(ui = HTML("Only one set of PVs<br/>can be selected<br/> as dependent!"), type = "error")
          linRegAllVars$linRegAvailVars <- rbind(isolate(linRegAllVars$linRegAvailVars),        linRegAllVars$linRegSelectedDepPVVars[nrow(linRegAllVars$linRegSelectedDepPVVars), , drop = FALSE])
          linRegAllVars$linRegAvailVars <- linRegAllVars$linRegAvailVars[complete.cases(linRegAllVars$linRegAvailVars[ , "Variables"]), , drop = FALSE]
          linRegAllVars$linRegSelectedDepPVVars <- isolate(linRegAllVars$linRegSelectedDepPVVars[-2, , drop = FALSE])
        }
      })
      observeEvent(eventExpr = input$linRegChooseSrcFile, {
        linRegAllVars$linRegSelectedIndepPVVars <- NULL
        linRegAllVars$linRegSelectedDepBckgVars <- NULL
        linRegAllVars$linRegSelectedDepPVVars <- NULL
      }, ignoreInit = TRUE)
      output$linRegStandardize <- renderUI({
        checkboxInput(inputId = "linRegStandardize", label = "Standardized coefficients", value = FALSE, width = "350px")
      })
      output$linRegShortcut <- renderUI({
        if(file.lin.reg$study %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "eTIMSS PSI", "preTIMSS", "TIMSS Advanced", "TiPi")) {
          checkboxInput(inputId = "linRegShortcut", label = "Use shortcut method for computing SE", value = FALSE, width = "350px")
        }
      })
      shinyFileSave(input, "linRegChooseOutFile", filetype = list(xlsx = "xlsx"), roots = available.volumes, updateFreq = 100000)
      output$linRegOpenOutput <- renderUI({
        if(length(parseSavePath(available.volumes, input$linRegChooseOutFile)$datapath) > 0) {
          checkboxInput(inputId = "linRegOpenOutput", label = "Open the output when done", value = TRUE, width = "250px")
        }
      })
      output$linRegShortcut <- renderUI({
        if(!is.null(file.lin.reg$study) && file.lin.reg$study %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "eTIMSS PSI", "preTIMSS", "TIMSS Advanced", "TiPi")) {
          checkboxInput(inputId = "linRegShortcut", label = "Use shortcut method for computing SE", value = FALSE, width = "350px")
        }
      })
      shinyFileSave(input, "linRegChooseOutFile", filetype = list(xlsx = "xlsx"), roots = available.volumes, updateFreq = 100000)
      output$linRegOpenOutput <- renderUI({
        if(length(parseSavePath(available.volumes, input$linRegChooseOutFile)$datapath) > 0) {
          checkboxInput(inputId = "linRegOpenOutput", label = "Open the output when done", value = TRUE, width = "250px")
        }
      })
      syntaxLinReg <- reactive({
        file.lin.reg$lin.reg.syntax <- paste0(
          'lsa.lin.reg(data.file = "', parseFilePaths(available.volumes, input$linRegChooseSrcFile)$datapath, '", ',
          if(length(linRegAllVars$linRegSelectedSplitVars[ , Variables]) == 1) {
            paste0('split.vars = "', linRegAllVars$linRegSelectedSplitVars[ , Variables], '"')
          } else if(length(linRegAllVars$linRegSelectedSplitVars[ , Variables]) > 1) {
            paste0('split.vars = c("', paste(linRegAllVars$linRegSelectedSplitVars[ , Variables], collapse = '", "'), '")')
          },
          if(!is.null(linRegAllVars$linRegSelectedDepBckgVars) && nrow(linRegAllVars$linRegSelectedDepBckgVars) == 1) {
            paste0(', bckg.dep.var = "', linRegAllVars$linRegSelectedDepBckgVars[ , Variables], '"')
          } else if(!is.null(linRegAllVars$linRegSelectedDepBckgVars) && nrow(linRegAllVars$linRegSelectedDepBckgVars) == 0) {
            NULL
          },
          if(!is.null(linRegAllVars$linRegSelectedDepPVVars) && nrow(linRegAllVars$linRegSelectedDepPVVars) == 1) {
            paste0(', PV.root.dep = "', linRegAllVars$linRegSelectedDepPVVars[ , Variables], '"')
          } else if(!is.null(linRegAllVars$linRegSelectedDepPVVars) && nrow(linRegAllVars$linRegSelectedDepPVVars) == 0) {
            NULL
          },
          if(length(linRegAllVars$linRegSelectedIndepCntBckgVars[ , Variables]) == 1) {
            paste0(', bckg.indep.cont.vars = "', linRegAllVars$linRegSelectedIndepCntBckgVars[ , Variables], '"')
          } else if(length(linRegAllVars$linRegSelectedIndepCntBckgVars[ , Variables]) > 1) {
            paste0(', bckg.indep.cont.vars = c("', paste(linRegAllVars$linRegSelectedIndepCntBckgVars[ , Variables], collapse = '", "'), '")')
          },
          if(length(linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]) == 1) {
            paste0(', bckg.indep.cat.vars = "', linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables], '"')
          } else if(length(linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]) > 1) {
            paste0(', bckg.indep.cat.vars = c("', paste(linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables], collapse = '", "'), '")')
          },
          if(length(linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]) == 1 && !is.null(new.lin.reg.contrasts$contrasts)) {
            paste0(', bckg.cat.contrasts = "', tolower(new.lin.reg.contrasts$contrasts), '"')
          } else if(length(linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]) > 1 && !is.null(new.lin.reg.contrasts$contrasts)) {
            paste0(', bckg.cat.contrasts = c("', paste(tolower(new.lin.reg.contrasts$contrasts), collapse = '", "'), '")')
          },
          if(length(linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]) == 1) {
            paste0(', bckg.ref.cats = ', unlist(lapply(X = file.lin.reg$var.unique.values[linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]], FUN = function(i) {
              which(i %in% new.lin.reg.contrasts$ref.cats)
            })))
          } else if(length(linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]) > 1) {
            paste0(', bckg.ref.cats = c(', paste(unlist(lapply(X = file.lin.reg$var.unique.values[linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables]], FUN = function(i) {
              which(i %in% new.lin.reg.contrasts$ref.cats)
            })), collapse = ", "), ')')
          },
          if(!is.null(linRegAllVars$linRegSelectedIndepPVVars) && length(linRegAllVars$linRegSelectedIndepPVVars[ , Variables]) == 1) {
            paste0(', PV.root.indep = "', linRegAllVars$linRegSelectedIndepPVVars[ , Variables], '"')
          } else if(!is.null(linRegAllVars$linRegSelectedIndepPVVars) && length(linRegAllVars$linRegSelectedIndepPVVars[ , Variables]) > 1) {
            paste0(', PV.root.indep = c("', paste(linRegAllVars$linRegSelectedIndepPVVars[ , Variables], collapse = '", "'), '")')
          },
          suppressWarnings(if(nrow(linRegSelectedInteractions() > 0)) {
            paste0(', interactions = list(', paste(unlist(apply(X = linRegSelectedInteractions(), MARGIN = 1, FUN = function(i) {
              paste0('c("', i[['Variable1']], '", "', i[['Variable2']], '")')
            }, simplify = FALSE)), collapse = ', '), ')')
          }),
          if(!is.null(input$linRegStandardize) && input$linRegStandardize == TRUE) {
            ", standardize = TRUE"
          },
          if(nrow(linRegAllVars$linRegSelectedWeightVar) == 1 && !is.null(file.lin.reg$default.weight) && linRegAllVars$linRegSelectedWeightVar[ , Variables] == file.lin.reg$default.weight) {
            NULL
          } else if(nrow(linRegAllVars$linRegSelectedWeightVar) == 1 && !is.null(file.lin.reg$default.weight) && linRegAllVars$linRegSelectedWeightVar[ , Variables] %in% all.studies.available.weights && linRegAllVars$linRegSelectedWeightVar[ , Variables] != file.lin.reg$default.weight) {
            paste0(', weight.var = "', linRegAllVars$linRegSelectedWeightVar[ , Variables], '"')
          } else if(nrow(linRegAllVars$linRegSelectedWeightVar) == 0) {
            NULL
          },
          if(!is.null(input$linRegInclMiss) && input$linRegInclMiss == TRUE) {
            ", include.missing = TRUE"
          },
          if(!is.null(input$linRegShortcut) && input$linRegShortcut == TRUE) {
            ", shortcut = TRUE"
          },
          paste0(', output.file = "', parseSavePath(available.volumes, input$linRegChooseOutFile)$datapath, '"'),
          if(!is.null(input$linRegOpenOutput) && input$linRegOpenOutput == FALSE) {
            ', open.output = FALSE'
          } else if(!is.null(input$linRegOpenOutput) && input$linRegOpenOutput == TRUE) {
            NULL
          },
          ')'
        )
      })
      output$linRegSyntaxHead <- renderText({
        if(length(parseSavePath(available.volumes, input$linRegChooseOutFile)$datapath) == 1) {
          HTML("Syntax")
        } else {
          return(NULL)
        }
      })
      output$linRegSyntax <- renderText({
        if(length(parseSavePath(available.volumes, input$linRegChooseOutFile)$datapath) == 1) {
          syntaxLinReg()
        } else {
          return(NULL)
        }
      })
      output$linRegExecBtnHead <- renderText({
        if(length(parseSavePath(available.volumes, input$linRegChooseOutFile)$datapath) == 1) {
          HTML("Press the button below to execute the syntax")
        } else {
          return(NULL)
        }
      })
      output$execLinReg <- renderUI({
        if(length(parseSavePath(available.volumes, input$linRegChooseOutFile)$datapath) == 1) {
          actionButton(inputId = "execLinReg", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      observe({
        if(
          nrow(rbindlist(l = list(linRegAllVars$linRegSelectedIndepCatBckgVars, linRegAllVars$linRegSelectedIndepCntBckgVars, linRegAllVars$linRegSelectedIndepPVVars))) > 0 && nrow(rbindlist(l = list(linRegAllVars$linRegSelectedDepBckgVars, linRegAllVars$linRegSelectedDepPVVars))) == 0 ||
          nrow(rbindlist(l = list(linRegAllVars$linRegSelectedIndepCatBckgVars, linRegAllVars$linRegSelectedIndepCntBckgVars, linRegAllVars$linRegSelectedIndepPVVars))) == 0 && nrow(rbindlist(l = list(linRegAllVars$linRegSelectedDepBckgVars, linRegAllVars$linRegSelectedDepPVVars))) > 0 ||
          nrow(rbindlist(l = list(linRegAllVars$linRegSelectedIndepCatBckgVars, linRegAllVars$linRegSelectedIndepCntBckgVars, linRegAllVars$linRegSelectedIndepPVVars))) == 0 && nrow(rbindlist(l = list(linRegAllVars$linRegSelectedDepBckgVars, linRegAllVars$linRegSelectedDepPVVars))) == 0 ||
          is.null(file.lin.reg$loaded) ||
          any(linRegAllVars$linRegSelectedIndepPVVars[ , Variables] %in% file.lin.reg$PV.sets == FALSE) ||
          !is.null(linRegAllVars$linRegSelectedDepPVVars) && nrow(linRegAllVars$linRegSelectedDepPVVars) > 0 && any(linRegAllVars$linRegSelectedDepPVVars[ , Variables] %in% file.lin.reg$PV.sets == FALSE) ||
          any(linRegAllVars$linRegSelectedDepBckgVars[ , Variables] %in% file.lin.reg$PV.sets == TRUE) ||
          any(linRegAllVars$linRegSelectedSplitVars[ , Variables] %in% file.lin.reg$PV.sets == TRUE) ||
          any(linRegAllVars$linRegSelectedIndepCatBckgVars[ , Variables] %in% file.lin.reg$PV.sets == TRUE) ||
          any(linRegAllVars$linRegSelectedIndepCntBckgVars[ , Variables] %in% file.lin.reg$PV.sets == TRUE) ||
          any(linRegAllVars$linRegSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE) ||
          is.null(file.lin.reg$default.weight) ||
          length(file.lin.reg$default.weight) == 0) {
          hide("linRegShortcut")
          hide("linRegStandardize")
          hide("linRegChooseOutFile")
          hide("linRegOpenOutput")
          hide("linRegSyntaxHead")
          hide("linRegSyntax")
          hide("linRegExecBtnHead")
          hide("execLinReg")
          hide("consoleLinReg")
        } else if (
          nrow(rbindlist(l = list(linRegAllVars$linRegSelectedIndepCatBckgVars, linRegAllVars$linRegSelectedIndepCntBckgVars))) > 0 && nrow(rbindlist(l = list(linRegAllVars$linRegSelectedIndepPVVars, linRegAllVars$linRegSelectedDepPVVars))) > 0 ||
          !is.null(file.lin.reg$loaded) ||
          any(linRegAllVars$linRegSelectedIndepPVVars[ , Variables] %in% file.lin.reg$PV.sets == TRUE) ||
          !is.null(linRegAllVars$linRegSelectedDepPVVars) && nrow(linRegAllVars$linRegSelectedDepPVVars) > 0 && any(linRegAllVars$linRegSelectedDepPVVars[ , Variables] %in% file.lin.reg$PV.sets == TRUE) ||
          any(linRegAllVars$linRegSelectedDepBckgVars[ , Variables] %in% file.lin.reg$PV.sets == FALSE) ||
          any(linRegAllVars$linRegSelectedSplitVars[ , Variables] %in% file.lin.reg$PV.sets == FALSE) ||
          any(linRegAllVars$linRegSelectedCatBckgVars[ , Variables] %in% file.lin.reg$PV.sets == FALSE) ||
          any(linRegAllVars$linRegSelectedCntBckgVars[ , Variables] %in% file.lin.reg$PV.sets == FALSE) ||
          any(linRegAllVars$linRegSelectedWeightVar[ , Variables] %in% all.studies.available.weights == TRUE) ||
          !is.null(file.lin.reg$default.weight) ||
          length(file.lin.reg$default.weight) != 0) {
          show("linRegShortcut")
          show("linRegStandardize")
          show("linRegChooseOutFile")
          show("linRegOpenOutput")
          show("linRegSyntaxHead")
          show("linRegSyntax")
          show("linRegExecBtnHead")
          show("execLinReg")
          show("consoleLinReg")
        }
        if(is.null(file.lin.reg$default.weight) || length(file.lin.reg$default.weight) == 0) {
          hide("linRegVariablesExplText")
          hide("linRegAllAvailableVars")
          hide("linRegArrowSelSplitVarsRight")
          hide("linRegArrowSelSplitVarsLeft")
          hide("linRegSplitVars")
          hide("linRegInclMiss")
          hide("linRegArrowSelIndepCatBckgVarsRight")
          hide("linRegArrowSelIndepCatBckgVarsLeft")
          hide("linRegArrowSelIndepCntBckgVarsRight")
          hide("linRegArrowSelIndepCntBckgVarsLeft")
          hide("linRegIndepCatCaption")
          hide("linRegIndepCatBckgVars")
          hide("linRegIndepCntBckgVars")
          hide("linRegArrowSelIndepPVsRight")
          hide("linRegArrowSelIndepPVsLeft")
          hide("linRegIndepPVVars")
          hide("linRegArrowSelIndepPVsRightDisbld")
          hide("linRegArrowSelIndepPVsLeftDisbld")
          hide("linRegIndepPVVarsDisbld")
          hide("linRegChooseDepType")
          hide("linRegArrowSelDepBckgVarsRight")
          hide("linRegArrowSelDepBckgVarsLeft")
          hide("linRegDepBckgVars")
          hide("linRegDepPVVars")
          hide("linRegDepPVVarsDisbld")
          hide("linRegInteractions")
          hide("linRegArrowSelWeightVarsRight")
          hide("linRegArrowSelWeightVarsLeft")
          hide("linRegWeightVar")
        } else if(!is.null(file.lin.reg$default.weight) || length(file.lin.reg$default.weight) != 0) {
          show("linRegVariablesExplText")
          show("linRegAllAvailableVars")
          show("linRegArrowSelSplitVarsRight")
          show("linRegArrowSelSplitVarsLeft")
          show("linRegSplitVars")
          show("linRegInclMiss")
          show("linRegArrowSelIndepCatBckgVarsRight")
          show("linRegArrowSelIndepCatBckgVarsLeft")
          show("linRegArrowSelIndepCntBckgVarsRight")
          show("linRegArrowSelIndepCntBckgVarsLeft")
          show("linRegIndepCatCaption")
          show("linRegIndepCatBckgVars")
          show("linRegIndepCntBckgVars")
          show("linRegArrowSelIndepPVsRight")
          show("linRegArrowSelIndepPVsLeft")
          show("linRegIndepPVVars")
          show("linRegArrowSelIndepPVsRightDisbld")
          show("linRegArrowSelIndepPVsLeftDisbld")
          show("linRegIndepPVVarsDisbld")
          show("linRegChooseDepType")
          show("linRegArrowSelDepBckgVarsRight")
          show("linRegArrowSelDepBckgVarsLeft")
          show("linRegDepBckgVars")
          show("linRegDepPVVars")
          show("linRegDepPVVarsDisbld")
          show("linRegInteractions")
          show("linRegArrowSelWeightVarsRight")
          show("linRegArrowSelWeightVarsLeft")
          show("linRegWeightVar")
        }
      })
    }
  })
  observeEvent(input$execLinReg, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consoleLinReg", "")
      tryCatch({
        expr = eval(parse(text = file.lin.reg$lin.reg.syntax))
        showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
      }, error = function(e) {
        message("", e)
        showNotification(ui = HTML(paste0("Something went wrong. Possible reasons: <br/>", paste(gsub(pattern = "\\n|Error: ", replacement = "<br/>", x = e)))), type = "error", duration = NULL)
      })
    },
    message = function(i) {
      shinyjs::html(id = "consoleLinReg", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consoleLinReg", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
  }, ignoreInit = TRUE)
  hide("binLogRegChooseOutFile")
  output$h1binLogReg <- renderText("Binary logistic regression")
  output$binLogRegIntro <- renderText({HTML("Select large-scale assessment .RData file to load.")})
  file.bin.log.reg <- reactiveValues(loaded = NULL, is.lsa.data = NULL, resp.type = NULL, study = NULL, cycle = NULL, country.ID = NULL, PV.sets = NULL, var.levels = NULL, var.num.values = NULL, var.char.values = NULL, var.missings = NULL, var.unique.values = NULL, default.weight = NULL, bin.log.reg.syntax = NULL)
  shinyFileChoose(input, "binLogRegChooseSrcFile", roots = available.volumes, filetype = list(RData = "RData"))
  observeEvent(eventExpr = input$binLogRegChooseSrcFile, {
    file.bin.log.reg$loaded <- NULL
    file.bin.log.reg$is.lsa.data <- FALSE
    file.bin.log.reg$resp.type <- NULL
    file.bin.log.reg$study <- NULL
    file.bin.log.reg$cycle <- NULL
    file.bin.log.reg$var.classes <- NULL
    file.bin.log.reg$default.weight <- NULL
    if(length(parseFilePaths(available.volumes, input$binLogRegChooseSrcFile)$datapath) > 0 && file.size(parseFilePaths(available.volumes, input$binLogRegChooseSrcFile)$datapath) > 104857600) {
      showNotification(ui = HTML('The size of the selected<br/>file is rather large. Please<br/>wait until the file is loaded.'), type = "message")
    }
    if(length(parseFilePaths(available.volumes, input$binLogRegChooseSrcFile)$datapath) > 0) {
      file.bin.log.reg$loaded <- get(load(parseFilePaths(available.volumes, input$binLogRegChooseSrcFile)$datapath))
      file.bin.log.reg$var.levels <- Filter(Negate(is.null), lapply(X = file.bin.log.reg$loaded, FUN = function(i) {
        if(is.null(attr(x = i, which = "levels"))) {
          NULL
        } else {
          attr(x = i, which = "levels")
        }
      }))
      file.bin.log.reg$var.num.values <- Filter(Negate(is.null), lapply(X = file.bin.log.reg$loaded, FUN = function(i) {
        if(!is.numeric(i)) {
          NULL
        } else {
          sort(unique(i[!is.na(i)]))
        }
      }))
      file.bin.log.reg$var.char.values <- Filter(Negate(is.null), lapply(X = file.bin.log.reg$loaded, FUN = function(i) {
        if(!is.character(i)) {
          NULL
        } else {
          unique(i[!is.na(i)])
        }
      }))
      file.bin.log.reg$missings <- Filter(Negate(is.null), lapply(X = file.bin.log.reg$loaded, FUN = function(i) {
        if(is.null(attr(x = i, which = "missings"))) {
          NULL
        } else {
          if(is.null(names(i))) {
            attr(x = i, which = "missings")
          } else {
            tmp.names.miss <- names(attr(x = i, which = "missings"))
            tmp.miss <- attr(x = i, which = "missings")
            names(tmp.miss) <- tmp.names.miss
          }
        }
      }))
      file.bin.log.reg$var.unique.values <- c(file.bin.log.reg$var.levels, file.bin.log.reg$var.num.values, file.bin.log.reg$var.char.values)
      tmp.names <- names(file.bin.log.reg$var.unique.values)
      file.bin.log.reg$var.unique.values <- lapply(names(file.bin.log.reg$var.unique.values), function(i) {
        setdiff(file.bin.log.reg$var.unique.values[[i]], file.bin.log.reg$missings[[i]])
      })
      names(file.bin.log.reg$var.unique.values) <- tmp.names
      if("lsa.data" %in% class(file.bin.log.reg$loaded)) {
        file.bin.log.reg$is.lsa.data <- TRUE
      } else {
        file.bin.log.reg$is.lsa.data <- FALSE
      }
      file.bin.log.reg$study <- attr(x = file.bin.log.reg$loaded, which = "study")
      file.bin.log.reg$cycle <- attr(x = file.bin.log.reg$loaded, which = "cycle")
      file.bin.log.reg$resp.type <- attr(x = file.bin.log.reg$loaded, which = "file.type")
      file.bin.log.reg$loaded <- data.table(Variables = names(file.bin.log.reg$loaded), Variable_Labels = sapply(X = file.bin.log.reg$loaded, FUN = function(j) {
        if(is.null(attr(x = j, which = "variable.label"))) {
          return(NA_character_)
        } else {
          attr(x = j, which = "variable.label")
        }
      }),
      order_col = 1:ncol(file.bin.log.reg$loaded))
      file.bin.log.reg$PV.sets <- NULL
      tmp.PV.names <- grep(pattern = paste(all.available.PVs, collapse = "|"), x = file.bin.log.reg$loaded[ , Variables], value = TRUE)
      if(length(tmp.PV.names) > 0) {
        collapsed.PVs <- collapse.loaded.file.PV.names(PV.vector = tmp.PV.names, vars.object = file.bin.log.reg$loaded)
        file.bin.log.reg$loaded <- file.bin.log.reg$loaded[!Variables %in% tmp.PV.names]
        file.bin.log.reg$loaded <- rbindlist(l = list(file.bin.log.reg$loaded, collapsed.PVs))
        setkeyv(x = file.bin.log.reg$loaded, cols = "order_col")
        file.bin.log.reg$PV.sets <- collapsed.PVs[ , Variables]
      }
      if(!is.null(file.bin.log.reg$study)) {
        file.bin.log.reg$default.weight <- define.default.weight(study = file.bin.log.reg$study, loaded.names.and.labels = file.bin.log.reg$loaded, respondent.type = file.bin.log.reg$resp.type)
      }
      file.bin.log.reg$country.ID <- NULL
      if("IDCNTRY" %in% file.bin.log.reg$loaded[ , Variables]) {
        file.bin.log.reg$country.ID <- "IDCNTRY"
      } else {
        file.bin.log.reg$country.ID <- "CNT"
      }
    }
    output$binLogRegSrcPathDisplay <- renderText({parseFilePaths(available.volumes, input$binLogRegChooseSrcFile)$datapath})
  }, ignoreInit = TRUE)
  observe({
    if(!is.null(file.bin.log.reg$loaded) && file.bin.log.reg$is.lsa.data == FALSE) {
      showNotification(ui = HTML('The data is not of class "lsa.data".<br/>Please check the file content.'), type = "error")
    } else if (!is.null(file.bin.log.reg$loaded) && file.bin.log.reg$is.lsa.data == TRUE) {
      output$binLogRegStudyName <- renderText({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Study: ', file.bin.log.reg$study))
        }
      })
      output$binLogRegStudyCycle <- renderText({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          HTML(paste0('Cycle: ', file.bin.log.reg$cycle))
        }
      })
      output$binLogRegRespHead <- renderText({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          HTML('<u>The file contains data from the following respondents:</u>')
        }
      })
      output$binLogRegRespAvailable <- renderText({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          HTML(file.merged.respondents[[file.bin.log.reg$resp.type]])
        }
      })
      output$binLogRegNoWeights <- renderText({
        if(!is.null(file.bin.log.reg$loaded) && is.null(file.bin.log.reg$default.weight) || !is.null(file.bin.log.reg$loaded) && length(file.bin.log.reg$default.weight) == 0) {
          HTML('Error: The loaded file does not contain any recognizable default weight variable. Such files cannot be analyzed on their own and must be merged with other respondents in advance.')
        } else {
          return(NULL)
        }
      })
      output$binLogRegVariablesExplText <- renderText({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          HTML('Use the panels below to select variables to compute binary logistic regression coefficients within groups specified by splitting variables.')
        }
      })
      bin.log.reg.initial.available.vars <- file.bin.log.reg$loaded[!Variables %in% c(file.bin.log.reg$default.weight, file.bin.log.reg$country.ID), ]
      bin.log.reg.initial.selected.split.vars <- file.bin.log.reg$loaded[Variables == file.bin.log.reg$country.ID, ]
      bin.log.reg.initial.selected.indep.cat.bckg.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      bin.log.reg.initial.selected.indep.cnt.bckg.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      bin.log.reg.initial.selected.indep.PV.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      bin.log.reg.initial.selected.dep.bin.vars <- data.table(Variables = as.character(), Variable_Labels = as.character(), order_col = as.numeric())
      bin.log.reg.initial.selected.weight.var <- file.bin.log.reg$loaded[Variables %in% file.bin.log.reg$default.weight, ]
      bin.log.reg.initial.interactions <- data.table(Variable1 = as.character(), Check = as.character(), Variable2 = as.character())
      binLogRegAllVars <- reactiveValues(binLogRegAvailVars = bin.log.reg.initial.available.vars, binLogRegSelectedSplitVars = bin.log.reg.initial.selected.split.vars, binLogRegSelectedIndepCatBckgVars = bin.log.reg.initial.selected.indep.cat.bckg.vars, binLogRegSelectedIndepCntBckgVars = bin.log.reg.initial.selected.indep.cnt.bckg.vars, binLogRegSelectedIndepPVVars = bin.log.reg.initial.selected.indep.PV.vars, binLogRegSelectedDepBinVars = bin.log.reg.initial.selected.dep.bin.vars, binLogRegSelectedWeightVar = bin.log.reg.initial.selected.weight.var)
      output$binLogRegArrowSelSplitVarsRight <- renderUI({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "binLogRegArrowSelSplitVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$binLogRegArrowSelSplitVarsLeft <- renderUI({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "binLogRegArrowSelSplitVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$binLogRegArrowSelIndepCatBckgVarsRight <- renderUI({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "binLogRegArrowSelIndepCatBckgVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px; margin-top: 9px")
        }
      })
      output$binLogRegArrowSelIndepCatBckgVarsLeft <- renderUI({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "binLogRegArrowSelIndepCatBckgVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$binLogRegArrowSelIndepCntBckgVarsRight <- renderUI({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "binLogRegArrowSelIndepCntBckgVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$binLogRegArrowSelIndepCntBckgVarsLeft <- renderUI({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "binLogRegArrowSelIndepCntBckgVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$binLogRegArrowSelIndepPVsRight <- renderUI({
        if(is.null(file.bin.log.reg$resp.type) || is.null(file.bin.log.reg$PV.sets)) {
          return(NULL)
        } else {
          actionButton(inputId = "binLogRegArrowSelIndepPVsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$binLogRegArrowSelIndepPVsLeft <- renderUI({
        if(is.null(file.bin.log.reg$resp.type) || is.null(file.bin.log.reg$PV.sets)) {
          return(NULL)
        } else {
          actionButton(inputId = "binLogRegArrowSelIndepPVsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$binLogRegArrowSelIndepPVsRightDisbld <- renderUI({
        if(is.null(file.bin.log.reg$resp.type) || is.null(file.bin.log.reg$PV.sets)) {
          actionButton(inputId = "binLogRegArrowSelIndepPVsRightDisbld", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      output$binLogRegArrowSelIndepPVsLeftDisbld <- renderUI({
        if(is.null(file.bin.log.reg$resp.type) || is.null(file.bin.log.reg$PV.sets)) {
          actionButton(inputId = "binLogRegArrowSelIndepPVsLeftDisbld", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #c6c6c6; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      output$binLogRegArrowSelDepBinVarsRight <- renderUI({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else if(!is.null(file.bin.log.reg$resp.type)) {
          actionButton(inputId = "binLogRegArrowSelDepBinVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$binLogRegArrowSelDepBinVarsLeft <- renderUI({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else if(!is.null(file.bin.log.reg$resp.type)) {
          actionButton(inputId = "binLogRegArrowSelDepBinVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$binLogRegArrowSelWeightVarsRight <- renderUI({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "binLogRegArrowSelWeightVarsRight", label = NULL, icon("angle-right"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      output$binLogRegArrowSelWeightVarsLeft <- renderUI({
        if(is.null(file.bin.log.reg$resp.type)) {
          return(NULL)
        } else {
          actionButton(inputId = "binLogRegArrowSelWeightVarsLeft", label = NULL, icon("angle-left"), width = "50px", style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        }
      })
      observeEvent(input$binLogRegArrowSelSplitVarsRight, {
        req(input$binLogRegAllAvailableVars_rows_selected)
        binLogRegAllVars$binLogRegSelectedSplitVars <- rbind(isolate(binLogRegAllVars$binLogRegSelectedSplitVars), binLogRegAllVars$binLogRegAvailVars[input$binLogRegAllAvailableVars_rows_selected, , drop = FALSE])
        binLogRegAllVars$binLogRegSelectedSplitVars <- binLogRegAllVars$binLogRegSelectedSplitVars[complete.cases(binLogRegAllVars$binLogRegSelectedSplitVars[ , "Variables"]), , drop = FALSE]
        binLogRegAllVars$binLogRegAvailVars <- isolate(binLogRegAllVars$binLogRegAvailVars[-input$binLogRegAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$binLogRegArrowSelSplitVarsLeft, {
        req(input$binLogRegSplitVars_rows_selected)
        binLogRegAllVars$binLogRegAvailVars <- rbind(isolate(binLogRegAllVars$binLogRegAvailVars),        binLogRegAllVars$binLogRegSelectedSplitVars[input$binLogRegSplitVars_rows_selected, , drop = FALSE])
        binLogRegAllVars$binLogRegAvailVars <- binLogRegAllVars$binLogRegAvailVars[complete.cases(binLogRegAllVars$binLogRegAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(binLogRegAllVars$binLogRegSelectedSplitVars) > 0) {
          binLogRegAllVars$binLogRegSelectedSplitVars <- isolate(binLogRegAllVars$binLogRegSelectedSplitVars[-input$binLogRegSplitVars_rows_selected, , drop = FALSE])
        }
        if(!file.bin.log.reg$country.ID %in% binLogRegAllVars$binLogRegSelectedSplitVars[ , Variables]) {
          showNotification(ui = HTML("Country ID <b>must always be</b> the first splitting variable!"), type = "error")
        }
        binLogRegAllVars$binLogRegSelectedSplitVars <- rbindlist(l = list(binLogRegAllVars$binLogRegSelectedSplitVars, binLogRegAllVars$binLogRegAvailVars[Variables == file.bin.log.reg$country.ID, ]))
        binLogRegAllVars$binLogRegAvailVars <- binLogRegAllVars$binLogRegAvailVars[Variables != file.bin.log.reg$country.ID, ]
      })
      observeEvent(input$binLogRegArrowSelIndepCatBckgVarsRight, {
        req(input$binLogRegAllAvailableVars_rows_selected)
        binLogRegAllVars$binLogRegSelectedIndepCatBckgVars <- rbind(isolate(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars), binLogRegAllVars$binLogRegAvailVars[input$binLogRegAllAvailableVars_rows_selected, , drop = FALSE])
        binLogRegAllVars$binLogRegSelectedIndepCatBckgVars <- binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[complete.cases(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , "Variables"]), , drop = FALSE]
        binLogRegAllVars$binLogRegAvailVars <- isolate(binLogRegAllVars$binLogRegAvailVars[-input$binLogRegAllAvailableVars_rows_selected, , drop = FALSE])
        session$sendCustomMessage("unbindDT", "binLogRegIndepCatBckgVars")
      })
      observeEvent(input$binLogRegArrowSelIndepCatBckgVarsLeft, {
        req(input$binLogRegIndepCatBckgVars_rows_selected)
        binLogRegAllVars$binLogRegAvailVars <- rbind(isolate(binLogRegAllVars$binLogRegAvailVars),        binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[input$binLogRegIndepCatBckgVars_rows_selected, , drop = FALSE])
        binLogRegAllVars$binLogRegAvailVars <- binLogRegAllVars$binLogRegAvailVars[complete.cases(binLogRegAllVars$binLogRegAvailVars[ , "Variables"]), , drop = FALSE]
        binLogRegAllVars$binLogRegSelectedIndepCatBckgVars <- isolate(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[-input$binLogRegIndepCatBckgVars_rows_selected, , drop = FALSE])
        session$sendCustomMessage("unbindDT", "binLogRegIndepCatBckgVars")
      })
      observeEvent(input$binLogRegArrowSelIndepCntBckgVarsRight, {
        req(input$binLogRegAllAvailableVars_rows_selected)
        binLogRegAllVars$binLogRegSelectedIndepCntBckgVars <- rbind(isolate(binLogRegAllVars$binLogRegSelectedIndepCntBckgVars), binLogRegAllVars$binLogRegAvailVars[input$binLogRegAllAvailableVars_rows_selected, , drop = FALSE])
        binLogRegAllVars$binLogRegSelectedIndepCntBckgVars <- binLogRegAllVars$binLogRegSelectedIndepCntBckgVars[complete.cases(binLogRegAllVars$binLogRegSelectedIndepCntBckgVars[ , "Variables"]), , drop = FALSE]
        if(nrow(binLogRegAllVars$binLogRegSelectedIndepCntBckgVars) > 0) {
          binLogRegAllVars$binLogRegAvailVars <- isolate(binLogRegAllVars$binLogRegAvailVars[-input$binLogRegAllAvailableVars_rows_selected, , drop = FALSE])
        }
      })
      observeEvent(input$binLogRegArrowSelIndepCntBckgVarsLeft, {
        req(input$binLogRegIndepCntBckgVars_rows_selected)
        binLogRegAllVars$binLogRegAvailVars <- rbind(isolate(binLogRegAllVars$binLogRegAvailVars),        binLogRegAllVars$binLogRegSelectedIndepCntBckgVars[input$binLogRegIndepCntBckgVars_rows_selected, , drop = FALSE])
        binLogRegAllVars$binLogRegAvailVars <- binLogRegAllVars$binLogRegAvailVars[complete.cases(binLogRegAllVars$binLogRegAvailVars[ , "Variables"]), , drop = FALSE]
        binLogRegAllVars$binLogRegSelectedIndepCntBckgVars <- isolate(binLogRegAllVars$binLogRegSelectedIndepCntBckgVars[-input$binLogRegIndepCntBckgVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$binLogRegArrowSelIndepPVsRight, {
        req(input$binLogRegAllAvailableVars_rows_selected)
        binLogRegAllVars$binLogRegSelectedIndepPVVars <- rbind(isolate(binLogRegAllVars$binLogRegSelectedIndepPVVars), binLogRegAllVars$binLogRegAvailVars[input$binLogRegAllAvailableVars_rows_selected, , drop = FALSE])
        binLogRegAllVars$binLogRegSelectedIndepPVVars <- binLogRegAllVars$binLogRegSelectedIndepPVVars[complete.cases(binLogRegAllVars$binLogRegSelectedIndepPVVars[ , "Variables"]), , drop = FALSE]
        if(nrow(binLogRegAllVars$binLogRegSelectedIndepPVVars) > 0) {
          binLogRegAllVars$binLogRegAvailVars <- isolate(binLogRegAllVars$binLogRegAvailVars[-input$binLogRegAllAvailableVars_rows_selected, , drop = FALSE])
        }
      })
      observeEvent(input$binLogRegArrowSelIndepPVsLeft, {
        req(input$binLogRegIndepPVVars_rows_selected)
        binLogRegAllVars$binLogRegAvailVars <- rbind(isolate(binLogRegAllVars$binLogRegAvailVars),        binLogRegAllVars$binLogRegSelectedIndepPVVars[input$binLogRegIndepPVVars_rows_selected, , drop = FALSE])
        binLogRegAllVars$binLogRegAvailVars <- binLogRegAllVars$binLogRegAvailVars[complete.cases(binLogRegAllVars$binLogRegAvailVars[ , "Variables"]), , drop = FALSE]
        binLogRegAllVars$binLogRegSelectedIndepPVVars <- isolate(binLogRegAllVars$binLogRegSelectedIndepPVVars[-input$binLogRegIndepPVVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$binLogRegArrowSelDepBinVarsRight, {
        req(input$binLogRegAllAvailableVars_rows_selected)
        binLogRegAllVars$binLogRegSelectedDepBinVars <- rbind(isolate(binLogRegAllVars$binLogRegSelectedDepBinVars), binLogRegAllVars$binLogRegAvailVars[input$binLogRegAllAvailableVars_rows_selected, , drop = FALSE])
        binLogRegAllVars$binLogRegSelectedDepBinVars <- binLogRegAllVars$binLogRegSelectedDepBinVars[complete.cases(binLogRegAllVars$binLogRegSelectedDepBinVars[ , "Variables"]), , drop = FALSE]
        binLogRegAllVars$binLogRegAvailVars <- isolate(binLogRegAllVars$binLogRegAvailVars[-input$binLogRegAllAvailableVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$binLogRegArrowSelDepBinVarsLeft, {
        req(input$binLogRegDepBinVars_rows_selected)
        binLogRegAllVars$binLogRegAvailVars <- rbind(isolate(binLogRegAllVars$binLogRegAvailVars),        binLogRegAllVars$binLogRegSelectedDepBinVars[input$binLogRegDepBinVars_rows_selected, , drop = FALSE])
        binLogRegAllVars$binLogRegAvailVars <- binLogRegAllVars$binLogRegAvailVars[complete.cases(binLogRegAllVars$binLogRegAvailVars[ , "Variables"]), , drop = FALSE]
        binLogRegAllVars$binLogRegSelectedDepBinVars <- isolate(binLogRegAllVars$binLogRegSelectedDepBinVars[-input$binLogRegDepBinVars_rows_selected, , drop = FALSE])
      })
      observeEvent(input$binLogRegArrowSelWeightVarsRight, {
        req(input$binLogRegAllAvailableVars_rows_selected)
        binLogRegAllVars$binLogRegSelectedWeightVar <- rbind(isolate(binLogRegAllVars$binLogRegSelectedWeightVar), binLogRegAllVars$binLogRegAvailVars[input$binLogRegAllAvailableVars_rows_selected, , drop = FALSE])
        binLogRegAllVars$binLogRegSelectedWeightVar <- binLogRegAllVars$binLogRegSelectedWeightVar[complete.cases(binLogRegAllVars$binLogRegSelectedWeightVar[ , "Variables"]), , drop = FALSE]
        if(nrow(binLogRegAllVars$binLogRegSelectedWeightVar) > 0) {
          binLogRegAllVars$binLogRegAvailVars <- isolate(binLogRegAllVars$binLogRegAvailVars[-input$binLogRegAllAvailableVars_rows_selected, , drop = FALSE])
        }
      })
      observeEvent(input$binLogRegArrowSelWeightVarsLeft, {
        req(input$binLogRegWeightVar_rows_selected)
        binLogRegAllVars$binLogRegAvailVars <- rbind(isolate(binLogRegAllVars$binLogRegAvailVars),        binLogRegAllVars$binLogRegSelectedWeightVar[input$binLogRegWeightVar_rows_selected, , drop = FALSE])
        binLogRegAllVars$binLogRegAvailVars <- binLogRegAllVars$binLogRegAvailVars[complete.cases(binLogRegAllVars$binLogRegAvailVars[ , "Variables"]), , drop = FALSE]
        if(nrow(binLogRegAllVars$binLogRegSelectedWeightVar) > 0) {
          binLogRegAllVars$binLogRegSelectedWeightVar <- isolate(binLogRegAllVars$binLogRegSelectedWeightVar[-input$binLogRegWeightVar_rows_selected, , drop = FALSE])
        }
      })
      generate.bin.log.reg.contr.new.inputs <- function(FUN, len, id, ...) {
        inputs <- character(len)
        lapply(seq_len(len), function(i) {
          inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
        })
      }
      generate.bin.log.reg.refcat.new.inputs <- function(FUN, id, ...) {
        as.character(FUN(id, label = NULL, ...))
      }
      gather.bin.log.reg.cat.new.inputs.data <- function(id, len) {
        unlist(lapply(seq_len(len), function(i) {
          value = input[[paste0(id, i)]]
          if (is.null(value)) NA else value
        }))
      }
      bin.log.reg.contrasts <- reactiveValues(values = NULL)
      new.bin.log.reg.contrasts <- reactiveValues(contrasts = NULL, ref.cats = NULL)
      observe({
        if(nrow(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars) > 0) {
          bin.log.reg.contrasts$values <- cbind(
            V1 = data.table(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]),
            V2 = data.table(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variable_Labels]),
            V3 = data.table(sapply(X = file.bin.log.reg$var.unique.values, FUN = function(i) {
              length(i)
            })[binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]]),
            V4 = if(any(sapply(X = file.bin.log.reg$var.unique.values[binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]], FUN = is.null)) == FALSE) {
              generate.bin.log.reg.contr.new.inputs(FUN = selectInput, id = 'binlogregcontrast', len = nrow(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars), choices = c("Dummy", "Deviation", "Simple"), width = "100%")
            } else {
              generate.bin.log.reg.contr.new.inputs(FUN = selectInput, id = 'binlogregcontrast', len = nrow(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars), choices = "PVs are added, check your input", width = "100%")
            },
            V5 = lapply(seq_along(1:nrow(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars)), function(i) {
              if(any(sapply(X = file.bin.log.reg$var.unique.values[binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]], FUN = is.null)) == FALSE) {
                generate.bin.log.reg.refcat.new.inputs(FUN = selectInput, id = paste0("binlogregrefcat", i), choices = file.bin.log.reg$var.unique.values[binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]][i], width = "100%")
              } else {
                generate.bin.log.reg.refcat.new.inputs(FUN = selectInput, id = paste0("binlogregrefcat", i), choices = "PVs are added, check your input", width = "100%")
              }
            }),
            V6 = data.table(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , order_col])
          )
          if(nrow(bin.log.reg.contrasts$values)) {
            new.bin.log.reg.contrasts$contrasts <- gather.bin.log.reg.cat.new.inputs.data(id = "binlogregcontrast", len = nrow(bin.log.reg.contrasts$values))
            new.bin.log.reg.contrasts$ref.cats <- gather.bin.log.reg.cat.new.inputs.data(id = "binlogregrefcat", len = nrow(bin.log.reg.contrasts$values))
          }
        }
      })
      output$binLogRegAllAvailableVars <- renderDT({
        setkeyv(x = binLogRegAllVars$binLogRegAvailVars, cols = "order_col")
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Available variables"),
      rownames = FALSE,
      filter = "top",
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables available"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 1461, scroller = TRUE
      ))
      output$binLogRegSplitVars <- renderDT({
        binLogRegAllVars$binLogRegSelectedSplitVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Split variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$binLogRegInclMiss <- renderUI({
        if(nrow(binLogRegAllVars$binLogRegSelectedSplitVars) == 0) {
          return(NULL)
        } else {
          checkboxInput(inputId = "binLogRegInclMiss", label = "Compute statistics for the missing values of the split variables", value = FALSE, width = "400px")
        }
      })
      output$binLogRegIndepCatCaption <- renderText({
        HTML("Independent background categorical variables")
      })
      output$binLogRegIndepCatBckgVars <- renderDT({
        if(nrow(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars) == 0) {
          data.table(Variables = as.character(), Variable_Labels = as.character(), n.cat = as.character(), contrast = as.character(), ref.cat = as.numeric(), order_col = as.character())
        } else {
          bin.log.reg.contrasts$values
        }
      },
      rownames = FALSE,
      selection = "single",
      colnames = c("Names", "Labels", "N cat.", "Contrast", "Ref. cat.", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      escape = FALSE,
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(width = '40px', targets = 2:4), list(className = 'dt-center', targets = 2:4), list(visible = FALSE, targets = 5)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        drawCallback = JS('function() {Shiny.bindAll(this.api().table().node());}'),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$binLogRegIndepCntBckgVars <- renderDT({
        binLogRegAllVars$binLogRegSelectedIndepCntBckgVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Independent background continuous variables"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$binLogRegIndepPVVars <- renderDT({
        if(is.null(file.bin.log.reg$PV.sets)) {
          return(NULL)
        } else {
          binLogRegAllVars$binLogRegSelectedIndepPVVars
        }
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Independent plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$binLogRegIndepPVVarsDisbld <- renderDT({
        if(is.null(file.bin.log.reg$PV.sets)) {
          data.table(V1 = as.character(), V2 = as.character())
        } else {
          return(NULL)
        }
      },
      select = "none",
      caption = htmltools::tags$caption(style = "color: #c6c6c6; font-weight: bold;", "Independent plausible values"),
      rownames = FALSE,
      colnames = c("Names", "Labels"),
      options = list(
        language = list(zeroRecords = "No variables can be selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#c6c6c6', 'color': '#ffffff'});", "}"),
        dom = "ti",
        ordering = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0)),
        headerCallback = JS("function(thead, data, start, end, display){$('th', thead).css('border-bottom', 'none');}"),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$binLogRegDepBinVars <- renderDT({
        binLogRegAllVars$binLogRegSelectedDepBinVars
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Dependent binary variable"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = "No variables have been selected"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      generate.interaction.checkboxes.bin.log.reg = function(FUN, len, id, ...) {
        inputs <- character(len)
        lapply(seq_len(len), function(i) {
          inputs[i] <- as.character(FUN(paste0(id, len, i), label = NULL, ...))
        })
      }
      gather.bin.log.reg.inter.inputs <- function(id, len) {
        sapply(seq_len(len), function(i) {
          value <- input[[paste0(id, len, i)]]
          if(is.null(value)) {
            NA
          } else {
            value
          }
        })
      }
      binLogRegPossibleInteractions <- reactive({
        if(!is.null(file.bin.log.reg$resp.type) && nrow(rbindlist(l = list(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars, binLogRegAllVars$binLogRegSelectedIndepCntBckgVars, binLogRegAllVars$binLogRegSelectedIndepPVVars))) > 1) {
          selected.vars <- c(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables], binLogRegAllVars$binLogRegSelectedIndepCntBckgVars[ , Variables], binLogRegAllVars$binLogRegSelectedIndepPVVars[ , Variables])
          tmp.interactions <- transpose(as.data.table(combn(x = selected.vars, m = 2)))
          data.table(Variable1 = tmp.interactions[ , V1], Check = generate.interaction.checkboxes.bin.log.reg(FUN = checkboxInput, len = nrow(tmp.interactions), id = "cbox_", width = "5px"), Variable2 = tmp.interactions[ , V2])
        } else {
          bin.log.reg.initial.interactions
        }
      })
      output$binLogRegInteractions <- renderDT({
        binLogRegPossibleInteractions()
      },
      server = FALSE,
      escape = FALSE,
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Define interactions"),
      rownames = FALSE,
      colnames = c("Variable 1", "", "Variable 2"),
      extensions = list("Scroller"),
      selection="none",
      class = "row-border stripe;compact cell-border;",
      options = list(
        language = list(zeroRecords = "No variables available for interactions"),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '20px', targets = 1), list(targets = 1, className = "small" ), list(className = 'dt-center', targets = c(0, 1, 2))),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      binLogRegSelectedInteractions <- reactive({
        if(nrow(binLogRegPossibleInteractions()) > 0) {
          binLogRegPossibleInteractions()[gather.bin.log.reg.inter.inputs(id = "cbox_", len = nrow(binLogRegPossibleInteractions())) == TRUE]
        } else {
          bin.log.reg.initial.interactions
        }
      })
      output$binLogRegWeightVar <- renderDT({
        binLogRegAllVars$binLogRegSelectedWeightVar
      },
      caption = htmltools::tags$caption(style = "color: black; font-weight: bold;", "Weight variable"),
      rownames = FALSE,
      colnames = c("Names", "Labels", "sortingcol"),
      class = "cell-border stripe;compact cell-border;",
      extensions = list("Scroller"),
      options = list(
        language = list(zeroRecords = paste0("No weight selected, default (", file.bin.log.reg$default.weight, ") will be used")),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#000000', 'color': '#ffffff'});", "}"),
        dom = "ti",
        searchHighlight = FALSE,
        searchDelay = 100,
        ordering = FALSE,
        pageLength = 5000,
        autoWidth = TRUE,
        columnDefs = list(list(width = '75px', targets = 0), list(visible = FALSE, targets = 2)),
        rowCallback = JS("function(r,d) {$(r).attr('height', '40px')}"),
        deferRender = TRUE, scrollY = 100, scroller = TRUE
      ))
      output$binLogRegSplitArePVs <- renderText({
        if(!is.null(binLogRegAllVars$binLogRegSelectedSplitVars) && any(binLogRegAllVars$binLogRegSelectedSplitVars[ , Variables] %in% file.bin.log.reg$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Split variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$binLogRegBckgCatArePVs <- renderText({
        if(!is.null(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars) && any(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables] %in% file.bin.log.reg$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Independent background <u>categorical</u> variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$binLogRegBckgCntArePVs <- renderText({
        if(!is.null(binLogRegAllVars$binLogRegSelectedIndepCntBckgVars) && any(binLogRegAllVars$binLogRegSelectedIndepCntBckgVars[ , Variables] %in% file.bin.log.reg$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Independent background <u>continuous</u> variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$binLogRegIndepPVsAreBckg <- renderText({
        if(!is.null(binLogRegAllVars$binLogRegSelectedIndepPVVars) && any(binLogRegAllVars$binLogRegSelectedIndepPVVars[ , Variables] %in% file.bin.log.reg$PV.sets == FALSE)) {
          HTML('Warning: One or more of the selected variables in "Independent <u>plausible</u> values" are background variables. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$binLogRegDepBinArePVs <- renderText({
        if(!is.null(binLogRegAllVars$binLogRegSelectedDepBinVars) && any(binLogRegAllVars$binLogRegSelectedDepBinVars[ , Variables] %in% file.bin.log.reg$PV.sets == TRUE)) {
          HTML('Warning: One or more of the selected variables in "Dependent background variables" are sets of PVs. Please check the added variables.')
        } else {
          return(NULL)
        }
      })
      output$binLogRegDepNotBin <- renderText({
        if(!is.null(binLogRegAllVars$binLogRegSelectedDepBinVars) && nrow(binLogRegAllVars$binLogRegSelectedDepBinVars) > 0 && length(unlist(file.bin.log.reg$var.unique.values[binLogRegAllVars$binLogRegSelectedDepBinVars[ , Variables]])) != 2) {
          HTML('Warning: The variable in "Dependent binary variable" is not a binary. Please check the added variable.')
        } else {
          return(NULL)
        }
      })
      output$binLogRegWgtsNotWgts <- renderText({
        if(!is.null(binLogRegAllVars$binLogRegSelectedWeightVar) && any(binLogRegAllVars$binLogRegSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE)) {
          HTML('Warning: The variable in "Weight variable" is not a weight. Please check the added variable.')
        } else {
          return(NULL)
        }
      })
      observe({
        if(nrow(binLogRegAllVars$binLogRegSelectedWeightVar) > 1) {
          showNotification(ui = HTML("Only one weight variable can be selected!"), type = "error")
          binLogRegAllVars$binLogRegAvailVars <- rbind(isolate(binLogRegAllVars$binLogRegAvailVars),        binLogRegAllVars$binLogRegSelectedWeightVar[nrow(binLogRegAllVars$binLogRegSelectedWeightVar), , drop = FALSE])
          binLogRegAllVars$binLogRegAvailVars <- binLogRegAllVars$binLogRegAvailVars[complete.cases(binLogRegAllVars$binLogRegAvailVars[ , "Variables"]), , drop = FALSE]
          binLogRegAllVars$binLogRegSelectedWeightVar <- isolate(binLogRegAllVars$binLogRegSelectedWeightVar[-2, , drop = FALSE])
        }
      })
      observe({
        if(!is.null(binLogRegAllVars$binLogRegSelectedDepBinVars) && nrow(binLogRegAllVars$binLogRegSelectedDepBinVars) > 1) {
          showNotification(ui = HTML("Only one background<br/>variable can be selected<br/>as dependent!"), type = "error")
          binLogRegAllVars$binLogRegAvailVars <- rbind(isolate(binLogRegAllVars$binLogRegAvailVars),        binLogRegAllVars$binLogRegSelectedDepBinVars[nrow(binLogRegAllVars$binLogRegSelectedDepBinVars), , drop = FALSE])
          binLogRegAllVars$binLogRegAvailVars <- binLogRegAllVars$binLogRegAvailVars[complete.cases(binLogRegAllVars$binLogRegAvailVars[ , "Variables"]), , drop = FALSE]
          binLogRegAllVars$binLogRegSelectedDepBinVars <- isolate(binLogRegAllVars$binLogRegSelectedDepBinVars[-2, , drop = FALSE])
        }
      })
      observeEvent(eventExpr = input$binLogRegChooseSrcFile, {
        binLogRegAllVars$binLogRegSelectedIndepPVVars <- NULL
        binLogRegAllVars$binLogRegSelectedDepBinVars <- NULL
      }, ignoreInit = TRUE)
      output$binLogRegWgtNorm <- renderUI({
        checkboxInput(inputId = "binLogRegWgtNorm", label = "Normalize the weights", value = FALSE, width = "350px")
      })
      output$binLogRegStandardize <- renderUI({
        checkboxInput(inputId = "binLogRegStandardize", label = "Standardized coefficients", value = FALSE, width = "350px")
      })
      output$binLogRegShortcut <- renderUI({
        if(!is.null(file.bin.log.reg$study) && file.bin.log.reg$study %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "eTIMSS PSI", "preTIMSS", "TIMSS Advanced", "TiPi")) {
          checkboxInput(inputId = "binLogRegShortcut", label = "Use shortcut method for computing SE", value = FALSE, width = "350px")
        }
      })
      shinyFileSave(input, "binLogRegChooseOutFile", filetype = list(xlsx = "xlsx"), roots = available.volumes, updateFreq = 100000)
      output$binLogRegOpenOutput <- renderUI({
        if(length(parseSavePath(available.volumes, input$binLogRegChooseOutFile)$datapath) > 0) {
          checkboxInput(inputId = "binLogRegOpenOutput", label = "Open the output when done", value = TRUE, width = "250px")
        }
      })
      syntaxBinLogReg <- reactive({
        file.bin.log.reg$bin.log.reg.syntax <- paste0(
          'lsa.bin.log.reg(data.file = "', parseFilePaths(available.volumes, input$binLogRegChooseSrcFile)$datapath, '", ',
          if(length(binLogRegAllVars$binLogRegSelectedSplitVars[ , Variables]) == 1) {
            paste0('split.vars = "', binLogRegAllVars$binLogRegSelectedSplitVars[ , Variables], '"')
          } else if(length(binLogRegAllVars$binLogRegSelectedSplitVars[ , Variables]) > 1) {
            paste0('split.vars = c("', paste(binLogRegAllVars$binLogRegSelectedSplitVars[ , Variables], collapse = '", "'), '")')
          },
          if(!is.null(binLogRegAllVars$binLogRegSelectedDepBinVars) && nrow(binLogRegAllVars$binLogRegSelectedDepBinVars) == 1) {
            paste0(', bin.dep.var = "', binLogRegAllVars$binLogRegSelectedDepBinVars[ , Variables], '"')
          } else if(!is.null(binLogRegAllVars$binLogRegSelectedDepBinVars) && nrow(binLogRegAllVars$binLogRegSelectedDepBinVars) == 0) {
            NULL
          },
          if(length(binLogRegAllVars$binLogRegSelectedIndepCntBckgVars[ , Variables]) == 1) {
            paste0(', bckg.indep.cont.vars = "', binLogRegAllVars$binLogRegSelectedIndepCntBckgVars[ , Variables], '"')
          } else if(length(binLogRegAllVars$binLogRegSelectedIndepCntBckgVars[ , Variables]) > 1) {
            paste0(', bckg.indep.cont.vars = c("', paste(binLogRegAllVars$binLogRegSelectedIndepCntBckgVars[ , Variables], collapse = '", "'), '")')
          },
          if(length(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]) == 1) {
            paste0(', bckg.indep.cat.vars = "', binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables], '"')
          } else if(length(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]) > 1) {
            paste0(', bckg.indep.cat.vars = c("', paste(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables], collapse = '", "'), '")')
          },
          if(length(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]) == 1 && !is.null(new.bin.log.reg.contrasts$contrasts)) {
            paste0(', bckg.cat.contrasts = "', tolower(new.bin.log.reg.contrasts$contrasts), '"')
          } else if(length(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]) > 1 && !is.null(new.bin.log.reg.contrasts$contrasts)) {
            paste0(', bckg.cat.contrasts = c("', paste(tolower(new.bin.log.reg.contrasts$contrasts), collapse = '", "'), '")')
          },
          if(length(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]) == 1) {
            paste0(', bckg.ref.cats = ', unlist(lapply(X = file.bin.log.reg$var.unique.values[binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]], FUN = function(i) {
              which(i %in% new.bin.log.reg.contrasts$ref.cats)
            })))
          } else if(length(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]) > 1) {
            paste0(', bckg.ref.cats = c(', paste(unlist(lapply(X = file.bin.log.reg$var.unique.values[binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables]], FUN = function(i) {
              which(i %in% new.bin.log.reg.contrasts$ref.cats)
            })), collapse = ", "), ')')
          },
          if(!is.null(binLogRegAllVars$binLogRegSelectedIndepPVVars) && length(binLogRegAllVars$binLogRegSelectedIndepPVVars[ , Variables]) == 1) {
            paste0(', PV.root.indep = "', binLogRegAllVars$binLogRegSelectedIndepPVVars[ , Variables], '"')
          } else if(!is.null(binLogRegAllVars$binLogRegSelectedIndepPVVars) && length(binLogRegAllVars$binLogRegSelectedIndepPVVars[ , Variables]) > 1) {
            paste0(', PV.root.indep = c("', paste(binLogRegAllVars$binLogRegSelectedIndepPVVars[ , Variables], collapse = '", "'), '")')
          },
          suppressWarnings(if(nrow(binLogRegSelectedInteractions() > 0)) {
            paste0(', interactions = list(', paste(unlist(apply(X = binLogRegSelectedInteractions(), MARGIN = 1, FUN = function(i) {
              paste0('c("', i[['Variable1']], '", "', i[['Variable2']], '")')
            }, simplify = FALSE)), collapse = ', '), ')')
          }),
          if(!is.null(input$binLogRegWgtNorm) && input$binLogRegWgtNorm == TRUE) {
            ", norm.weight = TRUE"
          },
          if(!is.null(input$binLogRegStandardize) && input$binLogRegStandardize == TRUE) {
            ", standardize = TRUE"
          },
          if(nrow(binLogRegAllVars$binLogRegSelectedWeightVar) == 1 && !is.null(file.bin.log.reg$default.weight) && binLogRegAllVars$binLogRegSelectedWeightVar[ , Variables] == file.bin.log.reg$default.weight) {
            NULL
          } else if(nrow(binLogRegAllVars$binLogRegSelectedWeightVar) == 1 && !is.null(file.bin.log.reg$default.weight) && binLogRegAllVars$binLogRegSelectedWeightVar[ , Variables] %in% all.studies.available.weights && binLogRegAllVars$binLogRegSelectedWeightVar[ , Variables] != file.bin.log.reg$default.weight) {
            paste0(', weight.var = "', binLogRegAllVars$binLogRegSelectedWeightVar[ , Variables], '"')
          } else if(nrow(binLogRegAllVars$binLogRegSelectedWeightVar) == 0) {
            NULL
          },
          if(!is.null(input$binLogRegInclMiss) && input$binLogRegInclMiss == TRUE) {
            ", include.missing = TRUE"
          },
          if(!is.null(input$binLogRegShortcut) && input$binLogRegShortcut == TRUE) {
            ", shortcut = TRUE"
          },
          paste0(', output.file = "', parseSavePath(available.volumes, input$binLogRegChooseOutFile)$datapath, '"'),
          if(!is.null(input$binLogRegOpenOutput) && input$binLogRegOpenOutput == FALSE) {
            ', open.output = FALSE'
          } else if(!is.null(input$binLogRegOpenOutput) && input$binLogRegOpenOutput == TRUE) {
            NULL
          },
          ')'
        )
      })
      output$binLogRegSyntaxHead <- renderText({
        if(length(parseSavePath(available.volumes, input$binLogRegChooseOutFile)$datapath) == 1) {
          HTML("Syntax")
        } else {
          return(NULL)
        }
      })
      output$binLogRegSyntax <- renderText({
        if(length(parseSavePath(available.volumes, input$binLogRegChooseOutFile)$datapath) == 1) {
          syntaxBinLogReg()
        } else {
          return(NULL)
        }
      })
      output$binLogRegExecBtnHead <- renderText({
        if(length(parseSavePath(available.volumes, input$binLogRegChooseOutFile)$datapath) == 1) {
          HTML("Press the button below to execute the syntax")
        } else {
          return(NULL)
        }
      })
      output$execBinLogReg <- renderUI({
        if(length(parseSavePath(available.volumes, input$binLogRegChooseOutFile)$datapath) == 1) {
          actionButton(inputId = "execBinLogReg", label = "Execute syntax", icon = icon("cogs"), style = "color: #ffffff; background-color: #000000; border-radius: 10px")
        } else {
          return(NULL)
        }
      })
      observe({
        if(
          nrow(rbindlist(l = list(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars, binLogRegAllVars$binLogRegSelectedIndepCntBckgVars, binLogRegAllVars$binLogRegSelectedIndepPVVars))) > 0 && !is.null(binLogRegAllVars$binLogRegSelectedDepBinVars) && nrow(binLogRegAllVars$binLogRegSelectedDepBinVars) == 0 ||
          nrow(rbindlist(l = list(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars, binLogRegAllVars$binLogRegSelectedIndepCntBckgVars, binLogRegAllVars$binLogRegSelectedIndepPVVars))) == 0 && !is.null(binLogRegAllVars$binLogRegSelectedDepBinVars) && nrow(binLogRegAllVars$binLogRegSelectedDepBinVars) > 0 ||
          nrow(rbindlist(l = list(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars, binLogRegAllVars$binLogRegSelectedIndepCntBckgVars, binLogRegAllVars$binLogRegSelectedIndepPVVars))) == 0 && !is.null(binLogRegAllVars$binLogRegSelectedDepBinVars) && nrow(binLogRegAllVars$binLogRegSelectedDepBinVars) == 0 ||
          is.null(file.bin.log.reg$loaded) ||
          !is.null(binLogRegAllVars$binLogRegSelectedIndepPVVars) && nrow(binLogRegAllVars$binLogRegSelectedIndepPVVars) > 0 && any(binLogRegAllVars$binLogRegSelectedIndepPVVars[ , Variables] %in% file.bin.log.reg$PV.sets == FALSE) ||
          !is.null(binLogRegAllVars$binLogRegSelectedDepBinVars) && nrow(binLogRegAllVars$binLogRegSelectedDepBinVars) > 0 && any(binLogRegAllVars$binLogRegSelectedDepBinVars[ , Variables] %in% file.bin.log.reg$PV.sets == TRUE) ||
          any(binLogRegAllVars$binLogRegSelectedSplitVars[ , Variables] %in% file.bin.log.reg$PV.sets == TRUE) ||
          any(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars[ , Variables] %in% file.bin.log.reg$PV.sets == TRUE) ||
          any(binLogRegAllVars$binLogRegSelectedIndepCntBckgVars[ , Variables] %in% file.bin.log.reg$PV.sets == TRUE) ||
          any(binLogRegAllVars$binLogRegSelectedWeightVar[ , Variables] %in% all.studies.available.weights == FALSE) ||
          is.null(file.bin.log.reg$default.weight) ||
          length(file.bin.log.reg$default.weight) == 0
        ) {
          hide("binLogRegWgtNorm")
          hide("binLogRegShortcut")
          hide("binLogRegStandardize")
          hide("binLogRegChooseOutFile")
          hide("binLogRegOpenOutput")
          hide("binLogRegSyntaxHead")
          hide("binLogRegSyntax")
          hide("binLogRegExecBtnHead")
          hide("execBinLogReg")
          hide("consoleBinLogReg")
        } else if (
          nrow(rbindlist(l = list(binLogRegAllVars$binLogRegSelectedIndepCatBckgVars, binLogRegAllVars$binLogRegSelectedIndepCntBckgVars))) > 0 && nrow(binLogRegAllVars$binLogRegSelectedIndepPVVars) > 0 ||
          !is.null(file.bin.log.reg$loaded) ||
          any(binLogRegAllVars$binLogRegSelectedIndepPVVars[ , Variables] %in% file.bin.log.reg$PV.sets == TRUE) ||
          any(binLogRegAllVars$binLogRegSelectedDepBinVars[ , Variables] %in% file.bin.log.reg$PV.sets == FALSE) ||
          any(binLogRegAllVars$binLogRegSelectedSplitVars[ , Variables] %in% file.bin.log.reg$PV.sets == FALSE) ||
          any(binLogRegAllVars$binLogRegSelectedCatBckgVars[ , Variables] %in% file.bin.log.reg$PV.sets == FALSE) ||
          any(binLogRegAllVars$binLogRegSelectedCntBckgVars[ , Variables] %in% file.bin.log.reg$PV.sets == FALSE) ||
          any(binLogRegAllVars$binLogRegSelectedWeightVar[ , Variables] %in% all.studies.available.weights == TRUE) ||
          !is.null(file.bin.log.reg$default.weight) ||
          length(file.bin.log.reg$default.weight) != 0) {
          show("binLogRegWgtNorm")
          show("binLogRegShortcut")
          show("binLogRegStandardize")
          show("binLogRegChooseOutFile")
          show("binLogRegOpenOutput")
          show("binLogRegSyntaxHead")
          show("binLogRegSyntax")
          show("binLogRegExecBtnHead")
          show("execBinLogReg")
          show("consoleBinLogReg")
        }
        if(is.null(file.bin.log.reg$default.weight) || length(file.bin.log.reg$default.weight) == 0) {
          hide("binLogRegVariablesExplText")
          hide("binLogRegAllAvailableVars")
          hide("binLogRegArrowSelSplitVarsRight")
          hide("binLogRegArrowSelSplitVarsLeft")
          hide("binLogRegSplitVars")
          hide("binLogRegInclMiss")
          hide("binLogRegArrowSelIndepCatBckgVarsRight")
          hide("binLogRegArrowSelIndepCatBckgVarsLeft")
          hide("binLogRegArrowSelIndepCntBckgVarsRight")
          hide("binLogRegArrowSelIndepCntBckgVarsLeft")
          hide("binLogRegIndepCatCaption")
          hide("binLogRegIndepCatBckgVars")
          hide("binLogRegIndepCntBckgVars")
          hide("binLogRegArrowSelIndepPVsRight")
          hide("binLogRegArrowSelIndepPVsLeft")
          hide("binLogRegIndepPVVars")
          hide("binLogRegArrowSelIndepPVsRightDisbld")
          hide("binLogRegArrowSelIndepPVsLeftDisbld")
          hide("binLogRegIndepPVVarsDisbld")
          hide("binLogRegChooseDepType")
          hide("binLogRegArrowSelDepBckgVarsRight")
          hide("binLogRegArrowSelDepBckgVarsLeft")
          hide("binLogRegDepBinVars")
          hide("binLogRegDepPVVars")
          hide("binLogRegDepPVVarsDisbld")
          hide("binLogRegInteractions")
          hide("binLogRegArrowSelWeightVarsRight")
          hide("binLogRegArrowSelWeightVarsLeft")
          hide("binLogRegWeightVar")
        } else if(!is.null(file.bin.log.reg$default.weight) || length(file.bin.log.reg$default.weight) != 0) {
          show("binLogRegVariablesExplText")
          show("binLogRegAllAvailableVars")
          show("binLogRegArrowSelSplitVarsRight")
          show("binLogRegArrowSelSplitVarsLeft")
          show("binLogRegSplitVars")
          show("binLogRegInclMiss")
          show("binLogRegArrowSelIndepCatBckgVarsRight")
          show("binLogRegArrowSelIndepCatBckgVarsLeft")
          show("binLogRegArrowSelIndepCntBckgVarsRight")
          show("binLogRegArrowSelIndepCntBckgVarsLeft")
          show("binLogRegIndepCatCaption")
          show("binLogRegIndepCatBckgVars")
          show("binLogRegIndepCntBckgVars")
          show("binLogRegArrowSelIndepPVsRight")
          show("binLogRegArrowSelIndepPVsLeft")
          show("binLogRegIndepPVVars")
          show("binLogRegArrowSelIndepPVsRightDisbld")
          show("binLogRegArrowSelIndepPVsLeftDisbld")
          show("binLogRegIndepPVVarsDisbld")
          show("binLogRegChooseDepType")
          show("binLogRegArrowSelDepBckgVarsRight")
          show("binLogRegArrowSelDepBckgVarsLeft")
          show("binLogRegDepBinVars")
          show("binLogRegDepPVVars")
          show("binLogRegDepPVVarsDisbld")
          show("binLogRegInteractions")
          show("binLogRegArrowSelWeightVarsRight")
          show("binLogRegArrowSelWeightVarsLeft")
          show("binLogRegWeightVar")
        }
      })
    }
  })
  observeEvent(input$execBinLogReg, {
    showNotification(ui = HTML("<br/>   Execution started.   <br/><br/>"), type = "message")
    withCallingHandlers({html("consoleBinLogReg", "")
      tryCatch({
        expr = eval(parse(text = file.bin.log.reg$bin.log.reg.syntax))
        showNotification(ui = HTML("<br/>   All operations complete!   <br/><br/>"), type = "message", duration = NULL)
      }, error = function(e) {
        message("", e)
        showNotification(ui = HTML(paste0("Something went wrong. Possible reasons: <br/>", paste(gsub(pattern = "\\n|Error: ", replacement = "<br/>", x = e)))), type = "error", duration = NULL)
      })
    },
    message = function(i) {
      shinyjs::html(id = "consoleBinLogReg", html = i$message, add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "consoleBinLogReg", html = paste0("Warning: ", w$message, "\n"), add = TRUE)
    })
    session$sendCustomMessage(type = "scrollCallback", 1)
  }, ignoreInit = TRUE)
  output$helpHeading <- renderText("Help")
  output$helpOnRALSAWebsite <- renderText({
    HTML("Visit the user guide section at <a href = http://ralsa.ineri.org/user-guide/, target = '_blank'> RALSA's dedicated website</a> for the userguide or use the following links for help on speciffic functionality:<br/><br/>")
  })
  output$helpRALSAWebsiteLinks <- renderText({
    HTML(
      "<a href = http://ralsa.ineri.org/installation-instructions, target = '_blank'>Installation instructions</a><br/>
<a href = http://ralsa.ineri.org/getting-started-with-ralsa, target = '_blank'>Getting started with RALSA</a><br/><br/>
Prepare data for analysis:<br/>
<ul><li><a href = http://ralsa.ineri.org/convert-data, target = '_blank'>Convert data (SPSS, or text in case of PISA prior 2015), print data properties on screen, select PISA countries for analysis</a></li></ul>
<ul><li><a href = http://ralsa.ineri.org/merge-data, target = '_blank'>Merge study data files from different countries and/or respondents</a></li></ul>
<ul><li><a href = http://ralsa.ineri.org/variable-dictionaries, target = '_blank'>Variable dictionaries (name, class, variable label, response categories/unique values, user-defined missing values)</a></li></ul>
<ul><li><a href = http://ralsa.ineri.org/data-diagnostics, target = '_blank'>Data diagnostic tables</a></li></ul>
<ul><li><a href = http://ralsa.ineri.org/recode-variables, target = '_blank'>Recode variables</a></li></ul>
Perform analyses:<br/>
<ul><li><a href = http://ralsa.ineri.org/percentages-and-means, target = '_blank'>Percentages of respondents in certain groups and averages (means, medians or modes) on variables of interest, per group</a></li></ul>
<ul><li><a href = http://ralsa.ineri.org/percentiles, target = '_blank'>Percentiles of continuous variables within groups of respondents</a></li></ul>
<ul><li><a href = http://ralsa.ineri.org/benchmarks, target = '_blank'>Percentages of respondents reaching or surpassing benchmarks of achievement</a></li></ul>
<ul><li><a href = http://ralsa.ineri.org/crosstabulations, target = '_blank'>Crosstabulations with Rao-Scott first- and second-order chi-square adjustments</a></li></ul>
<ul><li><a href = http://ralsa.ineri.org/correlations, target = '_blank'>Correlations (Pearson or Spearman)</a></li></ul>
<ul><li><a href = http://ralsa.ineri.org/linear-regression, target = '_blank'>Linear regression</a></li></ul>
<ul><li><a href = http://ralsa.ineri.org/binary-logistic-regression, target = '_blank'>Binary logistic regression</a></li></ul>"
    )
  })
  output$exitHeading <- renderText("Press the button below to exit RALSA")
  observeEvent(input$closeGUI, {
    js$closeWindow()
    stopApp()
  })
}
