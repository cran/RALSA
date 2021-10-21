### Do not use the objects and functions from this file on their own. They are intended for the functions of the RALSA package.

import.data <- function(path) {
  tmp <- load(path)
  return(get(tmp))
}
produce.analysis.data.table <- function(data.object, object.variables, action.arguments, imported.file.attributes) {
  if(imported.file.attributes[["lsa.study"]] %in% design.weight.variables[["IEA.JK2.studies"]]) {
    DESIGN <- "JRR"
  } else if(imported.file.attributes[["lsa.study"]] %in% c(design.weight.variables[["IEA.BRR.studies"]], design.weight.variables[["OECD.BRR.studies"]])) {
    DESIGN <- "BRR"
  }
  assign(x = "DESIGN", value = DESIGN, envir = parent.frame())
  data.object <- data.object[ , mget(unique(c(key(data.object), unname(unlist(object.variables[!names(object.variables) %in% c("PV.root.avg", "PV.root.prctls", "PV.root.bench", "PV.root.corr", "PV.root.dep", "PV.root.indep")])))))]
  bckg.vars.in.data <- unname(unlist(Filter(Negate(is.null), object.variables[c("bckg.var", "bckg.corr.vars", "bckg.avg.vars", "bckg.prctls.vars", "bckg.dep.var", "bckg.indep.cont.vars", "bckg.indep.cat.vars", "bin.dep.var")])))
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
  if(imported.file.attributes[["lsa.study"]] %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "TIMSS Advanced", "TiPi")) {
    data.object <- produce.jk.reps.data(data = data.object, weight.var = object.variables[["weight.var"]], jk.zones = object.variables[["jk.zones"]], jk.replicates = object.variables[["rep.ind"]], shortcut = action.arguments[["shortcut"]])
  } else if(imported.file.attributes[["lsa.study"]] %in% c("CivED", "ICCS", "ICILS", "SITES")) {
    data.object <- produce.jk.reps.data(data = data.object, weight.var = object.variables[["weight.var"]], jk.zones = object.variables[["jk.zones"]], jk.replicates = object.variables[["rep.ind"]], shortcut = TRUE)
  }
  return(data.object)
}
all.missing.values.combinations <- list("not administered", "not admin.", "not admin", "n. admin.", "noz admin", "missing", "MISSING", c("not admin.", "missing"), c("crossed out, not interpretable", "not reached", "not admin.", "missing"), c("crossed out, not interpretable", "not reached", "not admin.", "omitted"), c("not admin.", "omitted"), c("omitted", "not admin."), c("not reached", "two or more responses, not interpretable", "not admin."), c("not reached", "two or more responses, not interpretable", "not admin.", "omitted (blank only)"), c("not reached", "two or more responses, uninterpretable, ...", "not admin."), c("not reached", "two or more responses, uninterpretable", "not admin."), c("NOT REACHED", "TWO OR MORE RESPONSES, UNINTERPRETABLE", "MISSING (BLANK ONLY)"), c("not reached", "uninterpretable", "not admin."), c("not reached", "uninterpretable", "not admin"), c("n. rea.", "n. adm."), c("n. rea.", "n. admin."), c("n.reach.", "n. admin."), c("not appl.", "not admin."), c("not appl", "not admin"), c(" not appl.", "not admin."), c("not applicable", "not admin."), c("not reached", "not admin."), "n. adm.", c("n. reached", "n. admin."), c("n.rea.", "n. adm."), c("n. reach.", "n. admin."), c("not reached", "not admin"), c("log.not appl.", "not admin."), c("Not Applicable", "Not Administered"), c("don't know", "not admin.", "missing"), c("Not reached", "INVALID", "not admin."), c("not reached", "INVALID", "not admin."), c("INVALID", "OMITTED OR INVALID"), c("INVALID", "OMITTED"), "OMITTED", c("NOT REACHED", "INVALID", "OMITTED"), c("NOT REACHED", "MISSING"), c("MISSING", "LOGICALLY NOT APPLICABLE"), c("LOGICALLY NOT APPLICABLE", "MISSING"), c("LOGICALLY NOT APPLICABLE", "INVALID", "OMITTED"), c("LOGICALLY NOT APPLICABLE", "INVALID", "OMITTED OR INVALID"), c("Not administered/missing by design", "Presented but not answered/invalid"), c("Logically not applicable", "Not administered/missing by design", "Presented but not answered/invalid"), c("Not applicable", "Not stated"), c("Not reached", "Not administered/missing by design", "Presented but not answered/invalid"), c("Notadministered/missing by design", "Presented but not answered/invalid"), c("Not administered/missing by design", "Presented but not answered/invalid"), "Not administered or missing by design", "Presented but not answered or invalid", c("Not administered or missing by design", "Presented but not answered or invalid"), "OMITTED OR INVALID", "Omitted or invalid", c("LOGICALLY NOT APPLICABLE", "OMITTED OR INVALID"), c("Logically not applicable", "Omitted or invalid"), c("Omitted or invalid", "Logically not applicable"), c("NOT REACHED", "OMITTED"), c("Not reached", "Omitted"), c("LOGICALLY NOT APPLICABLE", "OMITTED"), c("OMITTED", "LOGICALLY NOT APPLICABLE"), c("LOGICALLY NOT APPLICABLE", "NOT REACHED", "OMITTED"), "NOT EXCLUDED", c("Not reached (default during data processing)", "Not administered", "Omitted or Invalid"), c("Logically not applicable", "Not reached", "Not administered", "Omitted or Invalid"), c("Default", "Not applicable", "Not stated"), c("Valid Skip", "Not Applicable", "Invalid", "No Response"), c("Valid Skip", "Not Reached", "Not Applicable", "Invalid", "No Response"), c("Not Applicable", "Invalid"), c("Not administered", "Omitted"), c("Invalid", "Not administered", "Omitted"), c("NOT REACHED", "INVALID RESPONSE", "OMITTED"), c("INVALID RESPONSE", "OMITTED"), c("Not reached", "Omitted or invalid"), c("Omitted or invalid", "Not reached"), c("Not applicable", "Omitted or invalid"), c("Not Reached", "Not Applicable", "Invalid", "No Response"))
fac.to.num.missing.codes <- c(99999991, 99999991, 99999991, 99999991, 99999991, 99999991, 99999991, 99999991, 99999991, 99999991, 99999992, 99999994, 99999995, 99999996, 99999996, 99999996, 99999996, 99999996, 99999996, 99999996, 99999996, 99999996, 99999996, 99999997, 99999997, 99999997, 99999997, 99999997, 99999997, 99999997, 99999997, 99999997, 99999997, 99999997, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999998, 99999999, 99999998, 99999998, 99999998, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999, 99999999)
names(fac.to.num.missing.codes) <- c(" not appl.", "Default", "log.not appl.", "LOGICALLY NOT APPLICABLE", "Logically not applicable", "not appl", "not appl.", "Not Applicable", "Not applicable", "not applicable", "don't know", "No Response", "Valid Skip", "n. rea.", "n. reach.", "n. reached", "n.rea.", "n.reach.", "NOT REACHED", "Not Reached", "Not reached", "not reached", "Not reached (default during data processing)", "crossed out, not interpretable", "INVALID", "Invalid", "INVALID RESPONSE", "Presented but not answered/invalid", "Presented but not answered or invalid", "two or more responses, not interpretable", "two or more responses, uninterpretable", "TWO OR MORE RESPONSES, UNINTERPRETABLE", "two or more responses, uninterpretable, ...", "uninterpretable", "n. adm.", "n. admin.", "not admin", "not admin.", "Not administered", "not administered", "Not administered/missing by design", "Not administered or missing by design", "Not stated", "Notadministered/missing by design", "noz admin", "missing", "MISSING", "MISSING (BLANK ONLY)", "NOT EXCLUDED", "OMITTED", "Omitted", "omitted", "omitted (blank only)", "OMITTED OR INVALID", "Omitted or invalid", "Omitted or Invalid")
get.analysis.and.design.vars <- function(x) {
  passed.args <- as.list(sys.call(which = -1))
  passed.args <- passed.args[c("split.vars", "bckg.var", "bckg.avg.vars", "bckg.prctls.vars", "bckg.corr.vars", "bckg.dep.var", "bckg.indep.cont.vars", "bckg.indep.cat.vars", "bin.dep.var", "PV.root.avg", "PV.root.prctls", "PV.root.bench", "PV.root.corr", "PV.root.dep", "PV.root.indep", "bckg.row.var", "bckg.col.var", "weight.var")]
  if(is.na(names(passed.args["weight.var"])) == TRUE) {
    if(attr(x, "study") %in% design.weight.variables[["IEA.JK2.studies"]]) {
      if(attr(x, "file.type") %in% design.weight.variables[["IEA.JK2.dflt.std.bckg.types"]]) {
        passed.args["weight.var"] <- intersect(design.weight.variables[["IEA.JK2.dflt.std.bckg.wgts"]], names(x))
        passed.args["jk.zones"] <- intersect(design.weight.variables[["IEA.JK2.dflt.std.bckg.zones"]], names(x))
        passed.args["rep.ind"] <- intersect(design.weight.variables[["IEA.JK2.dflt.std.bckg.rep.ind"]], names(x))
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
  if(all(unlist(unname(passed.args[!names(passed.args) %in% c("PV.root.avg", "PV.root.prctls", "PV.root.bench", "PV.root.corr", "PV.root.dep", "PV.root.indep")])) %in% names(x)) != TRUE
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
  passed.args <- passed.args[c("include.missing", "shortcut", "output.file", "open.output", "executed.analysis.function", "prctls")]
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
                      "TIMSS Advanced",
                      "TiPi"),
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
  eTIMSS = c(400, 475, 550, 625),
  preTIMSS = c(400, 475, 550, 625),
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
      model.stats <- summary(j)[c("r.squared", "df", "adj.r.squared", "fstatistic")]
      model.stats[["r.squared"]] <- data.table(V1 = "r.squared", V2 = model.stats[["r.squared"]])
      model.stats[["adj.r.squared"]] <- data.table(V1 = "adj.r.squared", V2 = model.stats[["adj.r.squared"]])
      model.stats[["fstatistic"]] <- data.table(V1 = "fstatistic", V2 = model.stats[["fstatistic"]][1])
      model.stats[["df"]] <- data.table(V1 = "df", V2 = model.stats[["df"]][2])
      model.stats <- rbindlist(model.stats, fill = TRUE)
      return(rbindlist(l = list(reg.coefficients, model.stats), fill = TRUE))
    })
    regression <- rbindlist(l = Map(f = cbind, key.cols.table, regression))
    new.var.names <- grep(pattern = paste(vars.vector, collapse = "|"), x = regression[ , V1], value = TRUE)
    regression[ , V1 := factor(x = V1, levels = c("(Intercept)", unique(new.var.names), "r.squared", "df", "adj.r.squared", "fstatistic"))]
    setkeyv(x = regression, cols = c(keys, "V1"))
    setnames(x = regression, old = "V2", new = i)
  })
  all.regressions <- Reduce(function(...) merge(..., all = TRUE), all.regressions)
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
        levels.j <- levels(j)
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
reshape.list.statistics.bckg <- function(estimate.object, estimate.name, bckg.vars.vector, weighting.variable, data.key.variables, new.names.vector, replication.weights, study.name, SE.design, multiply.columns = 1) {
  object.type <- deparse(substitute(estimate.object))
  if(object.type == "percentages" && !is.null(bckg.vars.vector)) {
    lapply(X = estimate.object, FUN = function(i) {
      setnames(x = i, grep(pattern = "V[[:digit:]]+", x = names(i), ignore.case = TRUE, value = TRUE), c(bckg.vars.vector, weighting.variable, replication.weights))
    })
  } else if(object.type == "percentages" && is.null(bckg.vars.vector)) {
    lapply(X = estimate.object, FUN = function(i) {
      setnames(x = i, grep(pattern = "V[[:digit:]]+", x = names(i), ignore.case = TRUE, value = TRUE), c(data.key.variables, weighting.variable, replication.weights))
    })
  } else if(object.type %in% c("bckg.means", "bckg.variances", "bckg.SDs")) {
    lapply(X = estimate.object, FUN = function(i) {
      setnames(x = i, grep(pattern = "V[[:digit:]]+", x = names(i), ignore.case = TRUE, value = TRUE), c(weighting.variable, replication.weights))
    })
  } else if(object.type == "bckg.prctls" && !is.null(bckg.vars.vector)) {
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
    if(study.name %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "TIMSS Advanced", "TiPi") && SE.design == FALSE) {
      i[ , paste0("_SE") := sqrt(0.5*sum.of.squares)]
    } else if(study.name %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "TIMSS Advanced", "TiPi") && SE.design == TRUE || study.name %in% c("CivED", "ICCS", "ICILS", "SITES")) {
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
  if(object.type == "bckg.prctls") {
    estimate.object <- lapply(X = estimate.object, FUN = function(i) {
      tmp <- split(x = i, by = c("Percentiles", "PRCTLS.VARS"))
      tmp <- lapply(X = tmp, FUN = function(j) {
        prctls.column <- grep(pattern = paste(paste0("Prctls_", bckg.vars.vector, "$"), collapse = "|"), x = colnames(j), value = TRUE)
        setnames(x = j, old = prctls.column, new = paste0(j[ , Percentiles][1], "_", j[ , PRCTLS.VARS][1]))
      })
      tmp <- lapply(X = tmp, FUN = function(j) {
        prctls.column <- grep(pattern = paste(paste0("Prctls_", bckg.vars.vector, "_SE$"), collapse = "|"), x = colnames(j), value = TRUE)
        setnames(x = j, old = prctls.column, new = paste0(j[ , Percentiles][1], "_", j[ , PRCTLS.VARS][1], "_SE"))
        j[ , c("Percentiles", "PRCTLS.VARS") := NULL]
      })
      tmp <- Reduce(function(...) merge(..., all = TRUE, no.dups = FALSE), tmp)
      return(tmp)
    })
    assign(x = "bckg.prctls", value = estimate.object, envir = parent.frame())
  } else {
    return(estimate.object)
  }
}
reshape.list.statistics.PV <- function(estimate.object, estimate.name, PV.vars.vector, weighting.variable, replication.weights, study.name, SE.design, multiply.columns = 1) {
  object.type <- deparse(substitute(estimate.object))
  if(object.type == "PV.prctls" && !is.null(PV.vars.vector)) {
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
      if(study.name %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "TIMSS Advanced", "TiPi") && SE.design == FALSE) {
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
  }
}
aggregate.PV.estimates <- function(estimate.object, estimate.name, root.PV, PV.vars.vector, data.key.variables, study.name, SE.design) {
  object.type <- deparse(substitute(estimate.object))
  if(study.name %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "TIMSS Advanced", "TiPi") && SE.design == FALSE || study.name %in% c("ICCS", "ICILS")) {
    lapply(X = estimate.object, FUN = function(i) {
      i[ , sampling.variance := Reduce(`+`, .SD), .SDcols = grep(pattern = "_SumSq", x = names(i))]
      i[ , sampling.variance := sampling.variance*(1/length(grep(pattern = "_SumSq", x = colnames(i))))]
    })
  } else if(study.name %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "TIMSS Advanced", "TiPi") && SE.design == TRUE || study.name %in% c("CivED", "SITES")) {
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
    if(study.name %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "TIMSS Advanced", "TiPi")) {
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
  all.estimate.columns <- grep(pattern = "^Percentages$|Percentages_.*|Mean_.*|Prctl_.*|Variance_.*|SD_.*|Correlation_|Coefficients|Odds_|Wald_|t_value|p_value|Estimate|Crosstab_", x = names(output.obj), value = TRUE)
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
                    if(!missing(used.shortcut) && study %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "TIMSS Advanced", "TiPi")) {
                      SHORTCUT = used.shortcut
                    },
                    NREPS = length(number.of.reps),
                    ANALYSIS_DATE = format(Sys.Date(), "%B %d, %Y"),
                    START_TIME = in.time,
                    END_TIME = format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"))
  if(!missing(used.shortcut) && study %in% c("PIRLS", "prePIRLS", "ePIRLS", "RLII", "TIMSS", "preTIMSS", "TIMSS Advanced", "TiPi")) {
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
export.results <- function(output.object, analysis.type, analysis.info.obj, model.stats.obj, destination.file, open.exported.file) {
  export.workbook <- createWorkbook(title = "Analysis created using RALSA (www.ralsa.ineri.org) provided by INERI (www.ineri.org)")
  header.row.style <- createStyle(fontColour = "#FFFFFF", bgFill = "#000000", border = c("left", "right"), borderColour = "#FFFFFF")
  call.cell.style <- createStyle(valign = "top", wrapText = TRUE)
  three.decimals.style <- createStyle(numFmt = "0.000")
  two.decimals.style <- createStyle(numFmt = "0.00")
  no.decimals.style <- createStyle(numFmt = "0")
  if(analysis.type %in% c("lsa.pcts.means", "lsa.prctls", "lsa.bench", "lsa.corr", "lsa.lin.reg", "lsa.bin.log.reg", "lsa.crosstabs")) {
    main.results.sheet.name <- "Estimates"
  }
  addWorksheet(wb = export.workbook, sheetName = main.results.sheet.name, tabColour = "#FF0000")
  setColWidths(wb = export.workbook, sheet = main.results.sheet.name, cols = 1:ncol(output.object), widths = "auto")
  addStyle(wb = export.workbook, sheet = main.results.sheet.name, style = header.row.style, rows = 1, cols = 1:ncol(output.object))
  freezePane(wb = export.workbook, sheet = main.results.sheet.name, firstRow = TRUE)
  cols.with.decimals <- names(Filter(is.numeric, output.object))
  cols.with.decimals <- grep(pattern = "n_Cases|CYCLE|NREPS|DURATION", x = cols.with.decimals, ignore.case = TRUE, invert = TRUE, value = TRUE)
  cols.with.decimals <- grep(pattern = paste(cols.with.decimals, collapse = "|"), x = colnames(output.object), ignore.case = TRUE)
  addStyle(wb = export.workbook, sheet = main.results.sheet.name, style = two.decimals.style, cols = cols.with.decimals, rows = 2:(nrow(output.object) + 1), gridExpand = TRUE)
  addStyle(wb = export.workbook, sheet = main.results.sheet.name, style = no.decimals.style, cols = grep(pattern = "n_cases", x = colnames(output.object)), rows = 2:(nrow(output.object) + 1), gridExpand = TRUE)
  addStyle(wb = export.workbook, sheet = main.results.sheet.name, style = three.decimals.style, cols = grep(pattern = "^p_", x = colnames(output.object)), rows = 2:(nrow(output.object) + 1), gridExpand = TRUE)
  writeData(wb = export.workbook, x = output.object, sheet = main.results.sheet.name)
  if(!missing(model.stats.obj)) {
    cols.with.decimals <- grep(pattern = paste(c("Estimate", "Estimate_SE", "t_value", "p_value"), collapse = "|"), x = colnames(model.stats.obj))
    addWorksheet(wb = export.workbook, sheetName = "Model statistics")
    writeData(wb = export.workbook, sheet = "Model statistics", x = model.stats.obj)
    addStyle(wb = export.workbook, sheet = "Model statistics", style = header.row.style, rows = 1, cols = 1:ncol(model.stats.obj))
    addStyle(wb = export.workbook, sheet = "Model statistics", style = two.decimals.style, cols = cols.with.decimals, rows = 2:(nrow(model.stats.obj) + 1), gridExpand = TRUE)
    setColWidths(wb = export.workbook, sheet = "Model statistics", cols = 1:ncol(model.stats.obj), widths = "auto")
  }
  addWorksheet(wb = export.workbook, sheetName = "Analysis information")
  writeData(wb = export.workbook, sheet = "Analysis information", x = analysis.info.obj)
  addStyle(wb = export.workbook, sheet = "Analysis information", style = header.row.style, rows = 1, cols = 1:ncol(analysis.info.obj))
  setColWidths(wb = export.workbook, sheet = "Analysis information", cols = 1:ncol(analysis.info.obj), widths = "auto")
  called.analysis.function <- paste0(gsub(pattern = "\\s+", replacement = " ", x = deparse(sys.call(1))))
  addWorksheet(wb = export.workbook, "Calling syntax")
  setRowHeights(wb = export.workbook, sheet = "Calling syntax", rows = 2, heights = 75)
  setColWidths(wb = export.workbook, sheet = "Calling syntax", cols = 1, widths = 120)
  addStyle(wb = export.workbook, sheet = "Calling syntax", style = call.cell.style, rows = 2, cols = 1)
  writeData(wb = export.workbook, sheet = "Calling syntax", x = "The following call can be used in R/Rstudio to perform the same analysis again:", startCol = 1, startRow = 1)
  writeData(wb = export.workbook, sheet = "Calling syntax", x = paste(called.analysis.function, collapse = ""), startCol = 1, startRow = 2)
  if(missing(destination.file)) {
    destination.file <- file.path(getwd(), "Analysis.xlsx")
  }
  does.file.exist <- file.exists(destination.file)
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

#############################################
# Global shiny object
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
