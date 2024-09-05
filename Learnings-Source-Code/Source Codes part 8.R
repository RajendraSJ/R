library(praise)
library(progress)
library(progressr)
library(purrr)
library(Rcrawler)

library(praise)
praise
list(`package:praise` = function (template = "You are ${adjective}!") 
{
  while (is_template(template)) {
    template <- replace_one_template(template)
  }
  template
}, function (template = "You are ${adjective}!") 
{
  while (is_template(template)) {
    template <- replace_one_template(template)
  }
  template
})
c("package:praise", "namespace:praise")
c(TRUE, FALSE)
c(FALSE, TRUE)
praise_parts
list(`package:praise` = list(adjective = c("ace", "amazing", "astonishing", "astounding", "awe-inspiring", "awesome", "badass", "beautiful", "bedazzling", "bee's knees", "best", "breathtaking", "brilliant", "cat's meow", "cat's pajamas", "classy", "cool", "dandy", "dazzling", "delightful", "divine", "doozie", "epic", "excellent", "exceptional", "exquisite", "extraordinary", "fabulous", "fantastic", "fantabulous", "fine", "finest", "first-class", "first-rate", "flawless", "funkadelic", "geometric", 
                                           "glorious", "gnarly", "good", "grand", "great", "groovy", "groundbreaking", "hunky-dory", "impeccable", "impressive", "incredible", "kickass", "kryptonian", "laudable", "legendary", "lovely", "luminous", "magnificent", "majestic", "marvelous", "mathematical", "mind-blowing", "neat", "outstanding", "peachy", "perfect", "phenomenal", "pioneering", "polished", "posh", "praiseworthy", "premium", "priceless", "prime", "primo", "rad", "remarkable", "riveting", "sensational", "shining", "slick", "smashing", 
                                           "solid", "spectacular", "splendid", "stellar", "striking", "stunning", "stupendous", "stylish", "sublime", "super", "super-duper", "super-excellent", "superb", "superior", "supreme", "swell", "terrific", "tiptop", "top-notch", "transcendent", "tremendous", "ultimate", "unreal", "well-made", "wicked", "wonderful", "wondrous", "world-class"), adverb = c("beautifully", "bravely", "brightly", "calmly", "carefully", "cautiously", "cheerfully", "clearly", "correctly", "courageously", "daringly", "deliberately", 
                                                                                                                                                                                                                                                                                                                                                                                                             "doubtfully", "eagerly", "easily", "elegantly", "enormously", "enthusiastically", "faithfully", "fast", "fondly", "fortunately", "frankly", "frantically", "generously", "gently", "gladly", "gracefully", "happily", "healthily", "honestly", "joyously", "justly", "kindly", "neatly", "openly", "patiently", "perfectly", "politely", "powerfully", "quickly", "quietly", "rapidly", "really", "regularly", "repeatedly", "rightfully", "seriously", "sharply", "smoothly", "speedily", "successfully", "swiftly", 
                                                                                                                                                                                                                                                                                                                                                                                                             "tenderly", "thoughtfully", "truthfully", "warmly", "well", "wisely"), adverb_manner = c("beautifully", "bravely", "brightly", "calmly", "carefully", "cautiously", "cheerfully", "clearly", "correctly", "courageously", "daringly", "deliberately", "doubtfully", "eagerly", "easily", "elegantly", "enormously", "enthusiastically", "faithfully", "fast", "fondly", "fortunately", "frankly", "frantically", "generously", "gently", "gladly", "gracefully", "happily", "healthily", "honestly", "joyously", "justly", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "kindly", "neatly", "openly", "patiently", "perfectly", "politely", "powerfully", "quickly", "quietly", "rapidly", "really", "regularly", "repeatedly", "rightfully", "seriously", "sharply", "smoothly", "speedily", "successfully", "swiftly", "tenderly", "thoughtfully", "truthfully", "warmly", "well", "wisely"), created = c("assembled", "brewed", "built", "created", "composed", "constructed", "designed", "devised", "forged", "formed", "initiated", "invented", "made", "organized", "planned", "prepared", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "set up"), creating = c("assembling", "brewing", "building", "creating", "composing", "constructing", "designing", "devising", "forging", "forming", "initiating", "inventing", "making", "organizing", "planning", "preparin", "setting up"), exclamation = c("ah", "aha", "ahh", "ahhh", "aw", "aww", "awww", "aye", "gee", "ha", "hah", "hmm", "ho-ho", "huh", "heh", "hooray", "hurrah", "hurray", "huzzah", "mhm", "mm", "mmh", "mmhm", "mmm", "oh", "ole", "uh-hu", "wee", "whee", "whoa", "wow", "wowie", "yahoo", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         "yay", "yeah", "yee-haw", "yikes", "yippie", "yow", "yowza"), rpackage = c("code", "library (or package?)", "package", "program", "project", "software", "R package")), list(adjective = c("ace", "amazing", "astonishing", "astounding", "awe-inspiring", "awesome", "badass", "beautiful", "bedazzling", "bee's knees", "best", "breathtaking", "brilliant", "cat's meow", "cat's pajamas", "classy", "cool", "dandy", "dazzling", "delightful", "divine", "doozie", "epic", "excellent", "exceptional", "exquisite", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "extraordinary", "fabulous", "fantastic", "fantabulous", "fine", "finest", "first-class", "first-rate", "flawless", "funkadelic", "geometric", "glorious", "gnarly", "good", "grand", "great", "groovy", "groundbreaking", "hunky-dory", "impeccable", "impressive", "incredible", "kickass", "kryptonian", "laudable", "legendary", "lovely", "luminous", "magnificent", "majestic", "marvelous", "mathematical", "mind-blowing", "neat", "outstanding", "peachy", "perfect", "phenomenal", "pioneering", "polished", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "posh", "praiseworthy", "premium", "priceless", "prime", "primo", "rad", "remarkable", "riveting", "sensational", "shining", "slick", "smashing", "solid", "spectacular", "splendid", "stellar", "striking", "stunning", "stupendous", "stylish", "sublime", "super", "super-duper", "super-excellent", "superb", "superior", "supreme", "swell", "terrific", "tiptop", "top-notch", "transcendent", "tremendous", "ultimate", "unreal", "well-made", "wicked", "wonderful", "wondrous", "world-class"), adverb = c("beautifully", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "bravely", "brightly", "calmly", "carefully", "cautiously", "cheerfully", "clearly", "correctly", "courageously", "daringly", "deliberately", "doubtfully", "eagerly", "easily", "elegantly", "enormously", "enthusiastically", "faithfully", "fast", "fondly", "fortunately", "frankly", "frantically", "generously", "gently", "gladly", "gracefully", "happily", "healthily", "honestly", "joyously", "justly", "kindly", "neatly", "openly", "patiently", "perfectly", "politely", "powerfully", "quickly", "quietly", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "rapidly", "really", "regularly", "repeatedly", "rightfully", "seriously", "sharply", "smoothly", "speedily", "successfully", "swiftly", "tenderly", "thoughtfully", "truthfully", "warmly", "well", "wisely"), adverb_manner = c("beautifully", "bravely", "brightly", "calmly", "carefully", "cautiously", "cheerfully", "clearly", "correctly", "courageously", "daringly", "deliberately", "doubtfully", "eagerly", "easily", "elegantly", "enormously", "enthusiastically", "faithfully", "fast", "fondly", "fortunately", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "frankly", "frantically", "generously", "gently", "gladly", "gracefully", "happily", "healthily", "honestly", "joyously", "justly", "kindly", "neatly", "openly", "patiently", "perfectly", "politely", "powerfully", "quickly", "quietly", "rapidly", "really", "regularly", "repeatedly", "rightfully", "seriously", "sharply", "smoothly", "speedily", "successfully", "swiftly", "tenderly", "thoughtfully", "truthfully", "warmly", "well", "wisely"), created = c("assembled", "brewed", "built", "created", "composed", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "constructed", "designed", "devised", "forged", "formed", "initiated", "invented", "made", "organized", "planned", "prepared", "set up"), creating = c("assembling", "brewing", "building", "creating", "composing", "constructing", "designing", "devising", "forging", "forming", "initiating", "inventing", "making", "organizing", "planning", "preparin", "setting up"), exclamation = c("ah", "aha", "ahh", "ahhh", "aw", "aww", "awww", "aye", "gee", "ha", "hah", "hmm", "ho-ho", "huh", "heh", "hooray", "hurrah", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "hurray", "huzzah", "mhm", "mm", "mmh", "mmhm", "mmm", "oh", "ole", "uh-hu", "wee", "whee", "whoa", "wow", "wowie", "yahoo", "yay", "yeah", "yee-haw", "yikes", "yippie", "yow", "yowza"), rpackage = c("code", "library (or package?)", "package", "program", "project", "software", "R package")))
c("package:praise", "namespace:praise")
c(TRUE, FALSE)
c(FALSE, TRUE)
> 
#############################################################################################
library(progress)
progress_bar
list(`package:progress` = <environment>, function (type, con) 
{
  bar <- NULL
  show_progress <- function(down, up) {
    if (type == "down") {
      total <- down[[1]]
      now <- down[[2]]
    }
    else {
      total <- up[[1]]
      now <- up[[2]]
    }
    if (total == 0 && now == 0) {
      bar <<- NULL
    }
    else if (total == 0) {
      cat("\rDownloading: ", bytes(now, digits = 2), "     ", sep = "", file = con)
      utils::flush.console()
    }
    else {
      if (is.null(bar)) {
        bar <<- utils::txtProgressBar(max = total, style = 3, file = con)
      }
      utils::setTxtProgressBar(bar, now)
      if (now == total) 
        close(bar)
    }
    TRUE
  }
  show_progress
}, <environment>)
c("package:progress", "namespace:httr", "namespace:progress")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
> 


##############################################################################################
library(progressr)

handler_ascii_alert : function (symbol = "\a", file = stderr(), intrusiveness = getOption("progressr.intrusiveness.auditory", 
                                                                                          5), target = c("terminal", "audio"), ...)  
  handler_beepr : function (initiate = 2L, update = 10L, finish = 11L, intrusiveness = getOption("progressr.intrusiveness.auditory", 
                                                                                                 5), target = "audio", ...)  
    handler_debug : function (interval = getOption("progressr.interval", 0), intrusiveness = getOption("progressr.intrusiveness.debug", 
                                                                                                       0), target = "terminal", uuid = FALSE, ...)  
      handler_filesize : function (file = "default.progress", intrusiveness = getOption("progressr.intrusiveness.file", 
                                                                                        5), target = "file", ...)  
        handler_newline : function (symbol = "\n", file = stderr(), intrusiveness = getOption("progressr.intrusiveness.debug", 
                                                                                              0), target = "terminal", ...)  
          handler_notifier : function (intrusiveness = getOption("progressr.intrusiveness.notifier", 10), target = "gui", 
                                       ...)  
            handler_pbcol : function (adjust = 0, pad = 1L, complete = function(s) crayon::bgBlue(crayon::white(s)), 
                                      incomplete = function(s) crayon::bgCyan(crayon::white(s)), intrusiveness = getOption("progressr.intrusiveness.terminal", 
                                                                                                                           1), target = "terminal", ...)  
              handler_pbmcapply : function (substyle = 3L, style = "ETA", file = stderr(), intrusiveness = getOption("progressr.intrusiveness.terminal", 
                                                                                                                     1), target = "terminal", ...)  
                handler_progress : function (format = ":spin [:bar] :percent :message", show_after = 0, intrusiveness = getOption("progressr.intrusiveness.terminal", 
                                                                                                                                  1), target = "terminal", ...)  
                  handler_rstudio : function (intrusiveness = getOption("progressr.intrusiveness.gui", 1), target = "gui", 
                                              title = function() format(Sys.time(), "Console %X"), ...)  
                    handler_shiny : function (intrusiveness = getOption("progressr.intrusiveness.gui", 1), target = "gui", 
                                              inputs = list(message = NULL, detail = "message"), ...)  
                      handler_tkprogressbar : function (intrusiveness = getOption("progressr.intrusiveness.gui", 1), target = "terminal", 
                                                        ...)  
                        handler_txtprogressbar : function (style = 3L, file = stderr(), intrusiveness = getOption("progressr.intrusiveness.terminal", 
                                                                                                                  1), target = "terminal", ...)  
                          handler_void : function (intrusiveness = 0, target = "void", enable = FALSE, ...)  
                            handler_winprogressbar : function (intrusiveness = getOption("progressr.intrusiveness.gui", 1), target = "gui", 
                                                               ...)  
                              handlers : function (..., append = FALSE, on_missing = c("error", "warning", "ignore"), default = handler_txtprogressbar, 
                                                   global = NULL)  
                                make_progression_handler : function (name, reporter = list(), handler = NULL, enable = getOption("progressr.enable", 
                                                                                                                                 interactive()), enable_after = getOption("progressr.enable_after", 0), times = getOption("progressr.times", 
                                                                                                                                                                                                                          +Inf), interval = getOption("progressr.interval", 0), intrusiveness = 1, clear = getOption("progressr.clear", 
                                                                                                                                                                                                                                                                                                                     TRUE), target = "terminal", ...)  
                                  progress : function (..., call = sys.call())  
                                    progress_aggregator : function (progress)  
                                      progress_progressr : function (...)  
                                        progression : function (message = character(0L), amount = 1, step = NULL, time = progression_time, 
                                                                ..., type = "update", class = NULL, progressor_uuid = NULL, progression_index = NULL, 
                                                                progression_time = Sys.time(), call = NULL, calls = sys.calls(), owner_session_uuid = NULL)  
                                          progressor : function (steps = length(along), along = NULL, offset = 0L, scale = 1L, transform = function(steps) scale * 
                                                                   steps + offset, message = character(0L), label = NA_character_, trace = FALSE, 
                                                                 initiate = TRUE, auto_finish = TRUE, on_exit = !identical(envir, globalenv()), 
                                                                 enable = getOption("progressr.enable", TRUE), envir = parent.frame())  
                                            slow_sum : function (x, delay = getOption("progressr.demo.delay", 1), stdout = FALSE, message = TRUE)  
                                              with_progress : function (expr, handlers = progressr::handlers(), cleanup = TRUE, delay_terminal = NULL, 
                                                                        delay_stdout = NULL, delay_conditions = NULL, interrupts = getOption("progressr.interrupts", 
                                                                                                                                             TRUE), interval = NULL, enable = NULL)  
                                                without_progress : function (expr)  
                                                  withProgressShiny : function (expr, ..., message = NULL, detail = NULL, inputs = list(message = NULL, 
                                                                                                                                        detail = "message"), env = parent.frame(), quoted = FALSE, handlers = c(shiny = handler_shiny, 
                                                                                                                                                                                                                mprintf("interrupt_reporter() ... skipping; not supported")
                                                                                                                                                                                                                return()
                                                                                                                                                                                                                }
do.call(reporter$interrupt, args = args)
.validate_internal_state("interrupt_reporter() ... done")
if (debug) 
  mprintf("interrupt_reporter() ... done")
}
finish_reporter <- function(p) {
  args <- reporter_args(progression = p)
  debug <- getOption("progressr.debug", FALSE)
  if (debug) {
    mprintf("finish_reporter() ...")
    mstr(args)
  }
  if (active && !finished) {
    do.call(reporter$finish, args = args)
  }
  else {
    if (debug) {
      why <- if (!active && !finished) {
        "not active"
      }
      else if (!active && finished) {
        "not active and already finished"
      }
      else if (active && finished) {
        "already finished"
      }
      message(sprintf("- Hmm ... got a request to 'finish' handler, but it's %s. Oh well, will finish it then", why))
    }
  }
  reset_internal_state()
  finished <<- TRUE
  if (debug) 
    message("- owner: ", deparse(owner))
  .validate_internal_state("finish_reporter() ... done")
  if (debug) 
    mprintf("finish_reporter() ... done")
}
is_owner <- function(p) {
  progressor_uuid <- p[["progressor_uuid"]]
  if (is.null(owner)) 
    owner <<- progressor_uuid
  (owner == progressor_uuid)
}
is_duplicated <- function(p) {
  progressor_uuid <- p[["progressor_uuid"]]
  session_uuid <- p[["session_uuid"]]
  progression_index <- p[["progression_index"]]
  progression_time <- p[["progression_time"]]
  progression_id <- sprintf("%s-%d-%s", session_uuid, progression_index, progression_time)
  db <- done[["progressor_uuid"]]
  res <- is.element(progression_id, db)
  if (!res) {
    db <- c(db, progression_id)
    done[["progressor_uuid"]] <<- db
  }
  res
}
if (is.null(handler)) {
  handler <- function(p) {
    stop_if_not(inherits(p, "progression"))
    if (is_fork_child()) 
      return(invisible(FALSE))
    if (inherits(p, "control_progression")) {
      type <- p[["type"]]
      if (type == "reset") {
        reset_internal_state()
        reset_reporter(p)
        .validate_internal_state(sprintf("handler(type=%s) ... end", type))
      }
      else if (type == "shutdown") {
        finish_reporter(p)
        .validate_internal_state(sprintf("handler(type=%s) ... end", type))
      }
      else if (type == "hide") {
        hide_reporter(p)
        .validate_internal_state(sprintf("handler(type=%s) ... end", type))
      }
      else if (type == "unhide") {
        unhide_reporter(p)
        .validate_internal_state(sprintf("handler(type=%s) ... end", type))
      }
      else if (type == "interrupt") {
        interrupt_reporter(p)
        .validate_internal_state(sprintf("handler(type=%s) ... end", type))
      }
      else {
        stop("Unknown 'control_progression' type: ", sQuote(type))
      }
      .validate_internal_state(sprintf("control_progression ... end", type))
      return(invisible(finished))
    }
    debug <- getOption("progressr.debug", FALSE)
    if (!is_owner(p)) {
      if (debug) 
        message("- not owner of this progression. Skipping")
      return(invisible(finished))
    }
    duplicated <- is_duplicated(p)
    type <- p[["type"]]
    if (debug) {
      mprintf("Progression calling handler %s ...", sQuote(type))
      mprintf("- progression:")
      mstr(p)
      mprintf("- progressor_uuid: %s", p[["progressor_uuid"]])
      mprintf("- progression_index: %s", p[["progression_index"]])
      mprintf("- duplicated: %s", duplicated)
    }
    if (duplicated) {
      if (debug) 
        mprintf("Progression calling handler %s ... condition already done", sQuote(type))
      return(invisible(finished))
    }
    else if (active && finished) {
      if (debug) 
        mprintf("Progression calling handler %s ... active but already finished", sQuote(type))
      return(invisible(finished))
    }
    if (type == "initiate") {
      if (active) {
        if (debug) 
          message("- cannot 'initiate' handler, because it is already active")
        return(invisible(finished))
      }
      max_steps <<- p[["steps"]]
      if (debug) 
        mstr(list(max_steps = max_steps))
      stop_if_not(!is.null(max_steps), is.numeric(max_steps), length(max_steps) == 1, max_steps >= 0)
      auto_finish <<- p[["auto_finish"]]
      times <- min(times, max_steps)
      if (debug) 
        mstr(list(auto_finish = auto_finish, times = times, interval = interval, intrusiveness = intrusiveness))
      times <- min(c(times/intrusiveness, max_steps), na.rm = TRUE)
      times <- max(times, 1)
      interval <- interval * intrusiveness
      if (debug) 
        mstr(list(times = times, interval = interval))
      milestones <<- if (times == 1) {
        c(max_steps)
      }
      else if (times == 2) {
        c(0, max_steps)
      }
      else {
        seq(from = 0, to = max_steps, length.out = times + 1)[-1]
      }
      timestamps <<- rep(as.POSIXct(NA), times = max_steps)
      timestamps[1] <<- Sys.time()
      step <<- 0
      message <<- character(0)
      if (debug) 
        mstr(list(finished = finished, milestones = milestones))
      initiate_reporter(p)
      prev_milestone <<- step
      .validate_internal_state(sprintf("handler(type=%s) ... end", type))
    }
    else if (type == "finish") {
      if (debug) 
        mstr(list(finished = finished, milestones = milestones))
      finish_reporter(p)
      .validate_internal_state("type=finish")
    }
    else if (type == "update") {
      if (!active) {
        if (debug) 
          message("- cannot 'update' handler, because it is not active")
        return(invisible(finished))
      }
      if (debug) 
        mstr(list(step = step, `p$amount` = p[["amount"]], `p$step` = p[["step"]], max_steps = max_steps))
      if (!is.null(p[["step"]])) {
        p[["amount"]] <- p[["step"]] - step
      }
      step <<- min(max(step + p[["amount"]], 0), max_steps)
      stop_if_not(step >= 0)
      msg <- conditionMessage(p)
      if (length(msg) > 0) 
        message <<- msg
      if (step > 0) 
        timestamps[step] <<- Sys.time()
      if (debug) 
        mstr(list(finished = finished, step = step, milestones = milestones, prev_milestone = prev_milestone, interval = interval))
      .validate_internal_state("type=update")
      if ((length(milestones) > 0 && step >= milestones[1]) || p[["amount"]] == 0) {
        skip <- FALSE
        if (interval > 0 && step > 0) {
          dt <- difftime(timestamps[step], timestamps[max(prev_milestone, 1)], units = "secs")
          skip <- (dt < interval)
          if (debug) 
            mstr(list(dt = dt, timestamps[step], timestamps[prev_milestone], skip = skip))
        }
        if (!skip) {
          if (debug) 
            mstr(list(milestones = milestones))
          update_reporter(p)
          if (p[["amount"]] > 0) 
            prev_milestone <<- step
        }
        if (p[["amount"]] > 0) {
          milestones <<- milestones[milestones > step]
          if (auto_finish && step == max_steps) {
            if (debug) 
              mstr(list(type = "finish (auto)", milestones = milestones))
            finish_reporter(p)
          }
        }
      }
      .validate_internal_state(sprintf("handler(type=%s) ... end", type))
    }
    else {
      if (type %in% c("reset", "shutdown", "hide", "unhide", "interrupt")) {
        stop("Unsupported 'progression' type. Was it meant to be a 'control_progression' condition?: ", sQuote(type))
      }
      else {
        stop("Unknown 'progression' type: ", sQuote(type))
      }
    }
    .validate_internal_state(sprintf("handler() ... end", type))
    if (debug) 
      mprintf("Progression calling handler %s ... done", sQuote(type))
    invisible(finished)
  }
}
class(handler) <- c(sprintf("%s_progression_handler", name), "progression_handler", "calling_handler", class(handler))
handler
})
c("package:progressr", "namespace:progressr")
c(TRUE, FALSE)
c(FALSE, TRUE)
progress
list(`package:progressr` = function (..., call = sys.call()) 
{
  action <- getOption("progressr.lifecycle.progress", "defunct")
  signal <- switch(action, deprecated = .Deprecated, defunct = .Defunct)
  signal(msg = sprintf("progress() is %s", action), package = .packageName)
  args <- list(...)
  if (length(args) == 1 && inherits(args[[1]], "condition")) {
    cond <- args[[1]]
    stop_if_not(inherits(cond, "progression"))
  }
  else {
    cond <- progression(..., call = call)
  }
  withRestarts(signalCondition(cond), muffleProgression = function(p) NULL)
  invisible(cond)
}, function (type = c("down", "up"), con = stdout()) 
{
  type <- match.arg(type)
  request(options = list(noprogress = FALSE, progressfunction = progress_bar(type, con)))
}, function (..., call = sys.call()) 
{
  action <- getOption("progressr.lifecycle.progress", "defunct")
  signal <- switch(action, deprecated = .Deprecated, defunct = .Defunct)
  signal(msg = sprintf("progress() is %s", action), package = .packageName)
  args <- list(...)
  if (length(args) == 1 && inherits(args[[1]], "condition")) {
    cond <- args[[1]]
    stop_if_not(inherits(cond, "progression"))
  }
  else {
    cond <- progression(..., call = call)
  }
  withRestarts(signalCondition(cond), muffleProgression = function(p) NULL)
  invisible(cond)
})
c("package:progressr", "namespace:httr", "namespace:progressr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
progress_aggregator
list(`package:progressr` = function (progress) 
{
  stop_if_not(inherits(progress, "progressor"))
  max_steps <- environment(progress)$steps
  handler <- function(p) {
    stop_if_not(inherits(p, "progression"))
    type <- p$type
    debug <- getOption("progressr.debug", FALSE)
    if (debug) {
      mprintf("Progression handler %s ...", sQuote(type))
      on.exit(mprintf("Progression handler %s ... done", sQuote(type)))
      mprintf("- progression:")
      mstr(p)
      mprintf("- progressor_uuid: %s", p$progressor_uuid)
      mprintf("- progression_index: %d", p$progression_index)
    }
    if (type == "initiate") {
    }
    else if (type == "finish") {
    }
    else if (type == "reset") {
    }
    else if (type == "shutdown") {
    }
    else if (type == "update") {
      progress(child = p)
    }
    else {
      if (type %in% c("reset", "shutdown", "hide", "unhide", "interrupt")) {
        stop("Unsupported 'progression' type. Was it meant to be a 'control_progression' condition?: ", sQuote(type))
      }
      else {
        stop("Unknown 'progression' type: ", sQuote(type))
      }
    }
    invokeRestart("muffleProgression")
  }
  handler <- make_progression_handler("progress_aggregator", handler = handler)
  fcn <- function(...) {
    with_progress(..., handlers = handler)
  }
  class(fcn) <- c("progress_aggregator", class(fcn))
  fcn
}, function (progress) 
{
  stop_if_not(inherits(progress, "progressor"))
  max_steps <- environment(progress)$steps
  handler <- function(p) {
    stop_if_not(inherits(p, "progression"))
    type <- p$type
    debug <- getOption("progressr.debug", FALSE)
    if (debug) {
      mprintf("Progression handler %s ...", sQuote(type))
      on.exit(mprintf("Progression handler %s ... done", sQuote(type)))
      mprintf("- progression:")
      mstr(p)
      mprintf("- progressor_uuid: %s", p$progressor_uuid)
      mprintf("- progression_index: %d", p$progression_index)
    }
    if (type == "initiate") {
    }
    else if (type == "finish") {
    }
    else if (type == "reset") {
    }
    else if (type == "shutdown") {
    }
    else if (type == "update") {
      progress(child = p)
    }
    else {
      if (type %in% c("reset", "shutdown", "hide", "unhide", "interrupt")) {
        stop("Unsupported 'progression' type. Was it meant to be a 'control_progression' condition?: ", sQuote(type))
      }
      else {
        stop("Unknown 'progression' type: ", sQuote(type))
      }
    }
    invokeRestart("muffleProgression")
  }
  handler <- make_progression_handler("progress_aggregator", handler = handler)
  fcn <- function(...) {
    with_progress(..., handlers = handler)
  }
  class(fcn) <- c("progress_aggregator", class(fcn))
  fcn
})
c("package:progressr", "namespace:progressr")
c(TRUE, FALSE)
c(FALSE, TRUE)
progress_progressr
list(`package:progressr` = function (...) 
{
  p <- NULL
  list(init = function(x, ...) {
    p <<- progressor(x, on_exit = FALSE)
  }, step = function() {
    p()
  }, term = function() {
    p(type = "finish")
  })
}, function (...) 
{
  p <- NULL
  list(init = function(x, ...) {
    p <<- progressor(x, on_exit = FALSE)
  }, step = function() {
    p()
  }, term = function() {
    p(type = "finish")
  })
})
c("package:progressr", "namespace:progressr")
c(TRUE, FALSE)
c(FALSE, TRUE)
progression
list(`package:progressr` = function (message = character(0), amount = 1, step = NULL, time = progression_time, ..., type = "update", class = NULL, progressor_uuid = NULL, progression_index = NULL, progression_time = Sys.time(), call = NULL, calls = sys.calls(), owner_session_uuid = NULL) 
{
  amount <- as.numeric(amount)
  time <- as.POSIXct(time)
  stop_if_not(is.character(type), length(type) == 1, !is.na(type))
  class <- as.character(class)
  if (inherits(progression_time, "POSIXct")) {
    progression_time <- format(progression_time, format = "%F %H:%M:%OS3 %z")
  }
  stop_if_not(length(progression_time) == 1, is.character(progression_time))
  args <- list(...)
  nargs <- length(args)
  if (nargs > 0) {
    names <- names(args)
    stop_if_not(!is.null(names), all(nzchar(names)), length(unique(names)) == nargs)
  }
  structure(list(owner_session_uuid = owner_session_uuid, progressor_uuid = progressor_uuid, session_uuid = session_uuid(), progression_index = progression_index, progression_time = progression_time, type = type, message = message, amount = amount, step = step, time = time, ..., call = call, calls = calls), class = c(class, "progression", "immediateCondition", "condition"))
}, function (message = character(0), amount = 1, step = NULL, time = progression_time, ..., type = "update", class = NULL, progressor_uuid = NULL, progression_index = NULL, progression_time = Sys.time(), call = NULL, calls = sys.calls(), owner_session_uuid = NULL) 
{
  amount <- as.numeric(amount)
  time <- as.POSIXct(time)
  stop_if_not(is.character(type), length(type) == 1, !is.na(type))
  class <- as.character(class)
  if (inherits(progression_time, "POSIXct")) {
    progression_time <- format(progression_time, format = "%F %H:%M:%OS3 %z")
  }
  stop_if_not(length(progression_time) == 1, is.character(progression_time))
  args <- list(...)
  nargs <- length(args)
  if (nargs > 0) {
    names <- names(args)
    stop_if_not(!is.null(names), all(nzchar(names)), length(unique(names)) == nargs)
  }
  structure(list(owner_session_uuid = owner_session_uuid, progressor_uuid = progressor_uuid, session_uuid = session_uuid(), progression_index = progression_index, progression_time = progression_time, type = type, message = message, amount = amount, step = step, time = time, ..., call = call, calls = calls), class = c(class, "progression", "immediateCondition", "condition"))
})
c("package:progressr", "namespace:progressr")
c(TRUE, FALSE)
c(FALSE, TRUE)
progressor
list(`package:progressr` = function (steps = length(along), along = NULL, offset = 0, scale = 1, transform = function(steps) scale * steps + offset, message = character(0), label = NA, trace = FALSE, initiate = TRUE, auto_finish = TRUE, on_exit = !identical(envir, globalenv()), enable = getOption("progressr.enable", TRUE), envir = parent.frame()) 
{
  stop_if_not(is.logical(enable), length(enable) == 1, !is.na(enable))
  if (!enable) 
    return(void_progressor)
  stop_if_not(!is.null(steps) || !is.null(along))
  stop_if_not(length(steps) == 1, is.numeric(steps), !is.na(steps), steps >= 0)
  stop_if_not(length(offset) == 1, is.numeric(offset), !is.na(offset))
  stop_if_not(length(scale) == 1, is.numeric(scale), !is.na(scale))
  stop_if_not(is.function(transform))
  label <- as.character(label)
  stop_if_not(length(label) == 1)
  steps <- transform(steps)
  stop_if_not(length(steps) == 1, is.numeric(steps), !is.na(steps), steps >= 0)
  stop_if_not(is.logical(on_exit), length(on_exit) == 1, !is.na(on_exit))
  if (identical(envir, globalenv())) {
    if (!progressr_in_globalenv()) {
      stop("A progressor must not be created in the global environment unless wrapped in a with_progress() or without_progress() call. Alternatively, create it inside a function or in a local() environment to make sure there is a finite life span of the progressor")
    }
    if (on_exit) {
      stop("It is not possible to create a progressor in the global environment with on_exit = TRUE")
    }
  }
  owner_session_uuid <- session_uuid(attributes = TRUE)
  progressor_count <<- progressor_count + 1
  progressor_uuid <- progressor_uuid(progressor_count)
  progression_index <- 0
  fcn <- function(message = character(0), ..., type = "update") {
    progression_index <<- progression_index + 1
    cond <- progression(type = type, message = message, ..., progressor_uuid = progressor_uuid, progression_index = progression_index, owner_session_uuid = owner_session_uuid, call = if (trace) 
      sys.call()
      else NULL, calls = if (trace) 
        sys.calls()
      else NULL)
    withRestarts(signalCondition(cond), muffleProgression = function(p) NULL)
    invisible(cond)
  }
  formals(fcn)$message <- message
  class(fcn) <- c("progressor", class(fcn))
  progressor_envir <- new.env(parent = getNamespace(.packageName))
  for (name in c("progression_index", "progressor_uuid", "owner_session_uuid", "progressor_count", "enable", "initiate", "auto_finish", "trace", "steps", "label", "offset", "scale")) {
    progressor_envir[[name]] <- get(name)
  }
  environment(fcn) <- progressor_envir
  if (exists("...progressor", mode = "function", envir = envir)) {
    ...progressor <- get("...progressor", mode = "function", envir = envir)
    ...progressor(type = "finish")
    do.call(unlockBinding, args = list("...progressor", env = envir))
    rm("...progressor", envir = envir)
  }
  if (initiate) {
    fcn(type = "initiate", steps = steps, auto_finish = auto_finish)
  }
  if (on_exit) {
    assign("...progressor", value = fcn, envir = envir)
    lockBinding("...progressor", env = envir)
    call <- call("...progressor", type = "finish")
    do.call(base::on.exit, args = list(call, add = TRUE), envir = envir)
  }
  fcn
}, function (steps = length(along), along = NULL, offset = 0, scale = 1, transform = function(steps) scale * steps + offset, message = character(0), label = NA, trace = FALSE, initiate = TRUE, auto_finish = TRUE, on_exit = !identical(envir, globalenv()), enable = getOption("progressr.enable", TRUE), envir = parent.frame()) 
{
  stop_if_not(is.logical(enable), length(enable) == 1, !is.na(enable))
  if (!enable) 
    return(void_progressor)
  stop_if_not(!is.null(steps) || !is.null(along))
  stop_if_not(length(steps) == 1, is.numeric(steps), !is.na(steps), steps >= 0)
  stop_if_not(length(offset) == 1, is.numeric(offset), !is.na(offset))
  stop_if_not(length(scale) == 1, is.numeric(scale), !is.na(scale))
  stop_if_not(is.function(transform))
  label <- as.character(label)
  stop_if_not(length(label) == 1)
  steps <- transform(steps)
  stop_if_not(length(steps) == 1, is.numeric(steps), !is.na(steps), steps >= 0)
  stop_if_not(is.logical(on_exit), length(on_exit) == 1, !is.na(on_exit))
  if (identical(envir, globalenv())) {
    if (!progressr_in_globalenv()) {
      stop("A progressor must not be created in the global environment unless wrapped in a with_progress() or without_progress() call. Alternatively, create it inside a function or in a local() environment to make sure there is a finite life span of the progressor")
    }
    if (on_exit) {
      stop("It is not possible to create a progressor in the global environment with on_exit = TRUE")
    }
  }
  owner_session_uuid <- session_uuid(attributes = TRUE)
  progressor_count <<- progressor_count + 1
  progressor_uuid <- progressor_uuid(progressor_count)
  progression_index <- 0
  fcn <- function(message = character(0), ..., type = "update") {
    progression_index <<- progression_index + 1
    cond <- progression(type = type, message = message, ..., progressor_uuid = progressor_uuid, progression_index = progression_index, owner_session_uuid = owner_session_uuid, call = if (trace) 
      sys.call()
      else NULL, calls = if (trace) 
        sys.calls()
      else NULL)
    withRestarts(signalCondition(cond), muffleProgression = function(p) NULL)
    invisible(cond)
  }
  formals(fcn)$message <- message
  class(fcn) <- c("progressor", class(fcn))
  progressor_envir <- new.env(parent = getNamespace(.packageName))
  for (name in c("progression_index", "progressor_uuid", "owner_session_uuid", "progressor_count", "enable", "initiate", "auto_finish", "trace", "steps", "label", "offset", "scale")) {
    progressor_envir[[name]] <- get(name)
  }
  environment(fcn) <- progressor_envir
  if (exists("...progressor", mode = "function", envir = envir)) {
    ...progressor <- get("...progressor", mode = "function", envir = envir)
    ...progressor(type = "finish")
    do.call(unlockBinding, args = list("...progressor", env = envir))
    rm("...progressor", envir = envir)
  }
  if (initiate) {
    fcn(type = "initiate", steps = steps, auto_finish = auto_finish)
  }
  if (on_exit) {
    assign("...progressor", value = fcn, envir = envir)
    lockBinding("...progressor", env = envir)
    call <- call("...progressor", type = "finish")
    do.call(base::on.exit, args = list(call, add = TRUE), envir = envir)
  }
  fcn
})
c("package:progressr", "namespace:progressr")
c(TRUE, FALSE)
c(FALSE, TRUE)
slow_sum
list(`package:progressr` = function (x, delay = getOption("progressr.demo.delay", 1), stdout = FALSE, message = TRUE) 
{
  p <- progressor(along = x)
  sum <- 0
  for (kk in seq_along(x)) {
    p(amount = 0)
    Sys.sleep(0.2 * delay)
    if (stdout) 
      cat(sprintf("O: Element #%d\n", kk))
    p(amount = 0)
    Sys.sleep(0.2 * delay)
    p(amount = 0)
    Sys.sleep(0.2 * delay)
    sum <- sum + x[kk]
    p(message = sprintf("P: Adding %g", kk))
    Sys.sleep(0.2 * delay)
    if (message) 
      message(sprintf("M: Added value %g", x[kk]))
    p(amount = 0)
    Sys.sleep(0.2 * delay)
  }
  p(amount = 0)
  sum
}, function (x, delay = getOption("progressr.demo.delay", 1), stdout = FALSE, message = TRUE) 
{
  p <- progressor(along = x)
  sum <- 0
  for (kk in seq_along(x)) {
    p(amount = 0)
    Sys.sleep(0.2 * delay)
    if (stdout) 
      cat(sprintf("O: Element #%d\n", kk))
    p(amount = 0)
    Sys.sleep(0.2 * delay)
    p(amount = 0)
    Sys.sleep(0.2 * delay)
    sum <- sum + x[kk]
    p(message = sprintf("P: Adding %g", kk))
    Sys.sleep(0.2 * delay)
    if (message) 
      message(sprintf("M: Added value %g", x[kk]))
    p(amount = 0)
    Sys.sleep(0.2 * delay)
  }
  p(amount = 0)
  sum
})
c("package:progressr", "namespace:progressr")
c(TRUE, FALSE)
c(FALSE, TRUE)
with_progress
list(`package:progressr` = function (expr, handlers = progressr::handlers(), cleanup = TRUE, delay_terminal = NULL, delay_stdout = NULL, delay_conditions = NULL, interrupts = getOption("progressr.interrupts", TRUE), interval = NULL, enable = NULL) 
{
  stop_if_not(is.logical(cleanup), length(cleanup) == 1, !is.na(cleanup))
  stop_if_not(is.logical(interrupts), length(interrupts) == 1, !is.na(interrupts))
  debug <- getOption("progressr.debug", FALSE)
  if (debug) {
    message("with_progress() ...")
    on.exit(message("with_progress() ... done"), add = TRUE)
  }
  if (length(handlers) == 0) {
    if (debug) 
      message("No progress handlers - skipping")
    return(expr)
  }
  options <- list()
  if (!is.null(enable)) {
    stop_if_not(is.logical(enable), length(enable) == 1, !is.na(enable))
    if (!enable) {
      if (debug) 
        message("Progress disabled - skipping")
      return(expr)
    }
    options[["progressr.enable"]] <- enable
  }
  if (!is.null(interval)) {
    stop_if_not(is.numeric(interval), length(interval) == 1, !is.na(interval))
    options[["progressr.interval"]] <- interval
  }
  if (length(options) > 0) {
    oopts <- options(options)
    on.exit(options(oopts), add = TRUE)
  }
  progressr_in_globalenv("allow")
  on.exit(progressr_in_globalenv("disallow"), add = TRUE)
  handlers <- as_progression_handler(handlers)
  if (length(handlers) == 0) {
    if (debug) 
      message("No remaining progress handlers - skipping")
    return(expr)
  }
  delays <- use_delays(handlers, terminal = delay_terminal, stdout = delay_stdout, conditions = delay_conditions)
  if (debug) {
    what <- c(if (delays$terminal) "terminal", if (delays$stdout) "stdout", delays$conditions)
    message("- Buffering: ", paste(sQuote(what), collapse = ", "))
  }
  calling_handler <- make_calling_handler(handlers)
  status <- "incomplete"
  if (cleanup) {
    on.exit({
      if (debug) message("- signaling 'shutdown' to all handlers")
      withCallingHandlers({
        withRestarts({
          signalCondition(control_progression("shutdown", status = status))
        }, muffleProgression = function(p) NULL)
      }, progression = calling_handler)
    }, add = TRUE)
  }
  stdout_file <- delay_stdout(delays, stdout_file = NULL)
  on.exit(flush_stdout(stdout_file), add = TRUE)
  conditions <- list()
  if (length(delays$conditions) > 0) {
    on.exit(flush_conditions(conditions), add = TRUE)
  }
  if (debug) 
    message("- signaling 'reset' to all handlers")
  withCallingHandlers({
    withRestarts({
      signalCondition(control_progression("reset"))
    }, muffleProgression = function(p) NULL)
  }, progression = calling_handler)
  progression_counter <- 0
  capture_conditions <- TRUE
  withCallingHandlers({
    res <- withVisible(expr)
  }, progression = function(p) {
    progression_counter <<- progression_counter + 1
    if (debug) 
      message(sprintf("- received a %s (n=%g)", sQuote(class(p)[1]), progression_counter))
    capture_conditions <<- FALSE
    on.exit(capture_conditions <<- TRUE)
    if (isTRUE(attr(delays$terminal, "flush"))) {
      if (length(conditions) > 0 || has_buffered_stdout(stdout_file)) {
        calling_handler(control_progression("hide"))
        stdout_file <<- flush_stdout(stdout_file, close = FALSE)
        conditions <<- flush_conditions(conditions)
        calling_handler(control_progression("unhide"))
      }
    }
    calling_handler(p)
  }, interrupt = function(c) {
    if (!interrupts) 
      return()
    suspendInterrupts({
      capture_conditions <<- FALSE
      on.exit(capture_conditions <<- TRUE)
      if (isTRUE(attr(delays$terminal, "flush"))) {
        if (length(conditions) > 0 || has_buffered_stdout(stdout_file)) {
          calling_handler(control_progression("hide"))
          stdout_file <<- flush_stdout(stdout_file, close = FALSE)
          conditions <<- flush_conditions(conditions)
        }
      }
      calling_handler(control_progression("interrupt"))
    })
  }, condition = function(c) {
    if (!capture_conditions || inherits(c, c("progression", "error"))) 
      return()
    if (debug) 
      message("- received a ", sQuote(class(c)[1]))
    if (inherits(c, delays$conditions)) {
      conditions[[length(conditions) + 1]] <<- c
      if (inherits(c, "message")) {
        invokeRestart("muffleMessage")
      }
      else if (inherits(c, "warning")) {
        invokeRestart("muffleWarning")
      }
      else if (inherits(c, "condition")) {
        restarts <- computeRestarts(c)
        for (restart in restarts) {
          name <- restart$name
          if (is.null(name)) 
            next
          if (!grepl("^muffle", name)) 
            next
          invokeRestart(restart)
          break
        }
      }
    }
  })
  status <- "ok"
  if (isTRUE(res$visible)) {
    res$value
  }
  else {
    invisible(res$value)
  }
}, function (expr, handlers = progressr::handlers(), cleanup = TRUE, delay_terminal = NULL, delay_stdout = NULL, delay_conditions = NULL, interrupts = getOption("progressr.interrupts", TRUE), interval = NULL, enable = NULL) 
{
  stop_if_not(is.logical(cleanup), length(cleanup) == 1, !is.na(cleanup))
  stop_if_not(is.logical(interrupts), length(interrupts) == 1, !is.na(interrupts))
  debug <- getOption("progressr.debug", FALSE)
  if (debug) {
    message("with_progress() ...")
    on.exit(message("with_progress() ... done"), add = TRUE)
  }
  if (length(handlers) == 0) {
    if (debug) 
      message("No progress handlers - skipping")
    return(expr)
  }
  options <- list()
  if (!is.null(enable)) {
    stop_if_not(is.logical(enable), length(enable) == 1, !is.na(enable))
    if (!enable) {
      if (debug) 
        message("Progress disabled - skipping")
      return(expr)
    }
    options[["progressr.enable"]] <- enable
  }
  if (!is.null(interval)) {
    stop_if_not(is.numeric(interval), length(interval) == 1, !is.na(interval))
    options[["progressr.interval"]] <- interval
  }
  if (length(options) > 0) {
    oopts <- options(options)
    on.exit(options(oopts), add = TRUE)
  }
  progressr_in_globalenv("allow")
  on.exit(progressr_in_globalenv("disallow"), add = TRUE)
  handlers <- as_progression_handler(handlers)
  if (length(handlers) == 0) {
    if (debug) 
      message("No remaining progress handlers - skipping")
    return(expr)
  }
  delays <- use_delays(handlers, terminal = delay_terminal, stdout = delay_stdout, conditions = delay_conditions)
  if (debug) {
    what <- c(if (delays$terminal) "terminal", if (delays$stdout) "stdout", delays$conditions)
    message("- Buffering: ", paste(sQuote(what), collapse = ", "))
  }
  calling_handler <- make_calling_handler(handlers)
  status <- "incomplete"
  if (cleanup) {
    on.exit({
      if (debug) message("- signaling 'shutdown' to all handlers")
      withCallingHandlers({
        withRestarts({
          signalCondition(control_progression("shutdown", status = status))
        }, muffleProgression = function(p) NULL)
      }, progression = calling_handler)
    }, add = TRUE)
  }
  stdout_file <- delay_stdout(delays, stdout_file = NULL)
  on.exit(flush_stdout(stdout_file), add = TRUE)
  conditions <- list()
  if (length(delays$conditions) > 0) {
    on.exit(flush_conditions(conditions), add = TRUE)
  }
  if (debug) 
    message("- signaling 'reset' to all handlers")
  withCallingHandlers({
    withRestarts({
      signalCondition(control_progression("reset"))
    }, muffleProgression = function(p) NULL)
  }, progression = calling_handler)
  progression_counter <- 0
  capture_conditions <- TRUE
  withCallingHandlers({
    res <- withVisible(expr)
  }, progression = function(p) {
    progression_counter <<- progression_counter + 1
    if (debug) 
      message(sprintf("- received a %s (n=%g)", sQuote(class(p)[1]), progression_counter))
    capture_conditions <<- FALSE
    on.exit(capture_conditions <<- TRUE)
    if (isTRUE(attr(delays$terminal, "flush"))) {
      if (length(conditions) > 0 || has_buffered_stdout(stdout_file)) {
        calling_handler(control_progression("hide"))
        stdout_file <<- flush_stdout(stdout_file, close = FALSE)
        conditions <<- flush_conditions(conditions)
        calling_handler(control_progression("unhide"))
      }
    }
    calling_handler(p)
  }, interrupt = function(c) {
    if (!interrupts) 
      return()
    suspendInterrupts({
      capture_conditions <<- FALSE
      on.exit(capture_conditions <<- TRUE)
      if (isTRUE(attr(delays$terminal, "flush"))) {
        if (length(conditions) > 0 || has_buffered_stdout(stdout_file)) {
          calling_handler(control_progression("hide"))
          stdout_file <<- flush_stdout(stdout_file, close = FALSE)
          conditions <<- flush_conditions(conditions)
        }
      }
      calling_handler(control_progression("interrupt"))
    })
  }, condition = function(c) {
    if (!capture_conditions || inherits(c, c("progression", "error"))) 
      return()
    if (debug) 
      message("- received a ", sQuote(class(c)[1]))
    if (inherits(c, delays$conditions)) {
      conditions[[length(conditions) + 1]] <<- c
      if (inherits(c, "message")) {
        invokeRestart("muffleMessage")
      }
      else if (inherits(c, "warning")) {
        invokeRestart("muffleWarning")
      }
      else if (inherits(c, "condition")) {
        restarts <- computeRestarts(c)
        for (restart in restarts) {
          name <- restart$name
          if (is.null(name)) 
            next
          if (!grepl("^muffle", name)) 
            next
          invokeRestart(restart)
          break
        }
      }
    }
  })
  status <- "ok"
  if (isTRUE(res$visible)) {
    res$value
  }
  else {
    invisible(res$value)
  }
})
c("package:progressr", "namespace:progressr")
c(TRUE, FALSE)
c(FALSE, TRUE)
without_progress
list(`package:progressr` = function (expr) 
{
  progressr_in_globalenv("allow")
  on.exit(progressr_in_globalenv("disallow"))
  withCallingHandlers({
    res <- withVisible(expr)
  }, progression = function(p) {
    invokeRestart("muffleProgression")
  })
  if (isTRUE(res$visible)) {
    res$value
  }
  else {
    invisible(res$value)
  }
}, function (expr) 
{
  progressr_in_globalenv("allow")
  on.exit(progressr_in_globalenv("disallow"))
  withCallingHandlers({
    res <- withVisible(expr)
  }, progression = function(p) {
    invokeRestart("muffleProgression")
  })
  if (isTRUE(res$visible)) {
    res$value
  }
  else {
    invisible(res$value)
  }
})
c("package:progressr", "namespace:progressr")
c(TRUE, FALSE)
c(FALSE, TRUE)
withProgressShiny
list(`package:progressr` = function (expr, ..., message = NULL, detail = NULL, inputs = list(message = NULL, detail = "message"), env = parent.frame(), quoted = FALSE, handlers = c(shiny = handler_shiny, progressr::handlers(default = NULL))) 
{
  if (!quoted) 
    expr <- substitute(expr)
  stop_if_not(is.list(inputs), all(names(inputs) %in% c("message", "detail")))
  stop_if_not("shiny" %in% names(handlers))
  if (sum(names(handlers) == "shiny") > 1) {
    warning("Detected a 'shiny' handler set via progressr::handlers()")
  }
  args <- list(message = message, detail = detail)
  for (name in names(args)) {
    input <- unique(attr(args[[name]], "input"))
    if (is.null(input)) 
      next
    unknown <- setdiff(input, c("message", "sticky_message", "non_sticky_message"))
    if (length(unknown) > 0) {
      stop(sprintf("Unknown value of attribute %s on argument %s: %s", sQuote("input"), sQuote(name), commaq(unknown)))
    }
    inputs[[name]] <- input
  }
  stop_if_not(is.list(inputs), !is.null(names(inputs)), all(names(inputs) %in% c("message", "detail")), all(vapply(inputs, FUN = function(x) {
    if (is.null(x)) return(TRUE)
    if (!is.character(x)) return(FALSE)
    x %in% c("message", "non_sticky_message", "sticky_message")
  }, FUN.VALUE = FALSE)))
  if (is.function(handlers$shiny) && !inherits(handlers$shiny, "progression_handler")) {
    tweaked_handler_shiny <- handlers$shiny
    if (!identical(inputs, formals(tweaked_handler_shiny)$inputs)) {
      formals(tweaked_handler_shiny)$inputs <- inputs
      handlers$shiny <- tweaked_handler_shiny
    }
  }
  expr <- bquote(progressr::with_progress({
    .(expr)
  }, handlers = .(handlers)))
  res <- withVisible(shiny::withProgress(expr, ..., message = message, detail = detail, env = env, quoted = TRUE))
  if (res$visible) 
    res$value
  else invisible(res$value)
}, function (expr, ..., message = NULL, detail = NULL, inputs = list(message = NULL, detail = "message"), env = parent.frame(), quoted = FALSE, handlers = c(shiny = handler_shiny, progressr::handlers(default = NULL))) 
{
  if (!quoted) 
    expr <- substitute(expr)
  stop_if_not(is.list(inputs), all(names(inputs) %in% c("message", "detail")))
  stop_if_not("shiny" %in% names(handlers))
  if (sum(names(handlers) == "shiny") > 1) {
    warning("Detected a 'shiny' handler set via progressr::handlers()")
  }
  args <- list(message = message, detail = detail)
  for (name in names(args)) {
    input <- unique(attr(args[[name]], "input"))
    if (is.null(input)) 
      next
    unknown <- setdiff(input, c("message", "sticky_message", "non_sticky_message"))
    if (length(unknown) > 0) {
      stop(sprintf("Unknown value of attribute %s on argument %s: %s", sQuote("input"), sQuote(name), commaq(unknown)))
    }
    inputs[[name]] <- input
  }
  stop_if_not(is.list(inputs), !is.null(names(inputs)), all(names(inputs) %in% c("message", "detail")), all(vapply(inputs, FUN = function(x) {
    if (is.null(x)) return(TRUE)
    if (!is.character(x)) return(FALSE)
    x %in% c("message", "non_sticky_message", "sticky_message")
  }, FUN.VALUE = FALSE)))
  if (is.function(handlers$shiny) && !inherits(handlers$shiny, "progression_handler")) {
    tweaked_handler_shiny <- handlers$shiny
    if (!identical(inputs, formals(tweaked_handler_shiny)$inputs)) {
      formals(tweaked_handler_shiny)$inputs <- inputs
      handlers$shiny <- tweaked_handler_shiny
    }
  }
  expr <- bquote(progressr::with_progress({
    .(expr)
  }, handlers = .(handlers)))
  res <- withVisible(shiny::withProgress(expr, ..., message = message, detail = detail, env = env, quoted = TRUE))
  if (res$visible) 
    res$value
  else invisible(res$value)
})
c("package:progressr", "namespace:progressr")
c(TRUE, FALSE)
c(FALSE, TRUE)
> 
                                                                                                                                                                                                               progressr::handlers(default = NULL)))  
                                                    

###################################################################################################
library(purrr)
%@%
  list(`package:purrr` = function (x, name) 
  {
    signal_soft_deprecated(paste_line("`%@%` is soft-deprecated as of purrr 0.3.0.", "Please use the operator provided in rlang instead."))
    attr(x, name, exact = TRUE)
  }, function (x, name) 
  {
    name <- as_string(ensym(name))
    if (isS4(x)) {
      eval_bare(expr(`@`(x, !!name)))
    }
    else {
      attr(x, name, exact = TRUE)
    }
  }, function (x, name) 
  {
    signal_soft_deprecated(paste_line("`%@%` is soft-deprecated as of purrr 0.3.0.", "Please use the operator provided in rlang instead."))
    attr(x, name, exact = TRUE)
  })
c("package:purrr", "namespace:rlang", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
%||%
  list(`package:purrr` = function (x, y) 
  {
    if (is_null(x)) 
      y
    else x
  }, function (l, r) 
    if (is.null(l)) r else l, function (a, b) 
    {
      if (length(a) > 0) 
        a
      else b
    }, function (a, b) 
    {
      if (!is.null(a)) 
        a
      else b
    }, function (L, R) 
      if (is.null(L)) R else L, function (x, y) 
      {
        if (is_null(x)) 
          y
        else x
      }, function (l, r) 
        if (is.null(l)) r else l, function (x, orElse) 
          if (!is.null(x)) x else orElse, function (lhs, rhs) 
          {
            if (!is.null(lhs)) {
              lhs
            }
            else {
              rhs
            }
          }, function (L, R) 
            if (is.null(L)) R else L, function (x, y) 
            {
              if (is.null(x)) 
                y
              else (x)
            }, function (lhs, rhs) 
            {
              if (is.null(lhs)) 
                rhs
              else lhs
            }, function (l, r) 
              if (is.null(l)) r else l, function (x, y) 
                if (is.null(x)) y else x, function (a, b) 
                {
                  if (!is.null(a)) 
                    a
                  else b
                }, function (a, b) 
                  if (!is.null(a)) a else b, function (l, r) 
                    if (is.null(l)) r else l, function (l, r) 
                      if (is.null(l)) r else l, function (x, y) 
                        if (is.null(x)) y else x, function (l, r) 
                          if (is.null(l)) r else l)
c("package:purrr", "namespace:ps", "namespace:httr", "namespace:ggplot2", "namespace:utils", "namespace:rlang", "namespace:callr", "namespace:Matrix", "namespace:pkgconfig", "namespace:stats", "namespace:lgr", "namespace:progressr", "namespace:crayon", "namespace:nlme", "namespace:gtable", "namespace:scales", "namespace:cli", "namespace:webdriver", "namespace:glue", "namespace:processx")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
%>%
  list(`package:purrr` = function (lhs, rhs) 
  {
    lhs <- substitute(lhs)
    rhs <- substitute(rhs)
    kind <- 1
    env <- parent.frame()
    lazy <- TRUE
    .External2(magrittr_pipe)
  }, `package:multicolor` = function (lhs, rhs) 
  {
    lhs <- substitute(lhs)
    rhs <- substitute(rhs)
    kind <- 1
    env <- parent.frame()
    lazy <- TRUE
    .External2(magrittr_pipe)
  }, function (lhs, rhs) 
  {
    lhs <- substitute(lhs)
    rhs <- substitute(rhs)
    kind <- 1
    env <- parent.frame()
    lazy <- TRUE
    .External2(magrittr_pipe)
  })
c("package:purrr", "package:multicolor", "namespace:magrittr")
c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, TRUE)
accumulate
list(`package:purrr` = function (.x, .f, ..., .init, .dir = c("forward", "backward")) 
{
  .dir <- arg_match(.dir, c("forward", "backward"))
  .f <- as_mapper(.f, ...)
  res <- reduce_impl(.x, .f, ..., .init = .init, .dir = .dir, .acc = TRUE)
  names(res) <- accumulate_names(names(.x), .init, .dir)
  if (all(map_int(res, length) == 1)) {
    res <- unlist(res, recursive = FALSE)
  }
  res
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}, function (.x, .f, ..., .init, .dir = c("forward", "backward")) 
{
  .dir <- arg_match(.dir, c("forward", "backward"))
  .f <- as_mapper(.f, ...)
  res <- reduce_impl(.x, .f, ..., .init = .init, .dir = .dir, .acc = TRUE)
  names(res) <- accumulate_names(names(.x), .init, .dir)
  if (all(map_int(res, length) == 1)) {
    res <- unlist(res, recursive = FALSE)
  }
  res
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
accumulate_right
list(`package:purrr` = function (.x, .f, ..., .init) 
{
  signal_soft_deprecated(paste_line("`accumulate_right()` is soft-deprecated as of purrr 0.3.0.", "Please use the new `.dir` argument of `accumulate()` instead.", "", "  # Before:", "  accumulate_right(x, f)", "", "  # After:", "  accumulate(x, f, .dir = \"backward\")", ""))
  f <- function(y, x) {
    .f(x, y, ...)
  }
  accumulate(.x, f, .init = .init, .dir = "backward")
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}, function (.x, .f, ..., .init) 
{
  signal_soft_deprecated(paste_line("`accumulate_right()` is soft-deprecated as of purrr 0.3.0.", "Please use the new `.dir` argument of `accumulate()` instead.", "", "  # Before:", "  accumulate_right(x, f)", "", "  # After:", "  accumulate(x, f, .dir = \"backward\")", ""))
  f <- function(y, x) {
    .f(x, y, ...)
  }
  accumulate(.x, f, .init = .init, .dir = "backward")
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
accumulate2
list(`package:purrr` = function (.x, .y, .f, ..., .init) 
{
  reduce2_impl(.x, .y, .f, ..., .init = .init, .acc = TRUE)
}, function (.x, .y, .f, ..., .init) 
{
  reduce2_impl(.x, .y, .f, ..., .init = .init, .acc = TRUE)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
array_branch
list(`package:purrr` = function (array, margin = NULL) 
{
  dims <- dim(array) %||% length(array)
  margin <- margin %||% seq_along(dims)
  if (length(margin) == 0) {
    list(array)
  }
  else if (is.null(dim(array))) {
    if (!identical(as.integer(margin), 1)) {
      abort(sprintf("`margin` must be `NULL` or `1` with 1D arrays, not `%s`", toString(margin)))
    }
    as.list(array)
  }
  else {
    flatten(apply(array, margin, list))
  }
}, function (array, margin = NULL) 
{
  dims <- dim(array) %||% length(array)
  margin <- margin %||% seq_along(dims)
  if (length(margin) == 0) {
    list(array)
  }
  else if (is.null(dim(array))) {
    if (!identical(as.integer(margin), 1)) {
      abort(sprintf("`margin` must be `NULL` or `1` with 1D arrays, not `%s`", toString(margin)))
    }
    as.list(array)
  }
  else {
    flatten(apply(array, margin, list))
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
array_tree
list(`package:purrr` = function (array, margin = NULL) 
{
  dims <- dim(array) %||% length(array)
  margin <- margin %||% seq_along(dims)
  if (length(margin) > 1) {
    new_margin <- ifelse(margin[-1] > margin[[1]], margin[-1] - 1, margin[-1])
    apply(array, margin[[1]], array_tree, new_margin)
  }
  else {
    array_branch(array, margin)
  }
}, function (array, margin = NULL) 
{
  dims <- dim(array) %||% length(array)
  margin <- margin %||% seq_along(dims)
  if (length(margin) > 1) {
    new_margin <- ifelse(margin[-1] > margin[[1]], margin[-1] - 1, margin[-1])
    apply(array, margin[[1]], array_tree, new_margin)
  }
  else {
    array_branch(array, margin)
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
as_function
list(`package:purrr` = function (...) 
{
  stop_defunct(paste_line("`as_function()` is defunct as of purrr 0.3.0.", "Please use `as_mapper()` or `rlang::as_function()` instead"))
  as_mapper(...)
}, function (x, env = global_env(), ..., arg = caller_arg(x), call = caller_env()) 
{
  check_dots_empty0(...)
  local_error_call(call)
  if (is_function(x)) {
    return(x)
  }
  if (is_quosure(x)) {
    mask <- eval_tidy(call2(environment), env = quo_get_env(x))
    fn <- new_function(pairlist2(... = ), quo_get_expr(x), mask)
    return(fn)
  }
  if (is_formula(x)) {
    if (length(x) > 2) {
      abort_coercion(x, x_type = "a two-sided formula", to_type = "a function", arg = arg)
    }
    env <- f_env(x)
    if (!is_environment(env)) {
      abort("Formula must carry an environment.")
    }
    args <- list(... = missing_arg(), .x = quote(..1), .y = quote(..2), . = quote(..1))
    fn <- new_function(args, f_rhs(x), env)
    fn <- structure(fn, class = c("rlang_lambda_function", "function"))
    return(fn)
  }
  if (is_string(x)) {
    return(get(x, envir = env, mode = "function"))
  }
  abort_coercion(x, "a function", arg = arg)
}, function (expr, envir = parent.frame(), enclos = baseenv(), ...) 
{
  fun_expr <- substitute(function() x, list(x = expr))
  eval(fun_expr, envir = envir, enclos = enclos, ...)
}, function (...) 
{
  stop_defunct(paste_line("`as_function()` is defunct as of purrr 0.3.0.", "Please use `as_mapper()` or `rlang::as_function()` instead"))
  as_mapper(...)
})
c("package:purrr", "namespace:rlang", "namespace:globals", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, TRUE)
as_mapper
list(`package:purrr` = function (.f, ...) 
{
  UseMethod("as_mapper")
}, function (.f, ...) 
{
  UseMethod("as_mapper")
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
as_vector
list(`package:purrr` = function (.x, .type = NULL) 
{
  if (can_simplify(.x, .type)) {
    unlist(.x)
  }
  else {
    stop("Cannot coerce .x to a vector", call. = FALSE)
  }
}, function (.x, .type = NULL) 
{
  if (can_simplify(.x, .type)) {
    unlist(.x)
  }
  else {
    stop("Cannot coerce .x to a vector", call. = FALSE)
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
assign_in
list(`package:purrr` = function (x, where, value) 
{
  chuck(x, !!!where)
  call <- reduce_subset_call(quote(x), as.list(where))
  call <- call("<-", call, value)
  eval_bare(call)
  x
}, function (x, where, value) 
{
  chuck(x, !!!where)
  call <- reduce_subset_call(quote(x), as.list(where))
  call <- call("<-", call, value)
  eval_bare(call)
  x
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
at_depth
list(`package:purrr` = function (.x, .depth, .f, ...) 
{
  stop_defunct("at_depth() is defunct, please use `map_depth()` instead")
}, function (.x, .depth, .f, ...) 
{
  stop_defunct("at_depth() is defunct, please use `map_depth()` instead")
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
attr_getter
list(`package:purrr` = function (attr) 
{
  force(attr)
  function(x) attr(x, attr, exact = TRUE)
}, function (attr) 
{
  force(attr)
  function(x) attr(x, attr, exact = TRUE)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
auto_browse
list(`package:purrr` = function (.f) 
{
  if (is_primitive(.f)) {
    abort("Can not auto_browse() primitive functions")
  }
  function(...) {
    withCallingHandlers(.f(...), error = function(e) {
      frame <- ctxt_frame(4)
      browse_in_frame(frame)
    }, warning = function(e) {
      if (getOption("warn") >= 2) {
        frame <- ctxt_frame(7)
        browse_in_frame(frame)
      }
    }, interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    })
  }
}, function (.f) 
{
  if (is_primitive(.f)) {
    abort("Can not auto_browse() primitive functions")
  }
  function(...) {
    withCallingHandlers(.f(...), error = function(e) {
      frame <- ctxt_frame(4)
      browse_in_frame(frame)
    }, warning = function(e) {
      if (getOption("warn") >= 2) {
        frame <- ctxt_frame(7)
        browse_in_frame(frame)
      }
    }, interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    })
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
chuck
list(`package:purrr` = function (.x, ...) 
{
  .Call(pluck_impl, x = .x, index = list2(...), missing = NULL, strict = TRUE)
}, function (.x, ...) 
{
  .Call(pluck_impl, x = .x, index = list2(...), missing = NULL, strict = TRUE)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
compact
list(`package:purrr` = function (.x, .p = identity) 
{
  .f <- as_mapper(.p)
  discard(.x, function(x) is_empty(.f(x)))
}, function (x) 
{
  empty <- vapply(x, is_empty, logical(1))
  x[!empty]
}, function (x) 
{
  null <- vapply(x, is.null, logical(1))
  x[!null]
}, function (.x) 
{
  Filter(length, .x)
}, function (.x) 
{
  Filter(length, .x)
}, function (.x) 
{
  Filter(length, .x)
}, function (x) 
{
  x[!vapply(x, is.null, FALSE)]
}, function (.x) 
{
  Filter(length, .x)
}, function (x) 
{
  is_null <- map_lgl(x, is.null)
  x[!is_null]
}, function (.x, .p = identity) 
{
  .f <- as_mapper(.p)
  discard(.x, function(x) is_empty(.f(x)))
})
c("package:purrr", "namespace:httr", "namespace:ggplot2", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:lgr", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)
compose
list(`package:purrr` = function (..., .dir = c("backward", "forward")) 
{
  .dir <- arg_match(.dir, c("backward", "forward"))
  fns <- map(list2(...), rlang::as_closure, env = caller_env())
  if (!length(fns)) {
    return(compose(function(x, ...) x))
  }
  if (.dir == "backward") {
    n <- length(fns)
    first_fn <- fns[[n]]
    fns <- rev(fns[-n])
  }
  else {
    first_fn <- fns[[1]]
    fns <- fns[-1]
  }
  composed <- function() {
    env <- env(caller_env(), `_fn` = first_fn)
    first_call <- sys.call()
    first_call[[1]] <- quote(`_fn`)
    env$`_out` <- .Call(purrr_eval, first_call, env)
    call <- quote(`_fn`(`_out`))
    for (fn in fns) {
      env$`_fn` <- fn
      env$`_out` <- .Call(purrr_eval, call, env)
    }
    env$`_out`
  }
  formals(composed) <- formals(first_fn)
  structure(composed, class = c("purrr_function_compose", "function"), first_fn = first_fn, fns = fns)
}, function (...) 
{
  funs = rev(lapply(list(...), match.fun))
  assert_list(funs, min.len = 1)
  function(...) {
    out = funs[[1]](...)
    out = funs[[1]](...)
    for (f in tail(funs, length(funs) - 1)) {
      out = f(out)
    }
    out
  }
}, function (..., .dir = c("backward", "forward")) 
{
  .dir <- arg_match(.dir, c("backward", "forward"))
  fns <- map(list2(...), rlang::as_closure, env = caller_env())
  if (!length(fns)) {
    return(compose(function(x, ...) x))
  }
  if (.dir == "backward") {
    n <- length(fns)
    first_fn <- fns[[n]]
    fns <- rev(fns[-n])
  }
  else {
    first_fn <- fns[[1]]
    fns <- fns[-1]
  }
  composed <- function() {
    env <- env(caller_env(), `_fn` = first_fn)
    first_call <- sys.call()
    first_call[[1]] <- quote(`_fn`)
    env$`_out` <- .Call(purrr_eval, first_call, env)
    call <- quote(`_fn`(`_out`))
    for (fn in fns) {
      env$`_fn` <- fn
      env$`_out` <- .Call(purrr_eval, call, env)
    }
    env$`_out`
  }
  formals(composed) <- formals(first_fn)
  structure(composed, class = c("purrr_function_compose", "function"), first_fn = first_fn, fns = fns)
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
cross
list(`package:purrr` = function (.l, .filter = NULL) 
{
  if (is_empty(.l)) {
    return(.l)
  }
  if (!is.null(.filter)) {
    .filter <- as_mapper(.filter)
  }
  n <- length(.l)
  lengths <- lapply(.l, length)
  names <- names(.l)
  factors <- cumprod(lengths)
  total_length <- factors[n]
  factors <- c(1, factors[-n])
  out <- replicate(total_length, vector("list", n), simplify = FALSE)
  for (i in seq_along(out)) {
    for (j in seq_len(n)) {
      index <- floor((i - 1)/factors[j])%%length(.l[[j]]) + 1
      out[[i]][[j]] <- .l[[j]][[index]]
    }
    names(out[[i]]) <- names
    if (!is.null(.filter)) {
      is_to_filter <- do.call(".filter", unname(out[[i]]))
      if (!is_bool(is_to_filter)) {
        abort(sprintf("The filter function must return a single logical `TRUE` or `FALSE`, not %s", as_predicate_friendly_type_of(is_to_filter)))
      }
      if (is_to_filter) {
        out[i] <- list(NULL)
      }
    }
  }
  compact(out)
}, function () 
{
  x <- if (has_cli) 
    cli::symbol$cross
  else "x"
  red(x)
}, function (.l, .filter = NULL) 
{
  if (is_empty(.l)) {
    return(.l)
  }
  if (!is.null(.filter)) {
    .filter <- as_mapper(.filter)
  }
  n <- length(.l)
  lengths <- lapply(.l, length)
  names <- names(.l)
  factors <- cumprod(lengths)
  total_length <- factors[n]
  factors <- c(1, factors[-n])
  out <- replicate(total_length, vector("list", n), simplify = FALSE)
  for (i in seq_along(out)) {
    for (j in seq_len(n)) {
      index <- floor((i - 1)/factors[j])%%length(.l[[j]]) + 1
      out[[i]][[j]] <- .l[[j]][[index]]
    }
    names(out[[i]]) <- names
    if (!is.null(.filter)) {
      is_to_filter <- do.call(".filter", unname(out[[i]]))
      if (!is_bool(is_to_filter)) {
        abort(sprintf("The filter function must return a single logical `TRUE` or `FALSE`, not %s", as_predicate_friendly_type_of(is_to_filter)))
      }
      if (is_to_filter) {
        out[i] <- list(NULL)
      }
    }
  }
  compact(out)
})
c("package:purrr", "namespace:rlang", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
cross_d
list(`package:purrr` = function (...) 
{
  warning("`cross_d()` is deprecated; please use `cross_df()` instead.", call. = FALSE)
  cross_df(...)
}, function (...) 
{
  warning("`cross_d()` is deprecated; please use `cross_df()` instead.", call. = FALSE)
  cross_df(...)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
cross_df
list(`package:purrr` = function (.l, .filter = NULL) 
{
  check_tibble()
  cross(.l, .filter = .filter) %>% transpose() %>% simplify_all() %>% tibble::as_tibble()
}, function (.l, .filter = NULL) 
{
  check_tibble()
  cross(.l, .filter = .filter) %>% transpose() %>% simplify_all() %>% tibble::as_tibble()
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
cross_n
list(`package:purrr` = function (...) 
{
  warning("`cross_n()` is deprecated; please use `cross()` instead.", call. = FALSE)
  cross(...)
}, function (...) 
{
  warning("`cross_n()` is deprecated; please use `cross()` instead.", call. = FALSE)
  cross(...)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
cross2
list(`package:purrr` = function (.x, .y, .filter = NULL) 
{
  cross(list(.x, .y), .filter = .filter)
}, function (.x, .y, .filter = NULL) 
{
  cross(list(.x, .y), .filter = .filter)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
cross3
list(`package:purrr` = function (.x, .y, .z, .filter = NULL) 
{
  cross(list(.x, .y, .z), .filter = .filter)
}, function (.x, .y, .z, .filter = NULL) 
{
  cross(list(.x, .y, .z), .filter = .filter)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
detect
list(`package:purrr` = function (.x, .f, ..., .dir = c("forward", "backward"), .right = NULL, .default = NULL) 
{
  .f <- as_predicate(.f, ..., .mapper = TRUE)
  .dir <- arg_match(.dir, c("forward", "backward"))
  for (i in index(.x, .dir, .right, "detect")) {
    if (.f(.x[[i]], ...)) {
      return(.x[[i]])
    }
  }
  .default
}, function (.x, .f, ..., .right = FALSE, .p = is_true) 
{
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}, function (.x, .f, ..., .right = FALSE, .p = is_true) 
{
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())
  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}, function (.x, .f, ..., .right = FALSE, .p = is_true) 
{
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}, function (.x, .p, ...) 
{
  for (i in seq_along(.x)) {
    .res = .p(.x[[i]], ...)
    if (!is.na(.res) && .res) {
      return(.x[[i]])
    }
  }
  return(NULL)
}, function (.x, .f, ..., .right = FALSE, .p = is_true) 
{
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())
  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}, function (.x, .f, ..., .right = FALSE, .p = is_true) 
{
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}, function (.x, .f, ..., .dir = c("forward", "backward"), .right = NULL, .default = NULL) 
{
  .f <- as_predicate(.f, ..., .mapper = TRUE)
  .dir <- arg_match(.dir, c("forward", "backward"))
  for (i in index(.x, .dir, .right, "detect")) {
    if (.f(.x[[i]], ...)) {
      return(.x[[i]])
    }
  }
  .default
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
detect_index
list(`package:purrr` = function (.x, .f, ..., .dir = c("forward", "backward"), .right = NULL) 
{
  .f <- as_predicate(.f, ..., .mapper = TRUE)
  .dir <- arg_match(.dir, c("forward", "backward"))
  for (i in index(.x, .dir, .right, "detect_index")) {
    if (.f(.x[[i]], ...)) {
      return(i)
    }
  }
  0
}, function (.x, .f, ..., .right = FALSE, .p = is_true) 
{
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0
}, function (.x, .f, ..., .right = FALSE, .p = is_true) 
{
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())
  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0
}, function (.x, .f, ..., .right = FALSE, .p = is_true) 
{
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0
}, function (.x, .f, ..., .right = FALSE, .p = is_true) 
{
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())
  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0
}, function (.x, .f, ..., .right = FALSE, .p = is_true) 
{
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0
}, function (.x, .f, ..., .dir = c("forward", "backward"), .right = NULL) 
{
  .f <- as_predicate(.f, ..., .mapper = TRUE)
  .dir <- arg_match(.dir, c("forward", "backward"))
  for (i in index(.x, .dir, .right, "detect_index")) {
    if (.f(.x[[i]], ...)) {
      return(i)
    }
  }
  0
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
discard
list(`package:purrr` = function (.x, .p, ...) 
{
  sel <- probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}, function (.x, .p, ...) 
{
  sel <- probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}, function (.x, .p, ...) 
{
  sel <- .rlang_purrr_probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}, function (.x, .p, ...) 
{
  sel <- probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}, function (.x, .p, ...) 
{
  UseMethod("discard")
}, function (.x, .p, ...) 
{
  sel <- .rlang_purrr_probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}, function (x, range = c(0, 1)) 
{
  force(range)
  x[x >= range[1] & x <= range[2]]
}, function (.x, .p, ...) 
{
  sel <- probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}, function (.x, .p, ...) 
{
  sel <- probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:scales", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
done
list(`package:purrr` = function (x) 
{
  new_box(maybe_missing(x), class = "rlang_box_done", empty = missing(x))
}, function (x) 
{
  new_box(maybe_missing(x), class = "rlang_box_done", empty = missing(x))
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
every
list(`package:purrr` = function (.x, .p, ...) 
{
  .p <- as_predicate(.p, ..., .mapper = TRUE, .deprecate = TRUE)
  for (i in seq_along(.x)) {
    val <- .p(.x[[i]], ...)
    if (is_false(val)) 
      return(FALSE)
    if (anyNA(val)) 
      return(NA)
  }
  TRUE
}, function (.x, .p, ...) 
{
  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) {
      return(FALSE)
    }
  }
  TRUE
}, function (.x, .p, ...) 
{
  .p <- as_function(.p, env = global_env())
  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) 
      return(FALSE)
  }
  TRUE
}, function (.x, .p, ...) 
{
  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) 
      return(FALSE)
  }
  TRUE
}, function (.x, .p, ...) 
{
  ok = all(map_lgl(.x, .p, ...), na.rm = FALSE)
  !is.na(ok) && ok
}, function (.x, .p, ...) 
{
  .p <- as_function(.p, env = global_env())
  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) 
      return(FALSE)
  }
  TRUE
}, function (.x, .p, ...) 
{
  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) 
      return(FALSE)
  }
  TRUE
}, function (.x, .p, ...) 
{
  .p <- as_predicate(.p, ..., .mapper = TRUE, .deprecate = TRUE)
  for (i in seq_along(.x)) {
    val <- .p(.x[[i]], ...)
    if (is_false(val)) 
      return(FALSE)
    if (anyNA(val)) 
      return(NA)
  }
  TRUE
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
exec
list(`package:purrr` = function (.fn, ..., .env = caller_env()) 
{
  .External2(ffi_exec, .fn, .env)
}, function (.fn, ..., .env = caller_env()) 
{
  .External2(ffi_exec, .fn, .env)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
flatten
list(`package:purrr` = function (.x) 
{
  .Call(flatten_impl, .x)
}, function (x) 
{
  .Call(ffi_squash, x, "list", is_spliced_bare, 1)
}, function (x, recursive = TRUE) 
{
  stopifnot(is.data.frame(x))
  nr <- nrow(x)
  dfcolumns <- vapply(x, is.data.frame, logical(1))
  if (!any(dfcolumns)) {
    return(x)
  }
  x <- if (recursive) {
    c(x[!dfcolumns], do.call(c, lapply(x[dfcolumns], flatten)))
  }
  else {
    c(x[!dfcolumns], do.call(c, x[dfcolumns]))
  }
  class(x) <- "data.frame"
  row.names(x) <- if (!nr) 
    character(0)
  else 1:nr
  x
}, function (.x) 
{
  .Call(flatten_impl, .x)
})
c("package:purrr", "namespace:rlang", "namespace:jsonlite", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, TRUE)
flatten_chr
list(`package:purrr` = function (.x) 
{
  .Call(vflatten_impl, .x, "character")
}, function (x) 
{
  .Call(ffi_squash, x, "character", is_spliced_bare, 1)
}, function (.x) 
{
  .Call(vflatten_impl, .x, "character")
})
c("package:purrr", "namespace:rlang", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
flatten_dbl
list(`package:purrr` = function (.x) 
{
  .Call(vflatten_impl, .x, "double")
}, function (x) 
{
  .Call(ffi_squash, x, "double", is_spliced_bare, 1)
}, function (.x) 
{
  .Call(vflatten_impl, .x, "double")
})
c("package:purrr", "namespace:rlang", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
flatten_df
list(`package:purrr` = function (.x, .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`flatten_dfr()` requires dplyr")
  }
  res <- .Call(flatten_impl, .x)
  dplyr::bind_rows(res, .id = .id)
}, function (.x, .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`flatten_dfr()` requires dplyr")
  }
  res <- .Call(flatten_impl, .x)
  dplyr::bind_rows(res, .id = .id)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
flatten_dfc
list(`package:purrr` = function (.x) 
{
  if (!is_installed("dplyr")) {
    abort("`flatten_dfc()` requires dplyr")
  }
  res <- .Call(flatten_impl, .x)
  dplyr::bind_cols(res)
}, function (.x) 
{
  if (!is_installed("dplyr")) {
    abort("`flatten_dfc()` requires dplyr")
  }
  res <- .Call(flatten_impl, .x)
  dplyr::bind_cols(res)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
flatten_dfr
list(`package:purrr` = function (.x, .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`flatten_dfr()` requires dplyr")
  }
  res <- .Call(flatten_impl, .x)
  dplyr::bind_rows(res, .id = .id)
}, function (.x, .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`flatten_dfr()` requires dplyr")
  }
  res <- .Call(flatten_impl, .x)
  dplyr::bind_rows(res, .id = .id)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
flatten_int
list(`package:purrr` = function (.x) 
{
  .Call(vflatten_impl, .x, "integer")
}, function (x) 
{
  .Call(ffi_squash, x, "integer", is_spliced_bare, 1)
}, function (.x) 
{
  .Call(vflatten_impl, .x, "integer")
})
c("package:purrr", "namespace:rlang", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
flatten_lgl
list(`package:purrr` = function (.x) 
{
  .Call(vflatten_impl, .x, "logical")
}, function (x) 
{
  .Call(ffi_squash, x, "logical", is_spliced_bare, 1)
}, function (.x) 
{
  .Call(vflatten_impl, .x, "logical")
})
c("package:purrr", "namespace:rlang", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
flatten_raw
list(`package:purrr` = function (.x) 
{
  .Call(vflatten_impl, .x, "raw")
}, function (x) 
{
  .Call(ffi_squash, x, "raw", is_spliced_bare, 1)
}, function (.x) 
{
  .Call(vflatten_impl, .x, "raw")
})
c("package:purrr", "namespace:rlang", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
has_element
list(`package:purrr` = function (.x, .y) 
{
  some(.x, identical, .y)
}, function (.x, .y) 
{
  for (i in seq_along(.x)) {
    if (identical(.x[[i]], .y)) {
      return(TRUE)
    }
  }
  return(FALSE)
}, function (.x, .y) 
{
  some(.x, identical, .y)
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
head_while
list(`package:purrr` = function (.x, .p, ...) 
{
  loc <- detect_index(.x, negate(.p), ...)
  if (loc == 0) 
    return(.x)
  .x[seq_len(loc - 1)]
}, function (.x, .p, ...) 
{
  loc <- detect_index(.x, negate(.p), ...)
  if (loc == 0) 
    return(.x)
  .x[seq_len(loc - 1)]
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
imap
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2(.x, vec_index(.x), .f, ...)
}, function (.x, .f, ...) 
{
  map2(.x, names(.x) %||% character(0), .f, ...)
}, function (.x, .f, ...) 
{
  map2(.x, vec_index(.x), .f, ...)
}, function (.x, .f, ...) 
{
  map2(.x, names(.x) %||% seq_along(.x), .f, ...)
}, function (.x, .f) 
{
  Map(.f, .x = .x, .y = seq_along(.x))
}, function (.x, .f, ...) 
{
  map2(.x, vecpurrr_index(.x), .f, ...)
}, function (.x, .f, ...) 
{
  .nn = names(.x) %??% seq_along(.x)
  setNames(mapply_list(.f, list(.x, .nn), list(...)), names(.x))
}, function (.x, .f, ...) 
{
  map2(.x, names(.x) %||% seq_along(.x), .f, ...)
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2(.x, vec_index(.x), .f, ...)
})
c("package:purrr", "namespace:tidyr", "namespace:pillar", "namespace:rlang", "namespace:mlr", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
imap_chr
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2_chr(.x, vec_index(.x), .f, ...)
}, function (.x, .f, ...) 
{
  .nn = names(.x) %??% seq_along(.x)
  setNames(pmap_chr(c(list(.x, .nn)), .f), names(.x))
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2_chr(.x, vec_index(.x), .f, ...)
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
imap_dbl
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2_dbl(.x, vec_index(.x), .f, ...)
}, function (.x, .f, ...) 
{
  .nn = names(.x) %??% seq_along(.x)
  setNames(pmap_dbl(c(list(.x, .nn)), .f), names(.x))
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2_dbl(.x, vec_index(.x), .f, ...)
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
imap_dfc
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2_dfc(.x, vec_index(.x), .f, ...)
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2_dfc(.x, vec_index(.x), .f, ...)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
imap_dfr
list(`package:purrr` = function (.x, .f, ..., .id = NULL) 
{
  .f <- as_mapper(.f, ...)
  map2_dfr(.x, vec_index(.x), .f, ..., .id = .id)
}, function (.x, .f, ..., .id = NULL) 
{
  .f <- as_mapper(.f, ...)
  map2_dfr(.x, vec_index(.x), .f, ..., .id = .id)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
imap_int
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2_int(.x, vec_index(.x), .f, ...)
}, function (.x, .f, ...) 
{
  .nn = names(.x) %??% seq_along(.x)
  setNames(pmap_int(c(list(.x, .nn)), .f), names(.x))
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2_int(.x, vec_index(.x), .f, ...)
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
imap_lgl
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2_lgl(.x, vec_index(.x), .f, ...)
}, function (.x, .f, ...) 
{
  .nn = names(.x) %??% seq_along(.x)
  setNames(pmap_lgl(c(list(.x, .nn)), .f), names(.x))
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2_lgl(.x, vec_index(.x), .f, ...)
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
imap_raw
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2_raw(.x, vec_index(.x), .f, ...)
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  map2_raw(.x, vec_index(.x), .f, ...)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
imodify
list(`package:purrr` = function (.x, .f, ...) 
{
  modify2(.x, vec_index(.x), .f, ...)
}, function (.x, .f, ...) 
{
  modify2(.x, vec_index(.x), .f, ...)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
insistently
list(`package:purrr` = function (f, rate = rate_backoff(), quiet = TRUE) 
{
  f <- as_mapper(f)
  force(quiet)
  if (!is_rate(rate)) {
    stop_bad_type(rate, "a rate", arg = "rate")
  }
  function(...) {
    rate_reset(rate)
    repeat {
      rate_sleep(rate, quiet = quiet)
      out <- capture_error(f(...), quiet = quiet)
      if (is_null(out$error)) {
        return(out$result)
      }
    }
  }
}, function (f, rate = rate_backoff(), quiet = TRUE) 
{
  f <- as_mapper(f)
  force(quiet)
  if (!is_rate(rate)) {
    stop_bad_type(rate, "a rate", arg = "rate")
  }
  function(...) {
    rate_reset(rate)
    repeat {
      rate_sleep(rate, quiet = quiet)
      out <- capture_error(f(...), quiet = quiet)
      if (is_null(out$error)) {
        return(out$result)
      }
    }
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
invoke
list(`package:purrr` = function (.f, .x = NULL, ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  args <- c(as.list(.x), list(...))
  do.call(.f, args, envir = .env)
}, function (.fn, .args = list(), ..., .env = caller_env(), .bury = c(".fn", "")) 
{
  warn_deprecated(c("`invoke()` is deprecated as of rlang 0.4.0.", "Please use `exec()` or `inject()` instead."))
  args <- c(.args, list(...))
  if (is_null(.bury) || !length(args)) {
    if (is_scalar_character(.fn)) {
      .fn <- env_get(.env, .fn, inherit = TRUE)
    }
    call <- call2(.fn, !!!args)
    return(.External2(ffi_eval, call, .env))
  }
  if (!is_character(.bury, 2)) {
    abort("`.bury` must be a character vector of length 2")
  }
  arg_prefix <- .bury[[2]]
  fn_nm <- .bury[[1]]
  buried_nms <- paste0(arg_prefix, seq_along(args))
  buried_args <- set_names(args, buried_nms)
  .env <- env_bury(.env, !!!buried_args)
  args <- set_names(buried_nms, names(args))
  args <- syms(args)
  if (is_function(.fn)) {
    env_bind(.env, `:=`(!!fn_nm, .fn))
    .fn <- fn_nm
  }
  call <- call2(.fn, !!!args)
  .External2(ffi_eval, call, .env)
}, function (.f, ..., .args = list(), .opts = list(), .seed = NA, .timeout = Inf) 
{
  if (length(.opts)) {
    assert_list(.opts, names = "unique")
    old_opts = options(.opts)
    if (getRversion() < "3.6.0") {
      nn = intersect(c("warnPartialMatchArgs", "warnPartialMatchAttr", "warnPartialMatchDollar"), names(old_opts))
      nn = nn[map_lgl(old_opts[nn], is.null)]
      old_opts[nn] = FALSE
    }
    on.exit(options(old_opts), add = TRUE)
  }
  if (is.finite(assert_number(.timeout, lower = 0))) {
    setTimeLimit(elapsed = .timeout, transient = TRUE)
    on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
  }
  if (!is.na(.seed)) {
    prev_seed = get_seed()
    on.exit(assign(".Random.seed", prev_seed, globalenv()), add = TRUE)
    set.seed(.seed)
  }
  call = match.call(expand.dots = FALSE)
  expr = as.call(c(list(call[[2]]), call$..., .args))
  eval.parent(expr, n = 1)
}, function (.f, .x = NULL, ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  args <- c(as.list(.x), list(...))
  do.call(.f, args, envir = .env)
})
c("package:purrr", "namespace:rlang", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, TRUE)
invoke_map
list(`package:purrr` = function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2(.f, .x, invoke, ..., .env = .env)
}, function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2(.f, .x, invoke, ..., .env = .env)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
invoke_map_chr
list(`package:purrr` = function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_chr(.f, .x, invoke, ..., .env = .env)
}, function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_chr(.f, .x, invoke, ..., .env = .env)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
invoke_map_dbl
list(`package:purrr` = function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_dbl(.f, .x, invoke, ..., .env = .env)
}, function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_dbl(.f, .x, invoke, ..., .env = .env)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
invoke_map_df
list(`package:purrr` = function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_dfr(.f, .x, invoke, ..., .env = .env)
}, function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_dfr(.f, .x, invoke, ..., .env = .env)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
invoke_map_dfc
list(`package:purrr` = function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_dfc(.f, .x, invoke, ..., .env = .env)
}, function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_dfc(.f, .x, invoke, ..., .env = .env)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
invoke_map_dfr
list(`package:purrr` = function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_dfr(.f, .x, invoke, ..., .env = .env)
}, function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_dfr(.f, .x, invoke, ..., .env = .env)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
invoke_map_int
list(`package:purrr` = function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_int(.f, .x, invoke, ..., .env = .env)
}, function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_int(.f, .x, invoke, ..., .env = .env)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
invoke_map_lgl
list(`package:purrr` = function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_lgl(.f, .x, invoke, ..., .env = .env)
}, function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_lgl(.f, .x, invoke, ..., .env = .env)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
invoke_map_raw
list(`package:purrr` = function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_raw(.f, .x, invoke, ..., .env = .env)
}, function (.f, .x = list(NULL), ..., .env = NULL) 
{
  .env <- .env %||% parent.frame()
  .f <- as_invoke_function(.f)
  map2_raw(.f, .x, invoke, ..., .env = .env)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_atomic
list(`package:purrr` = function (x, n = NULL) 
{
  .Call(ffi_is_atomic, x, n)
}, function (x, n = NULL) 
{
  .Call(ffi_is_atomic, x, n)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_bare_atomic
list(`package:purrr` = function (x, n = NULL) 
{
  !is.object(x) && is_atomic(x, n)
}, function (x, n = NULL) 
{
  !is.object(x) && is_atomic(x, n)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_bare_character
list(`package:purrr` = function (x, n = NULL) 
{
  !is.object(x) && is_character(x, n)
}, function (x, n = NULL) 
{
  !is.object(x) && is_character(x, n)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_bare_double
list(`package:purrr` = function (x, n = NULL) 
{
  !is.object(x) && is_double(x, n)
}, function (x, n = NULL) 
{
  !is.object(x) && is_double(x, n)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_bare_integer
list(`package:purrr` = function (x, n = NULL) 
{
  !is.object(x) && is_integer(x, n)
}, function (x, n = NULL) 
{
  !is.object(x) && is_integer(x, n)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_bare_list
list(`package:purrr` = function (x, n = NULL) 
{
  !is.object(x) && is_list(x, n)
}, function (x, n = NULL) 
{
  !is.object(x) && is_list(x, n)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_bare_logical
list(`package:purrr` = function (x, n = NULL) 
{
  !is.object(x) && is_logical(x, n)
}, function (x, n = NULL) 
{
  !is.object(x) && is_logical(x, n)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_bare_numeric
list(`package:purrr` = function (x, n = NULL) 
{
  if (!is_null(n) && length(x) != n) 
    return(FALSE)
  !is.object(x) && typeof(x) %in% c("double", "integer")
}, function (x, n = NULL) 
{
  if (!is_null(n) && length(x) != n) 
    return(FALSE)
  !is.object(x) && typeof(x) %in% c("double", "integer")
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_bare_vector
list(`package:purrr` = function (x, n = NULL) 
{
  is_bare_atomic(x) || is_bare_list(x, n)
}, function (x, n = NULL) 
{
  is_bare_atomic(x) || is_bare_list(x, n)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_character
list(`package:purrr` = function (x, n = NULL) 
{
  .Call(ffi_is_character, x, n, NULL, NULL)
}, function (x, n = NULL) 
{
  .Call(ffi_is_character, x, n, NULL, NULL)
}, function (x, n = NULL) 
{
  if (typeof(x) != "character") 
    return(FALSE)
  if (!is_null(n)) {
    if (is_scalar_integerish(n) && length(x) != n) 
      return(FALSE)
    else if (is_function(n) && !n(length(x))) 
      return(FALSE)
  }
  TRUE
})
c("package:purrr", "namespace:rlang", "namespace:tidyselect")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
is_double
list(`package:purrr` = function (x, n = NULL, finite = NULL) 
{
  .Call(ffi_is_double, x, n, finite)
}, function (x, n = NULL, finite = NULL) 
{
  .Call(ffi_is_double, x, n, finite)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_empty
list(`package:purrr` = function (x) 
  length(x) == 0, function (x) 
    length(x) == 0, function (x) 
      length(x) == 0, function (x) 
      {
        identical(length(x), 0)
      })
c("package:purrr", "namespace:httr", "namespace:rlang", "namespace:lgr")
c(TRUE, FALSE, FALSE, FALSE)
c(FALSE, TRUE, TRUE, FALSE)
is_formula
list(`package:purrr` = function (x, scoped = NULL, lhs = NULL) 
{
  .Call(ffi_is_formula, x, scoped, lhs)
}, function (x, scoped = NULL, lhs = NULL) 
{
  .Call(ffi_is_formula, x, scoped, lhs)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_function
list(`package:purrr` = function (x) 
{
  .Call(ffi_is_function, x)
}, function (x) 
{
  .Call(ffi_is_function, x)
}, function (expr) 
{
  is.symbol(expr) || is.function(expr)
})
c("package:purrr", "namespace:rlang", "namespace:promises")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
is_integer
list(`package:purrr` = function (x, n = NULL) 
{
  .Call(ffi_is_integer, x, n)
}, function (x, n = NULL) 
{
  .Call(ffi_is_integer, x, n)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_list
list(`package:purrr` = function (x, n = NULL) 
{
  .Call(ffi_is_list, x, n)
}, function (x, n = NULL) 
{
  .Call(ffi_is_list, x, n)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_logical
list(`package:purrr` = function (x, n = NULL) 
{
  .Call(ffi_is_logical, x, n)
}, function (x, n = NULL) 
{
  .Call(ffi_is_logical, x, n)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_null
list(`package:purrr` = .Primitive("is.null"), .Primitive("is.null"))
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_numeric
list(`package:purrr` = function (x) 
{
  warning("Deprecated", call. = FALSE)
  is_integer(x) || is_double(x)
}, function (x) 
{
  warning("Deprecated", call. = FALSE)
  is_integer(x) || is_double(x)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_rate
list(`package:purrr` = function (x) 
{
  inherits(x, "purrr_rate")
}, function (x) 
{
  inherits(x, "purrr_rate")
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_scalar_atomic
list(`package:purrr` = function (x) 
{
  .Call(ffi_is_atomic, x, 1)
}, function (x) 
{
  .Call(ffi_is_atomic, x, 1)
}, function (x) 
{
  is.atomic(x) && is_scalar(x)
})
c("package:purrr", "namespace:rlang", "namespace:lgr")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
is_scalar_character
list(`package:purrr` = function (x) 
{
  is_character(x, n = 1)
}, function (x) 
{
  is_character(x, n = 1)
}, function (x) 
{
  is.character(x) && is_scalar(x)
})
c("package:purrr", "namespace:rlang", "namespace:lgr")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
is_scalar_double
list(`package:purrr` = function (x) 
{
  .Call(ffi_is_double, x, 1, NULL)
}, function (x) 
{
  .Call(ffi_is_double, x, 1, NULL)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_scalar_integer
list(`package:purrr` = function (x) 
{
  .Call(ffi_is_integer, x, 1)
}, function (x) 
{
  .Call(ffi_is_integer, x, 1)
}, function (x) 
{
  is.integer(x) && is_scalar(x)
})
c("package:purrr", "namespace:rlang", "namespace:lgr")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
is_scalar_list
list(`package:purrr` = function (x) 
{
  .Call(ffi_is_list, x, 1)
}, function (x) 
{
  .Call(ffi_is_list, x, 1)
}, function (x) 
{
  is.list(x) && is_scalar(x)
})
c("package:purrr", "namespace:rlang", "namespace:lgr")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
is_scalar_logical
list(`package:purrr` = function (x) 
{
  .Call(ffi_is_logical, x, 1)
}, function (x) 
{
  .Call(ffi_is_logical, x, 1)
}, function (x) 
{
  is.logical(x) && is_scalar(x)
})
c("package:purrr", "namespace:rlang", "namespace:lgr")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
is_scalar_numeric
list(`package:purrr` = function (x) 
{
  warning("Deprecated", call. = FALSE)
  is_scalar_integer(x) || is_scalar_double(x)
}, function (x) 
{
  is.numeric(x) && is_scalar(x)
}, function (x) 
{
  warning("Deprecated", call. = FALSE)
  is_scalar_integer(x) || is_scalar_double(x)
})
c("package:purrr", "namespace:lgr", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
is_scalar_vector
list(`package:purrr` = function (x) 
{
  .Call(ffi_is_vector, x, 1)
}, function (x) 
{
  .Call(ffi_is_vector, x, 1)
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)
is_vector
list(`package:purrr` = function (x, n = NULL) 
{
  .Call(ffi_is_vector, x, n)
}, function (x, n = NULL) 
{
  .Call(ffi_is_vector, x, n)
}, function (x) 
{
  is.atomic(x) || is.list(x)
})
c("package:purrr", "namespace:rlang", "namespace:lgr")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
iwalk
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  walk2(.x, vec_index(.x), .f, ...)
}, function (.x, .f, ...) 
{
  .nn = names(.x) %??% seq_along(.x)
  for (.i in seq_along(.x)) {
    .f(.x[[.i]], .nn[.i], ...)
  }
  invisible(.x)
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  walk2(.x, vec_index(.x), .f, ...)
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
keep
list(`package:purrr` = function (.x, .p, ...) 
{
  sel <- probe(.x, .p, ...)
  .x[!is.na(sel) & sel]
}, function (.x, .f, ...) 
{
  .x[probe(.x, .f, ...)]
}, function (.x, .f, ...) 
{
  .x[.rlang_purrr_probe(.x, .f, ...)]
}, function (.x, .f, ...) 
{
  .x[probe(.x, .f, ...)]
}, function (.x, .f, ...) 
{
  UseMethod("keep")
}, function (.x, .f, ...) 
{
  .x[.rlang_purrr_probe(.x, .f, ...)]
}, function (.x, .f, ...) 
{
  .x[probe(.x, .f, ...)]
}, function (.x, .p, ...) 
{
  sel <- probe(.x, .p, ...)
  .x[!is.na(sel) & sel]
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
lift
list(`package:purrr` = function (..f, ..., .unnamed = FALSE) 
{
  force(..f)
  defaults <- list(...)
  function(.x = list(), ...) {
    if (.unnamed) {
      .x <- unname(.x)
    }
    do.call("..f", c(.x, defaults, list(...)))
  }
}, function (..f, ..., .unnamed = FALSE) 
{
  force(..f)
  defaults <- list(...)
  function(.x = list(), ...) {
    if (.unnamed) {
      .x <- unname(.x)
    }
    do.call("..f", c(.x, defaults, list(...)))
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
lift_dl
list(`package:purrr` = function (..f, ..., .unnamed = FALSE) 
{
  force(..f)
  defaults <- list(...)
  function(.x = list(), ...) {
    if (.unnamed) {
      .x <- unname(.x)
    }
    do.call("..f", c(.x, defaults, list(...)))
  }
}, function (..f, ..., .unnamed = FALSE) 
{
  force(..f)
  defaults <- list(...)
  function(.x = list(), ...) {
    if (.unnamed) {
      .x <- unname(.x)
    }
    do.call("..f", c(.x, defaults, list(...)))
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
lift_dv
list(`package:purrr` = function (..f, ..., .unnamed = FALSE) 
{
  force(..f)
  defaults <- list(...)
  function(.x, ...) {
    if (.unnamed) {
      .x <- unname(.x)
    }
    .x <- as.list(.x)
    do.call("..f", c(.x, defaults, list(...)))
  }
}, function (..f, ..., .unnamed = FALSE) 
{
  force(..f)
  defaults <- list(...)
  function(.x, ...) {
    if (.unnamed) {
      .x <- unname(.x)
    }
    .x <- as.list(.x)
    do.call("..f", c(.x, defaults, list(...)))
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
lift_ld
list(`package:purrr` = function (..f, ...) 
{
  force(..f)
  defaults <- list(...)
  function(...) {
    do.call("..f", c(list(list(...)), defaults))
  }
}, function (..f, ...) 
{
  force(..f)
  defaults <- list(...)
  function(...) {
    do.call("..f", c(list(list(...)), defaults))
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
lift_lv
list(`package:purrr` = function (..f, ...) 
{
  force(..f)
  defaults <- list(...)
  function(.x, ...) {
    do.call("..f", c(list(as.list(.x)), defaults, list(...)))
  }
}, function (..f, ...) 
{
  force(..f)
  defaults <- list(...)
  function(.x, ...) {
    do.call("..f", c(list(as.list(.x)), defaults, list(...)))
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
lift_vd
list(`package:purrr` = function (..f, ..., .type) 
{
  force(..f)
  defaults <- list(...)
  if (missing(.type)) 
    .type <- NULL
  function(...) {
    x <- as_vector(list(...), .type)
    do.call("..f", c(list(x), defaults))
  }
}, function (..f, ..., .type) 
{
  force(..f)
  defaults <- list(...)
  if (missing(.type)) 
    .type <- NULL
  function(...) {
    x <- as_vector(list(...), .type)
    do.call("..f", c(list(x), defaults))
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
lift_vl
list(`package:purrr` = function (..f, ..., .type) 
{
  force(..f)
  defaults <- list(...)
  if (missing(.type)) 
    .type <- NULL
  function(.x = list(), ...) {
    x <- as_vector(.x, .type)
    do.call("..f", c(list(x), defaults, list(...)))
  }
}, function (..f, ..., .type) 
{
  force(..f)
  defaults <- list(...)
  if (missing(.type)) 
    .type <- NULL
  function(.x = list(), ...) {
    x <- as_vector(.x, .type)
    do.call("..f", c(list(x), defaults, list(...)))
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
list_along
list(`package:purrr` = function (x) 
{
  vector("list", length(x))
}, function (x) 
{
  vector("list", length(x))
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
list_merge
list(`package:purrr` = function (.x, ...) 
{
  list_recurse(.x, list2(...), c)
}, function (.x, ...) 
{
  list_recurse(.x, list2(...), c)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
list_modify
list(`package:purrr` = function (.x, ...) 
{
  list_recurse(.x, list2(...), function(x, y) y)
}, function (.x, ...) 
{
  list_recurse(.x, list2(...), function(x, y) y)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
lmap
list(`package:purrr` = function (.x, .f, ...) 
{
  lmap_at(.x, seq_along(.x), .f, ...)
}, function (.x, .f, ...) 
{
  lmap_at(.x, seq_along(.x), .f, ...)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
lmap_at
list(`package:purrr` = function (.x, .at, .f, ...) 
{
  if (is_formula(.f)) {
    .f <- as_mapper(.f, ...)
  }
  where <- at_selection(names(.x), .at)
  sel <- inv_which(.x, where)
  out <- vector("list", length(.x))
  for (i in seq_along(.x)) {
    res <- if (sel[[i]]) {
      .f(.x[i], ...)
    }
    else {
      .x[i]
    }
    stopifnot(is.list(res))
    out[[i]] <- res
  }
  maybe_as_data_frame(flatten(out), .x)
}, function (.x, .at, .f, ...) 
{
  if (is_formula(.f)) {
    .f <- as_mapper(.f, ...)
  }
  where <- at_selection(names(.x), .at)
  sel <- inv_which(.x, where)
  out <- vector("list", length(.x))
  for (i in seq_along(.x)) {
    res <- if (sel[[i]]) {
      .f(.x[i], ...)
    }
    else {
      .x[i]
    }
    stopifnot(is.list(res))
    out[[i]] <- res
  }
  maybe_as_data_frame(flatten(out), .x)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
lmap_if
list(`package:purrr` = function (.x, .p, .f, ..., .else = NULL) 
{
  sel <- probe(.x, .p)
  .x <- lmap_at(.x, which(sel), .f, ...)
  if (!is_null(.else)) {
    .x <- lmap_at(.x, which(!sel), .else, ...)
  }
  .x
}, function (.x, .p, .f, ..., .else = NULL) 
{
  sel <- probe(.x, .p)
  .x <- lmap_at(.x, which(sel), .f, ...)
  if (!is_null(.else)) {
    .x <- lmap_at(.x, which(!sel), .else, ...)
  }
  .x
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
map
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map_impl, environment(), ".x", ".f", "list")
}, function (x, ...) 
{
  get(".listenv.map", envir = parent.env(x), inherits = FALSE)
}, function (.x, .f, ...) 
{
  lapply(.x, .f, ...)
}, function (.x, .f, ...) 
{
  .f <- as_function(.f, env = global_env())
  lapply(.x, .f, ...)
}, function (.x, .f, ...) 
{
  lapply(.x, .f, ...)
}, function (.x, .f, ...) 
{
  if (is.function(.f)) {
    lapply(.x, .f, ...)
  }
  else {
    lapply(.x, `[[`, .f, ...)
  }
}, function (.x, .f, ...) 
{
  .f <- as_function(.f, env = global_env())
  lapply(.x, .f, ...)
}, function (.x, .f, ...) 
{
  lapply(.x, .f, ...)
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map_impl, environment(), ".x", ".f", "list")
})
c("package:purrr", "namespace:listenv", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
map_at
list(`package:purrr` = function (.x, .at, .f, ...) 
{
  where <- at_selection(names(.x), .at)
  sel <- inv_which(.x, where)
  out <- list_along(.x)
  out[sel] <- map(.x[sel], .f, ...)
  out[!sel] <- .x[!sel]
  set_names(out, names(.x))
}, function (.x, .at, .f, ...) 
{
  UseMethod("map_at")
}, function (.x, .at, .f, ...) 
{
  where <- at_selection(names(.x), .at)
  sel <- inv_which(.x, where)
  out <- list_along(.x)
  out[sel] <- map(.x[sel], .f, ...)
  out[!sel] <- .x[!sel]
  set_names(out, names(.x))
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
map_call
list(`package:purrr` = function (.x, .f, ...) 
{
  .Defunct("`map_call()` is deprecated. Please use `invoke()` instead.")
}, function (.x, .f, ...) 
{
  .Defunct("`map_call()` is deprecated. Please use `invoke()` instead.")
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
map_chr
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map_impl, environment(), ".x", ".f", "character")
}, function (.x, .f, ...) 
{
  vapply(X = .x, FUN = .f, FUN.VALUE = character(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, character(1), ...)
}, function (.x, .f, ...) 
{
  .rlang_purrr_map_mold(.x, .f, character(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, character(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, NA, ...)
}, function (.x, .f, ...) 
{
  .rlang_purrr_map_mold(.x, .f, character(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, character(1), ...)
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map_impl, environment(), ".x", ".f", "character")
})
c("package:purrr", "namespace:ps", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
map_dbl
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map_impl, environment(), ".x", ".f", "double")
}, function (.x, .f, ...) 
{
  vapply(X = .x, FUN = .f, FUN.VALUE = double(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, double(1), ...)
}, function (.x, .f, ...) 
{
  .rlang_purrr_map_mold(.x, .f, double(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, double(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, NA, ...)
}, function (.x, .f, ...) 
{
  .rlang_purrr_map_mold(.x, .f, double(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, double(1), ...)
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map_impl, environment(), ".x", ".f", "double")
})
c("package:purrr", "namespace:ps", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
map_depth
list(`package:purrr` = function (.x, .depth, .f, ..., .ragged = FALSE) 
{
  if (!is_integerish(.depth, n = 1, finite = TRUE)) {
    abort("`.depth` must be a single number")
  }
  if (.depth < 0) {
    .depth <- vec_depth(.x) + .depth
  }
  .f <- as_mapper(.f, ...)
  map_depth_rec(.x, .depth, .f, ..., .ragged = .ragged, .atomic = FALSE)
}, function (.x, .depth, .f, ..., .ragged = FALSE) 
{
  if (!is_integerish(.depth, n = 1, finite = TRUE)) {
    abort("`.depth` must be a single number")
  }
  if (.depth < 0) {
    .depth <- vec_depth(.x) + .depth
  }
  .f <- as_mapper(.f, ...)
  map_depth_rec(.x, .depth, .f, ..., .ragged = .ragged, .atomic = FALSE)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
map_df
list(`package:purrr` = function (.x, .f, ..., .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`map_df()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map(.x, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}, function (.x, .f, ..., .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`map_df()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map(.x, .f, ...)
  dplyr::bind_rows(res, .id = .id)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
map_dfc
list(`package:purrr` = function (.x, .f, ...) 
{
  if (!is_installed("dplyr")) {
    abort("`map_dfc()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map(.x, .f, ...)
  dplyr::bind_cols(res)
}, function (.x, .f, ...) 
{
  if (!is_installed("dplyr")) {
    abort("`map_dfc()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map(.x, .f, ...)
  dplyr::bind_cols(res)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
map_dfr
list(`package:purrr` = function (.x, .f, ..., .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`map_df()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map(.x, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}, function (.x, .f, ..., .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`map_df()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map(.x, .f, ...)
  dplyr::bind_rows(res, .id = .id)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
map_if
list(`package:purrr` = function (.x, .p, .f, ..., .else = NULL) 
{
  sel <- probe(.x, .p)
  out <- list_along(.x)
  out[sel] <- map(.x[sel], .f, ...)
  if (is_null(.else)) {
    out[!sel] <- .x[!sel]
  }
  else {
    out[!sel] <- map(.x[!sel], .else, ...)
  }
  set_names(out, names(.x))
}, function (.x, .p, .f, ...) 
{
  matches <- probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}, function (.x, .p, .f, ...) 
{
  matches <- .rlang_purrr_probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}, function (.x, .p, .f, ...) 
{
  matches <- probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}, function (.x, .p, .f, ...) 
{
  UseMethod("map_if")
}, function (.x, .p, .f, ...) 
{
  matches <- .rlang_purrr_probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}, function (.x, .p, .f, ...) 
{
  matches <- probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  .x
}, function (.x, .p, .f, ..., .else = NULL) 
{
  sel <- probe(.x, .p)
  out <- list_along(.x)
  out[sel] <- map(.x[sel], .f, ...)
  if (is_null(.else)) {
    out[!sel] <- .x[!sel]
  }
  else {
    out[!sel] <- map(.x[!sel], .else, ...)
  }
  set_names(out, names(.x))
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
map_int
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map_impl, environment(), ".x", ".f", "integer")
}, function (.x, .f, ...) 
{
  vapply(X = .x, FUN = .f, FUN.VALUE = integer(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, integer(1), ...)
}, function (.x, .f, ...) 
{
  .rlang_purrr_map_mold(.x, .f, integer(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, integer(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, NA, ...)
}, function (.x, .f, ...) 
{
  .rlang_purrr_map_mold(.x, .f, integer(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, integer(1), ...)
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map_impl, environment(), ".x", ".f", "integer")
})
c("package:purrr", "namespace:ps", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
map_lgl
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map_impl, environment(), ".x", ".f", "logical")
}, function (.x, .f, ...) 
{
  vapply(X = .x, FUN = .f, FUN.VALUE = logical(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, logical(1), ...)
}, function (.x, .f, ...) 
{
  .rlang_purrr_map_mold(.x, .f, logical(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, logical(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, NA, ...)
}, function (.x, .f, ...) 
{
  .rlang_purrr_map_mold(.x, .f, logical(1), ...)
}, function (.x, .f, ...) 
{
  map_mold(.x, .f, logical(1), ...)
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map_impl, environment(), ".x", ".f", "logical")
})
c("package:purrr", "namespace:ps", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
map_raw
list(`package:purrr` = function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map_impl, environment(), ".x", ".f", "raw")
}, function (.x, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map_impl, environment(), ".x", ".f", "raw")
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
map2
list(`package:purrr` = function (.x, .y, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map2_impl, environment(), ".x", ".y", ".f", "list")
}, function (.x, .y, .f, ...) 
{
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  }
  else {
    set_names(out, NULL)
  }
}, function (.x, .y, .f, ...) 
{
  .f <- as_function(.f, env = global_env())
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  }
  else {
    set_names(out, NULL)
  }
}, function (.x, .y, .f, ...) 
{
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  }
  else {
    set_names(out, NULL)
  }
}, function (.x, .y, .f, ...) 
{
  .f <- as_function(.f, env = global_env())
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  }
  else {
    set_names(out, NULL)
  }
}, function (.x, .y, .f, ...) 
{
  Map(.f, .x, .y, ...)
}, function (.x, .y, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map2_impl, environment(), ".x", ".y", ".f", "list")
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
map2_chr
list(`package:purrr` = function (.x, .y, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map2_impl, environment(), ".x", ".y", ".f", "character")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "character")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "character")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "character")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "character")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "character")
}, function (.x, .y, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map2_impl, environment(), ".x", ".y", ".f", "character")
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
map2_dbl
list(`package:purrr` = function (.x, .y, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map2_impl, environment(), ".x", ".y", ".f", "double")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "double")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "double")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "double")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "double")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "double")
}, function (.x, .y, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map2_impl, environment(), ".x", ".y", ".f", "double")
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
map2_df
list(`package:purrr` = function (.x, .y, .f, ..., .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`map2_dfr()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map2(.x, .y, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}, function (.x, .y, .f, ..., .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`map2_dfr()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map2(.x, .y, .f, ...)
  dplyr::bind_rows(res, .id = .id)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
map2_dfc
list(`package:purrr` = function (.x, .y, .f, ...) 
{
  if (!is_installed("dplyr")) {
    abort("`map2_dfc()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map2(.x, .y, .f, ...)
  dplyr::bind_cols(res)
}, function (.x, .y, .f, ...) 
{
  if (!is_installed("dplyr")) {
    abort("`map2_dfc()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map2(.x, .y, .f, ...)
  dplyr::bind_cols(res)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
map2_dfr
list(`package:purrr` = function (.x, .y, .f, ..., .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`map2_dfr()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map2(.x, .y, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}, function (.x, .y, .f, ..., .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`map2_dfr()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- map2(.x, .y, .f, ...)
  dplyr::bind_rows(res, .id = .id)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
map2_int
list(`package:purrr` = function (.x, .y, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map2_impl, environment(), ".x", ".y", ".f", "integer")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "integer")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "integer")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "integer")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "integer")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "integer")
}, function (.x, .y, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map2_impl, environment(), ".x", ".y", ".f", "integer")
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
map2_lgl
list(`package:purrr` = function (.x, .y, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map2_impl, environment(), ".x", ".y", ".f", "logical")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "logical")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "logical")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "logical")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "logical")
}, function (.x, .y, .f, ...) 
{
  as.vector(map2(.x, .y, .f, ...), "logical")
}, function (.x, .y, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map2_impl, environment(), ".x", ".y", ".f", "logical")
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
map2_raw
list(`package:purrr` = function (.x, .y, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map2_impl, environment(), ".x", ".y", ".f", "raw")
}, function (.x, .y, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  .Call(map2_impl, environment(), ".x", ".y", ".f", "raw")
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
modify
list(`package:purrr` = function (.x, .f, ...) 
{
  UseMethod("modify")
}, function (.x, .f, ...) 
{
  UseMethod("modify")
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
modify_at
list(`package:purrr` = function (.x, .at, .f, ...) 
{
  UseMethod("modify_at")
}, function (.x, .at, .f, ...) 
{
  UseMethod("modify_at")
}, function (.x, .at, .f, ...) 
{
  UseMethod("modify_at")
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, TRUE)
modify_depth
list(`package:purrr` = function (.x, .depth, .f, ..., .ragged = .depth < 0) 
{
  if (!is_integerish(.depth, n = 1, finite = TRUE)) {
    abort("`.depth` must be a single number")
  }
  UseMethod("modify_depth")
}, function (.x, .depth, .f, ..., .ragged = .depth < 0) 
{
  if (!is_integerish(.depth, n = 1, finite = TRUE)) {
    abort("`.depth` must be a single number")
  }
  UseMethod("modify_depth")
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
modify_if
list(`package:purrr` = function (.x, .p, .f, ..., .else = NULL) 
{
  UseMethod("modify_if")
}, function (.x, .p, .f, ...) 
{
  UseMethod("modify_if")
}, function (.x, .p, .f, ..., .else = NULL) 
{
  UseMethod("modify_if")
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
modify_in
list(`package:purrr` = function (.x, .where, .f, ...) 
{
  .where <- as.list(.where)
  .f <- rlang::as_function(.f)
  value <- .f(chuck(.x, !!!.where), ...)
  assign_in(.x, .where, value)
}, function (.x, .where, .f, ...) 
{
  .where <- as.list(.where)
  .f <- rlang::as_function(.f)
  value <- .f(chuck(.x, !!!.where), ...)
  assign_in(.x, .where, value)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
modify2
list(`package:purrr` = function (.x, .y, .f, ...) 
{
  UseMethod("modify2")
}, function (.x, .y, .f, ...) 
{
  UseMethod("modify2")
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
negate
list(`package:purrr` = function (.p) 
{
  compose(`!`, as_mapper(.p))
}, function (.p) 
{
  function(...) !.p(...)
}, function (.p) 
{
  .p <- as_function(.p, env = global_env())
  function(...) !.p(...)
}, function (.p) 
{
  function(...) !.p(...)
}, function (.p) 
{
  .p <- as_function(.p, env = global_env())
  function(...) !.p(...)
}, function (.p) 
{
  function(...) !.p(...)
}, function (.p) 
{
  compose(`!`, as_mapper(.p))
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
none
list(`package:purrr` = function (.x, .p, ...) 
{
  every(.x, negate(.p), ...)
}, function (.x, .p, ...) 
{
  every(.x, negate(.p), ...)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
partial
list(`package:purrr` = function (.f, ..., .env = NULL, .lazy = NULL, .first = NULL) 
{
  args <- enquos(...)
  if (has_name(args, "...f")) {
    stop_defunct("`...f` has been renamed to `.f` as of purrr 0.3.0.")
  }
  fn_expr <- enexpr(.f)
  fn <- switch(typeof(.f), builtin = , special = as_closure(.f), closure = .f, abort(sprintf("`.f` must be a function, not %s", friendly_type_of(.f))))
  if (!is_null(.env)) {
    signal_soft_deprecated(paste_line("The `.env` argument is soft-deprecated as of purrr 0.3.0.", ))
  }
  if (!is_null(.lazy)) {
    signal_soft_deprecated(paste_line("The `.lazy` argument is soft-deprecated as of purrr 0.3.0.", "Please unquote the arguments that should be evaluated once and for all.", "", "  # Before:", "  partial(fn, u = runif(1), n = rnorm(1), .lazy = FALSE)", "", "  # After:", "  partial(fn, u = !!runif(1), n = !!rnorm(1))  # All constant", "  partial(fn, u = !!runif(1), n = rnorm(1))    # First constant"))
    if (!.lazy) {
      args <- map(args, eval_tidy, env = caller_env())
    }
  }
  if (!is_null(.first)) {
    signal_soft_deprecated(paste_line("The `.first` argument is soft-deprecated as of purrr 0.3.0.", "Please pass a `... =` argument instead.", "", "  # Before:", "  partial(fn, x = 1, y = 2, .first = FALSE)", "", "  # After:", "  partial(fn, x = 1, y = 2, ... = )  # Partialised arguments last", "  partial(fn, x = 1, ... = , y = 2)  # Partialised arguments around"))
  }
  if (is_false(.first)) {
    call <- call_modify(call2(fn), ... = , !!!args)
  }
  else {
    call <- call_modify(call2(fn), !!!args, ... = )
  }
  call <- new_quosure(call, caller_env())
  call <- quo_invert(call)
  mask <- new_data_mask(env())
  partialised <- function(...) {
    env_bind(mask, ... = env_get(current_env(), "..."))
    eval_tidy(call, mask)
  }
  structure(partialised, class = c("purrr_function_partial", "function"), body = call, fn = fn_expr)
}, function (.f, ..., .env = NULL, .lazy = NULL, .first = NULL) 
{
  args <- enquos(...)
  if (has_name(args, "...f")) {
    stop_defunct("`...f` has been renamed to `.f` as of purrr 0.3.0.")
  }
  fn_expr <- enexpr(.f)
  fn <- switch(typeof(.f), builtin = , special = as_closure(.f), closure = .f, abort(sprintf("`.f` must be a function, not %s", friendly_type_of(.f))))
  if (!is_null(.env)) {
    signal_soft_deprecated(paste_line("The `.env` argument is soft-deprecated as of purrr 0.3.0.", ))
  }
  if (!is_null(.lazy)) {
    signal_soft_deprecated(paste_line("The `.lazy` argument is soft-deprecated as of purrr 0.3.0.", "Please unquote the arguments that should be evaluated once and for all.", "", "  # Before:", "  partial(fn, u = runif(1), n = rnorm(1), .lazy = FALSE)", "", "  # After:", "  partial(fn, u = !!runif(1), n = !!rnorm(1))  # All constant", "  partial(fn, u = !!runif(1), n = rnorm(1))    # First constant"))
    if (!.lazy) {
      args <- map(args, eval_tidy, env = caller_env())
    }
  }
  if (!is_null(.first)) {
    signal_soft_deprecated(paste_line("The `.first` argument is soft-deprecated as of purrr 0.3.0.", "Please pass a `... =` argument instead.", "", "  # Before:", "  partial(fn, x = 1, y = 2, .first = FALSE)", "", "  # After:", "  partial(fn, x = 1, y = 2, ... = )  # Partialised arguments last", "  partial(fn, x = 1, ... = , y = 2)  # Partialised arguments around"))
  }
  if (is_false(.first)) {
    call <- call_modify(call2(fn), ... = , !!!args)
  }
  else {
    call <- call_modify(call2(fn), !!!args, ... = )
  }
  call <- new_quosure(call, caller_env())
  call <- quo_invert(call)
  mask <- new_data_mask(env())
  partialised <- function(...) {
    env_bind(mask, ... = env_get(current_env(), "..."))
    eval_tidy(call, mask)
  }
  structure(partialised, class = c("purrr_function_partial", "function"), body = call, fn = fn_expr)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
pluck
list(`package:purrr` = function (.x, ..., .default = NULL) 
{
  .Call(pluck_impl, x = .x, index = list2(...), missing = .default, strict = FALSE)
}, function (.x, .f) 
{
  map(.x, `[[`, .f)
}, function (.x, .f) 
{
  map(.x, `[[`, .f)
}, function (.x, .f) 
{
  map(.x, `[[`, .f)
}, function (.x, ..., .default = NULL) 
{
  .Call(pluck_impl, x = .x, index = list2(...), missing = .default, strict = FALSE)
})
c("package:purrr", "namespace:pillar", "namespace:tibble", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, TRUE, TRUE)
pluck<-
  list(`package:purrr` = function (.x, ..., value) 
  {
    assign_in(.x, list2(...), value)
  }, function (.x, ..., value) 
  {
    assign_in(.x, list2(...), value)
  })
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
pmap
list(`package:purrr` = function (.l, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  .Call(pmap_impl, environment(), ".l", ".f", "list")
}, function (.l, .f, ...) 
{
  args <- args_recycle(.l)
  do.call("mapply", c(FUN = list(quote(.f)), args, MoreArgs = quote(list(...)), SIMPLIFY = FALSE, USE.NAMES = FALSE))
}, function (.l, .f, ...) 
{
  .f <- as.function(.f)
  args <- .rlang_purrr_args_recycle(.l)
  do.call("mapply", c(FUN = list(quote(.f)), args, MoreArgs = quote(list(...)), SIMPLIFY = FALSE, USE.NAMES = FALSE))
}, function (.l, .f, ...) 
{
  args <- args_recycle(.l)
  do.call("mapply", c(FUN = list(quote(.f)), args, MoreArgs = quote(list(...)), SIMPLIFY = FALSE, USE.NAMES = FALSE))
}, function (.x, .f, ...) 
{
  mapply_list(.f, .x, list(...))
}, function (.l, .f, ...) 
{
  .f <- as.function(.f)
  args <- .rlang_purrr_args_recycle(.l)
  do.call("mapply", c(FUN = list(quote(.f)), args, MoreArgs = quote(list(...)), SIMPLIFY = FALSE, USE.NAMES = FALSE))
}, function (.l, .f, ...) 
{
  args <- args_recycle(.l)
  do.call("mapply", c(FUN = list(quote(.f)), args, MoreArgs = quote(list(...)), SIMPLIFY = FALSE, USE.NAMES = FALSE))
}, function (.l, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  .Call(pmap_impl, environment(), ".l", ".f", "list")
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
pmap_chr
list(`package:purrr` = function (.l, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  .Call(pmap_impl, environment(), ".l", ".f", "character")
}, function (.x, .f, ...) 
{
  out = mapply_list(.f, .x, list(...))
  tryCatch(as.vector(out, "character"), warning = function(w) stop("Cannot convert to character"))
}, function (.l, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  .Call(pmap_impl, environment(), ".l", ".f", "character")
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
pmap_dbl
list(`package:purrr` = function (.l, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  .Call(pmap_impl, environment(), ".l", ".f", "double")
}, function (.x, .f, ...) 
{
  out = mapply_list(.f, .x, list(...))
  tryCatch(as.vector(out, "double"), warning = function(w) stop("Cannot convert to double"))
}, function (.l, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  .Call(pmap_impl, environment(), ".l", ".f", "double")
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
pmap_df
list(`package:purrr` = function (.l, .f, ..., .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`pmap_dfr()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- pmap(.l, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}, function (.l, .f, ..., .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`pmap_dfr()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- pmap(.l, .f, ...)
  dplyr::bind_rows(res, .id = .id)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
pmap_dfc
list(`package:purrr` = function (.l, .f, ...) 
{
  if (!is_installed("dplyr")) {
    abort("`pmap_dfc()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- pmap(.l, .f, ...)
  dplyr::bind_cols(res)
}, function (.l, .f, ...) 
{
  if (!is_installed("dplyr")) {
    abort("`pmap_dfc()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- pmap(.l, .f, ...)
  dplyr::bind_cols(res)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
pmap_dfr
list(`package:purrr` = function (.l, .f, ..., .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`pmap_dfr()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- pmap(.l, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}, function (.l, .f, ..., .id = NULL) 
{
  if (!is_installed("dplyr")) {
    abort("`pmap_dfr()` requires dplyr")
  }
  .f <- as_mapper(.f, ...)
  res <- pmap(.l, .f, ...)
  dplyr::bind_rows(res, .id = .id)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
pmap_int
list(`package:purrr` = function (.l, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  .Call(pmap_impl, environment(), ".l", ".f", "integer")
}, function (.x, .f, ...) 
{
  out = mapply_list(.f, .x, list(...))
  tryCatch(as.vector(out, "integer"), warning = function(w) stop("Cannot convert to integer"))
}, function (.l, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  .Call(pmap_impl, environment(), ".l", ".f", "integer")
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
pmap_lgl
list(`package:purrr` = function (.l, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  .Call(pmap_impl, environment(), ".l", ".f", "logical")
}, function (.x, .f, ...) 
{
  out = mapply_list(.f, .x, list(...))
  tryCatch(as.vector(out, "logical"), warning = function(w) stop("Cannot convert to logical"))
}, function (.l, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  .Call(pmap_impl, environment(), ".l", ".f", "logical")
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
pmap_raw
list(`package:purrr` = function (.l, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  .Call(pmap_impl, environment(), ".l", ".f", "raw")
}, function (.l, .f, ...) 
{
  .f <- as_mapper(.f, ...)
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  .Call(pmap_impl, environment(), ".l", ".f", "raw")
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
possibly
list(`package:purrr` = function (.f, otherwise, quiet = TRUE) 
{
  .f <- as_mapper(.f)
  force(otherwise)
  function(...) {
    tryCatch(.f(...), error = function(e) {
      if (!quiet) 
        message("Error: ", e$message)
      otherwise
    }, interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    })
  }
}, function (.f, otherwise, quiet = TRUE) 
{
  .f <- as_mapper(.f)
  force(otherwise)
  function(...) {
    tryCatch(.f(...), error = function(e) {
      if (!quiet) 
        message("Error: ", e$message)
      otherwise
    }, interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    })
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
prepend
list(`package:purrr` = function (x, values, before = NULL) 
{
  n <- length(x)
  stopifnot(is.null(before) || (before > 0 && before <= n))
  if (is.null(before) || before == 1) {
    c(values, x)
  }
  else {
    c(x[1:(before - 1)], values, x[before:n])
  }
}, function (x, values, before = NULL) 
{
  n <- length(x)
  stopifnot(is.null(before) || (before > 0 && before <= n))
  if (is.null(before) || before == 1) {
    c(values, x)
  }
  else {
    c(x[1:(before - 1)], values, x[before:n])
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
pwalk
list(`package:purrr` = function (.l, .f, ...) 
{
  pmap(.l, .f, ...)
  invisible(.l)
}, function (.x, .f, ...) 
{
  .wrapper = function(...) {
    .f(...)
    NULL
  }
  pmap(.x, .wrapper, ...)
  invisible(.x)
}, function (.l, .f, ...) 
{
  pmap(.l, .f, ...)
  invisible(.l)
})
c("package:purrr", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
quietly
list(`package:purrr` = function (.f) 
{
  .f <- as_mapper(.f)
  function(...) capture_output(.f(...))
}, function (.f) 
{
  .f <- as_mapper(.f)
  function(...) capture_output(.f(...))
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
rate_backoff
list(`package:purrr` = function (pause_base = 1, pause_cap = 60, pause_min = 1, max_times = 3, jitter = TRUE) 
{
  stopifnot(is_quantity(pause_base), is_quantity(pause_cap), is_quantity(pause_min))
  new_rate("purrr_rate_backoff", pause_base = pause_base, pause_cap = pause_cap, pause_min = pause_min, max_times = max_times, jitter = jitter)
}, function (pause_base = 1, pause_cap = 60, pause_min = 1, max_times = 3, jitter = TRUE) 
{
  stopifnot(is_quantity(pause_base), is_quantity(pause_cap), is_quantity(pause_min))
  new_rate("purrr_rate_backoff", pause_base = pause_base, pause_cap = pause_cap, pause_min = pause_min, max_times = max_times, jitter = jitter)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
rate_delay
list(`package:purrr` = function (pause = 1, max_times = Inf) 
{
  stopifnot(is_quantity(pause))
  new_rate("purrr_rate_delay", pause = pause, max_times = max_times, jitter = FALSE)
}, function (pause = 1, max_times = Inf) 
{
  stopifnot(is_quantity(pause))
  new_rate("purrr_rate_delay", pause = pause, max_times = max_times, jitter = FALSE)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
rate_reset
list(`package:purrr` = function (rate) 
{
  stopifnot(is_rate(rate))
  rate$state$i <- 0
  invisible(rate)
}, function (rate) 
{
  stopifnot(is_rate(rate))
  rate$state$i <- 0
  invisible(rate)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
rate_sleep
list(`package:purrr` = function (rate, quiet = TRUE) 
{
  stopifnot(is_rate(rate))
  i <- rate_count(rate)
  if (i > rate$max_times) {
    stop_rate_expired(rate)
  }
  if (i == rate$max_times) {
    stop_rate_excess(rate)
  }
  if (i == 0) {
    rate_bump_count(rate)
    signal_rate_init(rate)
    return(invisible())
  }
  on.exit(rate_bump_count(rate))
  UseMethod("rate_sleep")
}, function (rate, quiet = TRUE) 
{
  stopifnot(is_rate(rate))
  i <- rate_count(rate)
  if (i > rate$max_times) {
    stop_rate_expired(rate)
  }
  if (i == rate$max_times) {
    stop_rate_excess(rate)
  }
  if (i == 0) {
    rate_bump_count(rate)
    signal_rate_init(rate)
    return(invisible())
  }
  on.exit(rate_bump_count(rate))
  UseMethod("rate_sleep")
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
rbernoulli
list(`package:purrr` = function (n, p = 0.5) 
{
  stats::runif(n) > (1 - p)
}, function (n, p = 0.5) 
{
  stats::runif(n) > (1 - p)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
rdunif
list(`package:purrr` = function (n, b, a = 1) 
{
  stopifnot(is.numeric(a), length(a) == 1)
  stopifnot(is.numeric(b), length(b) == 1)
  a1 <- min(a, b)
  b1 <- max(a, b)
  sample(b1 - a1 + 1, n, replace = TRUE) + a1 - 1
}, function (n, b, a = 1) 
{
  stopifnot(is.numeric(a), length(a) == 1)
  stopifnot(is.numeric(b), length(b) == 1)
  a1 <- min(a, b)
  b1 <- max(a, b)
  sample(b1 - a1 + 1, n, replace = TRUE) + a1 - 1
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
reduce
list(`package:purrr` = function (.x, .f, ..., .init, .dir = c("forward", "backward")) 
{
  reduce_impl(.x, .f, ..., .init = .init, .dir = .dir)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}, function (.x, .f, ..., .init, .dir = c("forward", "backward")) 
{
  reduce_impl(.x, .f, ..., .init = .init, .dir = .dir)
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
reduce_right
list(`package:purrr` = function (.x, .f, ..., .init) 
{
  signal_soft_deprecated(paste_line("`reduce_right()` is soft-deprecated as of purrr 0.3.0.", "Please use the new `.dir` argument of `reduce()` instead.", "", "  # Before:", "  reduce_right(1:3, f)", "", "  # After:", "  reduce(1:3, f, .dir = \"backward\")  # New algorithm", "  reduce(rev(1:3), f)                # Same algorithm as reduce_right()", ""))
  .x <- rev(.x)
  reduce_impl(.x, .f, ..., .dir = "forward", .init = .init)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}, function (.x, .f, ..., .init) 
{
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}, function (.x, .f, ..., .init) 
{
  signal_soft_deprecated(paste_line("`reduce_right()` is soft-deprecated as of purrr 0.3.0.", "Please use the new `.dir` argument of `reduce()` instead.", "", "  # Before:", "  reduce_right(1:3, f)", "", "  # After:", "  reduce(1:3, f, .dir = \"backward\")  # New algorithm", "  reduce(rev(1:3), f)                # Same algorithm as reduce_right()", ""))
  .x <- rev(.x)
  reduce_impl(.x, .f, ..., .dir = "forward", .init = .init)
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
reduce2
list(`package:purrr` = function (.x, .y, .f, ..., .init) 
{
  reduce2_impl(.x, .y, .f, ..., .init = .init, .left = TRUE)
}, function (.x, .y, .f, ..., .init) 
{
  reduce2_impl(.x, .y, .f, ..., .init = .init, .left = TRUE)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
reduce2_right
list(`package:purrr` = function (.x, .y, .f, ..., .init) 
{
  signal_soft_deprecated(paste_line("`reduce2_right()` is soft-deprecated as of purrr 0.3.0.", "Please reverse your vectors and use `reduce2()` instead.", "", "  # Before:", "  reduce2_right(x, y, f)", "", "  # After:", "  reduce2(rev(x), rev(y), f)", ""))
  reduce2_impl(.x, .y, .f, ..., .init = .init, .left = FALSE)
}, function (.x, .y, .f, ..., .init) 
{
  signal_soft_deprecated(paste_line("`reduce2_right()` is soft-deprecated as of purrr 0.3.0.", "Please reverse your vectors and use `reduce2()` instead.", "", "  # Before:", "  reduce2_right(x, y, f)", "", "  # After:", "  reduce2(rev(x), rev(y), f)", ""))
  reduce2_impl(.x, .y, .f, ..., .init = .init, .left = FALSE)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
rep_along
list(`package:purrr` = function (along, x) 
{
  rep_len(x, length(along))
}, function (along, x) 
{
  rep_len(x, length(along))
}, function (x, y) 
{
  if (length(y) == 0) 
    return(NULL)
  rep(x, length(y))
})
c("package:purrr", "namespace:rlang", "namespace:gtable")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
rerun
list(`package:purrr` = function (.n, ...) 
{
  dots <- quos(...)
  if (length(dots) == 1 && !has_names(dots)) {
    dots <- dots[[1]]
    eval_dots <- eval_tidy
  }
  else {
    eval_dots <- function(x) lapply(x, eval_tidy)
  }
  out <- vector("list", .n)
  for (i in seq_len(.n)) {
    out[[i]] <- eval_dots(dots)
  }
  out
}, function (.n, ...) 
{
  dots <- quos(...)
  if (length(dots) == 1 && !has_names(dots)) {
    dots <- dots[[1]]
    eval_dots <- eval_tidy
  }
  else {
    eval_dots <- function(x) lapply(x, eval_tidy)
  }
  out <- vector("list", .n)
  for (i in seq_len(.n)) {
    out[[i]] <- eval_dots(dots)
  }
  out
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
safely
list(`package:purrr` = function (.f, otherwise = NULL, quiet = TRUE) 
{
  .f <- as_mapper(.f)
  function(...) capture_error(.f(...), otherwise, quiet)
}, function (.f, otherwise = NULL, quiet = TRUE) 
{
  .f <- as_mapper(.f)
  function(...) capture_error(.f(...), otherwise, quiet)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
set_names
list(`package:purrr` = function (x, nm = x, ...) 
{
  mold <- x
  .Call(ffi_set_names, x, mold, nm, environment())
}, function (x, nm = x, ...) 
{
  mold <- x
  .Call(ffi_set_names, x, mold, nm, environment())
}, function (x, nm = x, ...) 
{
  if (is.function(nm)) {
    nm = map_chr(names2(x), nm)
  }
  names(x) = nm
  x
}, .Primitive("names<-"), function (x, n) 
{
  names(x) <- n
  x
})
c("package:purrr", "namespace:rlang", "namespace:mlr3misc", "namespace:magrittr", "namespace:processx")
c(TRUE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, TRUE, FALSE, FALSE, FALSE)
simplify
list(`package:purrr` = function (.x, .type = NULL) 
{
  if (can_simplify(.x, .type)) {
    unlist(.x)
  }
  else {
    .x
  }
}, function (x) 
{
  if (length(x) == 2 && is_symbol(x[[1]], "~")) {
    return(simplify(x[[2]]))
  }
  if (length(x) < 3) {
    return(list(x))
  }
  op <- x[[1]]
  a <- x[[2]]
  b <- x[[3]]
  if (is_symbol(op, c("+", "*", "~"))) {
    c(simplify(a), simplify(b))
  }
  else if (is_symbol(op, "-")) {
    c(simplify(a), expr(-!!simplify(b)))
  }
  else {
    list(x)
  }
}, function (x, simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE, simplifyDate = simplifyVector, homoList = TRUE, flatten = FALSE, columnmajor = FALSE, simplifySubMatrix = simplifyMatrix) 
{
  if (!is.list(x) || !length(x)) {
    return(x)
  }
  if (isTRUE(simplifyDataFrame) && is.recordlist(x)) {
    mydf <- simplifyDataFrame(x, flatten = flatten, simplifyMatrix = simplifySubMatrix)
    if (isTRUE(simplifyDate) && is.data.frame(mydf) && is.datelist(mydf)) {
      return(parse_date(mydf[["$date"]]))
    }
    return(mydf)
  }
  if (isTRUE(simplifyVector) && is.null(names(x)) && is.scalarlist(x)) {
    return(list_to_vec(x))
  }
  out <- lapply(x, simplify, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame, simplifyMatrix = simplifySubMatrix, columnmajor = columnmajor, flatten = flatten)
  if (isTRUE(simplifyVector) && is.scalarlist(out) && all(vapply(out, inherits, logical(1), "POSIXt"))) {
    return(structure(list_to_vec(out), class = c("POSIXct", "POSIXt")))
  }
  if (isTRUE(simplifyMatrix) && isTRUE(simplifyVector) && is.matrixlist(out) && all(unlist(vapply(x, is.scalarlist, logical(1))))) {
    if (isTRUE(columnmajor)) {
      return(do.call(cbind, out))
    }
    else {
      return(do.call(rbind, out))
    }
  }
  if (isTRUE(simplifyMatrix) && is.arraylist(out)) {
    if (isTRUE(columnmajor)) {
      return(array(data = do.call(cbind, out), dim = c(dim(out[[1]]), length(out))))
    }
    else {
      return(array(data = do.call(rbind, lapply(out, as.vector)), dim = c(length(out), dim(out[[1]]))))
    }
  }
  if (isTRUE(homoList) && is.null(names(out))) {
    isemptylist <- vapply(out, identical, logical(1), list())
    if (any(isemptylist) & !all(isemptylist)) {
      if (all(vapply(out[!isemptylist], is.data.frame, logical(1)))) {
        for (i in which(isemptylist)) {
          out[[i]] <- data.frame()
        }
        return(out)
      }
      if (all(vapply(out[!isemptylist], function(z) {
        isTRUE(is.vector(z) && is.atomic(z))
      }, logical(1)))) {
        for (i in which(isemptylist)) {
          out[[i]] <- vector(mode = typeof(out[[which(!isemptylist)[1]]]))
        }
        return(out)
      }
    }
  }
  if (isTRUE(simplifyDate) && is.datelist(out)) {
    return(parse_date(out[["$date"]]))
  }
  return(out)
}, function (.x, .type = NULL) 
{
  if (can_simplify(.x, .type)) {
    unlist(.x)
  }
  else {
    .x
  }
})
c("package:purrr", "namespace:ggplot2", "namespace:jsonlite", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, TRUE)
simplify_all
list(`package:purrr` = function (.x, .type = NULL) 
{
  map(.x, simplify, .type = .type)
}, function (.x, .type = NULL) 
{
  map(.x, simplify, .type = .type)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
slowly
list(`package:purrr` = function (f, rate = rate_delay(), quiet = TRUE) 
{
  f <- as_mapper(f)
  force(quiet)
  if (!is_rate(rate)) {
    stop_bad_type(rate, "a rate", arg = "rate")
  }
  function(...) {
    rate_sleep(rate, quiet = quiet)
    f(...)
  }
}, function (f, rate = rate_delay(), quiet = TRUE) 
{
  f <- as_mapper(f)
  force(quiet)
  if (!is_rate(rate)) {
    stop_bad_type(rate, "a rate", arg = "rate")
  }
  function(...) {
    rate_sleep(rate, quiet = quiet)
    f(...)
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
some
list(`package:purrr` = function (.x, .p, ...) 
{
  .p <- as_predicate(.p, ..., .mapper = TRUE, .deprecate = TRUE)
  val <- FALSE
  for (i in seq_along(.x)) {
    val <- val || .p(.x[[i]], ...)
    if (is_true(val)) 
      return(TRUE)
  }
  val
}, function (.x, .p, ...) 
{
  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) {
      return(TRUE)
    }
  }
  FALSE
}, function (.x, .p, ...) 
{
  .p <- as_function(.p, env = global_env())
  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) 
      return(TRUE)
  }
  FALSE
}, function (.x, .p, ...) 
{
  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) 
      return(TRUE)
  }
  FALSE
}, function (.x, .p, ...) 
{
  any(map_lgl(.x, .p, ...), na.rm = TRUE)
}, function (.x, .p, ...) 
{
  .p <- as_function(.p, env = global_env())
  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) 
      return(TRUE)
  }
  FALSE
}, function (.x, .p, ...) 
{
  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) 
      return(TRUE)
  }
  FALSE
}, function (.x, .p, ...) 
{
  .p <- as_predicate(.p, ..., .mapper = TRUE, .deprecate = TRUE)
  val <- FALSE
  for (i in seq_along(.x)) {
    val <- val || .p(.x[[i]], ...)
    if (is_true(val)) 
      return(TRUE)
  }
  val
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:tibble", "namespace:mlr3misc", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
splice
list(`package:purrr` = function (...) 
{
  splice_if(list(...), is_bare_list)
}, function (x) 
{
  .Call(ffi_new_splice_box, x)
}, function (...) 
{
  splice_if(list(...), is_bare_list)
})
c("package:purrr", "namespace:rlang", "namespace:purrr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
tail_while
list(`package:purrr` = function (.x, .p, ...) 
{
  loc <- detect_index(.x, negate(.p), ..., .dir = "backward")
  if (loc == 0) 
    return(.x)
  .x[-seq_len(loc)]
}, function (.x, .p, ...) 
{
  loc <- detect_index(.x, negate(.p), ..., .dir = "backward")
  if (loc == 0) 
    return(.x)
  .x[-seq_len(loc)]
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
transpose
list(`package:purrr` = function (.l, .names = NULL) 
{
  .Call(transpose_impl, .l, .names)
}, function (data, ps = NULL, filter_na = TRUE, trafo = TRUE) 
{
  assert_data_table(data)
  assert_flag(filter_na)
  assert_flag(trafo)
  xs = transpose_list(data)
  if (filter_na) {
    xs = map(xs, function(x) Filter(Negate(is_scalar_na), x))
  }
  if (!is.null(ps) && trafo) {
    if (ps$has_trafo) 
      xs = map(xs, function(x) ps$trafo(x, ps))
  }
  return(xs)
}, function (.l) 
{
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  }
  else {
    fields <- set_names(inner_names)
  }
  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}, function (.l) 
{
  if (!length(.l)) {
    return(.l)
  }
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  }
  else {
    fields <- set_names(inner_names)
    .l <- map(.l, function(x) {
      if (is.null(names(x))) {
        set_names(x, inner_names)
      }
      else {
        x
      }
    })
  }
  .l <- map(.l, as.list)
  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}, function (l, fill = NA, ignore.empty = FALSE, keep.names = NULL, make.names = NULL) 
{
  if (!is.null(make.names)) {
    stopifnot(length(make.names) == 1)
    if (is.character(make.names)) {
      m = chmatch(make.names, names(l))
      if (is.na(m)) 
        stop("make.names='", make.names, "' not found in names of input")
      make.names = m
    }
    else {
      make.names = as.integer(make.names)
      if (is.na(make.names) || make.names < 1 || make.names > length(l)) 
        stop("make.names=", make.names, " is out of range [1,ncol=", length(l), "]")
    }
    colnames = as.character(l[[make.names]])
    l = if (is.data.table(l)) 
      l[, -make.names, with = FALSE]
    else l[-make.names]
  }
  ans = .Call(Ctranspose, l, fill, ignore.empty, keep.names)
  if (!is.null(make.names)) 
    setattr(ans, "names", c(keep.names, colnames))
  else if (is.data.frame(l)) 
    setattr(ans, "names", c(keep.names, paste0("V", seq_len(length(ans) - length(keep.names)))))
  if (is.data.table(l)) 
    setDT(ans)
  else if (is.data.frame(l)) 
    setDF(ans)
  ans[]
}, function (.l) 
{
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  }
  else {
    fields <- set_names(inner_names)
  }
  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}, function (.l) 
{
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  }
  else {
    fields <- set_names(inner_names)
  }
  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}, function (.l) 
{
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  }
  else {
    fields <- set_names(inner_names)
  }
  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}, function (.l, .names = NULL) 
{
  .Call(transpose_impl, .l, .names)
})
c("package:purrr", "namespace:paradox", "namespace:pillar", "namespace:rlang", "namespace:data.table", "namespace:tibble", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
update_list
list(`package:purrr` = function (.x, ...) 
{
  dots <- dots_list(...)
  formulas <- map_lgl(dots, is_bare_formula, lhs = FALSE, scoped = TRUE)
  dots <- map_if(dots, formulas, as_quosure)
  dots <- map_if(dots, is_quosure, eval_tidy, data = .x)
  list_recurse(.x, dots, function(x, y) y)
}, function (.x, ...) 
{
  dots <- dots_list(...)
  formulas <- map_lgl(dots, is_bare_formula, lhs = FALSE, scoped = TRUE)
  dots <- map_if(dots, formulas, as_quosure)
  dots <- map_if(dots, is_quosure, eval_tidy, data = .x)
  list_recurse(.x, dots, function(x, y) y)
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
vec_depth
list(`package:purrr` = function (x) 
{
  if (is_null(x)) {
    0
  }
  else if (is_atomic(x)) {
    1
  }
  else if (is_list(x)) {
    depths <- map_int(x, vec_depth)
    1 + max(depths, 0)
  }
  else {
    abort("`x` must be a vector")
  }
}, function (x) 
{
  if (is_null(x)) {
    0
  }
  else if (is_atomic(x)) {
    1
  }
  else if (is_list(x)) {
    depths <- map_int(x, vec_depth)
    1 + max(depths, 0)
  }
  else {
    abort("`x` must be a vector")
  }
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
walk
list(`package:purrr` = function (.x, .f, ...) 
{
  map(.x, .f, ...)
  invisible(.x)
}, function (.x, .f, ...) 
{
  map(.x, .f, ...)
  invisible(.x)
}, function (.x, .f, ...) 
{
  map(.x, .f, ...)
  invisible(.x)
}, function (.x, .f, ...) 
{
  for (i in seq_along(.x)) {
    .f(.x[[i]], ...)
  }
  NULL
}, function (.x, .f, ...) 
{
  map(.x, .f, ...)
  invisible(.x)
}, function (.x, .f, ...) 
{
  for (i in seq_along(.x)) {
    .f(.x[[i]], ...)
  }
  invisible(.x)
}, function (.x, .f, ...) 
{
  for (.xi in .x) {
    .f(.xi, ...)
  }
  invisible(.x)
}, function (.x, .f, ...) 
{
  map(.x, .f, ...)
  invisible(.x)
}, function (.x, .f, ...) 
{
  map(.x, .f, ...)
  invisible(.x)
}, function (.x, .f, ...) 
{
  map(.x, .f, ...)
  invisible(.x)
})
c("package:purrr", "namespace:pillar", "namespace:rlang", "namespace:htmltools", "namespace:tibble", "namespace:lgr", "namespace:mlr3misc", "namespace:dplyr", "namespace:vctrs", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
walk2
list(`package:purrr` = function (.x, .y, .f, ...) 
{
  map2(.x, .y, .f, ...)
  invisible(.x)
}, function (.x, .y, .f, ...) 
{
  if (length(.x) != length(.y)) {
    stop(".x and .y must be the same length.")
  }
  for (i in seq_along(.x)) {
    .f(.x[[i]], .y[[i]], ...)
  }
  NULL
}, function (.x, .f, ...) 
{
  .wrapper = function(...) {
    .f(...)
    NULL
  }
  map(.x, .wrapper, ...)
  invisible(.x)
}, function (.x, .y, .f, ...) 
{
  map2(.x, .y, .f, ...)
  invisible(.x)
})
c("package:purrr", "namespace:htmltools", "namespace:mlr3misc", "namespace:purrr")
c(TRUE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, FALSE, TRUE)
when
list(`package:purrr` = function (., ...) 
{
  dots <- list(...)
  names <- names(dots)
  named <- if (is.null(names)) 
    rep(FALSE, length(dots))
  else names != ""
  if (sum(!named) == 0) 
    stop("At least one matching condition is needed.", call. = FALSE)
  is_formula <- vapply(dots, function(dot) identical(class(dot), "formula"), logical(1))
  env <- new.env(parent = parent.frame())
  env[["."]] <- .
  if (sum(named) > 0) 
    for (i in which(named)) env[[names[i]]] <- dots[[i]]
  result <- NULL
  for (i in which(!named)) {
    if (is_formula[i]) {
      action <- length(dots[[i]])
      if (action == 2 || is_true(eval(dots[[i]][[2]], env, env))) {
        result <- eval(dots[[i]][[action]], env, env)
        break
      }
    }
    else {
      result <- dots[[i]]
    }
  }
  result
}, function (., ...) 
{
  dots <- list(...)
  names <- names(dots)
  named <- if (is.null(names)) 
    rep(FALSE, length(dots))
  else names != ""
  if (sum(!named) == 0) 
    stop("At least one matching condition is needed.", call. = FALSE)
  is_formula <- vapply(dots, function(dot) identical(class(dot), "formula"), logical(1))
  env <- new.env(parent = parent.frame())
  env[["."]] <- .
  if (sum(named) > 0) 
    for (i in which(named)) env[[names[i]]] <- dots[[i]]
  result <- NULL
  for (i in which(!named)) {
    if (is_formula[i]) {
      action <- length(dots[[i]])
      if (action == 2 || is_true(eval(dots[[i]][[2]], env, env))) {
        result <- eval(dots[[i]][[action]], env, env)
        break
      }
    }
    else {
      result <- dots[[i]]
    }
  }
  result
})
c("package:purrr", "namespace:purrr")
c(TRUE, FALSE)
c(FALSE, TRUE)
zap
list(`package:purrr` = function () 
{
  `zap!`
}, function () 
{
  `zap!`
})
c("package:purrr", "namespace:rlang")
c(TRUE, FALSE)
c(FALSE, TRUE)

############################################################################################33
library(Rcrawler)
browser_path
list(`package:Rcrawler` = function () 
{
  if (is_windows()) {
    path <- Sys.getenv("APPDATA", "")
    path <- if (dir_exists(path)) 
      file.path(path, "PhantomJS")
  }
  else if (is_osx()) {
    path <- "~/Library/Application Support"
    path <- if (dir_exists(path)) 
      file.path(path, "PhantomJS")
  }
  else {
    path <- "~/bin"
  }
  path <- c(path, system.file("PhantomJS", package = "webdriver"))
  path
}, function () 
{
  if (is_windows()) {
    path <- Sys.getenv("APPDATA", "")
    path <- if (dir_exists(path)) 
      file.path(path, "PhantomJS")
  }
  else if (is_osx()) {
    path <- "~/Library/Application Support"
    path <- if (dir_exists(path)) 
      file.path(path, "PhantomJS")
  }
  else {
    path <- "~/bin"
  }
  path <- c(path, system.file("PhantomJS", package = "webdriver"))
  path
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
ContentScraper
list(`package:Rcrawler` = function (Url, HTmlText, browser, XpathPatterns, CssPatterns, PatternsName, ExcludeXpathPat, ExcludeCSSPat, ManyPerPattern = FALSE, astext = TRUE, asDataFrame = FALSE, encod) 
{
  if (!missing(Url) && !missing(HTmlText)) {
    stop("Please supply Url or HTmlText, not both !")
  }
  if (!missing(XpathPatterns) && !missing(CssPatterns)) {
    stop("Please supply XpathPatterns or CssPatterns, not both !")
  }
  if (!missing(ExcludeXpathPat) && !missing(ExcludeCSSPat)) {
    stop("Please supply ExcludeXpathPat or ExcludeCSSPat, not both !")
  }
  if (!missing(XpathPatterns) && !missing(PatternsName)) {
    if (length(XpathPatterns) != length(PatternsName)) 
      stop("PatternsName & XpathPatterns parameters must have the same length ")
  }
  if (!missing(CssPatterns) && !missing(PatternsName)) {
    if (length(CssPatterns) != length(PatternsName)) 
      stop("PatternsName & CssPatterns parameters must have the same length ")
  }
  if (!missing(ExcludeCSSPat)) {
    if (is.vector(ExcludeCSSPat)) {
      ExcludeXpathPat <- unlist(lapply(ExcludeCSSPat, FUN = function(x) {
        tryCatch(selectr::css_to_xpath(x, prefix = "//"), error = function(e) stop("Unable to translate supplied css selector, Please check CssPatterns syntax !"))
      }))
    }
  }
  if (!missing(CssPatterns)) {
    if (is.vector(CssPatterns)) {
      XpathPatterns <- unlist(lapply(CssPatterns, FUN = function(x) {
        tryCatch(selectr::css_to_xpath(x, prefix = "//"), error = function(e) stop("Unable to translate supplied css selector, Please check CssPatterns syntax !"))
      }))
    }
    else {
      stop("CssPatterns parameter must be a vector with at least one element !")
    }
  }
  content <- list()
  if (!missing(Url) && missing(HTmlText)) {
    pos <- 1
    for (Ur in Url) {
      if (missing(browser)) {
        pageinfo <- LinkExtractor(url = Ur, encod = encod)
      }
      else {
        pageinfo <- LinkExtractor(url = Ur, encod = encod, Browser = browser)
      }
      if (pageinfo$Info$Status_code == 200) {
        HTmlText <- pageinfo[[1]][[10]]
        x <- xml2::read_html(HTmlText, encoding = encod)
        if (ManyPerPattern) {
          if (astext && (missing(ExcludeXpathPat) || is.null(ExcludeXpathPat))) {
            invisible(xml2::xml_remove(xml_find_all(x, "//script")))
            contentx <- lapply(XpathPatterns, function(n) {
              tryCatch(xml2::xml_text(xml2::xml_find_all(x, n)), error = function(e) "NA")
            })
          }
          else {
            contentx <- lapply(XpathPatterns, function(n) {
              tryCatch(paste(xml2::xml_find_all(x, n)), error = function(e) "")
            })
          }
          if ((!missing(ExcludeCSSPat) || !missing(ExcludeXpathPat))) {
            if (!is.null(ExcludeXpathPat)) {
              ToExcludeL <- lapply(ExcludeXpathPat, function(n) {
                tryCatch(paste(xml2::xml_find_all(x, n)), error = function(e) NULL)
              })
              ToExclude <- unlist(ToExcludeL)
              if (!is.null(ToExclude) && length(ToExclude) > 0) {
                for (i in 1:length(ToExclude)) {
                  for (j in 1:length(contentx)) {
                    if (length(contentx[[j]]) > 1) {
                      for (k in 1:length(contentx[[j]])) {
                        if (grepl(ToExclude[[i]], contentx[[j]][[k]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                          contentx[[j]][[k]] <- gsub(ToExclude[[i]], "", contentx[[j]][[k]], fixed = TRUE)
                        }
                      }
                    }
                    else if (length(contentx[[j]]) == 1) {
                      if (grepl(ToExclude[[i]], contentx[[j]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                        contentx[[j]] <- gsub(ToExclude[[i]], "", contentx[[j]], fixed = TRUE)
                      }
                    }
                  }
                }
              }
              if (astext) {
                for (j in 1:length(contentx)) {
                  if (length(contentx[[j]]) > 0) {
                    for (k in 1:length(contentx[[j]])) {
                      contentx[[j]][[k]] <- RemoveTags(contentx[[j]][[k]])
                    }
                  }
                }
              }
            }
          }
          if (!missing(PatternsName)) {
            for (i in 1:length(contentx)) {
              if (length(contentx[[i]]) > 0) {
                names(contentx)[i] <- PatternsName[[i]]
              }
            }
          }
        }
        else {
          if (astext && (missing(ExcludeXpathPat) || is.null(ExcludeXpathPat))) {
            invisible(xml_remove(xml_find_all(x, "//script")))
            contentx <- lapply(XpathPatterns, function(n) {
              tryCatch(xml2::xml_text(xml_find_first(x, n)), error = function(e) "")
            })
          }
          else {
            contentx <- lapply(XpathPatterns, function(n) {
              tryCatch(paste(xml2::xml_find_first(x, n)), error = function(e) "")
            })
          }
          if (!missing(ExcludeCSSPat) || !missing(ExcludeXpathPat)) {
            if (!is.null(ExcludeXpathPat)) {
              ToExcludeL <- lapply(ExcludeXpathPat, function(n) {
                tryCatch(paste(xml2::xml_find_all(x, n)), error = function(e) NULL)
              })
              ToExclude <- unlist(ToExcludeL)
              if (!is.null(ToExclude) && length(ToExclude) > 0) {
                for (i in 1:length(ToExclude)) {
                  for (j in 1:length(contentx)) {
                    if (length(contentx[[j]]) > 1) {
                      for (k in 1:length(contentx[[j]])) {
                        if (grepl(ToExclude[[i]], contentx[[j]][[k]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                          contentx[[j]][[k]] <- gsub(ToExclude[[i]], "", contentx[[j]][[k]], fixed = TRUE)
                        }
                      }
                    }
                    else if (length(contentx[[j]]) == 1) {
                      if (grepl(ToExclude[[i]], contentx[[j]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                        contentx[[j]] <- gsub(ToExclude[[i]], "", contentx[[j]], fixed = TRUE)
                      }
                    }
                  }
                }
              }
              if (astext) {
                for (j in 1:length(contentx)) {
                  if (length(contentx[[j]]) > 0) {
                    for (k in 1:length(contentx[[j]])) {
                      contentx[[j]][[k]] <- RemoveTags(contentx[[j]][[k]])
                    }
                  }
                }
              }
            }
          }
          if (!missing(PatternsName)) {
            for (i in 1:length(contentx)) {
              names(contentx)[i] <- PatternsName[[i]]
            }
          }
        }
      }
      else {
        contentx <- paste0("HTTP error code:", pageinfo$Info$Status_code)
      }
      if (length(Url) > 1) 
        content <- c(content, list(contentx))
      else content <- c(content, contentx)
      cat(pos, "..", sep = "")
      pos <- pos + 1
      flush.console()
      Sys.sleep(1)
    }
  }
  if (missing(Url) && !missing(HTmlText)) {
    x <- xml2::read_html(HTmlText, encoding = encod)
    if (ManyPerPattern) {
      if (astext && (missing(ExcludeXpathPat) || is.null(ExcludeXpathPat))) {
        invisible(xml2::xml_remove(xml_find_all(x, "//script")))
        contentx <- lapply(XpathPatterns, function(n) {
          tryCatch(xml2::xml_text(xml2::xml_find_all(x, n)), error = function(e) "NA")
        })
      }
      else {
        contentx <- lapply(XpathPatterns, function(n) {
          tryCatch(paste(xml2::xml_find_all(x, n)), error = function(e) "")
        })
      }
      if ((!missing(ExcludeCSSPat) || !missing(ExcludeXpathPat))) {
        if (!is.null(ExcludeXpathPat)) {
          ToExcludeL <- lapply(ExcludeXpathPat, function(n) {
            tryCatch(paste(xml2::xml_find_all(x, n)), error = function(e) NULL)
          })
          ToExclude <- unlist(ToExcludeL)
          if (!is.null(ToExclude) && length(ToExclude) > 0) {
            for (i in 1:length(ToExclude)) {
              for (j in 1:length(contentx)) {
                if (length(contentx[[j]]) > 1) {
                  for (k in 1:length(contentx[[j]])) {
                    if (grepl(ToExclude[[i]], contentx[[j]][[k]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                      contentx[[j]][[k]] <- gsub(ToExclude[[i]], "", contentx[[j]][[k]], fixed = TRUE)
                    }
                  }
                }
                else if (length(contentx[[j]]) == 1) {
                  if (grepl(ToExclude[[i]], contentx[[j]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                    contentx[[j]] <- gsub(ToExclude[[i]], "", contentx[[j]], fixed = TRUE)
                  }
                }
              }
            }
          }
          if (astext) {
            for (j in 1:length(contentx)) {
              if (length(contentx[[j]]) > 0) {
                for (k in 1:length(contentx[[j]])) {
                  contentx[[j]][[k]] <- RemoveTags(contentx[[j]][[k]])
                }
              }
            }
          }
        }
      }
      if (!missing(PatternsName)) {
        for (i in 1:length(contentx)) {
          if (length(contentx[[i]]) > 0) {
            names(contentx)[i] <- PatternsName[[i]]
          }
        }
      }
    }
    else {
      if (astext && (missing(ExcludeXpathPat) || is.null(ExcludeXpathPat))) {
        invisible(xml_remove(xml_find_all(x, "//script")))
        contentx <- lapply(XpathPatterns, function(n) {
          tryCatch(xml2::xml_text(xml_find_first(x, n)), error = function(e) "")
        })
      }
      else {
        contentx <- lapply(XpathPatterns, function(n) {
          tryCatch(paste(xml2::xml_find_first(x, n)), error = function(e) "")
        })
      }
      if (!missing(ExcludeCSSPat) || !missing(ExcludeXpathPat)) {
        if (!is.null(ExcludeXpathPat)) {
          ToExcludeL <- lapply(ExcludeXpathPat, function(n) {
            tryCatch(paste(xml2::xml_find_all(x, n)), error = function(e) NULL)
          })
          ToExclude <- unlist(ToExcludeL)
          if (!is.null(ToExclude) && length(ToExclude) > 0) {
            for (i in 1:length(ToExclude)) {
              for (j in 1:length(contentx)) {
                if (length(contentx[[j]]) > 1) {
                  for (k in 1:length(contentx[[j]])) {
                    if (grepl(ToExclude[[i]], contentx[[j]][[k]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                      contentx[[j]][[k]] <- gsub(ToExclude[[i]], "", contentx[[j]][[k]], fixed = TRUE)
                    }
                  }
                }
                else if (length(contentx[[j]]) == 1) {
                  if (grepl(ToExclude[[i]], contentx[[j]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                    contentx[[j]] <- gsub(ToExclude[[i]], "", contentx[[j]], fixed = TRUE)
                  }
                }
              }
            }
          }
          if (astext) {
            for (j in 1:length(contentx)) {
              if (length(contentx[[j]]) > 0) {
                for (k in 1:length(contentx[[j]])) {
                  contentx[[j]][[k]] <- RemoveTags(contentx[[j]][[k]])
                }
              }
            }
          }
        }
      }
      if (!missing(PatternsName)) {
        for (i in 1:length(contentx)) {
          names(contentx)[i] <- PatternsName[[i]]
        }
      }
    }
    content <- c(content, contentx)
  }
  if (asDataFrame) {
    content <- data.frame(do.call("rbind", NormalizeForExcel(content)))
  }
  return(content)
}, function (Url, HTmlText, browser, XpathPatterns, CssPatterns, PatternsName, ExcludeXpathPat, ExcludeCSSPat, ManyPerPattern = FALSE, astext = TRUE, asDataFrame = FALSE, encod) 
{
  if (!missing(Url) && !missing(HTmlText)) {
    stop("Please supply Url or HTmlText, not both !")
  }
  if (!missing(XpathPatterns) && !missing(CssPatterns)) {
    stop("Please supply XpathPatterns or CssPatterns, not both !")
  }
  if (!missing(ExcludeXpathPat) && !missing(ExcludeCSSPat)) {
    stop("Please supply ExcludeXpathPat or ExcludeCSSPat, not both !")
  }
  if (!missing(XpathPatterns) && !missing(PatternsName)) {
    if (length(XpathPatterns) != length(PatternsName)) 
      stop("PatternsName & XpathPatterns parameters must have the same length ")
  }
  if (!missing(CssPatterns) && !missing(PatternsName)) {
    if (length(CssPatterns) != length(PatternsName)) 
      stop("PatternsName & CssPatterns parameters must have the same length ")
  }
  if (!missing(ExcludeCSSPat)) {
    if (is.vector(ExcludeCSSPat)) {
      ExcludeXpathPat <- unlist(lapply(ExcludeCSSPat, FUN = function(x) {
        tryCatch(selectr::css_to_xpath(x, prefix = "//"), error = function(e) stop("Unable to translate supplied css selector, Please check CssPatterns syntax !"))
      }))
    }
  }
  if (!missing(CssPatterns)) {
    if (is.vector(CssPatterns)) {
      XpathPatterns <- unlist(lapply(CssPatterns, FUN = function(x) {
        tryCatch(selectr::css_to_xpath(x, prefix = "//"), error = function(e) stop("Unable to translate supplied css selector, Please check CssPatterns syntax !"))
      }))
    }
    else {
      stop("CssPatterns parameter must be a vector with at least one element !")
    }
  }
  content <- list()
  if (!missing(Url) && missing(HTmlText)) {
    pos <- 1
    for (Ur in Url) {
      if (missing(browser)) {
        pageinfo <- LinkExtractor(url = Ur, encod = encod)
      }
      else {
        pageinfo <- LinkExtractor(url = Ur, encod = encod, Browser = browser)
      }
      if (pageinfo$Info$Status_code == 200) {
        HTmlText <- pageinfo[[1]][[10]]
        x <- xml2::read_html(HTmlText, encoding = encod)
        if (ManyPerPattern) {
          if (astext && (missing(ExcludeXpathPat) || is.null(ExcludeXpathPat))) {
            invisible(xml2::xml_remove(xml_find_all(x, "//script")))
            contentx <- lapply(XpathPatterns, function(n) {
              tryCatch(xml2::xml_text(xml2::xml_find_all(x, n)), error = function(e) "NA")
            })
          }
          else {
            contentx <- lapply(XpathPatterns, function(n) {
              tryCatch(paste(xml2::xml_find_all(x, n)), error = function(e) "")
            })
          }
          if ((!missing(ExcludeCSSPat) || !missing(ExcludeXpathPat))) {
            if (!is.null(ExcludeXpathPat)) {
              ToExcludeL <- lapply(ExcludeXpathPat, function(n) {
                tryCatch(paste(xml2::xml_find_all(x, n)), error = function(e) NULL)
              })
              ToExclude <- unlist(ToExcludeL)
              if (!is.null(ToExclude) && length(ToExclude) > 0) {
                for (i in 1:length(ToExclude)) {
                  for (j in 1:length(contentx)) {
                    if (length(contentx[[j]]) > 1) {
                      for (k in 1:length(contentx[[j]])) {
                        if (grepl(ToExclude[[i]], contentx[[j]][[k]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                          contentx[[j]][[k]] <- gsub(ToExclude[[i]], "", contentx[[j]][[k]], fixed = TRUE)
                        }
                      }
                    }
                    else if (length(contentx[[j]]) == 1) {
                      if (grepl(ToExclude[[i]], contentx[[j]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                        contentx[[j]] <- gsub(ToExclude[[i]], "", contentx[[j]], fixed = TRUE)
                      }
                    }
                  }
                }
              }
              if (astext) {
                for (j in 1:length(contentx)) {
                  if (length(contentx[[j]]) > 0) {
                    for (k in 1:length(contentx[[j]])) {
                      contentx[[j]][[k]] <- RemoveTags(contentx[[j]][[k]])
                    }
                  }
                }
              }
            }
          }
          if (!missing(PatternsName)) {
            for (i in 1:length(contentx)) {
              if (length(contentx[[i]]) > 0) {
                names(contentx)[i] <- PatternsName[[i]]
              }
            }
          }
        }
        else {
          if (astext && (missing(ExcludeXpathPat) || is.null(ExcludeXpathPat))) {
            invisible(xml_remove(xml_find_all(x, "//script")))
            contentx <- lapply(XpathPatterns, function(n) {
              tryCatch(xml2::xml_text(xml_find_first(x, n)), error = function(e) "")
            })
          }
          else {
            contentx <- lapply(XpathPatterns, function(n) {
              tryCatch(paste(xml2::xml_find_first(x, n)), error = function(e) "")
            })
          }
          if (!missing(ExcludeCSSPat) || !missing(ExcludeXpathPat)) {
            if (!is.null(ExcludeXpathPat)) {
              ToExcludeL <- lapply(ExcludeXpathPat, function(n) {
                tryCatch(paste(xml2::xml_find_all(x, n)), error = function(e) NULL)
              })
              ToExclude <- unlist(ToExcludeL)
              if (!is.null(ToExclude) && length(ToExclude) > 0) {
                for (i in 1:length(ToExclude)) {
                  for (j in 1:length(contentx)) {
                    if (length(contentx[[j]]) > 1) {
                      for (k in 1:length(contentx[[j]])) {
                        if (grepl(ToExclude[[i]], contentx[[j]][[k]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                          contentx[[j]][[k]] <- gsub(ToExclude[[i]], "", contentx[[j]][[k]], fixed = TRUE)
                        }
                      }
                    }
                    else if (length(contentx[[j]]) == 1) {
                      if (grepl(ToExclude[[i]], contentx[[j]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                        contentx[[j]] <- gsub(ToExclude[[i]], "", contentx[[j]], fixed = TRUE)
                      }
                    }
                  }
                }
              }
              if (astext) {
                for (j in 1:length(contentx)) {
                  if (length(contentx[[j]]) > 0) {
                    for (k in 1:length(contentx[[j]])) {
                      contentx[[j]][[k]] <- RemoveTags(contentx[[j]][[k]])
                    }
                  }
                }
              }
            }
          }
          if (!missing(PatternsName)) {
            for (i in 1:length(contentx)) {
              names(contentx)[i] <- PatternsName[[i]]
            }
          }
        }
      }
      else {
        contentx <- paste0("HTTP error code:", pageinfo$Info$Status_code)
      }
      if (length(Url) > 1) 
        content <- c(content, list(contentx))
      else content <- c(content, contentx)
      cat(pos, "..", sep = "")
      pos <- pos + 1
      flush.console()
      Sys.sleep(1)
    }
  }
  if (missing(Url) && !missing(HTmlText)) {
    x <- xml2::read_html(HTmlText, encoding = encod)
    if (ManyPerPattern) {
      if (astext && (missing(ExcludeXpathPat) || is.null(ExcludeXpathPat))) {
        invisible(xml2::xml_remove(xml_find_all(x, "//script")))
        contentx <- lapply(XpathPatterns, function(n) {
          tryCatch(xml2::xml_text(xml2::xml_find_all(x, n)), error = function(e) "NA")
        })
      }
      else {
        contentx <- lapply(XpathPatterns, function(n) {
          tryCatch(paste(xml2::xml_find_all(x, n)), error = function(e) "")
        })
      }
      if ((!missing(ExcludeCSSPat) || !missing(ExcludeXpathPat))) {
        if (!is.null(ExcludeXpathPat)) {
          ToExcludeL <- lapply(ExcludeXpathPat, function(n) {
            tryCatch(paste(xml2::xml_find_all(x, n)), error = function(e) NULL)
          })
          ToExclude <- unlist(ToExcludeL)
          if (!is.null(ToExclude) && length(ToExclude) > 0) {
            for (i in 1:length(ToExclude)) {
              for (j in 1:length(contentx)) {
                if (length(contentx[[j]]) > 1) {
                  for (k in 1:length(contentx[[j]])) {
                    if (grepl(ToExclude[[i]], contentx[[j]][[k]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                      contentx[[j]][[k]] <- gsub(ToExclude[[i]], "", contentx[[j]][[k]], fixed = TRUE)
                    }
                  }
                }
                else if (length(contentx[[j]]) == 1) {
                  if (grepl(ToExclude[[i]], contentx[[j]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                    contentx[[j]] <- gsub(ToExclude[[i]], "", contentx[[j]], fixed = TRUE)
                  }
                }
              }
            }
          }
          if (astext) {
            for (j in 1:length(contentx)) {
              if (length(contentx[[j]]) > 0) {
                for (k in 1:length(contentx[[j]])) {
                  contentx[[j]][[k]] <- RemoveTags(contentx[[j]][[k]])
                }
              }
            }
          }
        }
      }
      if (!missing(PatternsName)) {
        for (i in 1:length(contentx)) {
          if (length(contentx[[i]]) > 0) {
            names(contentx)[i] <- PatternsName[[i]]
          }
        }
      }
    }
    else {
      if (astext && (missing(ExcludeXpathPat) || is.null(ExcludeXpathPat))) {
        invisible(xml_remove(xml_find_all(x, "//script")))
        contentx <- lapply(XpathPatterns, function(n) {
          tryCatch(xml2::xml_text(xml_find_first(x, n)), error = function(e) "")
        })
      }
      else {
        contentx <- lapply(XpathPatterns, function(n) {
          tryCatch(paste(xml2::xml_find_first(x, n)), error = function(e) "")
        })
      }
      if (!missing(ExcludeCSSPat) || !missing(ExcludeXpathPat)) {
        if (!is.null(ExcludeXpathPat)) {
          ToExcludeL <- lapply(ExcludeXpathPat, function(n) {
            tryCatch(paste(xml2::xml_find_all(x, n)), error = function(e) NULL)
          })
          ToExclude <- unlist(ToExcludeL)
          if (!is.null(ToExclude) && length(ToExclude) > 0) {
            for (i in 1:length(ToExclude)) {
              for (j in 1:length(contentx)) {
                if (length(contentx[[j]]) > 1) {
                  for (k in 1:length(contentx[[j]])) {
                    if (grepl(ToExclude[[i]], contentx[[j]][[k]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                      contentx[[j]][[k]] <- gsub(ToExclude[[i]], "", contentx[[j]][[k]], fixed = TRUE)
                    }
                  }
                }
                else if (length(contentx[[j]]) == 1) {
                  if (grepl(ToExclude[[i]], contentx[[j]], fixed = TRUE) && nchar(ToExclude[[i]]) != 0) {
                    contentx[[j]] <- gsub(ToExclude[[i]], "", contentx[[j]], fixed = TRUE)
                  }
                }
              }
            }
          }
          if (astext) {
            for (j in 1:length(contentx)) {
              if (length(contentx[[j]]) > 0) {
                for (k in 1:length(contentx[[j]])) {
                  contentx[[j]][[k]] <- RemoveTags(contentx[[j]][[k]])
                }
              }
            }
          }
        }
      }
      if (!missing(PatternsName)) {
        for (i in 1:length(contentx)) {
          names(contentx)[i] <- PatternsName[[i]]
        }
      }
    }
    content <- c(content, contentx)
  }
  if (asDataFrame) {
    content <- data.frame(do.call("rbind", NormalizeForExcel(content)))
  }
  return(content)
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
Drv_fetchpage
list(`package:Rcrawler` = function (url, browser) 
{
  if (missing(browser)) 
    stop("browser argument is missing! use run_browser() to build a browser object or LoginSession() for pages requiring authentification")
  if (missing(url)) 
    stop("url argument is missing! you need to provide the url to be fetched")
  if (length(browser) < 3) {
    browser$session$initialize(port = browser$process$port)
  }
  browser$session$go(url)
  if (length(browser) == 3) {
    if (grepl(browser$loginInfo$LoginURL, browser$session$getUrl())) {
      LoginSession(Browser = browser, LoginURL = browser$loginInfo$LoginURL, LoginCredentials = browser$loginInfo$LoginCredentials, cssLoginFields = browser$loginInfo$cssLoginFields, cssLoginButton = browser$loginInfo$cssLoginButton, cssRadioToCheck = browser$loginInfo$cssRadioToCheck, XpathLoginFields = browser$loginInfo$XpathLoginFields, XpathLoginButton = browser$loginInfo$XpathLoginButton, browser$loginInfo$XpathRadioToCheck)
      browser$session$go(url)
    }
  }
  sc = as.character(browser$session$getSource())
  x <- browser$session$readLog(type = "har")
  xjson <- tryCatch(jsonlite::fromJSON(x$message, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(xjson) && length(xjson) > 0) {
    if (length(xjson$log$entries) > 1) {
      xjson <- xjson$log$entries
      if (substring(url, nchar(url)) == "/") {
        url2 <- substr(url, 1, nchar(url) - 1)
      }
      else url2 <- paste0(url, "/")
      for (i in 1:length(xjson)) {
        if (url2 == xjson[[i]]$request$url || url == xjson[[i]]$request$url || grepl(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", url), "(\\/)?$"), xjson[[i]]$request$url)) {
          p <- xjson[[i]]
        }
      }
      if (exists("p")) {
        if ("status" %in% names(p$response)) {
          status_c <- p$response$status
        }
        else status_c <- 200
        if (length(p$response$headers) > 0) {
          for (i in 1:length(p$response$headers)) {
            if ("name" %in% names(p$response$headers[[i]])) {
              if (grepl("^content-type$", p$response$headers[[i]]$name, ignore.case = TRUE)) {
                content_type <- p$response$headers[[i]]$value
              }
              else content_type <- get_contenttype(sc)
            }
            else content_type <- get_contenttype(sc)
          }
        }
        else content_type <- get_contenttype(sc)
      }
      else {
        status_c <- 200
        content_type <- get_contenttype(sc)
      }
    }
    else {
      status_c <- 200
      content_type <- get_contenttype(sc)
    }
  }
  else {
    status_c <- 200
    content_type <- "html"
  }
  page <- list(status_code = status_c, PageSource = sc, headers = list(`content-type` = content_type))
  page
}, function (url, browser) 
{
  if (missing(browser)) 
    stop("browser argument is missing! use run_browser() to build a browser object or LoginSession() for pages requiring authentification")
  if (missing(url)) 
    stop("url argument is missing! you need to provide the url to be fetched")
  if (length(browser) < 3) {
    browser$session$initialize(port = browser$process$port)
  }
  browser$session$go(url)
  if (length(browser) == 3) {
    if (grepl(browser$loginInfo$LoginURL, browser$session$getUrl())) {
      LoginSession(Browser = browser, LoginURL = browser$loginInfo$LoginURL, LoginCredentials = browser$loginInfo$LoginCredentials, cssLoginFields = browser$loginInfo$cssLoginFields, cssLoginButton = browser$loginInfo$cssLoginButton, cssRadioToCheck = browser$loginInfo$cssRadioToCheck, XpathLoginFields = browser$loginInfo$XpathLoginFields, XpathLoginButton = browser$loginInfo$XpathLoginButton, browser$loginInfo$XpathRadioToCheck)
      browser$session$go(url)
    }
  }
  sc = as.character(browser$session$getSource())
  x <- browser$session$readLog(type = "har")
  xjson <- tryCatch(jsonlite::fromJSON(x$message, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(xjson) && length(xjson) > 0) {
    if (length(xjson$log$entries) > 1) {
      xjson <- xjson$log$entries
      if (substring(url, nchar(url)) == "/") {
        url2 <- substr(url, 1, nchar(url) - 1)
      }
      else url2 <- paste0(url, "/")
      for (i in 1:length(xjson)) {
        if (url2 == xjson[[i]]$request$url || url == xjson[[i]]$request$url || grepl(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", url), "(\\/)?$"), xjson[[i]]$request$url)) {
          p <- xjson[[i]]
        }
      }
      if (exists("p")) {
        if ("status" %in% names(p$response)) {
          status_c <- p$response$status
        }
        else status_c <- 200
        if (length(p$response$headers) > 0) {
          for (i in 1:length(p$response$headers)) {
            if ("name" %in% names(p$response$headers[[i]])) {
              if (grepl("^content-type$", p$response$headers[[i]]$name, ignore.case = TRUE)) {
                content_type <- p$response$headers[[i]]$value
              }
              else content_type <- get_contenttype(sc)
            }
            else content_type <- get_contenttype(sc)
          }
        }
        else content_type <- get_contenttype(sc)
      }
      else {
        status_c <- 200
        content_type <- get_contenttype(sc)
      }
    }
    else {
      status_c <- 200
      content_type <- get_contenttype(sc)
    }
  }
  else {
    status_c <- 200
    content_type <- "html"
  }
  page <- list(status_code = status_c, PageSource = sc, headers = list(`content-type` = content_type))
  page
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
Getencoding
list(`package:Rcrawler` = function (url) 
{
  base <- strsplit(gsub("http://|https://", "", url), "/")[[c(1, 1)]]
  pag <- tryCatch(httr::GET(url, httr::user_agent("Mozilla/5.0 (Windows NT 6.3; WOW64; rv:42.0) Gecko/20100101 Firefox/42.0"), httr::timeout(5), httr::add_headers(Origin = base)), error = function(e) NULL)
  if (!is.null(pag)) {
    head <- pag$headers$`content-type`
    enc <- tryCatch(gsub("(.*)=(.*)", "\\2", regmatches(head, gregexpr("charset=([^']*)", head))[[1]]), error = function(e) NA)
    page <- NA
    if (length(enc) != 0) {
      if (!is.na(enc)) {
        page <- as.character(httr::content(pag, type = "htmlTreeParse", as = "text", encoding = enc))
      }
    }
    if (is.na(page)) {
      page <- as.character(httr::content(pag, type = "htmlTreeParse", as = "text", encoding = "UTF-8"))
    }
    if (is.na(page)) {
      page <- as.character(httr::content(pag, type = "htmlTreeParse", as = "text", encoding = "ISO-8859-1"))
    }
    if (!is.na(page)) {
      if (grepl("<meta http-equiv=\"Content-Type\"", page)) {
        enc <- xml2::xml_attr(xml2::xml_find_first(read_html(page), "//meta[@http-equiv='Content-Type']"), "content")
        enc <- gsub(".*charset=([^']*)", "\\1", enc)
      }
      else if (grepl("<meta charset=", page)) {
        enc <- regmatches(page, gregexpr("charset=([^'>/]*)", page))[[1]][1]
        enc <- gsub("(.*)=\"([^\">]*)\"", "\\2", enc)
      }
    }
    enc <- toupper(enc)
    enc <- gsub("^\\s+|\\s+$", "", enc)
  }
  else {
    enc = "NULL"
  }
  return(enc)
}, function (url) 
{
  base <- strsplit(gsub("http://|https://", "", url), "/")[[c(1, 1)]]
  pag <- tryCatch(httr::GET(url, httr::user_agent("Mozilla/5.0 (Windows NT 6.3; WOW64; rv:42.0) Gecko/20100101 Firefox/42.0"), httr::timeout(5), httr::add_headers(Origin = base)), error = function(e) NULL)
  if (!is.null(pag)) {
    head <- pag$headers$`content-type`
    enc <- tryCatch(gsub("(.*)=(.*)", "\\2", regmatches(head, gregexpr("charset=([^']*)", head))[[1]]), error = function(e) NA)
    page <- NA
    if (length(enc) != 0) {
      if (!is.na(enc)) {
        page <- as.character(httr::content(pag, type = "htmlTreeParse", as = "text", encoding = enc))
      }
    }
    if (is.na(page)) {
      page <- as.character(httr::content(pag, type = "htmlTreeParse", as = "text", encoding = "UTF-8"))
    }
    if (is.na(page)) {
      page <- as.character(httr::content(pag, type = "htmlTreeParse", as = "text", encoding = "ISO-8859-1"))
    }
    if (!is.na(page)) {
      if (grepl("<meta http-equiv=\"Content-Type\"", page)) {
        enc <- xml2::xml_attr(xml2::xml_find_first(read_html(page), "//meta[@http-equiv='Content-Type']"), "content")
        enc <- gsub(".*charset=([^']*)", "\\1", enc)
      }
      else if (grepl("<meta charset=", page)) {
        enc <- regmatches(page, gregexpr("charset=([^'>/]*)", page))[[1]][1]
        enc <- gsub("(.*)=\"([^\">]*)\"", "\\2", enc)
      }
    }
    enc <- toupper(enc)
    enc <- gsub("^\\s+|\\s+$", "", enc)
  }
  else {
    enc = "NULL"
  }
  return(enc)
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
install_browser
list(`package:Rcrawler` = function (version = "2.1.1", baseURL = "https://github.com/wch/webshot/releases/download/v0.3.1/") 
{
  if (!grepl("/$", baseURL)) 
    baseURL <- paste0(baseURL, "/")
  owd <- setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)
  if (is_windows()) {
    zipfile <- sprintf("phantomjs-%s-windows.zip", version)
    download(paste0(baseURL, zipfile), zipfile, mode = "wb")
    utils::unzip(zipfile)
    zipdir <- sub(".zip$", "", zipfile)
    exec <- file.path(zipdir, "bin", "phantomjs.exe")
  }
  else if (is_osx()) {
    zipfile <- sprintf("phantomjs-%s-macosx.zip", version)
    download(paste0(baseURL, zipfile), zipfile, mode = "wb")
    utils::unzip(zipfile)
    zipdir <- sub(".zip$", "", zipfile)
    exec <- file.path(zipdir, "bin", "phantomjs")
    Sys.chmod(exec, "0755")
  }
  else if (is_linux()) {
    zipfile <- sprintf("phantomjs-%s-linux-%s.tar.bz2", version, if (grepl("64", Sys.info()[["machine"]])) 
      "x86_64"
      else "i686")
    download(paste0(baseURL, zipfile), zipfile, mode = "wb")
    utils::untar(zipfile)
    zipdir <- sub(".tar.bz2$", "", zipfile)
    exec <- file.path(zipdir, "bin", "phantomjs")
    Sys.chmod(exec, "0755")
  }
  else {
    message("Sorry, this platform is not supported.")
    return(invisible())
  }
  success <- FALSE
  dirs <- phantom_paths()
  for (destdir in dirs) {
    dir.create(destdir, showWarnings = FALSE)
    success <- file.copy(exec, destdir, overwrite = TRUE)
    if (success) 
      break
  }
  unlink(c(zipdir, zipfile), recursive = TRUE)
  if (!success) 
    stop("Unable to install PhantomJS to any of these dirs: ", paste(dirs, collapse = ", "))
  message("phantomjs has been installed to ", normalizePath(destdir))
  invisible()
}, function (version = "2.1.1", baseURL = "https://github.com/wch/webshot/releases/download/v0.3.1/") 
{
  if (!grepl("/$", baseURL)) 
    baseURL <- paste0(baseURL, "/")
  owd <- setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)
  if (is_windows()) {
    zipfile <- sprintf("phantomjs-%s-windows.zip", version)
    download(paste0(baseURL, zipfile), zipfile, mode = "wb")
    utils::unzip(zipfile)
    zipdir <- sub(".zip$", "", zipfile)
    exec <- file.path(zipdir, "bin", "phantomjs.exe")
  }
  else if (is_osx()) {
    zipfile <- sprintf("phantomjs-%s-macosx.zip", version)
    download(paste0(baseURL, zipfile), zipfile, mode = "wb")
    utils::unzip(zipfile)
    zipdir <- sub(".zip$", "", zipfile)
    exec <- file.path(zipdir, "bin", "phantomjs")
    Sys.chmod(exec, "0755")
  }
  else if (is_linux()) {
    zipfile <- sprintf("phantomjs-%s-linux-%s.tar.bz2", version, if (grepl("64", Sys.info()[["machine"]])) 
      "x86_64"
      else "i686")
    download(paste0(baseURL, zipfile), zipfile, mode = "wb")
    utils::untar(zipfile)
    zipdir <- sub(".tar.bz2$", "", zipfile)
    exec <- file.path(zipdir, "bin", "phantomjs")
    Sys.chmod(exec, "0755")
  }
  else {
    message("Sorry, this platform is not supported.")
    return(invisible())
  }
  success <- FALSE
  dirs <- phantom_paths()
  for (destdir in dirs) {
    dir.create(destdir, showWarnings = FALSE)
    success <- file.copy(exec, destdir, overwrite = TRUE)
    if (success) 
      break
  }
  unlink(c(zipdir, zipfile), recursive = TRUE)
  if (!success) 
    stop("Unable to install PhantomJS to any of these dirs: ", paste(dirs, collapse = ", "))
  message("phantomjs has been installed to ", normalizePath(destdir))
  invisible()
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
LinkExtractor
list(`package:Rcrawler` = function (url, id, lev, IndexErrPages, Useragent, Timeout = 6, use_proxy = NULL, URLlenlimit = 255, urlExtfilter, urlregexfilter, encod, urlbotfiler, removeparams, removeAllparams = FALSE, ExternalLInks = FALSE, urlsZoneXpath = NULL, Browser, RenderingDelay = 0) 
{
  if (!missing(Browser) && !is.null(use_proxy)) 
    stop("unfortunately, phantomjs can't be configured to use proxy")
  nblinks <- 0
  pageinfo <- list()
  links2 <- vector()
  linkl <- list()
  links <- vector()
  Extlinks <- vector()
  if (!missing(Browser)) {
    if (length(Browser) < 2) 
      stop("please setup a web driver using run_browser()")
  }
  base <- strsplit(gsub("http://|https://", "", url), "/")[[c(1, 1)]]
  if (missing(urlbotfiler)) 
    urlbotfiler <- " "
  if (missing(id)) 
    id <- sample(1:1000, 1)
  if (missing(lev)) 
    lev <- 1
  if (missing(IndexErrPages)) 
    errstat <- c(200)
  else errstat <- c(200, IndexErrPages)
  if (missing(Useragent)) 
    Useragent = "Mozilla/5.0 (Windows NT 6.3; WOW64; rv:42.0) Firefox/42.0"
  if (missing(urlExtfilter)) 
    urlExtfilter <- c("flv", "mov", "swf", "txt", "xml", "js", "css", "zip", "gz", "rar", "7z", "tgz", "tar", "z", "gzip", "bzip", "tar", "mp3", "mp4", "aac", "wav", "au", "wmv", "avi", "mpg", "mpeg", "pdf", "doc", "docx", "xls", "xlsx", "ppt", "pptx", "jpg", "jpeg", "png", "gif", "psd", "ico", "bmp", "odt", "ods", "odp", "odb", "odg", "odf")
  if (missing(urlregexfilter)) {
    urlregexfilter <- ".*"
  }
  else {
    urlregexfilter <- paste(urlregexfilter, collapse = "|")
  }
  if (!missing(Browser)) {
    page <- tryCatch(Drv_fetchpage(url = url, browser = Browser), error = function(e) NULL)
  }
  else {
    if (is.null(use_proxy)) {
      page <- tryCatch(httr::GET(url, httr::user_agent(Useragent), httr::timeout(Timeout), httr::add_headers(Origin = base)), error = function(e) list(NULL, e))
    }
    else {
      page <- tryCatch(httr::GET(url, httr::user_agent(Useragent), use_proxy, httr::timeout(Timeout), httr::add_headers(Origin = base)), error = function(e) list(NULL, e))
    }
  }
  if (length(page) == 2) {
    if (grepl("Timeout was reached", page[[2]]$message)) {
      page <- tryCatch(httr::GET(url, httr::user_agent(Useragent), httr::add_headers(Origin = base)), error = function(e) list(NULL, e))
      if (length(page) == 2) {
        if (grepl("Timeout was reached", page[[2]]$message)) {
          cat("warning ! Unable to fetch the website using GET request , try to use web driver method (run_browser func see manual)")
        }
        page <- NULL
      }
    }
  }
  if (!is.null(page)) {
    if (page$status_code %in% errstat) {
      if (grepl("html", page$headers$`content-type`, ignore.case = TRUE)) {
        if (missing(Browser)) {
          if (missing(encod)) {
            x <- as.character(httr::content(page, type = "htmlTreeParse", as = "text", encoding = "UTF-8"))
            cont <- x
          }
          else {
            x <- as.character(httr::content(page, type = "htmlTreeParse", as = "text", encoding = encod))
            cont <- x
          }
          if (is.na(cont)) {
            x <- as.character(httr::content(page, type = "htmlTreeParse", as = "text", encoding = "ISO-8859-1"))
            cont <- x
          }
          links <- vector()
          x <- xml2::read_html(x)
          if (!is.null(urlsZoneXpath)) {
            for (h in 1:length(urlsZoneXpath)) {
              zonex <- tryCatch(xml2::xml_find_all(x, urlsZoneXpath[[h]]), error = function(e) NULL)
              if (!is.null(zonex)) {
                li <- xml2::xml_find_all(zonex, ".//a/@href")
                li <- as.vector(paste(li))
                li <- gsub(" href=\"(.*)\"", "\\1", li)
                links <- c(links, li)
              }
            }
          }
          else {
            links <- xml2::xml_find_all(x, "//a/@href")
            links <- as.vector(paste(links))
            links <- gsub(" href=\"(.*)\"", "\\1", links)
          }
        }
        else {
          links <- vector()
          Sys.sleep(RenderingDelay)
          x <- page$PageSource
          cont <- x
          if (!is.null(urlsZoneXpath)) {
            w <- 1
            for (h in 1:length(urlsZoneXpath)) {
              zonex <- tryCatch(Browser$session$findElement(xpath = urlsZoneXpath[[h]]), error = function(e) NULL)
              if (!is.null(zonex)) {
                linksel <- tryCatch(zonex$findElements(xpath = ".//*/a"), error = function(e) NULL)
                for (l in linksel) {
                  if (length(l$getAttribute("href")) != 0) {
                    links <- c(links, l$getAttribute("href"))
                  }
                  else {
                    linkl[[w]] <- l
                    w <- w + 1
                  }
                }
              }
            }
          }
          else {
            linksel <- Browser$session$findElements(xpath = "//*/a")
            w <- 1
            links <- vector()
            for (l in linksel) {
              if (length(l$getAttribute("href")) != 0) {
                links <- c(links, l$getAttribute("href"))
              }
              else {
                linkl[[w]] <- l
                w <- w + 1
              }
            }
          }
        }
        links <- unique(links)
        domain0 <- strsplit(gsub("http://|https://|www\\.", "", url), "/")[[c(1, 1)]]
        domain <- paste(domain0, "/", sep = "")
        links <- LinkNormalization(links, url)
        if (!missing(removeparams)) {
          if (removeparams != "") {
            links <- sapply(links, function(x) Linkparamsfilter(x, removeparams), USE.NAMES = FALSE)
          }
        }
        if (removeAllparams) {
          links <- sapply(links, function(x) Linkparamsfilter(x, removeAllparams = TRUE), USE.NAMES = FALSE)
        }
        links <- unique(links)
        if (!missing(urlbotfiler)) {
          links <- links[!links %like% paste(urlbotfiler, collapse = "|")]
        }
        if (length(links) != 0) {
          for (s in 1:length(links)) {
            if (!is.na(links[s])) {
              if (nchar(links[s]) <= URLlenlimit) {
                ext <- tools::file_ext(sub("\\?.+", "", basename(links[s])))
                if (grepl(domain, links[s]) && !(links[s] %in% links2) && !(ext %in% urlExtfilter) && grepl(pattern = urlregexfilter, x = links[s])) {
                  links2 <- c(links2, links[s])
                  nblinks <- nblinks + 1
                }
                if (ExternalLInks) {
                  if (!grepl(domain, links[s]) && !(links[s] %in% Extlinks) && !(ext %in% urlExtfilter)) {
                    Extlinks <- c(Extlinks, links[s])
                    nblinks <- nblinks + 1
                  }
                }
                else {
                  Extlinks <- vector()
                }
              }
            }
          }
        }
        else {
          links2 <- vector()
          linkl <- list()
          Extlinks <- vector()
        }
      }
      else {
        links2 <- vector()
        cont <- "NULL"
        linkl <- list()
        Extlinks <- vector()
      }
    }
    else {
      links2 <- vector()
      cont <- "NULL"
      linkl <- list()
      Extlinks <- vector()
    }
    if (cont == "NULL") {
      titre <- "NULL"
    }
    else {
      titre <- tryCatch(xml2::xml_text(xml2::xml_find_first(xml2::read_html(cont), "//*/title")), error = function(e) NULL)
    }
    contenttype <- tryCatch(gsub("(.*)\\;.*", "\\1", page$headers$`content-type`), error = function(e) "NA")
    if (page$headers$`content-type` == "html") {
      contentencod <- GetEncodingHTML(cont)
    }
    else {
      contentencod <- tryCatch(gsub("(.*)=(.*)", "\\2", gsub(".*\\;.", "\\1", page$headers$`content-type`)), error = function(e) "NA")
    }
    pageinfo <- list(Id = id, Url = url, Crawl_status = "finished", Crawl_level = lev, SumLinks = nblinks, "", Status_code = page$status_code, Content_type = contenttype, Encoding = contentencod, Source_page = cont, Title = titre)
  }
  else {
    links2 <- vector()
    Extlinks <- vector()
    pageinfo <- list(Id = id, Url = url, Crawl_status = "NULL", Crawl_level = lev, SumLinks = "", Status_code = "", Content_type = "", Encoding = "", Source_page = "", Title = "")
  }
  if (missing(Browser)) {
    paquet <- list(Info = pageinfo, InternalLinks = links2, ExternalLinks = Extlinks)
  }
  else {
    paquet <- list(Info = pageinfo, InternalLinks = links2, ExternalLinks = Extlinks, OtherLinksTags = linkl)
  }
  return(paquet)
}, function (url, id, lev, IndexErrPages, Useragent, Timeout = 6, use_proxy = NULL, URLlenlimit = 255, urlExtfilter, urlregexfilter, encod, urlbotfiler, removeparams, removeAllparams = FALSE, ExternalLInks = FALSE, urlsZoneXpath = NULL, Browser, RenderingDelay = 0) 
{
  if (!missing(Browser) && !is.null(use_proxy)) 
    stop("unfortunately, phantomjs can't be configured to use proxy")
  nblinks <- 0
  pageinfo <- list()
  links2 <- vector()
  linkl <- list()
  links <- vector()
  Extlinks <- vector()
  if (!missing(Browser)) {
    if (length(Browser) < 2) 
      stop("please setup a web driver using run_browser()")
  }
  base <- strsplit(gsub("http://|https://", "", url), "/")[[c(1, 1)]]
  if (missing(urlbotfiler)) 
    urlbotfiler <- " "
  if (missing(id)) 
    id <- sample(1:1000, 1)
  if (missing(lev)) 
    lev <- 1
  if (missing(IndexErrPages)) 
    errstat <- c(200)
  else errstat <- c(200, IndexErrPages)
  if (missing(Useragent)) 
    Useragent = "Mozilla/5.0 (Windows NT 6.3; WOW64; rv:42.0) Firefox/42.0"
  if (missing(urlExtfilter)) 
    urlExtfilter <- c("flv", "mov", "swf", "txt", "xml", "js", "css", "zip", "gz", "rar", "7z", "tgz", "tar", "z", "gzip", "bzip", "tar", "mp3", "mp4", "aac", "wav", "au", "wmv", "avi", "mpg", "mpeg", "pdf", "doc", "docx", "xls", "xlsx", "ppt", "pptx", "jpg", "jpeg", "png", "gif", "psd", "ico", "bmp", "odt", "ods", "odp", "odb", "odg", "odf")
  if (missing(urlregexfilter)) {
    urlregexfilter <- ".*"
  }
  else {
    urlregexfilter <- paste(urlregexfilter, collapse = "|")
  }
  if (!missing(Browser)) {
    page <- tryCatch(Drv_fetchpage(url = url, browser = Browser), error = function(e) NULL)
  }
  else {
    if (is.null(use_proxy)) {
      page <- tryCatch(httr::GET(url, httr::user_agent(Useragent), httr::timeout(Timeout), httr::add_headers(Origin = base)), error = function(e) list(NULL, e))
    }
    else {
      page <- tryCatch(httr::GET(url, httr::user_agent(Useragent), use_proxy, httr::timeout(Timeout), httr::add_headers(Origin = base)), error = function(e) list(NULL, e))
    }
  }
  if (length(page) == 2) {
    if (grepl("Timeout was reached", page[[2]]$message)) {
      page <- tryCatch(httr::GET(url, httr::user_agent(Useragent), httr::add_headers(Origin = base)), error = function(e) list(NULL, e))
      if (length(page) == 2) {
        if (grepl("Timeout was reached", page[[2]]$message)) {
          cat("warning ! Unable to fetch the website using GET request , try to use web driver method (run_browser func see manual)")
        }
        page <- NULL
      }
    }
  }
  if (!is.null(page)) {
    if (page$status_code %in% errstat) {
      if (grepl("html", page$headers$`content-type`, ignore.case = TRUE)) {
        if (missing(Browser)) {
          if (missing(encod)) {
            x <- as.character(httr::content(page, type = "htmlTreeParse", as = "text", encoding = "UTF-8"))
            cont <- x
          }
          else {
            x <- as.character(httr::content(page, type = "htmlTreeParse", as = "text", encoding = encod))
            cont <- x
          }
          if (is.na(cont)) {
            x <- as.character(httr::content(page, type = "htmlTreeParse", as = "text", encoding = "ISO-8859-1"))
            cont <- x
          }
          links <- vector()
          x <- xml2::read_html(x)
          if (!is.null(urlsZoneXpath)) {
            for (h in 1:length(urlsZoneXpath)) {
              zonex <- tryCatch(xml2::xml_find_all(x, urlsZoneXpath[[h]]), error = function(e) NULL)
              if (!is.null(zonex)) {
                li <- xml2::xml_find_all(zonex, ".//a/@href")
                li <- as.vector(paste(li))
                li <- gsub(" href=\"(.*)\"", "\\1", li)
                links <- c(links, li)
              }
            }
          }
          else {
            links <- xml2::xml_find_all(x, "//a/@href")
            links <- as.vector(paste(links))
            links <- gsub(" href=\"(.*)\"", "\\1", links)
          }
        }
        else {
          links <- vector()
          Sys.sleep(RenderingDelay)
          x <- page$PageSource
          cont <- x
          if (!is.null(urlsZoneXpath)) {
            w <- 1
            for (h in 1:length(urlsZoneXpath)) {
              zonex <- tryCatch(Browser$session$findElement(xpath = urlsZoneXpath[[h]]), error = function(e) NULL)
              if (!is.null(zonex)) {
                linksel <- tryCatch(zonex$findElements(xpath = ".//*/a"), error = function(e) NULL)
                for (l in linksel) {
                  if (length(l$getAttribute("href")) != 0) {
                    links <- c(links, l$getAttribute("href"))
                  }
                  else {
                    linkl[[w]] <- l
                    w <- w + 1
                  }
                }
              }
            }
          }
          else {
            linksel <- Browser$session$findElements(xpath = "//*/a")
            w <- 1
            links <- vector()
            for (l in linksel) {
              if (length(l$getAttribute("href")) != 0) {
                links <- c(links, l$getAttribute("href"))
              }
              else {
                linkl[[w]] <- l
                w <- w + 1
              }
            }
          }
        }
        links <- unique(links)
        domain0 <- strsplit(gsub("http://|https://|www\\.", "", url), "/")[[c(1, 1)]]
        domain <- paste(domain0, "/", sep = "")
        links <- LinkNormalization(links, url)
        if (!missing(removeparams)) {
          if (removeparams != "") {
            links <- sapply(links, function(x) Linkparamsfilter(x, removeparams), USE.NAMES = FALSE)
          }
        }
        if (removeAllparams) {
          links <- sapply(links, function(x) Linkparamsfilter(x, removeAllparams = TRUE), USE.NAMES = FALSE)
        }
        links <- unique(links)
        if (!missing(urlbotfiler)) {
          links <- links[!links %like% paste(urlbotfiler, collapse = "|")]
        }
        if (length(links) != 0) {
          for (s in 1:length(links)) {
            if (!is.na(links[s])) {
              if (nchar(links[s]) <= URLlenlimit) {
                ext <- tools::file_ext(sub("\\?.+", "", basename(links[s])))
                if (grepl(domain, links[s]) && !(links[s] %in% links2) && !(ext %in% urlExtfilter) && grepl(pattern = urlregexfilter, x = links[s])) {
                  links2 <- c(links2, links[s])
                  nblinks <- nblinks + 1
                }
                if (ExternalLInks) {
                  if (!grepl(domain, links[s]) && !(links[s] %in% Extlinks) && !(ext %in% urlExtfilter)) {
                    Extlinks <- c(Extlinks, links[s])
                    nblinks <- nblinks + 1
                  }
                }
                else {
                  Extlinks <- vector()
                }
              }
            }
          }
        }
        else {
          links2 <- vector()
          linkl <- list()
          Extlinks <- vector()
        }
      }
      else {
        links2 <- vector()
        cont <- "NULL"
        linkl <- list()
        Extlinks <- vector()
      }
    }
    else {
      links2 <- vector()
      cont <- "NULL"
      linkl <- list()
      Extlinks <- vector()
    }
    if (cont == "NULL") {
      titre <- "NULL"
    }
    else {
      titre <- tryCatch(xml2::xml_text(xml2::xml_find_first(xml2::read_html(cont), "//*/title")), error = function(e) NULL)
    }
    contenttype <- tryCatch(gsub("(.*)\\;.*", "\\1", page$headers$`content-type`), error = function(e) "NA")
    if (page$headers$`content-type` == "html") {
      contentencod <- GetEncodingHTML(cont)
    }
    else {
      contentencod <- tryCatch(gsub("(.*)=(.*)", "\\2", gsub(".*\\;.", "\\1", page$headers$`content-type`)), error = function(e) "NA")
    }
    pageinfo <- list(Id = id, Url = url, Crawl_status = "finished", Crawl_level = lev, SumLinks = nblinks, "", Status_code = page$status_code, Content_type = contenttype, Encoding = contentencod, Source_page = cont, Title = titre)
  }
  else {
    links2 <- vector()
    Extlinks <- vector()
    pageinfo <- list(Id = id, Url = url, Crawl_status = "NULL", Crawl_level = lev, SumLinks = "", Status_code = "", Content_type = "", Encoding = "", Source_page = "", Title = "")
  }
  if (missing(Browser)) {
    paquet <- list(Info = pageinfo, InternalLinks = links2, ExternalLinks = Extlinks)
  }
  else {
    paquet <- list(Info = pageinfo, InternalLinks = links2, ExternalLinks = Extlinks, OtherLinksTags = linkl)
  }
  return(paquet)
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
LinkNormalization
list(`package:Rcrawler` = function (links, current) 
{
  protocole <- strsplit(current, "/")[[c(1, 1)]]
  base <- strsplit(gsub("http://|https://", "", current), "/")[[c(1, 1)]]
  base2 <- strsplit(gsub("http://|https://|www\\.", "", current), "/")[[c(1, 1)]]
  rlinks <- c()
  for (t in 1:length(links)) {
    if (!is.null(links[t]) && length(links[t]) == 1) {
      if (!is.na(links[t])) {
        if (substr(links[t], 1, 2) != "//") {
          if (sum(gregexpr("http", links[t], fixed = TRUE)[[1]] > 0) < 2) {
            if (grepl("^\\s|\\s+$", links[t])) {
              links[t] <- gsub("^\\s|\\s+$", "", links[t], perl = TRUE)
            }
            if (substr(links[t], 1, 1) == "/") {
              links[t] <- paste0(protocole, "//", base, links[t])
            }
            else if (substr(links[t], 1, 2) == "./") {
              if (substring(current, nchar(current)) == "/") {
                links[t] <- paste0(current, gsub("\\./", "", links[t]))
              }
              else {
                links[t] <- paste0(current, gsub("\\./", "/", links[t]))
              }
            }
            else if (substr(links[t], 1, 3) == "www") {
              links[t] <- paste0(protocole, "//", links[t])
            }
            else if (substr(links[t], 1, 1) == "?") {
              if (substring(current, nchar(current)) == "/") {
                links[t] <- paste0(current, links[t])
              }
              else {
                links[t] <- paste0(current, "/", links[t])
              }
            }
            else if (grepl(pattern = paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", base2, ignore.case = TRUE), ".*"), x = links[t])) {
              if (grepl(pattern = "www", current, ignore.case = TRUE)) {
                links[t] <- paste0(protocole, "//www.", links[t])
              }
              else {
                links[t] <- paste0(protocole, "//", links[t])
              }
            }
            else if (substr(links[t], 1, 7) != "http://" && substr(links[t], 1, 8) != "https://" && substr(links[t], 1, 3) != "www" && grepl(pattern = paste0("[A-Za-z]*", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", paste0(".", base2), ".*")), x = links[t], ignore.case = TRUE)) {
              links[t] <- paste0(protocole, "//", links[t])
            }
            else if (substr(links[t], 1, 7) != "http://" && substr(links[t], 1, 8) != "https://" && substr(links[t], 1, 3) != "www" && !grepl(pattern = paste0(".*", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", base2, ".*")), x = links[t], ignore.case = TRUE)) {
              if (substring(current, nchar(current)) == "/") {
                links[t] <- paste0(current, links[t])
              }
              else {
                links[t] <- paste0(current, "/", links[t])
              }
            }
            if (grepl("#", links[t])) {
              links[t] <- gsub("\\#(.*)", "", links[t])
            }
            rlinks <- c(rlinks, links[t])
          }
        }
      }
    }
  }
  rlinks <- unique(rlinks)
  return(rlinks)
}, function (links, current) 
{
  protocole <- strsplit(current, "/")[[c(1, 1)]]
  base <- strsplit(gsub("http://|https://", "", current), "/")[[c(1, 1)]]
  base2 <- strsplit(gsub("http://|https://|www\\.", "", current), "/")[[c(1, 1)]]
  rlinks <- c()
  for (t in 1:length(links)) {
    if (!is.null(links[t]) && length(links[t]) == 1) {
      if (!is.na(links[t])) {
        if (substr(links[t], 1, 2) != "//") {
          if (sum(gregexpr("http", links[t], fixed = TRUE)[[1]] > 0) < 2) {
            if (grepl("^\\s|\\s+$", links[t])) {
              links[t] <- gsub("^\\s|\\s+$", "", links[t], perl = TRUE)
            }
            if (substr(links[t], 1, 1) == "/") {
              links[t] <- paste0(protocole, "//", base, links[t])
            }
            else if (substr(links[t], 1, 2) == "./") {
              if (substring(current, nchar(current)) == "/") {
                links[t] <- paste0(current, gsub("\\./", "", links[t]))
              }
              else {
                links[t] <- paste0(current, gsub("\\./", "/", links[t]))
              }
            }
            else if (substr(links[t], 1, 3) == "www") {
              links[t] <- paste0(protocole, "//", links[t])
            }
            else if (substr(links[t], 1, 1) == "?") {
              if (substring(current, nchar(current)) == "/") {
                links[t] <- paste0(current, links[t])
              }
              else {
                links[t] <- paste0(current, "/", links[t])
              }
            }
            else if (grepl(pattern = paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", base2, ignore.case = TRUE), ".*"), x = links[t])) {
              if (grepl(pattern = "www", current, ignore.case = TRUE)) {
                links[t] <- paste0(protocole, "//www.", links[t])
              }
              else {
                links[t] <- paste0(protocole, "//", links[t])
              }
            }
            else if (substr(links[t], 1, 7) != "http://" && substr(links[t], 1, 8) != "https://" && substr(links[t], 1, 3) != "www" && grepl(pattern = paste0("[A-Za-z]*", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", paste0(".", base2), ".*")), x = links[t], ignore.case = TRUE)) {
              links[t] <- paste0(protocole, "//", links[t])
            }
            else if (substr(links[t], 1, 7) != "http://" && substr(links[t], 1, 8) != "https://" && substr(links[t], 1, 3) != "www" && !grepl(pattern = paste0(".*", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", base2, ".*")), x = links[t], ignore.case = TRUE)) {
              if (substring(current, nchar(current)) == "/") {
                links[t] <- paste0(current, links[t])
              }
              else {
                links[t] <- paste0(current, "/", links[t])
              }
            }
            if (grepl("#", links[t])) {
              links[t] <- gsub("\\#(.*)", "", links[t])
            }
            rlinks <- c(rlinks, links[t])
          }
        }
      }
    }
  }
  rlinks <- unique(rlinks)
  return(rlinks)
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
Linkparameters
list(`package:Rcrawler` = function (URL) 
{
  str <- URL
  if (grepl("\\?", str)) {
    no_param <- gregexpr("\\&", str)[[1]]
    paramsv <- vector()
    if (no_param[1] < 0) {
      paramsv <- sub(".*\\?(.*)", "\\1", str)
    }
    else {
      pa1 <- ".*\\?(.*)"
      if (length(gregexpr("\\&", str)[[1]]) >= 1) {
        for (k in 1:(length(gregexpr("\\&", str)[[1]]))) {
          pa1 <- paste(pa1, "\\&(.*)", sep = "")
        }
        for (k in 1:(length(gregexpr("\\&", str)[[1]]) + 1)) {
          pa <- paste("\\", k, sep = "")
          paramsv <- c(paramsv, sub(pa1, pa, str))
        }
      }
    }
  }
  else paramsv <- "NULL"
  return(paramsv)
}, function (URL) 
{
  str <- URL
  if (grepl("\\?", str)) {
    no_param <- gregexpr("\\&", str)[[1]]
    paramsv <- vector()
    if (no_param[1] < 0) {
      paramsv <- sub(".*\\?(.*)", "\\1", str)
    }
    else {
      pa1 <- ".*\\?(.*)"
      if (length(gregexpr("\\&", str)[[1]]) >= 1) {
        for (k in 1:(length(gregexpr("\\&", str)[[1]]))) {
          pa1 <- paste(pa1, "\\&(.*)", sep = "")
        }
        for (k in 1:(length(gregexpr("\\&", str)[[1]]) + 1)) {
          pa <- paste("\\", k, sep = "")
          paramsv <- c(paramsv, sub(pa1, pa, str))
        }
      }
    }
  }
  else paramsv <- "NULL"
  return(paramsv)
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
Linkparamsfilter
list(`package:Rcrawler` = function (URL, params, removeAllparams = FALSE) 
{
  if (missing(URL)) 
    stop("You need to provide URL argument")
  if (!missing(params) && removeAllparams) 
    warning("params argument is omitted because remove all remove all parameter")
  if (missing(params) && !removeAllparams) 
    stop("you should specify parameters to be deleted ")
  str <- URL
  if (!removeAllparams) {
    paramsv <- Linkparameters(str)
    if (!is.null(paramsv)) {
      paramsid <- unlist(lapply(paramsv, FUN = function(el) gsub("(.*)\\=.*", "\\1", el)))
      nbparams <- length(paramsid)
      for (i in 1:length(params)) {
        if (params[i] %chin% paramsid) {
          paramsv <- Linkparameters(str)
          nbparams <- length(unlist(lapply(paramsv, FUN = function(el) gsub("(.*)\\=.*", "\\1", el))))
          if (nbparams > 1) {
            paramsi <- grep(paste("^", params[i], "$", sep = ""), paramsid)
            if (paramsi > 1) {
              pat <- paste("&", paramsv[paramsi], sep = "")
              str <- gsub(pat, "", str)
            }
            else {
              pat <- paste(paramsv[paramsi], "&", sep = "")
              str <- gsub(pat, "", str)
            }
          }
          else {
            paramsi <- grep(paste("^", params[i], "$", sep = ""), paramsid)
            pat <- paste("\\?", paramsv[paramsi], sep = "")
            str <- gsub(pat, "", str)
          }
        }
      }
    }
    else str <- URL
  }
  else {
    str <- gsub("\\?(.*)", "", str)
  }
  return(str)
}, function (URL, params, removeAllparams = FALSE) 
{
  if (missing(URL)) 
    stop("You need to provide URL argument")
  if (!missing(params) && removeAllparams) 
    warning("params argument is omitted because remove all remove all parameter")
  if (missing(params) && !removeAllparams) 
    stop("you should specify parameters to be deleted ")
  str <- URL
  if (!removeAllparams) {
    paramsv <- Linkparameters(str)
    if (!is.null(paramsv)) {
      paramsid <- unlist(lapply(paramsv, FUN = function(el) gsub("(.*)\\=.*", "\\1", el)))
      nbparams <- length(paramsid)
      for (i in 1:length(params)) {
        if (params[i] %chin% paramsid) {
          paramsv <- Linkparameters(str)
          nbparams <- length(unlist(lapply(paramsv, FUN = function(el) gsub("(.*)\\=.*", "\\1", el))))
          if (nbparams > 1) {
            paramsi <- grep(paste("^", params[i], "$", sep = ""), paramsid)
            if (paramsi > 1) {
              pat <- paste("&", paramsv[paramsi], sep = "")
              str <- gsub(pat, "", str)
            }
            else {
              pat <- paste(paramsv[paramsi], "&", sep = "")
              str <- gsub(pat, "", str)
            }
          }
          else {
            paramsi <- grep(paste("^", params[i], "$", sep = ""), paramsid)
            pat <- paste("\\?", paramsv[paramsi], sep = "")
            str <- gsub(pat, "", str)
          }
        }
      }
    }
    else str <- URL
  }
  else {
    str <- gsub("\\?(.*)", "", str)
  }
  return(str)
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
ListProjects
list(`package:Rcrawler` = function (DIR) 
{
  result <- ""
  if (missing(DIR)) {
    result <- list.files(paste0(getwd()), pattern = ".*-[0-9]{6}")
  }
  else {
    result <- list.files(paste0(DIR), pattern = ".*-[0-9]{6}")
  }
  if (length(result) == 0) 
    result <- "no project yet"
  return(result)
}, function (DIR) 
{
  result <- ""
  if (missing(DIR)) {
    result <- list.files(paste0(getwd()), pattern = ".*-[0-9]{6}")
  }
  else {
    result <- list.files(paste0(DIR), pattern = ".*-[0-9]{6}")
  }
  if (length(result) == 0) 
    result <- "no project yet"
  return(result)
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
LoadHTMLFiles
list(`package:Rcrawler` = function (ProjectName, type = "vector", max) 
{
  Listfiles <- list.files(paste0(getwd(), "/", ProjectName), pattern = ".*\\.html")
  if (type == "vector") 
    result <- vector()
  else if (type == "list") 
    result <- list()
  else stop("Unknown type , choose either vector or list")
  if (length(Listfiles) > 0) {
    if (missing(max)) {
      for (f in Listfiles) {
        filetxt <- readChar(paste0(getwd(), "/", ProjectName, "/", f), file.info(paste0(getwd(), "/", ProjectName, "/", f))$size)
        result <- c(result, filetxt)
      }
    }
    else {
      i = 1
      while (i <= max) {
        filetxt <- readChar(paste0(getwd(), "/", ProjectName, "/", Listfiles[i]), file.info(paste0(getwd(), "/", ProjectName, "/", Listfiles[i]))$size)
        result <- c(result, filetxt)
        i <- i + 1
      }
    }
  }
  else stop("Folder does not contain any html file")
  return(result)
}, function (ProjectName, type = "vector", max) 
{
  Listfiles <- list.files(paste0(getwd(), "/", ProjectName), pattern = ".*\\.html")
  if (type == "vector") 
    result <- vector()
  else if (type == "list") 
    result <- list()
  else stop("Unknown type , choose either vector or list")
  if (length(Listfiles) > 0) {
    if (missing(max)) {
      for (f in Listfiles) {
        filetxt <- readChar(paste0(getwd(), "/", ProjectName, "/", f), file.info(paste0(getwd(), "/", ProjectName, "/", f))$size)
        result <- c(result, filetxt)
      }
    }
    else {
      i = 1
      while (i <= max) {
        filetxt <- readChar(paste0(getwd(), "/", ProjectName, "/", Listfiles[i]), file.info(paste0(getwd(), "/", ProjectName, "/", Listfiles[i]))$size)
        result <- c(result, filetxt)
        i <- i + 1
      }
    }
  }
  else stop("Folder does not contain any html file")
  return(result)
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
LoginSession
list(`package:Rcrawler` = function (Browser, LoginURL, LoginCredentials, cssLoginFields, cssLoginButton, cssRadioToCheck, XpathLoginFields, XpathLoginButton, XpathRadioToCheck) 
{
  if (missing(Browser)) 
    stop("browser argument should be specified, use run_browser() to create a browser object")
  if (missing(LoginURL)) 
    stop("LoginURL argument should be specified")
  if (missing(LoginCredentials)) 
    stop("LoginCredentials argument should be specified,eg: c(\"email@acc.com\",\"password\")")
  if (missing(cssLoginFields) && missing(XpathLoginFields)) 
    stop("You should provide either cssLoginFields OR XpathLoginFields (css or xpath of Login Credential fields)")
  if (missing(cssLoginButton) && missing(XpathLoginButton)) 
    stop("You should provide either cssLoginButton OR XpathLoginButton (css or xpath of Login Button)")
  Browser$session$initialize(port = Browser$process$port)
  Browser$session$go(LoginURL)
  if (!missing(cssLoginFields) && !is.null(cssLoginFields)) {
    for (i in 1:length(cssLoginFields)) {
      e <- Browser$session$findElement(css = cssLoginFields[i])
      e$setValue(LoginCredentials[i])
    }
    XpathLoginFields <- NULL
  }
  if (!missing(XpathLoginFields) && !is.null(XpathLoginFields)) {
    for (i in 1:length(XpathLoginFields)) {
      e <- Browser$session$findElement(xpath = XpathLoginFields[i])
      e$setValue(LoginCredentials[i])
    }
    cssLoginFields <- NULL
  }
  if (!missing(cssRadioToCheck) && !is.null(cssRadioToCheck)) {
    for (i in 1:length(cssRadioToCheck)) {
      e <- Browser$session$findElement(css = cssRadioToCheck[i])
      e$click()
    }
    XpathRadioToCheck <- NULL
  }
  if (!missing(XpathRadioToCheck) && !is.null(XpathRadioToCheck)) {
    for (i in 1:length(XpathRadioToCheck)) {
      e <- Browser$session$findElement(xpath = XpathRadioToCheck[i])
      e$click()
    }
    cssRadioToCheck <- NULL
  }
  if (missing(cssRadioToCheck) && missing(XpathRadioToCheck)) {
    cssRadioToCheck <- NULL
    XpathRadioToCheck <- NULL
  }
  if (!missing(cssLoginButton) && !is.null(cssLoginButton)) {
    e <- Browser$session$findElement(css = cssLoginButton)
    e$click()
    XpathLoginButton <- NULL
  }
  if (!missing(XpathLoginButton) && !is.null(XpathLoginButton)) {
    e <- Browser$session$findElement(xpath = XpathLoginButton)
    e$click()
    cssLoginButton <- NULL
  }
  Browser[["loginInfo"]] <- list(LoginURL = LoginURL, LoginCredentials = LoginCredentials, cssLoginFields = cssLoginFields, cssLoginButton = cssLoginButton, cssRadioToCheck = cssRadioToCheck, XpathLoginFields = XpathLoginFields, XpathLoginButton = XpathLoginButton, XpathRadioToCheck = XpathRadioToCheck)
  Browser
}, function (Browser, LoginURL, LoginCredentials, cssLoginFields, cssLoginButton, cssRadioToCheck, XpathLoginFields, XpathLoginButton, XpathRadioToCheck) 
{
  if (missing(Browser)) 
    stop("browser argument should be specified, use run_browser() to create a browser object")
  if (missing(LoginURL)) 
    stop("LoginURL argument should be specified")
  if (missing(LoginCredentials)) 
    stop("LoginCredentials argument should be specified,eg: c(\"email@acc.com\",\"password\")")
  if (missing(cssLoginFields) && missing(XpathLoginFields)) 
    stop("You should provide either cssLoginFields OR XpathLoginFields (css or xpath of Login Credential fields)")
  if (missing(cssLoginButton) && missing(XpathLoginButton)) 
    stop("You should provide either cssLoginButton OR XpathLoginButton (css or xpath of Login Button)")
  Browser$session$initialize(port = Browser$process$port)
  Browser$session$go(LoginURL)
  if (!missing(cssLoginFields) && !is.null(cssLoginFields)) {
    for (i in 1:length(cssLoginFields)) {
      e <- Browser$session$findElement(css = cssLoginFields[i])
      e$setValue(LoginCredentials[i])
    }
    XpathLoginFields <- NULL
  }
  if (!missing(XpathLoginFields) && !is.null(XpathLoginFields)) {
    for (i in 1:length(XpathLoginFields)) {
      e <- Browser$session$findElement(xpath = XpathLoginFields[i])
      e$setValue(LoginCredentials[i])
    }
    cssLoginFields <- NULL
  }
  if (!missing(cssRadioToCheck) && !is.null(cssRadioToCheck)) {
    for (i in 1:length(cssRadioToCheck)) {
      e <- Browser$session$findElement(css = cssRadioToCheck[i])
      e$click()
    }
    XpathRadioToCheck <- NULL
  }
  if (!missing(XpathRadioToCheck) && !is.null(XpathRadioToCheck)) {
    for (i in 1:length(XpathRadioToCheck)) {
      e <- Browser$session$findElement(xpath = XpathRadioToCheck[i])
      e$click()
    }
    cssRadioToCheck <- NULL
  }
  if (missing(cssRadioToCheck) && missing(XpathRadioToCheck)) {
    cssRadioToCheck <- NULL
    XpathRadioToCheck <- NULL
  }
  if (!missing(cssLoginButton) && !is.null(cssLoginButton)) {
    e <- Browser$session$findElement(css = cssLoginButton)
    e$click()
    XpathLoginButton <- NULL
  }
  if (!missing(XpathLoginButton) && !is.null(XpathLoginButton)) {
    e <- Browser$session$findElement(xpath = XpathLoginButton)
    e$click()
    cssLoginButton <- NULL
  }
  Browser[["loginInfo"]] <- list(LoginURL = LoginURL, LoginCredentials = LoginCredentials, cssLoginFields = cssLoginFields, cssLoginButton = cssLoginButton, cssRadioToCheck = cssRadioToCheck, XpathLoginFields = XpathLoginFields, XpathLoginButton = XpathLoginButton, XpathRadioToCheck = XpathRadioToCheck)
  Browser
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
13)
RobotParser
list(`package:Rcrawler` = function (website, useragent) 
{
  URLrobot <- paste(website, "/robots.txt", sep = "")
  bots <- httr::GET(URLrobot, httr::user_agent("Mozilla/5.0 (Windows NT 6.3; WOW64; rv:42.0) Gecko/20100101 Firefox/42.0"), httr::timeout(5))
  bots <- as.character(httr::content(bots, as = "text", encoding = "UTF-8"))
  write(bots, file = "robots.txt")
  bots <- readLines("robots.txt")
  if (missing(useragent)) 
    useragent <- "Rcrawler"
  useragent <- c(useragent, "*")
  ua_positions <- which(grepl("[Uu]ser-[Aa]gent:[ ].+", bots))
  Disallow_dir <- vector()
  allow_dir <- vector()
  for (i in 1:length(useragent)) {
    if (useragent[i] == "*") 
      useragent[i] <- "\\*"
    Gua_pos <- which(grepl(paste("[Uu]ser-[Aa]gent:[ ]{0,}", useragent[i], "$", sep = ""), bots))
    if (length(Gua_pos) != 0) {
      Gua_rules_start <- Gua_pos + 1
      Gua_rules_end <- ua_positions[which(ua_positions == Gua_pos) + 1] - 1
      if (is.na(Gua_rules_end)) 
        Gua_rules_end <- length(bots)
      Gua_rules <- bots[Gua_rules_start:Gua_rules_end]
      Disallow_rules <- Gua_rules[grep("[Dd]isallow", Gua_rules)]
      Disallow_dir <- c(Disallow_dir, gsub(".*\\:.", "", Disallow_rules))
      allow_rules <- Gua_rules[grep("^[Aa]llow", Gua_rules)]
      allow_dir <- c(allow_dir, gsub(".*\\:.", "", allow_rules))
    }
  }
  if ("/" %in% Disallow_dir) {
    Blocked = TRUE
    print("This bot is blocked from the site")
  }
  else {
    Blocked = FALSE
  }
  Rules <- list(Allow = allow_dir, Disallow = Disallow_dir, Blocked = Blocked)
  return(Rules)
}, function (website, useragent) 
{
  URLrobot <- paste(website, "/robots.txt", sep = "")
  bots <- httr::GET(URLrobot, httr::user_agent("Mozilla/5.0 (Windows NT 6.3; WOW64; rv:42.0) Gecko/20100101 Firefox/42.0"), httr::timeout(5))
  bots <- as.character(httr::content(bots, as = "text", encoding = "UTF-8"))
  write(bots, file = "robots.txt")
  bots <- readLines("robots.txt")
  if (missing(useragent)) 
    useragent <- "Rcrawler"
  useragent <- c(useragent, "*")
  ua_positions <- which(grepl("[Uu]ser-[Aa]gent:[ ].+", bots))
  Disallow_dir <- vector()
  allow_dir <- vector()
  for (i in 1:length(useragent)) {
    if (useragent[i] == "*") 
      useragent[i] <- "\\*"
    Gua_pos <- which(grepl(paste("[Uu]ser-[Aa]gent:[ ]{0,}", useragent[i], "$", sep = ""), bots))
    if (length(Gua_pos) != 0) {
      Gua_rules_start <- Gua_pos + 1
      Gua_rules_end <- ua_positions[which(ua_positions == Gua_pos) + 1] - 1
      if (is.na(Gua_rules_end)) 
        Gua_rules_end <- length(bots)
      Gua_rules <- bots[Gua_rules_start:Gua_rules_end]
      Disallow_rules <- Gua_rules[grep("[Dd]isallow", Gua_rules)]
      Disallow_dir <- c(Disallow_dir, gsub(".*\\:.", "", Disallow_rules))
      allow_rules <- Gua_rules[grep("^[Aa]llow", Gua_rules)]
      allow_dir <- c(allow_dir, gsub(".*\\:.", "", allow_rules))
    }
  }
  if ("/" %in% Disallow_dir) {
    Blocked = TRUE
    print("This bot is blocked from the site")
  }
  else {
    Blocked = FALSE
  }
  Rules <- list(Allow = allow_dir, Disallow = Disallow_dir, Blocked = Blocked)
  return(Rules)
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
run_browser
list(`package:Rcrawler` = function (debugLevel = "DEBUG", timeout = 5000) 
{
  Drv <- webdriver::run_phantomjs(debugLevel = debugLevel, timeout = timeout)
  Ses <- Session$new(port = Drv$port)
  list(session = Ses, process = Drv)
}, function (debugLevel = "DEBUG", timeout = 5000) 
{
  Drv <- webdriver::run_phantomjs(debugLevel = debugLevel, timeout = timeout)
  Ses <- Session$new(port = Drv$port)
  list(session = Ses, process = Drv)
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)
stop_browser
list(`package:Rcrawler` = function (browser) 
{
  browser$session$delete()
  browser$process$process$suspend()
  browser$process$process$finalize()
  browser$process$process$kill_tree()
}, function (browser) 
{
  browser$session$delete()
  browser$process$process$suspend()
  browser$process$process$finalize()
  browser$process$process$kill_tree()
})
c("package:Rcrawler", "namespace:Rcrawler")
c(TRUE, FALSE)
c(FALSE, TRUE)

                         