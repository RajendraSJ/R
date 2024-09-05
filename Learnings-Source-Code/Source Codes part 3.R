library(DT)              #7
library(fastmatch)       #1616
library(fortunes)        #1694
library(fun)             #1839
library(geosphere)       #2918

library(DT)
%>%
  list(`package:DT` = function (lhs, rhs) 
  {
    lhs <- substitute(lhs)
    rhs <- substitute(rhs)
    kind <- 1
    env <- parent.frame()
    lazy <- TRUE
    .External2(magrittr_pipe)
  }, `package:dplyr` = function (lhs, rhs) 
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
c("package:DT", "package:dplyr", "namespace:magrittr")
c(TRUE, TRUE, FALSE)
c(FALSE, TRUE, TRUE)
addRow
list(`package:DT` = function (proxy, data, resetPaging = TRUE) 
{
  if ((is.matrix(data) || is.data.frame(data)) && nrow(data) != 1) 
    stop("'data' must be of only one row")
  rn <- rownames(data)
  if (!is.null(rn)) 
    rn <- I(rn)
  invokeRemote(proxy, "addRow", list(unname(as.list(data)), rn, resetPaging))
}, function (proxy, data, resetPaging = TRUE) 
{
  if ((is.matrix(data) || is.data.frame(data)) && nrow(data) != 1) 
    stop("'data' must be of only one row")
  rn <- rownames(data)
  if (!is.null(rn)) 
    rn <- I(rn)
  invokeRemote(proxy, "addRow", list(unname(as.list(data)), rn, resetPaging))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
clearSearch
list(`package:DT` = function (proxy) 
{
  updateSearch(proxy, list(global = "", columns = ""))
}, function (proxy) 
{
  updateSearch(proxy, list(global = "", columns = ""))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
coerceValue
list(`package:DT` = function (val, old) 
{
  if (is.integer(old)) 
    return(as.integer(val))
  if (is.numeric(old)) 
    return(as.numeric(val))
  if (is.character(old)) 
    return(as.character(val))
  if (inherits(old, "Date")) 
    return(as.Date(val))
  if (inherits(old, c("POSIXlt", "POSIXct"))) {
    val = strptime(val, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (inherits(old, "POSIXlt")) 
      return(val)
    return(as.POSIXct(val))
  }
  if (is.factor(old)) {
    i = val %in% levels(old)
    if (all(i)) 
      return(val)
    warning("New value(s) \"", paste(val[!i], collapse = ", "), "\" not in the original factor levels: \"", paste(levels(old), collapse = ", "), "\"; will be coerced to NA.")
    val[!i] = NA
    return(val)
  }
  warning("The data type is not supported: ", classes(old))
  val
}, function (val, old) 
{
  if (is.integer(old)) 
    return(as.integer(val))
  if (is.numeric(old)) 
    return(as.numeric(val))
  if (is.character(old)) 
    return(as.character(val))
  if (inherits(old, "Date")) 
    return(as.Date(val))
  if (inherits(old, c("POSIXlt", "POSIXct"))) {
    val = strptime(val, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (inherits(old, "POSIXlt")) 
      return(val)
    return(as.POSIXct(val))
  }
  if (is.factor(old)) {
    i = val %in% levels(old)
    if (all(i)) 
      return(val)
    warning("New value(s) \"", paste(val[!i], collapse = ", "), "\" not in the original factor levels: \"", paste(levels(old), collapse = ", "), "\"; will be coerced to NA.")
    val[!i] = NA
    return(val)
  }
  warning("The data type is not supported: ", classes(old))
  val
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
colReorder
list(`package:DT` = function (proxy, order, origOrder = FALSE) 
{
  invokeRemote(proxy, "colReorder", list(order, origOrder))
}, function (proxy, order, origOrder = FALSE) 
{
  invokeRemote(proxy, "colReorder", list(order, origOrder))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
datatable
list(`package:DT` = function (data, options = list(), class = "display", callback = JS("return table;"), rownames, colnames, container, caption = NULL, filter = c("none", "bottom", "top"), escape = TRUE, style = "auto", width = NULL, height = NULL, elementId = NULL, fillContainer = getOption("DT.fillContainer", NULL), autoHideNavigation = getOption("DT.autoHideNavigation", NULL), selection = c("multiple", "single", "none"), extensions = list(), plugins = NULL, editable = FALSE) 
{
  oop = base::options(stringsAsFactors = FALSE)
  on.exit(base::options(oop), add = TRUE)
  options = modifyList(getOption("DT.options", list()), if (is.function(options)) 
    options()
    else options)
  if (is.character(btnOpts <- options[["buttons"]])) 
    options[["buttons"]] = as.list(btnOpts)
  params = list()
  attr(params, "TOJSON_ARGS") = getOption("DT.TOJSON_ARGS")
  if (crosstalk::is.SharedData(data)) {
    params$crosstalkOptions = list(key = data$key(), group = data$groupName())
    data = data$data(withSelection = FALSE, withFilter = TRUE, withKey = FALSE)
  }
  rn = if (missing(rownames) || isTRUE(rownames)) 
    base::rownames(data)
  else {
    if (is.character(rownames)) 
      rownames
  }
  hideDataTable = FALSE
  if (is.null(data) || identical(ncol(data), 0)) {
    data = matrix(ncol = 0, nrow = NROW(data))
    hideDataTable = TRUE
  }
  else if (length(dim(data)) != 2) {
    str(data)
    stop("'data' must be 2-dimensional (e.g. data frame or matrix)")
  }
  if (is.data.frame(data)) {
    data = as.data.frame(data)
    numc = unname(which(vapply(data, is.numeric, logical(1))))
  }
  else {
    if (!is.matrix(data)) 
      stop("'data' must be either a matrix or a data frame, and cannot be ", classes(data), " (you may need to coerce it to matrix or data frame)")
    numc = if (is.numeric(data)) 
      seq_len(ncol(data))
    data = as.data.frame(data)
  }
  if (!is.null(rn)) {
    data = cbind(` ` = rn, data)
    numc = numc + 1
  }
  options[["columnDefs"]] = colDefsTgtHandle(options[["columnDefs"]], base::colnames(data))
  if (length(numc)) {
    undefined_numc = setdiff(numc - 1, classNameDefinedColumns(options, ncol(data)))
    if (length(undefined_numc)) 
      options = appendColumnDefs(options, list(className = "dt-right", targets = undefined_numc))
  }
  if (is.null(options[["order"]])) 
    options$order = list()
  if (is.null(options[["autoWidth"]])) 
    options$autoWidth = FALSE
  if (is.null(options[["orderClasses"]])) 
    options$orderClasses = FALSE
  cn = base::colnames(data)
  if (missing(colnames)) {
    colnames = cn
  }
  else if (!is.null(names(colnames))) {
    i = convertIdx(colnames, cn)
    cn[i] = names(colnames)
    colnames = cn
  }
  if (ncol(data) - length(colnames) == 1) 
    colnames = c(" ", colnames)
  if (length(colnames) && colnames[1] == " ") 
    options = appendColumnDefs(options, list(orderable = FALSE, targets = 0))
  style = normalizeStyle(style)
  if (grepl("^bootstrap", style)) 
    class = DT2BSClass(class)
  if (style != "default") 
    params$style = style
  if (isTRUE(fillContainer)) 
    class = paste(class, "fill-container")
  if (is.character(filter)) 
    filter = list(position = match.arg(filter))
  filter = modifyList(list(position = "none", clear = TRUE, plain = FALSE, vertical = FALSE, opacity = 1), filter)
  filterHTML = as.character(filterRow(data, !is.null(rn) && colnames[1] == " ", filter))
  if (filter$position == "top") 
    options$orderCellsTop = TRUE
  params$filter = filter$position
  params$vertical = filter$vertical
  if (filter$position != "none") 
    params$filterHTML = filterHTML
  if (missing(container)) {
    container = tags$table(tableHeader(colnames, escape), class = class)
  }
  else {
    params$class = class
  }
  attr(options, "escapeIdx") = escapeToConfig(escape, colnames)
  if (is.list(extensions)) {
    extensions = names(extensions)
  }
  else if (!is.character(extensions)) {
    stop("'extensions' must be either a character vector or a named list")
  }
  params$extensions = if (length(extensions)) 
    as.list(extensions)
  if ("Responsive" %in% extensions && is.null(options$responsive)) {
    options$responsive = TRUE
  }
  params$caption = captionString(caption)
  if (isTRUE(editable)) 
    editable = "cell"
  if (is.character(editable)) 
    editable = list(target = editable, disable = list(columns = NULL))
  if (is.list(editable)) {
    editable$numeric = makeEditableNumericField(editable$numeric, data, rn)
    editable$area = makeEditableAreaField(editable$area, data, rn)
    params$editable = editable
  }
  if (!identical(class(callback), class(JS("")))) 
    stop("The 'callback' argument only accept a value returned from JS()")
  if (length(options$pageLength) && length(options$lengthMenu) == 0) {
    if (!isFALSE(options$lengthChange)) 
      options$lengthMenu = sort(unique(c(options$pageLength, 10, 25, 50, 100)))
    if (identical(options$lengthMenu, c(10, 25, 50, 100))) 
      options$lengthMenu = NULL
  }
  if (!is.null(options[["search"]]) && !is.list(options[["search"]])) 
    stop("The value of `search` in `options` must be NULL or a list")
  if (!is.null(fillContainer)) 
    params$fillContainer = fillContainer
  if (!is.null(autoHideNavigation)) {
    if (isTRUE(autoHideNavigation) && length(options$pageLength) == 0) 
      warning("`autoHideNavigation` will be ignored if the `pageLength` option is not provided.", immediate. = TRUE)
    params$autoHideNavigation = autoHideNavigation
  }
  params = structure(modifyList(params, list(data = data, container = as.character(container), options = options, callback = if (!missing(callback)) JS("function(table) {", callback, "}"))), colnames = cn, rownames = length(rn) > 0)
  if (inShiny() || length(params$crosstalkOptions)) {
    if (is.character(selection)) {
      selection = list(mode = match.arg(selection))
    }
    selection = modifyList(list(mode = "multiple", selected = NULL, target = "row", selectable = NULL), selection, keep.null = TRUE)
    if (grepl("^row", selection$target) && is.character(selection$selected) && length(rn)) {
      selection$selected = match(selection$selected, rn)
    }
    params$selection = validateSelection(selection)
    if ("Select" %in% extensions && selection$mode != "none") 
      warning("The Select extension can't work properly with DT's own ", "selection implemention and is only recommended in the client mode. ", "If you really want to use the Select extension please set ", "`selection = 'none'`", immediate. = TRUE)
  }
  deps = DTDependencies(style)
  deps = c(deps, unlist(lapply(extensions, extDependency, style, options), recursive = FALSE))
  if (params$filter != "none") 
    deps = c(deps, filterDependencies())
  if (isTRUE(options$searchHighlight)) 
    deps = c(deps, list(pluginDependency("searchHighlight")))
  if (length(plugins)) 
    deps = c(deps, lapply(plugins, pluginDependency))
  deps = c(deps, crosstalk::crosstalkLibs())
  if (isTRUE(fillContainer)) {
    width = NULL
    height = NULL
  }
  htmlwidgets::createWidget("datatables", if (hideDataTable) 
    NULL
    else params, package = "DT", width = width, height = height, elementId = elementId, sizingPolicy = htmlwidgets::sizingPolicy(knitr.figure = FALSE, knitr.defaultWidth = "100%", knitr.defaultHeight = "auto"), dependencies = deps, preRenderHook = function(instance) {
      data = instance[["x"]][["data"]]
      if (object.size(data) > 1500000 && getOption("DT.warn.size", TRUE)) 
        warning("It seems your data is too big for client-side DataTables. You may ", "consider server-side processing: https://rstudio.github.io/DT/server.html")
      data = escapeData(data, escape, colnames)
      data = unname(data)
      instance$x$data = data
      instance
    })
}, function (data, options = list(), class = "display", callback = JS("return table;"), rownames, colnames, container, caption = NULL, filter = c("none", "bottom", "top"), escape = TRUE, style = "auto", width = NULL, height = NULL, elementId = NULL, fillContainer = getOption("DT.fillContainer", NULL), autoHideNavigation = getOption("DT.autoHideNavigation", NULL), selection = c("multiple", "single", "none"), extensions = list(), plugins = NULL, editable = FALSE) 
{
  oop = base::options(stringsAsFactors = FALSE)
  on.exit(base::options(oop), add = TRUE)
  options = modifyList(getOption("DT.options", list()), if (is.function(options)) 
    options()
    else options)
  if (is.character(btnOpts <- options[["buttons"]])) 
    options[["buttons"]] = as.list(btnOpts)
  params = list()
  attr(params, "TOJSON_ARGS") = getOption("DT.TOJSON_ARGS")
  if (crosstalk::is.SharedData(data)) {
    params$crosstalkOptions = list(key = data$key(), group = data$groupName())
    data = data$data(withSelection = FALSE, withFilter = TRUE, withKey = FALSE)
  }
  rn = if (missing(rownames) || isTRUE(rownames)) 
    base::rownames(data)
  else {
    if (is.character(rownames)) 
      rownames
  }
  hideDataTable = FALSE
  if (is.null(data) || identical(ncol(data), 0)) {
    data = matrix(ncol = 0, nrow = NROW(data))
    hideDataTable = TRUE
  }
  else if (length(dim(data)) != 2) {
    str(data)
    stop("'data' must be 2-dimensional (e.g. data frame or matrix)")
  }
  if (is.data.frame(data)) {
    data = as.data.frame(data)
    numc = unname(which(vapply(data, is.numeric, logical(1))))
  }
  else {
    if (!is.matrix(data)) 
      stop("'data' must be either a matrix or a data frame, and cannot be ", classes(data), " (you may need to coerce it to matrix or data frame)")
    numc = if (is.numeric(data)) 
      seq_len(ncol(data))
    data = as.data.frame(data)
  }
  if (!is.null(rn)) {
    data = cbind(` ` = rn, data)
    numc = numc + 1
  }
  options[["columnDefs"]] = colDefsTgtHandle(options[["columnDefs"]], base::colnames(data))
  if (length(numc)) {
    undefined_numc = setdiff(numc - 1, classNameDefinedColumns(options, ncol(data)))
    if (length(undefined_numc)) 
      options = appendColumnDefs(options, list(className = "dt-right", targets = undefined_numc))
  }
  if (is.null(options[["order"]])) 
    options$order = list()
  if (is.null(options[["autoWidth"]])) 
    options$autoWidth = FALSE
  if (is.null(options[["orderClasses"]])) 
    options$orderClasses = FALSE
  cn = base::colnames(data)
  if (missing(colnames)) {
    colnames = cn
  }
  else if (!is.null(names(colnames))) {
    i = convertIdx(colnames, cn)
    cn[i] = names(colnames)
    colnames = cn
  }
  if (ncol(data) - length(colnames) == 1) 
    colnames = c(" ", colnames)
  if (length(colnames) && colnames[1] == " ") 
    options = appendColumnDefs(options, list(orderable = FALSE, targets = 0))
  style = normalizeStyle(style)
  if (grepl("^bootstrap", style)) 
    class = DT2BSClass(class)
  if (style != "default") 
    params$style = style
  if (isTRUE(fillContainer)) 
    class = paste(class, "fill-container")
  if (is.character(filter)) 
    filter = list(position = match.arg(filter))
  filter = modifyList(list(position = "none", clear = TRUE, plain = FALSE, vertical = FALSE, opacity = 1), filter)
  filterHTML = as.character(filterRow(data, !is.null(rn) && colnames[1] == " ", filter))
  if (filter$position == "top") 
    options$orderCellsTop = TRUE
  params$filter = filter$position
  params$vertical = filter$vertical
  if (filter$position != "none") 
    params$filterHTML = filterHTML
  if (missing(container)) {
    container = tags$table(tableHeader(colnames, escape), class = class)
  }
  else {
    params$class = class
  }
  attr(options, "escapeIdx") = escapeToConfig(escape, colnames)
  if (is.list(extensions)) {
    extensions = names(extensions)
  }
  else if (!is.character(extensions)) {
    stop("'extensions' must be either a character vector or a named list")
  }
  params$extensions = if (length(extensions)) 
    as.list(extensions)
  if ("Responsive" %in% extensions && is.null(options$responsive)) {
    options$responsive = TRUE
  }
  params$caption = captionString(caption)
  if (isTRUE(editable)) 
    editable = "cell"
  if (is.character(editable)) 
    editable = list(target = editable, disable = list(columns = NULL))
  if (is.list(editable)) {
    editable$numeric = makeEditableNumericField(editable$numeric, data, rn)
    editable$area = makeEditableAreaField(editable$area, data, rn)
    params$editable = editable
  }
  if (!identical(class(callback), class(JS("")))) 
    stop("The 'callback' argument only accept a value returned from JS()")
  if (length(options$pageLength) && length(options$lengthMenu) == 0) {
    if (!isFALSE(options$lengthChange)) 
      options$lengthMenu = sort(unique(c(options$pageLength, 10, 25, 50, 100)))
    if (identical(options$lengthMenu, c(10, 25, 50, 100))) 
      options$lengthMenu = NULL
  }
  if (!is.null(options[["search"]]) && !is.list(options[["search"]])) 
    stop("The value of `search` in `options` must be NULL or a list")
  if (!is.null(fillContainer)) 
    params$fillContainer = fillContainer
  if (!is.null(autoHideNavigation)) {
    if (isTRUE(autoHideNavigation) && length(options$pageLength) == 0) 
      warning("`autoHideNavigation` will be ignored if the `pageLength` option is not provided.", immediate. = TRUE)
    params$autoHideNavigation = autoHideNavigation
  }
  params = structure(modifyList(params, list(data = data, container = as.character(container), options = options, callback = if (!missing(callback)) JS("function(table) {", callback, "}"))), colnames = cn, rownames = length(rn) > 0)
  if (inShiny() || length(params$crosstalkOptions)) {
    if (is.character(selection)) {
      selection = list(mode = match.arg(selection))
    }
    selection = modifyList(list(mode = "multiple", selected = NULL, target = "row", selectable = NULL), selection, keep.null = TRUE)
    if (grepl("^row", selection$target) && is.character(selection$selected) && length(rn)) {
      selection$selected = match(selection$selected, rn)
    }
    params$selection = validateSelection(selection)
    if ("Select" %in% extensions && selection$mode != "none") 
      warning("The Select extension can't work properly with DT's own ", "selection implemention and is only recommended in the client mode. ", "If you really want to use the Select extension please set ", "`selection = 'none'`", immediate. = TRUE)
  }
  deps = DTDependencies(style)
  deps = c(deps, unlist(lapply(extensions, extDependency, style, options), recursive = FALSE))
  if (params$filter != "none") 
    deps = c(deps, filterDependencies())
  if (isTRUE(options$searchHighlight)) 
    deps = c(deps, list(pluginDependency("searchHighlight")))
  if (length(plugins)) 
    deps = c(deps, lapply(plugins, pluginDependency))
  deps = c(deps, crosstalk::crosstalkLibs())
  if (isTRUE(fillContainer)) {
    width = NULL
    height = NULL
  }
  htmlwidgets::createWidget("datatables", if (hideDataTable) 
    NULL
    else params, package = "DT", width = width, height = height, elementId = elementId, sizingPolicy = htmlwidgets::sizingPolicy(knitr.figure = FALSE, knitr.defaultWidth = "100%", knitr.defaultHeight = "auto"), dependencies = deps, preRenderHook = function(instance) {
      data = instance[["x"]][["data"]]
      if (object.size(data) > 1500000 && getOption("DT.warn.size", TRUE)) 
        warning("It seems your data is too big for client-side DataTables. You may ", "consider server-side processing: https://rstudio.github.io/DT/server.html")
      data = escapeData(data, escape, colnames)
      data = unname(data)
      instance$x$data = data
      instance
    })
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
dataTableAjax
list(`package:DT` = function (session, data, rownames, filter = dataTablesFilter, outputId) 
{
  oop = options(stringsAsFactors = FALSE)
  on.exit(options(oop), add = TRUE)
  if (missing(outputId)) 
    outputId = getCurrentOutputName(session)
  if (is.null(outputId)) 
    outputId = basename(tempfile(""))
  rn = if (missing(rownames) || isTRUE(rownames)) 
    base::rownames(data)
  else {
    if (is.character(rownames)) 
      rownames
  }
  data = as.data.frame(data)
  if (length(rn)) 
    data = cbind(` ` = rn, data)
  sessionDataURL(session, data, outputId, filter)
}, function (session, data, rownames, filter = dataTablesFilter, outputId) 
{
  oop = options(stringsAsFactors = FALSE)
  on.exit(options(oop), add = TRUE)
  if (missing(outputId)) 
    outputId = getCurrentOutputName(session)
  if (is.null(outputId)) 
    outputId = basename(tempfile(""))
  rn = if (missing(rownames) || isTRUE(rownames)) 
    base::rownames(data)
  else {
    if (is.character(rownames)) 
      rownames
  }
  data = as.data.frame(data)
  if (length(rn)) 
    data = cbind(` ` = rn, data)
  sessionDataURL(session, data, outputId, filter)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
dataTableOutput
list(`package:DT` = function (outputId, width = "100%", height = "auto") 
{
  htmltools::attachDependencies(htmlwidgets::shinyWidgetOutput(outputId, "datatables", width, height, package = "DT"), crosstalk::crosstalkLibs(), append = TRUE)
}, function (outputId, width = "100%", height = "auto") 
{
  htmltools::attachDependencies(htmlwidgets::shinyWidgetOutput(outputId, "datatables", width, height, package = "DT"), crosstalk::crosstalkLibs(), append = TRUE)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
dataTableProxy
list(`package:DT` = function (outputId, session = shiny::getDefaultReactiveDomain(), deferUntilFlush = TRUE) 
{
  if (is.null(session)) 
    stop("dataTableProxy() must be called from the server function of a Shiny app")
  structure(list(id = session$ns(outputId), rawId = outputId, session = session, deferUntilFlush = deferUntilFlush), class = "dataTableProxy")
}, function (outputId, session = shiny::getDefaultReactiveDomain(), deferUntilFlush = TRUE) 
{
  if (is.null(session)) 
    stop("dataTableProxy() must be called from the server function of a Shiny app")
  structure(list(id = session$ns(outputId), rawId = outputId, session = session, deferUntilFlush = deferUntilFlush), class = "dataTableProxy")
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
doColumnSearch
list(`package:DT` = function (x, search_string, options = list()) 
{
  if (length(search_string) == 0 || search_string == "") 
    return(seq_along(x))
  if (is.numeric(x) || is.Date(x)) {
    which(filterRange(x, search_string))
  }
  else if (is.factor(x)) {
    which(x %in% fromJSON(search_string))
  }
  else if (is.logical(x)) {
    which(x %in% as.logical(fromJSON(search_string)))
  }
  else {
    grep2(search_string, as.character(x), fixed = !(options$regex %||% FALSE), ignore.case = options$caseInsensitive %||% TRUE)
  }
}, function (x, search_string, options = list()) 
{
  if (length(search_string) == 0 || search_string == "") 
    return(seq_along(x))
  if (is.numeric(x) || is.Date(x)) {
    which(filterRange(x, search_string))
  }
  else if (is.factor(x)) {
    which(x %in% fromJSON(search_string))
  }
  else if (is.logical(x)) {
    which(x %in% as.logical(fromJSON(search_string)))
  }
  else {
    grep2(search_string, as.character(x), fixed = !(options$regex %||% FALSE), ignore.case = options$caseInsensitive %||% TRUE)
  }
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
doGlobalSearch
list(`package:DT` = function (data, search_string, options = list()) 
{
  n = nrow(data)
  if (length(v <- search_string) > 0) {
    if (options$smart %||% TRUE) {
      v = unlist(strsplit(gsub("^\\s+|\\s+$", "", v), "\\s+"))
    }
  }
  if (length(v) == 0) 
    v = ""
  m = if ((nv <- length(v)) > 1) 
    array(FALSE, c(dim(data), nv))
  else logical(n)
  if (!identical(v, "")) {
    for (j in seq_len(ncol(data))) {
      for (k in seq_len(nv)) {
        i0 = grep2(v[k], as.character(data[, j]), fixed = !(options$regex %||% FALSE), ignore.case = options$caseInsensitive %||% TRUE)
        if (nv > 1) 
          m[i0, j, k] = TRUE
        else m[i0] = TRUE
      }
    }
    which(if (nv > 1) 
      apply(m, 1, function(z) all(colSums(z) > 0))
      else m)
  }
  else seq_len(n)
}, function (data, search_string, options = list()) 
{
  n = nrow(data)
  if (length(v <- search_string) > 0) {
    if (options$smart %||% TRUE) {
      v = unlist(strsplit(gsub("^\\s+|\\s+$", "", v), "\\s+"))
    }
  }
  if (length(v) == 0) 
    v = ""
  m = if ((nv <- length(v)) > 1) 
    array(FALSE, c(dim(data), nv))
  else logical(n)
  if (!identical(v, "")) {
    for (j in seq_len(ncol(data))) {
      for (k in seq_len(nv)) {
        i0 = grep2(v[k], as.character(data[, j]), fixed = !(options$regex %||% FALSE), ignore.case = options$caseInsensitive %||% TRUE)
        if (nv > 1) 
          m[i0, j, k] = TRUE
        else m[i0] = TRUE
      }
    }
    which(if (nv > 1) 
      apply(m, 1, function(z) all(colSums(z) > 0))
      else m)
  }
  else seq_len(n)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
DTOutput
list(`package:DT` = function (outputId, width = "100%", height = "auto") 
{
  htmltools::attachDependencies(htmlwidgets::shinyWidgetOutput(outputId, "datatables", width, height, package = "DT"), crosstalk::crosstalkLibs(), append = TRUE)
}, function (outputId, width = "100%", height = "auto") 
{
  htmltools::attachDependencies(htmlwidgets::shinyWidgetOutput(outputId, "datatables", width, height, package = "DT"), crosstalk::crosstalkLibs(), append = TRUE)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
editData
list(`package:DT` = function (data, info, proxy = NULL, rownames = TRUE, resetPaging = FALSE, ...) 
{
  for (r in split(info, info$col)) {
    i = r$row
    j = r$col + !rownames
    v = r$value
    j = j[1]
    if (j == 0) {
      rownames(data)[i] = v
    }
    else {
      if (is.factor(data[[j]]) && !all(v %in% levels(data[[j]]))) {
        levels(data[[j]]) <- unique(c(levels(data[[j]]), v))
      }
      data[i, j] = coerceValue(v, data[i, j, drop = TRUE])
    }
  }
  if (is.character(proxy)) 
    proxy = dataTableProxy(proxy)
  if (inherits(proxy, "dataTableProxy")) {
    replaceData(proxy, data, resetPaging = resetPaging, rownames = rownames, ...)
  }
  data
}, function (data, info, proxy = NULL, rownames = TRUE, resetPaging = FALSE, ...) 
{
  for (r in split(info, info$col)) {
    i = r$row
    j = r$col + !rownames
    v = r$value
    j = j[1]
    if (j == 0) {
      rownames(data)[i] = v
    }
    else {
      if (is.factor(data[[j]]) && !all(v %in% levels(data[[j]]))) {
        levels(data[[j]]) <- unique(c(levels(data[[j]]), v))
      }
      data[i, j] = coerceValue(v, data[i, j, drop = TRUE])
    }
  }
  if (is.character(proxy)) 
    proxy = dataTableProxy(proxy)
  if (inherits(proxy, "dataTableProxy")) {
    replaceData(proxy, data, resetPaging = resetPaging, rownames = rownames, ...)
  }
  data
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
formatCurrency
list(`package:DT` = function (table, columns, currency = "$", interval = 3, mark = ",", digits = 2, dec.mark = getOption("OutDec"), before = TRUE, zero.print = NULL, rows = NULL) 
{
  currency = gsub("'", "\\\\'", currency)
  mark = gsub("'", "\\\\'", mark)
  formatColumns(table, columns, tplCurrency, currency, interval, mark, digits, dec.mark, before, zero.print, rows = rows)
}, function (table, columns, currency = "$", interval = 3, mark = ",", digits = 2, dec.mark = getOption("OutDec"), before = TRUE, zero.print = NULL, rows = NULL) 
{
  currency = gsub("'", "\\\\'", currency)
  mark = gsub("'", "\\\\'", mark)
  formatColumns(table, columns, tplCurrency, currency, interval, mark, digits, dec.mark, before, zero.print, rows = rows)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
formatDate
list(`package:DT` = function (table, columns, method = "toDateString", params = NULL, rows = NULL) 
{
  if (!inherits(table, "datatables")) 
    stop("Invalid table argument; a table object created from datatable() was expected")
  x = table$x
  if (x$filter != "none") {
    if (inherits(columns, "formula")) 
      columns = all.vars(columns)
    colnames = base::attr(x, "colnames", exact = TRUE)
    rownames = base::attr(x, "rownames", exact = TRUE)
    if (is.null(params)) 
      params = list()
    cols = sprintf("%d", name2int(columns, colnames, rownames))
    x$filterDateFmt = as.list(x$filterDateFmt)
    for (col in cols) x$filterDateFmt[[col]] = list(method = method, params = toJSON(params))
    table$x = x
  }
  formatColumns(table, columns, tplDate, method, params, rows = rows)
}, function (table, columns, method = "toDateString", params = NULL, rows = NULL) 
{
  if (!inherits(table, "datatables")) 
    stop("Invalid table argument; a table object created from datatable() was expected")
  x = table$x
  if (x$filter != "none") {
    if (inherits(columns, "formula")) 
      columns = all.vars(columns)
    colnames = base::attr(x, "colnames", exact = TRUE)
    rownames = base::attr(x, "rownames", exact = TRUE)
    if (is.null(params)) 
      params = list()
    cols = sprintf("%d", name2int(columns, colnames, rownames))
    x$filterDateFmt = as.list(x$filterDateFmt)
    for (col in cols) x$filterDateFmt[[col]] = list(method = method, params = toJSON(params))
    table$x = x
  }
  formatColumns(table, columns, tplDate, method, params, rows = rows)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
formatPercentage
list(`package:DT` = function (table, columns, digits = 0, interval = 3, mark = ",", dec.mark = getOption("OutDec"), zero.print = NULL, rows = NULL) 
{
  formatColumns(table, columns, tplPercentage, digits, interval, mark, dec.mark, zero.print, rows = rows)
}, function (table, columns, digits = 0, interval = 3, mark = ",", dec.mark = getOption("OutDec"), zero.print = NULL, rows = NULL) 
{
  formatColumns(table, columns, tplPercentage, digits, interval, mark, dec.mark, zero.print, rows = rows)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
formatRound
list(`package:DT` = function (table, columns, digits = 2, interval = 3, mark = ",", dec.mark = getOption("OutDec"), zero.print = NULL, rows = NULL) 
{
  formatColumns(table, columns, tplRound, digits, interval, mark, dec.mark, zero.print, rows = rows)
}, function (table, columns, digits = 2, interval = 3, mark = ",", dec.mark = getOption("OutDec"), zero.print = NULL, rows = NULL) 
{
  formatColumns(table, columns, tplRound, digits, interval, mark, dec.mark, zero.print, rows = rows)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
formatSignif
list(`package:DT` = function (table, columns, digits = 2, interval = 3, mark = ",", dec.mark = getOption("OutDec"), zero.print = NULL, rows = NULL) 
{
  formatColumns(table, columns, tplSignif, digits, interval, mark, dec.mark, zero.print, rows = rows)
}, function (table, columns, digits = 2, interval = 3, mark = ",", dec.mark = getOption("OutDec"), zero.print = NULL, rows = NULL) 
{
  formatColumns(table, columns, tplSignif, digits, interval, mark, dec.mark, zero.print, rows = rows)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
formatString
list(`package:DT` = function (table, columns, prefix = "", suffix = "", rows = NULL) 
{
  formatColumns(table, columns, tplString, prefix, suffix, rows = rows)
}, function (table, columns, prefix = "", suffix = "", rows = NULL) 
{
  formatColumns(table, columns, tplString, prefix, suffix, rows = rows)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
formatStyle
list(`package:DT` = function (table, columns, valueColumns = columns, target = c("cell", "row"), fontWeight = NULL, color = NULL, backgroundColor = NULL, background = NULL, ...) 
{
  styles = dropNULL(list(fontWeight = fontWeight, color = color, backgroundColor = backgroundColor, background = background, ...))
  formatColumns(table, columns, tplStyle, valueColumns, match.arg(target), styles, appendTo = "rowCallback")
}, function (table, columns, valueColumns = columns, target = c("cell", "row"), fontWeight = NULL, color = NULL, backgroundColor = NULL, background = NULL, ...) 
{
  styles = dropNULL(list(fontWeight = fontWeight, color = color, backgroundColor = backgroundColor, background = background, ...))
  formatColumns(table, columns, tplStyle, valueColumns, match.arg(target), styles, appendTo = "rowCallback")
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
hideCols
list(`package:DT` = function (proxy, hide, reset = FALSE) 
{
  invokeRemote(proxy, "hideCols", list(hide, reset))
}, function (proxy, hide, reset = FALSE) 
{
  invokeRemote(proxy, "hideCols", list(hide, reset))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
JS
list(`package:DT` = function (...) 
{
  x <- c(...)
  if (is.null(x)) 
    return()
  if (!is.character(x)) 
    stop("The arguments for JS() must be a character vector")
  x <- paste(x, collapse = "\n")
  structure(x, class = unique(c("JS_EVAL", oldClass(x))))
}, function (...) 
{
  x <- c(...)
  if (is.null(x)) 
    return()
  if (!is.character(x)) 
    stop("The arguments for JS() must be a character vector")
  x <- paste(x, collapse = "\n")
  structure(x, class = unique(c("JS_EVAL", oldClass(x))))
})
c("package:DT", "namespace:htmlwidgets")
c(TRUE, FALSE)
c(FALSE, TRUE)
reloadData
list(`package:DT` = function (proxy, resetPaging = TRUE, clearSelection = c("all", "none", "row", "column", "cell")) 
{
  if ("all" %in% clearSelection) 
    clearSelection = c("row", "column", "cell")
  invokeRemote(proxy, "reloadData", list(resetPaging, clearSelection))
}, function (proxy, resetPaging = TRUE, clearSelection = c("all", "none", "row", "column", "cell")) 
{
  if ("all" %in% clearSelection) 
    clearSelection = c("row", "column", "cell")
  invokeRemote(proxy, "reloadData", list(resetPaging, clearSelection))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
renderDataTable
list(`package:DT` = function (expr, server = TRUE, env = parent.frame(), quoted = FALSE, funcFilter = dataTablesFilter, ...) 
{
  if (!quoted) 
    expr = substitute(expr)
  outputInfoEnv = new.env(parent = emptyenv())
  outputInfoEnv[["outputName"]] = NULL
  outputInfoEnv[["session"]] = NULL
  exprFunc = shiny::exprToFunction(expr, env, quoted = TRUE)
  argFunc = shiny::exprToFunction(list(..., server = server), env, quoted = FALSE)
  widgetFunc = function() {
    opts = options(DT.datatable.shiny = TRUE)
    on.exit(options(opts), add = TRUE)
    instance = exprFunc()
    if (promises::is.promising(instance)) {
      promises::then(instance, processWidget)
    }
    else {
      processWidget(instance)
    }
  }
  processWidget = function(instance) {
    args = argFunc()
    server = args$server
    args$server = NULL
    if (!all(c("datatables", "htmlwidget") %in% class(instance))) {
      instance = do.call(datatable, c(list(instance), args))
    }
    else if (length(args) != 0) {
      warning("renderDataTable ignores ... arguments when expr yields a datatable object; see ?renderDataTable")
    }
    if (server && !is.null(instance[["x"]])) {
      if (!is.null(instance$x$crosstalkOptions$group)) {
        stop("Crosstalk only works with DT client mode: DT::renderDataTable({...}, server=FALSE)")
      }
      origData = instance[["x"]][["data"]]
      instance$x$data = NULL
      options = instance[["x"]][["options"]]
      if (isTRUE(instance[["x"]][["autoHideNavigation"]])) 
        warning("`autoHideNavigation` only works with DT client mode and it will be ignored", immediate. = TRUE, call. = FALSE)
      if (is.character(options[["ajax"]])) {
        options$ajax = list(url = options$ajax)
      }
      if (is.null(options[["ajax"]][["url"]])) {
        url = sessionDataURL(outputInfoEnv[["session"]], origData, outputInfoEnv[["outputName"]], funcFilter)
        options$ajax$url = url
      }
      instance$x$options = fixServerOptions(options)
      if ("Select" %in% as.character(instance$x$extensions)) 
        warning("The Select extension is not able to work with the server-side ", "processing mode properly. It's recommended to use the Select extension ", "only in the client-side processing mode (by setting `server = FALSE` ", "in `DT::renderDT()`) or use DT's own selection implementations (", "see the `selection` argument in ?DT::datatable).", immediate. = TRUE, call. = FALSE)
    }
    instance
  }
  renderFunc = htmlwidgets::shinyRenderWidget(widgetFunc(), dataTableOutput, environment(), FALSE)
  func = if ("cacheHint" %in% names(formals(shiny::markRenderFunction))) {
    cacheHint = if (server) 
      FALSE
    else list(label = "renderDataTable", userExpr = expr)
    shiny::markRenderFunction(uiFunc = dataTableOutput, renderFunc = function(shinysession, name, ...) {
      domain = tempVarsPromiseDomain(outputInfoEnv, outputName = name, session = shinysession)
      removeTimestampFromSnapshot(name)
      promises::with_promise_domain(domain, renderFunc())
    }, cacheHint = cacheHint)
  }
  else {
    shiny::markRenderFunction(uiFunc = dataTableOutput, renderFunc = function(shinysession, name, ...) {
      domain = tempVarsPromiseDomain(outputInfoEnv, outputName = name, session = shinysession)
      removeTimestampFromSnapshot(name)
      promises::with_promise_domain(domain, renderFunc())
    })
  }
  func = shiny::snapshotPreprocessOutput(func, function(value) {
    gsub("\"ajax\"\\s*:\\s*\\{\\s*\"url\"\\s*:\\s*\"[^\"]*\"\\s*,?", "\"ajax\":{", value)
  })
  shiny::registerInputHandler("DT.cellInfo", function(val, ...) {
    opts = options(stringsAsFactors = FALSE)
    on.exit(options(opts), add = TRUE)
    val = lapply(val, as.data.frame)
    do.call(rbind, val)
  }, TRUE)
  func
}, function (expr, options = NULL, searchDelay = 500, callback = "function(oTable) {}", escape = TRUE, env = parent.frame(), quoted = FALSE, outputArgs = list()) 
{
  if (in_devmode()) {
    shinyDeprecated("0.11.1", "shiny::renderDataTable()", "DT::renderDataTable()", details = "See <https://rstudio.github.io/DT/shiny.html> for more information")
  }
  func <- installExprFunction(expr, "func", env, quoted, label = "renderDataTable")
  renderFunc <- function(shinysession, name, ...) {
    if (is.function(options)) 
      options <- options()
    options <- checkDT9(options)
    res <- checkAsIs(options)
    hybrid_chain(func(), function(data) {
      if (length(dim(data)) != 2) 
        return()
      if (is.data.frame(data)) 
        data <- as.data.frame(data)
      action <- shinysession$registerDataObj(name, data, dataTablesJSON)
      colnames <- colnames(data)
      if (is.character(escape)) {
        escape <- stats::setNames(seq_len(ncol(data)), colnames)[escape]
        if (any(is.na(escape))) 
          stop("Some column names in the 'escape' argument not found in data")
      }
      colnames[escape] <- htmlEscape(colnames[escape])
      if (!is.logical(escape)) {
        if (!is.numeric(escape)) 
          stop("'escape' must be TRUE, FALSE, or a numeric vector, or column names")
        escape <- paste(escape, collapse = ",")
      }
      list(colnames = colnames, action = action, options = res$options, evalOptions = if (length(res$eval)) I(res$eval), searchDelay = searchDelay, callback = paste(callback, collapse = "\n"), escape = escape)
    })
  }
  renderFunc <- markRenderFunction(dataTableOutput, renderFunc, outputArgs, cacheHint = FALSE)
  renderFunc <- snapshotPreprocessOutput(renderFunc, function(value) {
    value$action <- NULL
    value
  })
  renderFunc
}, function (expr, server = TRUE, env = parent.frame(), quoted = FALSE, funcFilter = dataTablesFilter, ...) 
{
  if (!quoted) 
    expr = substitute(expr)
  outputInfoEnv = new.env(parent = emptyenv())
  outputInfoEnv[["outputName"]] = NULL
  outputInfoEnv[["session"]] = NULL
  exprFunc = shiny::exprToFunction(expr, env, quoted = TRUE)
  argFunc = shiny::exprToFunction(list(..., server = server), env, quoted = FALSE)
  widgetFunc = function() {
    opts = options(DT.datatable.shiny = TRUE)
    on.exit(options(opts), add = TRUE)
    instance = exprFunc()
    if (promises::is.promising(instance)) {
      promises::then(instance, processWidget)
    }
    else {
      processWidget(instance)
    }
  }
  processWidget = function(instance) {
    args = argFunc()
    server = args$server
    args$server = NULL
    if (!all(c("datatables", "htmlwidget") %in% class(instance))) {
      instance = do.call(datatable, c(list(instance), args))
    }
    else if (length(args) != 0) {
      warning("renderDataTable ignores ... arguments when expr yields a datatable object; see ?renderDataTable")
    }
    if (server && !is.null(instance[["x"]])) {
      if (!is.null(instance$x$crosstalkOptions$group)) {
        stop("Crosstalk only works with DT client mode: DT::renderDataTable({...}, server=FALSE)")
      }
      origData = instance[["x"]][["data"]]
      instance$x$data = NULL
      options = instance[["x"]][["options"]]
      if (isTRUE(instance[["x"]][["autoHideNavigation"]])) 
        warning("`autoHideNavigation` only works with DT client mode and it will be ignored", immediate. = TRUE, call. = FALSE)
      if (is.character(options[["ajax"]])) {
        options$ajax = list(url = options$ajax)
      }
      if (is.null(options[["ajax"]][["url"]])) {
        url = sessionDataURL(outputInfoEnv[["session"]], origData, outputInfoEnv[["outputName"]], funcFilter)
        options$ajax$url = url
      }
      instance$x$options = fixServerOptions(options)
      if ("Select" %in% as.character(instance$x$extensions)) 
        warning("The Select extension is not able to work with the server-side ", "processing mode properly. It's recommended to use the Select extension ", "only in the client-side processing mode (by setting `server = FALSE` ", "in `DT::renderDT()`) or use DT's own selection implementations (", "see the `selection` argument in ?DT::datatable).", immediate. = TRUE, call. = FALSE)
    }
    instance
  }
  renderFunc = htmlwidgets::shinyRenderWidget(widgetFunc(), dataTableOutput, environment(), FALSE)
  func = if ("cacheHint" %in% names(formals(shiny::markRenderFunction))) {
    cacheHint = if (server) 
      FALSE
    else list(label = "renderDataTable", userExpr = expr)
    shiny::markRenderFunction(uiFunc = dataTableOutput, renderFunc = function(shinysession, name, ...) {
      domain = tempVarsPromiseDomain(outputInfoEnv, outputName = name, session = shinysession)
      removeTimestampFromSnapshot(name)
      promises::with_promise_domain(domain, renderFunc())
    }, cacheHint = cacheHint)
  }
  else {
    shiny::markRenderFunction(uiFunc = dataTableOutput, renderFunc = function(shinysession, name, ...) {
      domain = tempVarsPromiseDomain(outputInfoEnv, outputName = name, session = shinysession)
      removeTimestampFromSnapshot(name)
      promises::with_promise_domain(domain, renderFunc())
    })
  }
  func = shiny::snapshotPreprocessOutput(func, function(value) {
    gsub("\"ajax\"\\s*:\\s*\\{\\s*\"url\"\\s*:\\s*\"[^\"]*\"\\s*,?", "\"ajax\":{", value)
  })
  shiny::registerInputHandler("DT.cellInfo", function(val, ...) {
    opts = options(stringsAsFactors = FALSE)
    on.exit(options(opts), add = TRUE)
    val = lapply(val, as.data.frame)
    do.call(rbind, val)
  }, TRUE)
  func
})
c("package:DT", "namespace:shiny", "namespace:DT")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
renderDT
list(`package:DT` = function (expr, server = TRUE, env = parent.frame(), quoted = FALSE, funcFilter = dataTablesFilter, ...) 
{
  if (!quoted) 
    expr = substitute(expr)
  outputInfoEnv = new.env(parent = emptyenv())
  outputInfoEnv[["outputName"]] = NULL
  outputInfoEnv[["session"]] = NULL
  exprFunc = shiny::exprToFunction(expr, env, quoted = TRUE)
  argFunc = shiny::exprToFunction(list(..., server = server), env, quoted = FALSE)
  widgetFunc = function() {
    opts = options(DT.datatable.shiny = TRUE)
    on.exit(options(opts), add = TRUE)
    instance = exprFunc()
    if (promises::is.promising(instance)) {
      promises::then(instance, processWidget)
    }
    else {
      processWidget(instance)
    }
  }
  processWidget = function(instance) {
    args = argFunc()
    server = args$server
    args$server = NULL
    if (!all(c("datatables", "htmlwidget") %in% class(instance))) {
      instance = do.call(datatable, c(list(instance), args))
    }
    else if (length(args) != 0) {
      warning("renderDataTable ignores ... arguments when expr yields a datatable object; see ?renderDataTable")
    }
    if (server && !is.null(instance[["x"]])) {
      if (!is.null(instance$x$crosstalkOptions$group)) {
        stop("Crosstalk only works with DT client mode: DT::renderDataTable({...}, server=FALSE)")
      }
      origData = instance[["x"]][["data"]]
      instance$x$data = NULL
      options = instance[["x"]][["options"]]
      if (isTRUE(instance[["x"]][["autoHideNavigation"]])) 
        warning("`autoHideNavigation` only works with DT client mode and it will be ignored", immediate. = TRUE, call. = FALSE)
      if (is.character(options[["ajax"]])) {
        options$ajax = list(url = options$ajax)
      }
      if (is.null(options[["ajax"]][["url"]])) {
        url = sessionDataURL(outputInfoEnv[["session"]], origData, outputInfoEnv[["outputName"]], funcFilter)
        options$ajax$url = url
      }
      instance$x$options = fixServerOptions(options)
      if ("Select" %in% as.character(instance$x$extensions)) 
        warning("The Select extension is not able to work with the server-side ", "processing mode properly. It's recommended to use the Select extension ", "only in the client-side processing mode (by setting `server = FALSE` ", "in `DT::renderDT()`) or use DT's own selection implementations (", "see the `selection` argument in ?DT::datatable).", immediate. = TRUE, call. = FALSE)
    }
    instance
  }
  renderFunc = htmlwidgets::shinyRenderWidget(widgetFunc(), dataTableOutput, environment(), FALSE)
  func = if ("cacheHint" %in% names(formals(shiny::markRenderFunction))) {
    cacheHint = if (server) 
      FALSE
    else list(label = "renderDataTable", userExpr = expr)
    shiny::markRenderFunction(uiFunc = dataTableOutput, renderFunc = function(shinysession, name, ...) {
      domain = tempVarsPromiseDomain(outputInfoEnv, outputName = name, session = shinysession)
      removeTimestampFromSnapshot(name)
      promises::with_promise_domain(domain, renderFunc())
    }, cacheHint = cacheHint)
  }
  else {
    shiny::markRenderFunction(uiFunc = dataTableOutput, renderFunc = function(shinysession, name, ...) {
      domain = tempVarsPromiseDomain(outputInfoEnv, outputName = name, session = shinysession)
      removeTimestampFromSnapshot(name)
      promises::with_promise_domain(domain, renderFunc())
    })
  }
  func = shiny::snapshotPreprocessOutput(func, function(value) {
    gsub("\"ajax\"\\s*:\\s*\\{\\s*\"url\"\\s*:\\s*\"[^\"]*\"\\s*,?", "\"ajax\":{", value)
  })
  shiny::registerInputHandler("DT.cellInfo", function(val, ...) {
    opts = options(stringsAsFactors = FALSE)
    on.exit(options(opts), add = TRUE)
    val = lapply(val, as.data.frame)
    do.call(rbind, val)
  }, TRUE)
  func
}, function (expr, server = TRUE, env = parent.frame(), quoted = FALSE, funcFilter = dataTablesFilter, ...) 
{
  if (!quoted) 
    expr = substitute(expr)
  outputInfoEnv = new.env(parent = emptyenv())
  outputInfoEnv[["outputName"]] = NULL
  outputInfoEnv[["session"]] = NULL
  exprFunc = shiny::exprToFunction(expr, env, quoted = TRUE)
  argFunc = shiny::exprToFunction(list(..., server = server), env, quoted = FALSE)
  widgetFunc = function() {
    opts = options(DT.datatable.shiny = TRUE)
    on.exit(options(opts), add = TRUE)
    instance = exprFunc()
    if (promises::is.promising(instance)) {
      promises::then(instance, processWidget)
    }
    else {
      processWidget(instance)
    }
  }
  processWidget = function(instance) {
    args = argFunc()
    server = args$server
    args$server = NULL
    if (!all(c("datatables", "htmlwidget") %in% class(instance))) {
      instance = do.call(datatable, c(list(instance), args))
    }
    else if (length(args) != 0) {
      warning("renderDataTable ignores ... arguments when expr yields a datatable object; see ?renderDataTable")
    }
    if (server && !is.null(instance[["x"]])) {
      if (!is.null(instance$x$crosstalkOptions$group)) {
        stop("Crosstalk only works with DT client mode: DT::renderDataTable({...}, server=FALSE)")
      }
      origData = instance[["x"]][["data"]]
      instance$x$data = NULL
      options = instance[["x"]][["options"]]
      if (isTRUE(instance[["x"]][["autoHideNavigation"]])) 
        warning("`autoHideNavigation` only works with DT client mode and it will be ignored", immediate. = TRUE, call. = FALSE)
      if (is.character(options[["ajax"]])) {
        options$ajax = list(url = options$ajax)
      }
      if (is.null(options[["ajax"]][["url"]])) {
        url = sessionDataURL(outputInfoEnv[["session"]], origData, outputInfoEnv[["outputName"]], funcFilter)
        options$ajax$url = url
      }
      instance$x$options = fixServerOptions(options)
      if ("Select" %in% as.character(instance$x$extensions)) 
        warning("The Select extension is not able to work with the server-side ", "processing mode properly. It's recommended to use the Select extension ", "only in the client-side processing mode (by setting `server = FALSE` ", "in `DT::renderDT()`) or use DT's own selection implementations (", "see the `selection` argument in ?DT::datatable).", immediate. = TRUE, call. = FALSE)
    }
    instance
  }
  renderFunc = htmlwidgets::shinyRenderWidget(widgetFunc(), dataTableOutput, environment(), FALSE)
  func = if ("cacheHint" %in% names(formals(shiny::markRenderFunction))) {
    cacheHint = if (server) 
      FALSE
    else list(label = "renderDataTable", userExpr = expr)
    shiny::markRenderFunction(uiFunc = dataTableOutput, renderFunc = function(shinysession, name, ...) {
      domain = tempVarsPromiseDomain(outputInfoEnv, outputName = name, session = shinysession)
      removeTimestampFromSnapshot(name)
      promises::with_promise_domain(domain, renderFunc())
    }, cacheHint = cacheHint)
  }
  else {
    shiny::markRenderFunction(uiFunc = dataTableOutput, renderFunc = function(shinysession, name, ...) {
      domain = tempVarsPromiseDomain(outputInfoEnv, outputName = name, session = shinysession)
      removeTimestampFromSnapshot(name)
      promises::with_promise_domain(domain, renderFunc())
    })
  }
  func = shiny::snapshotPreprocessOutput(func, function(value) {
    gsub("\"ajax\"\\s*:\\s*\\{\\s*\"url\"\\s*:\\s*\"[^\"]*\"\\s*,?", "\"ajax\":{", value)
  })
  shiny::registerInputHandler("DT.cellInfo", function(val, ...) {
    opts = options(stringsAsFactors = FALSE)
    on.exit(options(opts), add = TRUE)
    val = lapply(val, as.data.frame)
    do.call(rbind, val)
  }, TRUE)
  func
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
replaceData
list(`package:DT` = function (proxy, data, ..., resetPaging = TRUE, clearSelection = "all") 
{
  dataTableAjax(proxy$session, data, ..., outputId = proxy$rawId)
  reloadData(proxy, resetPaging, clearSelection)
}, function (proxy, data, ..., resetPaging = TRUE, clearSelection = "all") 
{
  dataTableAjax(proxy$session, data, ..., outputId = proxy$rawId)
  reloadData(proxy, resetPaging, clearSelection)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
saveWidget
list(`package:DT` = function (widget, file, selfcontained = TRUE, libdir = NULL, background = "white", title = class(widget)[[1]], knitrOptions = list()) 
{
  if (grepl("^#", background, perl = TRUE)) {
    bgcol <- grDevices::col2rgb(background, alpha = TRUE)
    background <- sprintf("rgba(%d,%d,%d,%f)", bgcol[1, 1], bgcol[2, 1], bgcol[3, 1], bgcol[4, 1]/255)
  }
  html <- toHTML(widget, standalone = TRUE, knitrOptions = knitrOptions)
  if (is.null(libdir)) {
    libdir <- paste(tools::file_path_sans_ext(basename(file)), "_files", sep = "")
  }
  if (selfcontained) {
    pandoc_save_markdown(html, file = file, libdir = libdir, background = background, title = title)
    if (!pandoc_available()) {
      stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
    }
    pandoc_self_contained_html(file, file)
    unlink(libdir, recursive = TRUE)
  }
  else {
    html <- tagList(tags$head(tags$title(title)), html)
    htmltools::save_html(html, file = file, libdir = libdir, background = background)
  }
  invisible(NULL)
}, function (widget, file, selfcontained = TRUE, libdir = NULL, background = "white", title = class(widget)[[1]], knitrOptions = list()) 
{
  if (grepl("^#", background, perl = TRUE)) {
    bgcol <- grDevices::col2rgb(background, alpha = TRUE)
    background <- sprintf("rgba(%d,%d,%d,%f)", bgcol[1, 1], bgcol[2, 1], bgcol[3, 1], bgcol[4, 1]/255)
  }
  html <- toHTML(widget, standalone = TRUE, knitrOptions = knitrOptions)
  if (is.null(libdir)) {
    libdir <- paste(tools::file_path_sans_ext(basename(file)), "_files", sep = "")
  }
  if (selfcontained) {
    pandoc_save_markdown(html, file = file, libdir = libdir, background = background, title = title)
    if (!pandoc_available()) {
      stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
    }
    pandoc_self_contained_html(file, file)
    unlink(libdir, recursive = TRUE)
  }
  else {
    html <- tagList(tags$head(tags$title(title)), html)
    htmltools::save_html(html, file = file, libdir = libdir, background = background)
  }
  invisible(NULL)
})
c("package:DT", "namespace:htmlwidgets")
c(TRUE, FALSE)
c(FALSE, TRUE)
selectCells
list(`package:DT` = function (proxy, selected, ignore.selectable = FALSE) 
{
  invokeRemote(proxy, "selectCells", list(selected, ignore.selectable))
}, function (proxy, selected, ignore.selectable = FALSE) 
{
  invokeRemote(proxy, "selectCells", list(selected, ignore.selectable))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
selectColumns
list(`package:DT` = function (proxy, selected, ignore.selectable = FALSE) 
{
  invokeRemote(proxy, "selectColumns", list(I_null(as.integer(selected)), ignore.selectable))
}, function (proxy, selected, ignore.selectable = FALSE) 
{
  invokeRemote(proxy, "selectColumns", list(I_null(as.integer(selected)), ignore.selectable))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
selectPage
list(`package:DT` = function (proxy, page) 
{
  invokeRemote(proxy, "selectPage", list(page))
}, function (proxy, page) 
{
  invokeRemote(proxy, "selectPage", list(page))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
selectRows
list(`package:DT` = function (proxy, selected, ignore.selectable = FALSE) 
{
  invokeRemote(proxy, "selectRows", list(I_null(as.integer(selected)), ignore.selectable))
}, function (proxy, selected, ignore.selectable = FALSE) 
{
  invokeRemote(proxy, "selectRows", list(I_null(as.integer(selected)), ignore.selectable))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
showCols
list(`package:DT` = function (proxy, show, reset = FALSE) 
{
  invokeRemote(proxy, "showCols", list(show, reset))
}, function (proxy, show, reset = FALSE) 
{
  invokeRemote(proxy, "showCols", list(show, reset))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
styleColorBar
list(`package:DT` = function (data, color, angle = 90) 
{
  rg = range(data, na.rm = TRUE, finite = TRUE)
  r1 = rg[1]
  r2 = rg[2]
  r = r2 - r1
  JS(sprintf("isNaN(parseFloat(value)) || value <= %f ? '' : 'linear-gradient(%fdeg, transparent ' + Math.max(%f - value, 0)/%f * 100 + '%%, %s ' + Math.max(%f - value, 0)/%f * 100 + '%%)'", r1, angle, r2, r, color, r2, r))
}, function (data, color, angle = 90) 
{
  rg = range(data, na.rm = TRUE, finite = TRUE)
  r1 = rg[1]
  r2 = rg[2]
  r = r2 - r1
  JS(sprintf("isNaN(parseFloat(value)) || value <= %f ? '' : 'linear-gradient(%fdeg, transparent ' + Math.max(%f - value, 0)/%f * 100 + '%%, %s ' + Math.max(%f - value, 0)/%f * 100 + '%%)'", r1, angle, r2, r, color, r2, r))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
styleEqual
list(`package:DT` = function (levels, values, default = NULL) 
{
  n = length(levels)
  if (length(values) == 1) 
    values <- rep(values, n)
  if (n != length(values)) 
    stop("length(levels) must be equal to length(values) when `values` is not a scalar")
  if (!is.null(default) && (!is.character(default) || length(default) != 1)) 
    stop("default must be null or a string")
  if (n == 0) 
    return("''")
  if (is.character(levels) || is.factor(levels)) 
    levels = htmlEscape(levels)
  levels = jsValues(levels)
  values = jsValues(values)
  js = ""
  for (i in seq_len(n)) {
    js = paste0(js, sprintf("value == %s ? %s : ", levels[i], values[i]))
  }
  default = jsValuesHandleNull(default)
  JS(paste0(js, default))
}, function (levels, values, default = NULL) 
{
  n = length(levels)
  if (length(values) == 1) 
    values <- rep(values, n)
  if (n != length(values)) 
    stop("length(levels) must be equal to length(values) when `values` is not a scalar")
  if (!is.null(default) && (!is.character(default) || length(default) != 1)) 
    stop("default must be null or a string")
  if (n == 0) 
    return("''")
  if (is.character(levels) || is.factor(levels)) 
    levels = htmlEscape(levels)
  levels = jsValues(levels)
  values = jsValues(values)
  js = ""
  for (i in seq_len(n)) {
    js = paste0(js, sprintf("value == %s ? %s : ", levels[i], values[i]))
  }
  default = jsValuesHandleNull(default)
  JS(paste0(js, default))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
styleInterval
list(`package:DT` = function (cuts, values) 
{
  n = length(cuts)
  if (n != length(values) - 1) 
    stop("length(cuts) must be equal to length(values) - 1")
  values = jsValues(values)
  if (n == 0) 
    return(values)
  if (!all(cuts == sort(cuts))) 
    stop("'cuts' must be sorted increasingly")
  js = "isNaN(parseFloat(value)) ? '' : "
  cuts = jsValues(cuts)
  for (i in seq_len(n)) {
    js = paste0(js, sprintf("value <= %s ? %s : ", cuts[i], values[i]))
  }
  JS(paste0(js, values[n + 1]))
}, function (cuts, values) 
{
  n = length(cuts)
  if (n != length(values) - 1) 
    stop("length(cuts) must be equal to length(values) - 1")
  values = jsValues(values)
  if (n == 0) 
    return(values)
  if (!all(cuts == sort(cuts))) 
    stop("'cuts' must be sorted increasingly")
  js = "isNaN(parseFloat(value)) ? '' : "
  cuts = jsValues(cuts)
  for (i in seq_len(n)) {
    js = paste0(js, sprintf("value <= %s ? %s : ", cuts[i], values[i]))
  }
  JS(paste0(js, values[n + 1]))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
styleRow
list(`package:DT` = function (rows, values, default = NULL) 
{
  n = length(rows)
  if (length(values) == 1) 
    values <- rep(values, n)
  if (n != length(values)) 
    stop("length(rows) must be equal to length(values) when `values` is not a scalar")
  if (!is.null(default) && (!is.character(default) || length(default) != 1)) 
    stop("default must be null or a string")
  if (n == 0) 
    return("''")
  values = jsValues(values)
  js = ""
  for (i in seq_len(n)) {
    row = as.integer(rows[[i]])
    js = paste0(js, sprintf("$.inArray(dataIndex + 1, [%s]) >= 0 ? %s : ", toString(row), values[i]))
  }
  default = jsValuesHandleNull(default)
  JS(paste0(js, default))
}, function (rows, values, default = NULL) 
{
  n = length(rows)
  if (length(values) == 1) 
    values <- rep(values, n)
  if (n != length(values)) 
    stop("length(rows) must be equal to length(values) when `values` is not a scalar")
  if (!is.null(default) && (!is.character(default) || length(default) != 1)) 
    stop("default must be null or a string")
  if (n == 0) 
    return("''")
  values = jsValues(values)
  js = ""
  for (i in seq_len(n)) {
    row = as.integer(rows[[i]])
    js = paste0(js, sprintf("$.inArray(dataIndex + 1, [%s]) >= 0 ? %s : ", toString(row), values[i]))
  }
  default = jsValuesHandleNull(default)
  JS(paste0(js, default))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
styleValue
list(`package:DT` = function () 
{
  JS("value")
}, function () 
{
  JS("value")
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
tableFooter
list(`package:DT` = function (names, escape = TRUE) 
{
  tableHead(names, "foot", escape)
}, function (names, escape = TRUE) 
{
  tableHead(names, "foot", escape)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
tableHeader
list(`package:DT` = function (names, escape = TRUE) 
{
  tableHead(names, "head", escape)
}, function (names, escape = TRUE) 
{
  tableHead(names, "head", escape)
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateCaption
list(`package:DT` = function (proxy, caption) 
{
  invokeRemote(proxy, "updateCaption", list(captionString(caption)))
}, function (proxy, caption) 
{
  invokeRemote(proxy, "updateCaption", list(captionString(caption)))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateFilters
list(`package:DT` = function (proxy, data) 
{
  filters = unname(columnFilters(data))
  invokeRemote(proxy, "updateFilters", list(filters))
}, function (proxy, data) 
{
  filters = unname(columnFilters(data))
  invokeRemote(proxy, "updateFilters", list(filters))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateSearch
list(`package:DT` = function (proxy, keywords = list(global = NULL, columns = NULL)) 
{
  global = keywords$global
  if (is.null(global)) {
    keywords["global"] = list(NULL)
  }
  else {
    if (!is.character(global) || length(global) != 1) 
      stop("keywords$global must be a character string")
  }
  columns = keywords$columns
  if (is.null(columns)) {
    keywords["columns"] = list(NULL)
  }
  else {
    if (is.character(columns)) {
      if (length(columns) == 0) 
        stop("The length of keywords$columns must be greater than zero if it is a character vector")
    }
    else if (is.list(columns)) {
      if (any(sapply(columns, length) > 1)) 
        stop("keywords$columns should be a list of NULL or character strings")
    }
    else stop("keywords$columns must be either a character vector or a list")
  }
  invokeRemote(proxy, "updateSearch", list(keywords))
}, function (proxy, keywords = list(global = NULL, columns = NULL)) 
{
  global = keywords$global
  if (is.null(global)) {
    keywords["global"] = list(NULL)
  }
  else {
    if (!is.character(global) || length(global) != 1) 
      stop("keywords$global must be a character string")
  }
  columns = keywords$columns
  if (is.null(columns)) {
    keywords["columns"] = list(NULL)
  }
  else {
    if (is.character(columns)) {
      if (length(columns) == 0) 
        stop("The length of keywords$columns must be greater than zero if it is a character vector")
    }
    else if (is.list(columns)) {
      if (any(sapply(columns, length) > 1)) 
        stop("keywords$columns should be a list of NULL or character strings")
    }
    else stop("keywords$columns must be either a character vector or a list")
  }
  invokeRemote(proxy, "updateSearch", list(keywords))
})
c("package:DT", "namespace:DT")
c(TRUE, FALSE)
c(FALSE, TRUE)


####################################################################################################
library(fastmatch)
%fin%
  list(`package:fastmatch` = function (x, table) 
    .Call(C_fmatch, x, table, 0, NULL, FALSE) > 0, function (x, table) 
      .Call(C_fmatch, x, table, 0, NULL, FALSE) > 0)
c("package:fastmatch", "namespace:fastmatch")
c(TRUE, FALSE)
c(FALSE, TRUE)
coalesce
list(`package:fastmatch` = function (x) 
  .Call(C_coalesce, x), `package:dplyr` = function (...) 
  {
    if (missing(..1)) {
      abort("At least one argument must be supplied.")
    }
    values <- list2(...)
    values <- fix_call(vec_cast_common(!!!values))
    values <- fix_call(vec_recycle_common(!!!values))
    x <- values[[1]]
    values <- values[-1]
    if (is.array(x) && length(dim(x)) > 1) {
      abort("Can't coalesce matrices.")
    }
    if (is.data.frame(x)) {
      df_coalesce(x, values)
    }
    else {
      vec_coalesce(x, values)
    }
  }, function (x) 
    .Call(C_coalesce, x), function (...) 
    {
      if (missing(..1)) {
        abort("At least one argument must be supplied.")
      }
      values <- list2(...)
      values <- fix_call(vec_cast_common(!!!values))
      values <- fix_call(vec_recycle_common(!!!values))
      x <- values[[1]]
      values <- values[-1]
      if (is.array(x) && length(dim(x)) > 1) {
        abort("Can't coalesce matrices.")
      }
      if (is.data.frame(x)) {
        df_coalesce(x, values)
      }
      else {
        vec_coalesce(x, values)
      }
    })
c("package:fastmatch", "package:dplyr", "namespace:fastmatch", "namespace:dplyr")
c(TRUE, TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, TRUE)
ctapply
list(`package:fastmatch` = function (X, INDEX, FUN, ..., MERGE = c) 
  .External(C_ctapply, parent.frame(), X, INDEX, FUN, MERGE, ...), function (X, INDEX, FUN, ..., MERGE = c) 
    .External(C_ctapply, parent.frame(), X, INDEX, FUN, MERGE, ...))
c("package:fastmatch", "namespace:fastmatch")
c(TRUE, FALSE)
c(FALSE, TRUE)
fmatch
list(`package:fastmatch` = function (x, table, nomatch = NA, incomparables = NULL) 
  .Call(C_fmatch, x, table, nomatch, incomparables, FALSE), function (x, table, nomatch = NA, incomparables = NULL) 
    .Call(C_fmatch, x, table, nomatch, incomparables, FALSE))
c("package:fastmatch", "namespace:fastmatch")
c(TRUE, FALSE)
c(FALSE, TRUE)
fmatch.hash
list(`package:fastmatch` = function (x, table, nomatch = NA, incomparables = NULL) 
  .Call(C_fmatch, x, table, nomatch, incomparables, TRUE), function (x, table, nomatch = NA, incomparables = NULL) 
    .Call(C_fmatch, x, table, nomatch, incomparables, TRUE))
c("package:fastmatch", "namespace:fastmatch")
c(TRUE, FALSE)
c(FALSE, TRUE)


#################################################################################################
library(fortunes)
fortune
list(`package:fortunes` = function (which = NULL, fortunes.data = NULL, fixed = TRUE, showMatches = FALSE, author = character(), ...) 
{
  if (is.null(fortunes.data)) {
    if (is.null(fortunes.env$fortunes.data)) 
      fortunes.env$fortunes.data <- read.fortunes()
    fortunes.data <- fortunes.env$fortunes.data
  }
  if (is.null(which) && !length(author)) {
    which <- sample.int(nrow(fortunes.data), 1)
  }
  else if (is.character(which) || length(author)) {
    if (length(author)) {
      if (is.null(fd.auth <- fortunes.data[, "author"])) {
        warning("'fortunes.data' does not have an \"author\" column")
      }
      else {
        fortunes.data <- fortunes.data[grep(author, fd.auth, useBytes = TRUE, fixed = fixed), ]
      }
    }
    if (is.character(which)) {
      fort <- apply(fortunes.data, 1, function(x) paste(x, collapse = " "))
      which1 <- grep(which, fort, useBytes = TRUE, fixed = fixed, ...)
      if (length(which1) < 1) 
        which1 <- grep(tolower(which), tolower(fort), useBytes = TRUE, fixed = TRUE, ...)
    }
    else {
      which1 <- seq_len(nrow(fortunes.data))
    }
    if (showMatches) 
      cat("Matching row numbers:", paste(which1, collapse = ", "), "\n")
    which <- which1
    if (length(which) > 1) 
      which <- sample(which, size = 1)
  }
  if (length(which) > 0 && which %in% seq(along = rownames(fortunes.data))) {
    structure(fortunes.data[which, ], class = "fortune")
  }
  else {
    character(0)
  }
}, function (which = NULL, fortunes.data = NULL, fixed = TRUE, showMatches = FALSE, author = character(), ...) 
{
  if (is.null(fortunes.data)) {
    if (is.null(fortunes.env$fortunes.data)) 
      fortunes.env$fortunes.data <- read.fortunes()
    fortunes.data <- fortunes.env$fortunes.data
  }
  if (is.null(which) && !length(author)) {
    which <- sample.int(nrow(fortunes.data), 1)
  }
  else if (is.character(which) || length(author)) {
    if (length(author)) {
      if (is.null(fd.auth <- fortunes.data[, "author"])) {
        warning("'fortunes.data' does not have an \"author\" column")
      }
      else {
        fortunes.data <- fortunes.data[grep(author, fd.auth, useBytes = TRUE, fixed = fixed), ]
      }
    }
    if (is.character(which)) {
      fort <- apply(fortunes.data, 1, function(x) paste(x, collapse = " "))
      which1 <- grep(which, fort, useBytes = TRUE, fixed = fixed, ...)
      if (length(which1) < 1) 
        which1 <- grep(tolower(which), tolower(fort), useBytes = TRUE, fixed = TRUE, ...)
    }
    else {
      which1 <- seq_len(nrow(fortunes.data))
    }
    if (showMatches) 
      cat("Matching row numbers:", paste(which1, collapse = ", "), "\n")
    which <- which1
    if (length(which) > 1) 
      which <- sample(which, size = 1)
  }
  if (length(which) > 0 && which %in% seq(along = rownames(fortunes.data))) {
    structure(fortunes.data[which, ], class = "fortune")
  }
  else {
    character(0)
  }
})
c("package:fortunes", "namespace:fortunes")
c(TRUE, FALSE)
c(FALSE, TRUE)
read.fortunes
list(`package:fortunes` = function (file = NULL) 
{
  if (!is.null(file)) {
    fortunes <- file[file.exists(file)]
  }
  else {
    path <- system.file("fortunes", package = "fortunes")
    datafiles <- list.files(path)
    if (!is.null(file) && file.exists(file.path(path, file))) {
      fortunes <- file.path(path, file)
    }
    else {
      if (length(file) > 0) 
        stop("sorry, ", sQuote(file), " not found")
      file <- datafiles[grep("\\.csv$", datafiles)]
      if (length(file) == 0) 
        stop("sorry, no fortunes data found")
      fortunes <- file.path(path, file)
    }
  }
  rval <- NULL
  for (file in fortunes) {
    rval <- rbind(rval, read.table(file, header = TRUE, sep = ";", quote = "\"", colClasses = "character"))
  }
  rval
}, function (file = NULL) 
{
  if (!is.null(file)) {
    fortunes <- file[file.exists(file)]
  }
  else {
    path <- system.file("fortunes", package = "fortunes")
    datafiles <- list.files(path)
    if (!is.null(file) && file.exists(file.path(path, file))) {
      fortunes <- file.path(path, file)
    }
    else {
      if (length(file) > 0) 
        stop("sorry, ", sQuote(file), " not found")
      file <- datafiles[grep("\\.csv$", datafiles)]
      if (length(file) == 0) 
        stop("sorry, no fortunes data found")
      fortunes <- file.path(path, file)
    }
  }
  rval <- NULL
  for (file in fortunes) {
    rval <- rbind(rval, read.table(file, header = TRUE, sep = ";", quote = "\"", colClasses = "character"))
  }
  rval
})
c("package:fortunes", "namespace:fortunes")
c(TRUE, FALSE)
c(FALSE, TRUE)



###############################################################################################
library(fun)
alzheimer_test
list(`package:fun` = function (char1 = c("9", "O", "M", "I", "F", "D"), char2 = c("6", "C", "N", "T", "E", "O"), nr = 10, nc = 30, seed = NULL, ...) 
{
  if (!interactive()) 
    return()
  cat("This is a REAL neurological test. Sit comfortably and be calm.\n\n")
  mlen = max(length(char1), length(char2), length(nr), length(nc))
  char1 = rep(char1, length = mlen)
  char2 = rep(char2, length = mlen)
  nr = rep(nr, length = mlen)
  nc = rep(nc, length = mlen)
  if (!is.null(seed)) 
    set.seed(seed, ...)
  tm1 = tm2 = ans = ans.u = ans.t = NULL
  for (j in 1:mlen) {
    x = rep(char1[j], nr[j] * nc[j])
    idx = sample(nr[j] * nc[j], 1)
    x[idx] = char2[j]
    mx = matrix(x, nr[j], nc[j])
    cat("\n\nTEST ", j, "\n")
    writeLines(formatUL(c(paste("Find the \"", char2[j], "\" below", sep = ""), "Do not use any cursor help"), offset = 2))
    cat("\n")
    m = menu(c("Ready, Go!", "Let me quit the test!"))
    if (m == 0 | m == 2) {
      j = j - 1
      break
    }
    tmp = Sys.time()
    cat("\n")
    cat(apply(mx, 1, paste, collapse = ""), sep = "\n")
    cat("\nFind it now?\n")
    m = menu(c("Yes! (Input the answer later)", "No... (See the answer later)"))
    tm1 = c(tm1, as.numeric(difftime(Sys.time(), tmp, units = "secs")))
    ans.true = c(ifelse(idx%%nr[j] == 0, nr[j], idx%%nr[j]), ceiling(idx/nr[j]))
    tmp = Sys.time()
    if (m == 0 | m == 2) {
      cat("\nCharacter \"", char2[j], "\" is at [", ifelse(idx%%nr[j] == 0, nr[j], idx%%nr[j]), ", ", idx%/%nr[j] + 1, "].\n\n\n", sep = "")
      ans = c(ans, 3)
      ans.user = c(NA, NA)
    }
    else {
      cat("\nPlease input the Row number and Column number respectively\n  when you find the character:\n")
      ans.user = scan(nmax = 2)
      if (length(ans.user) == 2 & is.numeric(ans.user)) {
        ans = c(ans, as.integer(all(ans.user == ans.true)))
      }
      else ans = c(ans, 2)
      if (ans[length(ans)] != 1) 
        cat("\nWrong answer! :( \nThe correct answer should be: ", ans.true, "\n\n\n")
    }
    ans.u = rbind(ans.u, ans.user)
    ans.t = rbind(ans.t, ans.true)
    tm2 = c(tm2, as.numeric(difftime(Sys.time(), tmp, units = "secs")))
    if (j < mlen) {
      for (i in 1:round(max(nc) * 1.5)) {
        cat("  [Take a rest and continue the next test> ", rep("-", i), ">\r", sep = "")
        flush.console()
        Sys.sleep(0.1)
      }
    }
    else {
      for (i in 1:round(max(nc) * 1.5)) {
        cat("  [All tests are finished; see the results> ", rep("-", i), ">\r", sep = "")
        flush.console()
        Sys.sleep(0.1)
      }
    }
  }
  if (j >= 1) {
    cat("\nThere are", sum(ans == 1), "correct answers in all", j, "tests.\n")
    res = data.frame(char1[1:j], char2[1:j], tm1[1:j], tm2[1:j], ans[1:j], ans.u[1:j, , drop = FALSE], ans.t[1:j, , drop = FALSE])
    return(res)
  }
}, function (char1 = c("9", "O", "M", "I", "F", "D"), char2 = c("6", "C", "N", "T", "E", "O"), nr = 10, nc = 30, seed = NULL, ...) 
{
  if (!interactive()) 
    return()
  cat("This is a REAL neurological test. Sit comfortably and be calm.\n\n")
  mlen = max(length(char1), length(char2), length(nr), length(nc))
  char1 = rep(char1, length = mlen)
  char2 = rep(char2, length = mlen)
  nr = rep(nr, length = mlen)
  nc = rep(nc, length = mlen)
  if (!is.null(seed)) 
    set.seed(seed, ...)
  tm1 = tm2 = ans = ans.u = ans.t = NULL
  for (j in 1:mlen) {
    x = rep(char1[j], nr[j] * nc[j])
    idx = sample(nr[j] * nc[j], 1)
    x[idx] = char2[j]
    mx = matrix(x, nr[j], nc[j])
    cat("\n\nTEST ", j, "\n")
    writeLines(formatUL(c(paste("Find the \"", char2[j], "\" below", sep = ""), "Do not use any cursor help"), offset = 2))
    cat("\n")
    m = menu(c("Ready, Go!", "Let me quit the test!"))
    if (m == 0 | m == 2) {
      j = j - 1
      break
    }
    tmp = Sys.time()
    cat("\n")
    cat(apply(mx, 1, paste, collapse = ""), sep = "\n")
    cat("\nFind it now?\n")
    m = menu(c("Yes! (Input the answer later)", "No... (See the answer later)"))
    tm1 = c(tm1, as.numeric(difftime(Sys.time(), tmp, units = "secs")))
    ans.true = c(ifelse(idx%%nr[j] == 0, nr[j], idx%%nr[j]), ceiling(idx/nr[j]))
    tmp = Sys.time()
    if (m == 0 | m == 2) {
      cat("\nCharacter \"", char2[j], "\" is at [", ifelse(idx%%nr[j] == 0, nr[j], idx%%nr[j]), ", ", idx%/%nr[j] + 1, "].\n\n\n", sep = "")
      ans = c(ans, 3)
      ans.user = c(NA, NA)
    }
    else {
      cat("\nPlease input the Row number and Column number respectively\n  when you find the character:\n")
      ans.user = scan(nmax = 2)
      if (length(ans.user) == 2 & is.numeric(ans.user)) {
        ans = c(ans, as.integer(all(ans.user == ans.true)))
      }
      else ans = c(ans, 2)
      if (ans[length(ans)] != 1) 
        cat("\nWrong answer! :( \nThe correct answer should be: ", ans.true, "\n\n\n")
    }
    ans.u = rbind(ans.u, ans.user)
    ans.t = rbind(ans.t, ans.true)
    tm2 = c(tm2, as.numeric(difftime(Sys.time(), tmp, units = "secs")))
    if (j < mlen) {
      for (i in 1:round(max(nc) * 1.5)) {
        cat("  [Take a rest and continue the next test> ", rep("-", i), ">\r", sep = "")
        flush.console()
        Sys.sleep(0.1)
      }
    }
    else {
      for (i in 1:round(max(nc) * 1.5)) {
        cat("  [All tests are finished; see the results> ", rep("-", i), ">\r", sep = "")
        flush.console()
        Sys.sleep(0.1)
      }
    }
  }
  if (j >= 1) {
    cat("\nThere are", sum(ans == 1), "correct answers in all", j, "tests.\n")
    res = data.frame(char1[1:j], char2[1:j], tm1[1:j], tm2[1:j], ans[1:j], ans.u[1:j, , drop = FALSE], ans.t[1:j, , drop = FALSE])
    return(res)
  }
})
c("package:fun", "namespace:fun")
c(TRUE, FALSE)
c(FALSE, TRUE)
gomoku
list(`package:fun` = function (n = 19) 
{
  if (!interactive()) 
    return()
  par(mar = rep(0, 4))
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "", ylab = "", bty = "o", lab = c(n, n, 1))
  segments(1, 1:n, n, 1:n)
  segments(1:n, 1, 1:n, n)
  points(rep(c(4, 10, 16), 3), rep(c(4, 10, 16), each = 3), pch = 19, cex = 1.2)
  box()
  playedlist <- NULL
  i <- 0
  repeat {
    for (j in 1:2) {
      repeat {
        l <- locator(1)
        l$x <- min(n, max(1, round(l$x)))
        l$y <- min(n, max(1, round(l$y)))
        xy <- paste(l, collapse = ":")
        if (!is.element(xy, playedlist)) 
          break
      }
      playedlist <- c(playedlist, xy)
      points(l, cex = 3, pch = c(19, 21)[j], bg = c("black", "white")[j])
      i <- i + 1
      if (i >= n^2) 
        break
    }
    if (i >= n^2) 
      break
  }
}, function (n = 19) 
{
  if (!interactive()) 
    return()
  par(mar = rep(0, 4))
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "", ylab = "", bty = "o", lab = c(n, n, 1))
  segments(1, 1:n, n, 1:n)
  segments(1:n, 1, 1:n, n)
  points(rep(c(4, 10, 16), 3), rep(c(4, 10, 16), each = 3), pch = 19, cex = 1.2)
  box()
  playedlist <- NULL
  i <- 0
  repeat {
    for (j in 1:2) {
      repeat {
        l <- locator(1)
        l$x <- min(n, max(1, round(l$x)))
        l$y <- min(n, max(1, round(l$y)))
        xy <- paste(l, collapse = ":")
        if (!is.element(xy, playedlist)) 
          break
      }
      playedlist <- c(playedlist, xy)
      points(l, cex = 3, pch = c(19, 21)[j], bg = c("black", "white")[j])
      i <- i + 1
      if (i >= n^2) 
        break
    }
    if (i >= n^2) 
      break
  }
})
c("package:fun", "namespace:fun")
c(TRUE, FALSE)
c(FALSE, TRUE)
htmlspecialchars
list(`package:fun` = function (string) 
{
  x = c("&", "\"", "'", "<", ">")
  subx = c("&amp;", "&quot;", "&#039;", "&lt;", "&gt;")
  for (i in seq_along(x)) {
    string = gsub(x[i], subx[i], string, fixed = TRUE)
  }
  string
}, function (string) 
{
  x = c("&", "\"", "'", "<", ">")
  subx = c("&amp;", "&quot;", "&#039;", "&lt;", "&gt;")
  for (i in seq_along(x)) {
    string = gsub(x[i], subx[i], string, fixed = TRUE)
  }
  string
})
c("package:fun", "namespace:fun")
c(TRUE, FALSE)
c(FALSE, TRUE)
lights_out
list(`package:fun` = function (width = 5, height = 5, steps = 3, cheat = FALSE, col.off = "black", col.on = "white", col.frame = "lightblue", seed = NULL) 
{
  if (!interactive()) 
    return()
  zmat <- mat.ini <- matrix(1, height, width)
  trans <- function(z, x, y) {
    nr <- nrow(z)
    nc <- ncol(z)
    mrow <- intersect(1:nr, (x - 1):(x + 1))
    mcol <- intersect(1:nc, (y - 1):(y + 1))
    z[x, y] <- z[x, y] * (-1)
    z[x, mcol] <- z[x, mcol] * (-1)
    z[mrow, y] <- z[mrow, y] * (-1)
    return(z)
  }
  if (!is.null(seed)) 
    set.seed(seed)
  grid.x <- sample(1:height, steps, replace = TRUE)
  grid.y <- sample(1:width, steps, replace = TRUE)
  if (cheat) {
    print(data.frame(row = grid.x, col = grid.y))
  }
  for (i in 1:steps) {
    zmat <- trans(zmat, grid.x[i], grid.y[i])
  }
  replot <- function(z) {
    nr <- nrow(z)
    nc <- ncol(z)
    xv <- rep(1:nc, rep(nr, nc))
    yv <- nr + +1 - rep(1:nr, nc)
    color <- ifelse(as.vector(z) == 1, col.off, col.on)
    symbols(xv, yv, rectangles = matrix(1, length(xv), 2), inches = FALSE, fg = col.frame, bg = color, add = TRUE)
  }
  par(mar = c(0, 0, 0, 0))
  plot(1, type = "n", asp = 1, xlab = "", ylab = "", xlim = c(0.5, width + 0.5), ylim = c(0.5, height + 0.5), axes = FALSE)
  replot(zmat)
  mousedown <- function(buttons, x, y) {
    nr <- nrow(zmat)
    nc <- ncol(zmat)
    plx <- round(grconvertX(x, "ndc", "user"))
    ply <- round(grconvertY(y, "ndc", "user"))
    if (plx < 1 | plx > nc | ply < 1 | ply > nr) {
      return(zmat)
    }
    zmat.trans <- trans(zmat, nr - ply + 1, plx)
    replot(zmat.trans)
    return(zmat.trans)
  }
  while (1) {
    if (!any(zmat == -1)) {
      cat("You win!")
      break
    }
    zmat <- getGraphicsEvent(prompt = "", onMouseDown = mousedown)
  }
}, function (width = 5, height = 5, steps = 3, cheat = FALSE, col.off = "black", col.on = "white", col.frame = "lightblue", seed = NULL) 
{
  if (!interactive()) 
    return()
  zmat <- mat.ini <- matrix(1, height, width)
  trans <- function(z, x, y) {
    nr <- nrow(z)
    nc <- ncol(z)
    mrow <- intersect(1:nr, (x - 1):(x + 1))
    mcol <- intersect(1:nc, (y - 1):(y + 1))
    z[x, y] <- z[x, y] * (-1)
    z[x, mcol] <- z[x, mcol] * (-1)
    z[mrow, y] <- z[mrow, y] * (-1)
    return(z)
  }
  if (!is.null(seed)) 
    set.seed(seed)
  grid.x <- sample(1:height, steps, replace = TRUE)
  grid.y <- sample(1:width, steps, replace = TRUE)
  if (cheat) {
    print(data.frame(row = grid.x, col = grid.y))
  }
  for (i in 1:steps) {
    zmat <- trans(zmat, grid.x[i], grid.y[i])
  }
  replot <- function(z) {
    nr <- nrow(z)
    nc <- ncol(z)
    xv <- rep(1:nc, rep(nr, nc))
    yv <- nr + +1 - rep(1:nr, nc)
    color <- ifelse(as.vector(z) == 1, col.off, col.on)
    symbols(xv, yv, rectangles = matrix(1, length(xv), 2), inches = FALSE, fg = col.frame, bg = color, add = TRUE)
  }
  par(mar = c(0, 0, 0, 0))
  plot(1, type = "n", asp = 1, xlab = "", ylab = "", xlim = c(0.5, width + 0.5), ylim = c(0.5, height + 0.5), axes = FALSE)
  replot(zmat)
  mousedown <- function(buttons, x, y) {
    nr <- nrow(zmat)
    nc <- ncol(zmat)
    plx <- round(grconvertX(x, "ndc", "user"))
    ply <- round(grconvertY(y, "ndc", "user"))
    if (plx < 1 | plx > nc | ply < 1 | ply > nr) {
      return(zmat)
    }
    zmat.trans <- trans(zmat, nr - ply + 1, plx)
    replot(zmat.trans)
    return(zmat.trans)
  }
  while (1) {
    if (!any(zmat == -1)) {
      cat("You win!")
      break
    }
    zmat <- getGraphicsEvent(prompt = "", onMouseDown = mousedown)
  }
})
c("package:fun", "namespace:fun")
c(TRUE, FALSE)
c(FALSE, TRUE)
mine_sweeper
list(`package:fun` = function (width = 10, height = 10, mines = 20, cheat = FALSE) 
{
  if (!interactive()) 
    return()
  if (mines >= width * height) {
    stop("Are you a terrorist??? Too many mines!")
  }
  if (width <= 0 | height <= 0 | mines <= 0) {
    stop("width, height and mines should be positive!")
  }
  width <- floor(width)
  height <- floor(height)
  mines <- floor(mines)
  m <- rep(0, width * height)
  mat.status <- matrix(m, height, width)
  mine.index <- sample(width * height, mines)
  m[mine.index] <- -10
  mine.mat <- matrix(m, height, width)
  search.mine <- which(mine.mat < 0, arr.ind = TRUE)
  mine.row <- search.mine[, 1]
  mine.col <- search.mine[, 2]
  for (i in 1:mines) {
    mrow <- intersect(1:height, (mine.row[i] - 1):(mine.row[i] + 1))
    mcol <- intersect(1:width, (mine.col[i] - 1):(mine.col[i] + 1))
    mine.mat[mrow, mcol] <- mine.mat[mrow, mcol] + 1
  }
  mine.mat <- ifelse(mine.mat < 0, -1, mine.mat)
  if (cheat) 
    print(mine.mat)
  plot.grid <- function(x, y, w = 1, h = 1, col1 = "#D6E3F0", col2 = "#92B0CA", slices = 10) {
    f <- colorRampPalette(c(col1, col2))
    cols <- f(slices)
    xs <- rep(x, slices)
    ys <- seq(y + 0.5 * h - 0.5 * h/slices, y - 0.5 * h + 0.5 * h/slices, length.out = slices)
    gwidth <- rep(w, slices)
    gheight <- rep(h/slices, slices)
    symbols(xs, ys, rectangles = cbind(gwidth, gheight), fg = cols, bg = cols, inches = FALSE, add = TRUE)
    polygon(x + c(-0.5, -0.5, 0.5, 0.5) * w, y + c(-0.5, 0.5, 0.5, -0.5) * h, border = "#777777")
  }
  par(mar = c(0, 0, 0, 0), bg = "#DDDDDD")
  plot(1, type = "n", asp = 1, xlab = "", ylab = "", xlim = c(0.5, width + 0.5), ylim = c(0.5, height + 0.5), axes = FALSE)
  if (.Device == "X11") {
    fixed <- X11Font("-*-fixed-*-*-*-*-*-*-*-*-*-*-*-*")
    X11Fonts(fixed = fixed)
    par(family = "fixed")
  }
  x.grid <- (width + 1)/2
  y.grid <- 1:height
  for (i in 1:height) plot.grid(x.grid, y.grid[i], w = width, h = 1)
  x0 <- x1 <- seq(1.5, by = 1, length.out = width - 1)
  y0 <- rep(0.5, width - 1)
  y1 <- y0 + height
  segments(x0, y0, x1, y1, col = "#777777")
  col.palette <- c("DarkBlue", "ForestGreen", "brown", "green", "blue", "yellow", "orange", "red")
  text.cex <- function() {
    ps <- par("ps")
    0.6 * min(dev.size(units = "px")/c(width, height))/ps
  }
  plot.num <- function(x, y, num) {
    for (i in 1:length(x)) plot.grid(x[i], y[i], col1 = "#FFFFFF", col2 = "#C8C8C8")
    pnum = num[num > 0]
    px = x[num > 0]
    py = y[num > 0]
    text(px, py, pnum, col = col.palette[pnum], cex = text.cex())
  }
  plot.mine <- function(x, y) {
    for (i in 1:length(x)) plot.grid(x[i], y[i], col1 = "#FFFFFF", col2 = "#C8C8C8")
    symbols(x, y, circles = rep(0.35, length(x)), inches = FALSE, fg = NULL, bg = "black", add = TRUE)
    op = par(lend = 2)
    segments(x - 0.4, y, x + 0.4, y, col = "black", lwd = 5)
    segments(x, y - 0.4, x, y + 0.4, col = "black", lwd = 5)
    d = 0.4/sqrt(2)
    segments(x - d, y - d, x + d, y + d, col = "black", lwd = 5)
    segments(x - d, y + d, x + d, y - d, col = "black", lwd = 5)
  }
  plot.mine.explode <- function(x, y) {
    plot.grid(x, y, col1 = "#FFFFFF", col2 = "#C8C8C8")
    star <- t(matrix(c(0.3, 0.4), 20, length(x)))
    symbols(x, y, stars = star, inches = FALSE, bg = "red", fg = NA, add = TRUE)
    symbols(x, y, stars = 0.7 * star, inches = FALSE, bg = "yellow", fg = NA, add = TRUE)
  }
  plot.flag <- function(x, y) {
    symbols(x + 0.075, y + 0.2, rectangles = matrix(rep(c(0.35, 0.2), rep(length(x), 2)), ncol = 2), inches = FALSE, fg = "red", bg = "red", add = TRUE)
    symbols(x, y - 0.25, rectangles = matrix(rep(c(0.6, 0.1), rep(length(x), 2)), ncol = 2), inches = FALSE, fg = "black", bg = "black", add = TRUE)
    segments(x - 0.1, y + 0.3, x - 0.1, y - 0.2)
  }
  search.zeroes <- function(pos, mat) {
    nr <- nrow(mat)
    nc <- ncol(mat)
    x <- ifelse(pos%%nr == 0, nr, pos%%nr)
    y <- ceiling(pos/nr)
    areas <- c(pos, (x > 1 & y > 1) * (pos - nr - 1), (y > 1) * (pos - nr), (x < nr & y > 1) * (pos - nr + 1), (x > 1) * (pos - 1), (x < nr) * (pos + 1), (x > 1 & y < nc) * (pos + nr - 1), (y < nc) * (pos + nr), (x < nr & y < nc) * (pos + nr + 1))
    areas <- unique(areas[areas != 0])
    zeroes <- intersect(areas, which(mat == 0))
    return(list(zeroes = zeroes, areas = areas))
  }
  mousedown <- function(buttons, x, y) {
    if (length(buttons) == 2) 
      buttons <- 2
    plx <- round(grconvertX(x, "ndc", "user"))
    ply <- round(grconvertY(y, "ndc", "user"))
    ms <- mat.status
    if (plx < 1 || plx > width || ply < 1 || ply > height || buttons == 1) {
      return(ms)
    }
    current.status <- ms[height + 1 - ply, plx]
    current.mat <- mine.mat[height + 1 - ply, plx]
    if (buttons == 0) {
      if (current.status == 0) {
        if (current.mat == -1) {
          plot.mine(mine.col, height + 1 - mine.row)
          plot.mine.explode(plx, ply)
          cat("Game Over!\n")
          return(-1)
        }
        else if (current.mat == 0) {
          pos <- height * plx + 1 - ply
          while (TRUE) {
            temp <- pos
            lst <- search.zeroes(pos, mine.mat)
            pos <- lst$zeroes
            if (length(pos) == length(temp)) {
              areas <- lst$areas
              areas.row <- ifelse(areas%%height == 0, height, areas%%height)
              areas.col <- ceiling(areas/height)
              plot.num(areas.col, height + 1 - areas.row, mine.mat[areas])
              ms[areas] <- 1
              break
            }
          }
          if (sum(ms == 1) == width * height - mines) {
            plot.flag(mine.col, height + 1 - mine.row)
            cat("You win!\n")
            return(1)
          }
          return(ms)
        }
        else {
          plot.num(plx, ply, current.mat)
          if (sum(ms == 1) == width * height - mines - 1) {
            plot.flag(mine.col, height + 1 - mine.row)
            cat("You win!\n")
            return(1)
          }
          ms[height + 1 - ply, plx] <- 1
          return(ms)
        }
      }
      else {
        return(ms)
      }
    }
    if (buttons == 2) {
      if (current.status == 0) {
        ms[height + 1 - ply, plx] <- 2
        plot.flag(plx, ply)
        return(ms)
      }
      else if (current.status == 2) {
        ms[height + 1 - ply, plx] <- 0
        plot.grid(plx, ply)
        return(ms)
      }
      else {
        return(ms)
      }
    }
    return(ms)
  }
  while (TRUE) {
    if (length(mat.status) == 1) 
      break
    mat.status <- getGraphicsEvent(prompt = "", onMouseDown = mousedown)
  }
}, function (width = 10, height = 10, mines = 20, cheat = FALSE) 
{
  if (!interactive()) 
    return()
  if (mines >= width * height) {
    stop("Are you a terrorist??? Too many mines!")
  }
  if (width <= 0 | height <= 0 | mines <= 0) {
    stop("width, height and mines should be positive!")
  }
  width <- floor(width)
  height <- floor(height)
  mines <- floor(mines)
  m <- rep(0, width * height)
  mat.status <- matrix(m, height, width)
  mine.index <- sample(width * height, mines)
  m[mine.index] <- -10
  mine.mat <- matrix(m, height, width)
  search.mine <- which(mine.mat < 0, arr.ind = TRUE)
  mine.row <- search.mine[, 1]
  mine.col <- search.mine[, 2]
  for (i in 1:mines) {
    mrow <- intersect(1:height, (mine.row[i] - 1):(mine.row[i] + 1))
    mcol <- intersect(1:width, (mine.col[i] - 1):(mine.col[i] + 1))
    mine.mat[mrow, mcol] <- mine.mat[mrow, mcol] + 1
  }
  mine.mat <- ifelse(mine.mat < 0, -1, mine.mat)
  if (cheat) 
    print(mine.mat)
  plot.grid <- function(x, y, w = 1, h = 1, col1 = "#D6E3F0", col2 = "#92B0CA", slices = 10) {
    f <- colorRampPalette(c(col1, col2))
    cols <- f(slices)
    xs <- rep(x, slices)
    ys <- seq(y + 0.5 * h - 0.5 * h/slices, y - 0.5 * h + 0.5 * h/slices, length.out = slices)
    gwidth <- rep(w, slices)
    gheight <- rep(h/slices, slices)
    symbols(xs, ys, rectangles = cbind(gwidth, gheight), fg = cols, bg = cols, inches = FALSE, add = TRUE)
    polygon(x + c(-0.5, -0.5, 0.5, 0.5) * w, y + c(-0.5, 0.5, 0.5, -0.5) * h, border = "#777777")
  }
  par(mar = c(0, 0, 0, 0), bg = "#DDDDDD")
  plot(1, type = "n", asp = 1, xlab = "", ylab = "", xlim = c(0.5, width + 0.5), ylim = c(0.5, height + 0.5), axes = FALSE)
  if (.Device == "X11") {
    fixed <- X11Font("-*-fixed-*-*-*-*-*-*-*-*-*-*-*-*")
    X11Fonts(fixed = fixed)
    par(family = "fixed")
  }
  x.grid <- (width + 1)/2
  y.grid <- 1:height
  for (i in 1:height) plot.grid(x.grid, y.grid[i], w = width, h = 1)
  x0 <- x1 <- seq(1.5, by = 1, length.out = width - 1)
  y0 <- rep(0.5, width - 1)
  y1 <- y0 + height
  segments(x0, y0, x1, y1, col = "#777777")
  col.palette <- c("DarkBlue", "ForestGreen", "brown", "green", "blue", "yellow", "orange", "red")
  text.cex <- function() {
    ps <- par("ps")
    0.6 * min(dev.size(units = "px")/c(width, height))/ps
  }
  plot.num <- function(x, y, num) {
    for (i in 1:length(x)) plot.grid(x[i], y[i], col1 = "#FFFFFF", col2 = "#C8C8C8")
    pnum = num[num > 0]
    px = x[num > 0]
    py = y[num > 0]
    text(px, py, pnum, col = col.palette[pnum], cex = text.cex())
  }
  plot.mine <- function(x, y) {
    for (i in 1:length(x)) plot.grid(x[i], y[i], col1 = "#FFFFFF", col2 = "#C8C8C8")
    symbols(x, y, circles = rep(0.35, length(x)), inches = FALSE, fg = NULL, bg = "black", add = TRUE)
    op = par(lend = 2)
    segments(x - 0.4, y, x + 0.4, y, col = "black", lwd = 5)
    segments(x, y - 0.4, x, y + 0.4, col = "black", lwd = 5)
    d = 0.4/sqrt(2)
    segments(x - d, y - d, x + d, y + d, col = "black", lwd = 5)
    segments(x - d, y + d, x + d, y - d, col = "black", lwd = 5)
  }
  plot.mine.explode <- function(x, y) {
    plot.grid(x, y, col1 = "#FFFFFF", col2 = "#C8C8C8")
    star <- t(matrix(c(0.3, 0.4), 20, length(x)))
    symbols(x, y, stars = star, inches = FALSE, bg = "red", fg = NA, add = TRUE)
    symbols(x, y, stars = 0.7 * star, inches = FALSE, bg = "yellow", fg = NA, add = TRUE)
  }
  plot.flag <- function(x, y) {
    symbols(x + 0.075, y + 0.2, rectangles = matrix(rep(c(0.35, 0.2), rep(length(x), 2)), ncol = 2), inches = FALSE, fg = "red", bg = "red", add = TRUE)
    symbols(x, y - 0.25, rectangles = matrix(rep(c(0.6, 0.1), rep(length(x), 2)), ncol = 2), inches = FALSE, fg = "black", bg = "black", add = TRUE)
    segments(x - 0.1, y + 0.3, x - 0.1, y - 0.2)
  }
  search.zeroes <- function(pos, mat) {
    nr <- nrow(mat)
    nc <- ncol(mat)
    x <- ifelse(pos%%nr == 0, nr, pos%%nr)
    y <- ceiling(pos/nr)
    areas <- c(pos, (x > 1 & y > 1) * (pos - nr - 1), (y > 1) * (pos - nr), (x < nr & y > 1) * (pos - nr + 1), (x > 1) * (pos - 1), (x < nr) * (pos + 1), (x > 1 & y < nc) * (pos + nr - 1), (y < nc) * (pos + nr), (x < nr & y < nc) * (pos + nr + 1))
    areas <- unique(areas[areas != 0])
    zeroes <- intersect(areas, which(mat == 0))
    return(list(zeroes = zeroes, areas = areas))
  }
  mousedown <- function(buttons, x, y) {
    if (length(buttons) == 2) 
      buttons <- 2
    plx <- round(grconvertX(x, "ndc", "user"))
    ply <- round(grconvertY(y, "ndc", "user"))
    ms <- mat.status
    if (plx < 1 || plx > width || ply < 1 || ply > height || buttons == 1) {
      return(ms)
    }
    current.status <- ms[height + 1 - ply, plx]
    current.mat <- mine.mat[height + 1 - ply, plx]
    if (buttons == 0) {
      if (current.status == 0) {
        if (current.mat == -1) {
          plot.mine(mine.col, height + 1 - mine.row)
          plot.mine.explode(plx, ply)
          cat("Game Over!\n")
          return(-1)
        }
        else if (current.mat == 0) {
          pos <- height * plx + 1 - ply
          while (TRUE) {
            temp <- pos
            lst <- search.zeroes(pos, mine.mat)
            pos <- lst$zeroes
            if (length(pos) == length(temp)) {
              areas <- lst$areas
              areas.row <- ifelse(areas%%height == 0, height, areas%%height)
              areas.col <- ceiling(areas/height)
              plot.num(areas.col, height + 1 - areas.row, mine.mat[areas])
              ms[areas] <- 1
              break
            }
          }
          if (sum(ms == 1) == width * height - mines) {
            plot.flag(mine.col, height + 1 - mine.row)
            cat("You win!\n")
            return(1)
          }
          return(ms)
        }
        else {
          plot.num(plx, ply, current.mat)
          if (sum(ms == 1) == width * height - mines - 1) {
            plot.flag(mine.col, height + 1 - mine.row)
            cat("You win!\n")
            return(1)
          }
          ms[height + 1 - ply, plx] <- 1
          return(ms)
        }
      }
      else {
        return(ms)
      }
    }
    if (buttons == 2) {
      if (current.status == 0) {
        ms[height + 1 - ply, plx] <- 2
        plot.flag(plx, ply)
        return(ms)
      }
      else if (current.status == 2) {
        ms[height + 1 - ply, plx] <- 0
        plot.grid(plx, ply)
        return(ms)
      }
      else {
        return(ms)
      }
    }
    return(ms)
  }
  while (TRUE) {
    if (length(mat.status) == 1) 
      break
    mat.status <- getGraphicsEvent(prompt = "", onMouseDown = mousedown)
  }
})
c("package:fun", "namespace:fun")
c(TRUE, FALSE)
c(FALSE, TRUE)
random_password
list(`package:fun` = function (length = 12, replace = FALSE, extended = TRUE) 
{
  x = if (extended) 
    intToUtf8(32:126, TRUE)
  else c(0:9, letters, LETTERS)
  paste(sample(x, size = length, replace = replace), collapse = "")
}, function (length = 12, replace = FALSE, extended = TRUE) 
{
  x = if (extended) 
    intToUtf8(32:126, TRUE)
  else c(0:9, letters, LETTERS)
  paste(sample(x, size = length, replace = replace), collapse = "")
})
c("package:fun", "namespace:fun")
c(TRUE, FALSE)
c(FALSE, TRUE)
shutdown
list(`package:fun` = function (wait = 0) 
{
  Sys.sleep(wait)
  if (.Platform$OS.type == "windows") {
    shell("shutdown -s -t 0", wait = FALSE)
  }
  else {
    system("shutdown -h now")
  }
}, function (wait = 0) 
{
  Sys.sleep(wait)
  if (.Platform$OS.type == "windows") {
    shell("shutdown -s -t 0", wait = FALSE)
  }
  else {
    system("shutdown -h now")
  }
})
c("package:fun", "namespace:fun")
c(TRUE, FALSE)
c(FALSE, TRUE)
sliding_puzzle
list(`package:fun` = function (size = c(3, 3), bg = "lightblue", z = NULL) 
{
  if (!interactive()) 
    return()
  if (!is.null(size)) {
    n <- size[1]
    m <- size[2]
  }
  if (length(size) == 1) 
    n <- m <- size
  if (!is.null(z)) {
    n <- dim(z)[1]
    m <- dim(z)[2]
    if (!is.null(size)) 
      warning("Because \"z\" is specified, parameter \"size\" will be ignored.")
  }
  z.right <- matrix(1:(n * m), n, byrow = TRUE)
  z.right[n, m] <- 0
  neg_seq.length <- function(x) {
    len <- 0
    for (i in 1:(length(x) - 1)) {
      tmp <- x[(i + 1):length(x)] - x[i]
      len <- len + sum(tmp < 0)
    }
    return(len)
  }
  len.right <- neg_seq.length(as.vector(z.right)) + n + m
  if (is.null(z)) {
    z <- matrix(sample(0:(n * m - 1)), n)
  }
  else {
    len.z <- neg_seq.length(as.vector(z)) + sum(which(z == 0, arr.ind = TRUE))
    if ((len.right%%2) != (len.z%%2)) 
      stop("The sliding puzzle is insoluble!")
  }
  len.z <- neg_seq.length(as.vector(z)) + sum(which(z == 0, arr.ind = TRUE))
  while ((len.right%%2) != (len.z%%2) | (all(z == z.right))) {
    z <- matrix(sample(0:(n * m - 1)), n)
    len.z <- neg_seq.length(as.vector(z)) + sum(which(z == 0, arr.ind = TRUE))
  }
  z[!z] <- NA
  replot <- function(z) {
    bg <- ifelse(z, bg, "white")
    fg <- ifelse(z, bg, "white")
    par(mar = c(0, 0, 0, 0), bg = "white")
    plot(c(0, m), c(0, n), type = "n", axes = FALSE, asp = 1, xlab = "", ylab = "")
    segments(0:m, rep(0, m + 1), 0:m, rep(n, m + 1), col = "grey", lwd = 2)
    segments(rep(0, n + 1), 0:n, rep(m, n + 1), 0:n, col = "grey", lwd = 2)
    symbols(0.5 + rep(0:(m - 1), each = n), 0.5 + rep((n - 1):0, m), squares = rep(0.9, n * m), add = TRUE, inches = FALSE, fg = as.vector(fg), bg = as.vector(bg))
    text(0.5 + rep(0:(m - 1), each = n), 0.5 + rep((n - 1):0, m), as.vector(z), cex = 3)
  }
  push <- function(x, begin, space) {
    tmp <- x[space]
    if (begin < space) {
      x[(begin + 1):space] <- x[begin:(space - 1)]
      x[begin] <- tmp
    }
    if (begin > space) {
      x[(begin - 1):space] <- x[begin:(space + 1)]
      x[begin] <- tmp
    }
    x
  }
  count <- 0
  mousedown <- function(buttons, x, y) {
    plx <- grconvertX(x, "ndc", "user")
    ply <- grconvertY(y, "ndc", "user")
    m.col <- ceiling(plx)
    m.row <- n - floor(ply)
    ind.NA <- which(is.na(z), arr.ind = TRUE)
    if (!xor(m.row == ind.NA[1], m.col == ind.NA[2])) 
      cat("Warning: Cannot push any number!\n")
    ind.NA <- which(is.na(z), arr.ind = TRUE)
    if (ind.NA[1] == m.row & ind.NA[2] != m.col) {
      z[m.row, ] <<- push(z[m.row, ], m.col, ind.NA[2])
      cat("step = ", count <<- count + 1, "\n")
    }
    if (ind.NA[1] != m.row & ind.NA[2] == m.col) {
      z[, m.col] <<- push(z[, m.col], m.row, ind.NA[1])
      cat("step = ", count <<- count + 1, "\n")
    }
    replot(z)
    flag <- z == z.right
    if (all(flag[!is.na(flag)])) {
      paste("You win! Time:", round((proc.time() - ptm)[3], 2), "seconds.")
    }
  }
  ptm <- proc.time()
  replot(z)
  getGraphicsEvent("Game begin!", onMouseDown = mousedown)
}, function (size = c(3, 3), bg = "lightblue", z = NULL) 
{
  if (!interactive()) 
    return()
  if (!is.null(size)) {
    n <- size[1]
    m <- size[2]
  }
  if (length(size) == 1) 
    n <- m <- size
  if (!is.null(z)) {
    n <- dim(z)[1]
    m <- dim(z)[2]
    if (!is.null(size)) 
      warning("Because \"z\" is specified, parameter \"size\" will be ignored.")
  }
  z.right <- matrix(1:(n * m), n, byrow = TRUE)
  z.right[n, m] <- 0
  neg_seq.length <- function(x) {
    len <- 0
    for (i in 1:(length(x) - 1)) {
      tmp <- x[(i + 1):length(x)] - x[i]
      len <- len + sum(tmp < 0)
    }
    return(len)
  }
  len.right <- neg_seq.length(as.vector(z.right)) + n + m
  if (is.null(z)) {
    z <- matrix(sample(0:(n * m - 1)), n)
  }
  else {
    len.z <- neg_seq.length(as.vector(z)) + sum(which(z == 0, arr.ind = TRUE))
    if ((len.right%%2) != (len.z%%2)) 
      stop("The sliding puzzle is insoluble!")
  }
  len.z <- neg_seq.length(as.vector(z)) + sum(which(z == 0, arr.ind = TRUE))
  while ((len.right%%2) != (len.z%%2) | (all(z == z.right))) {
    z <- matrix(sample(0:(n * m - 1)), n)
    len.z <- neg_seq.length(as.vector(z)) + sum(which(z == 0, arr.ind = TRUE))
  }
  z[!z] <- NA
  replot <- function(z) {
    bg <- ifelse(z, bg, "white")
    fg <- ifelse(z, bg, "white")
    par(mar = c(0, 0, 0, 0), bg = "white")
    plot(c(0, m), c(0, n), type = "n", axes = FALSE, asp = 1, xlab = "", ylab = "")
    segments(0:m, rep(0, m + 1), 0:m, rep(n, m + 1), col = "grey", lwd = 2)
    segments(rep(0, n + 1), 0:n, rep(m, n + 1), 0:n, col = "grey", lwd = 2)
    symbols(0.5 + rep(0:(m - 1), each = n), 0.5 + rep((n - 1):0, m), squares = rep(0.9, n * m), add = TRUE, inches = FALSE, fg = as.vector(fg), bg = as.vector(bg))
    text(0.5 + rep(0:(m - 1), each = n), 0.5 + rep((n - 1):0, m), as.vector(z), cex = 3)
  }
  push <- function(x, begin, space) {
    tmp <- x[space]
    if (begin < space) {
      x[(begin + 1):space] <- x[begin:(space - 1)]
      x[begin] <- tmp
    }
    if (begin > space) {
      x[(begin - 1):space] <- x[begin:(space + 1)]
      x[begin] <- tmp
    }
    x
  }
  count <- 0
  mousedown <- function(buttons, x, y) {
    plx <- grconvertX(x, "ndc", "user")
    ply <- grconvertY(y, "ndc", "user")
    m.col <- ceiling(plx)
    m.row <- n - floor(ply)
    ind.NA <- which(is.na(z), arr.ind = TRUE)
    if (!xor(m.row == ind.NA[1], m.col == ind.NA[2])) 
      cat("Warning: Cannot push any number!\n")
    ind.NA <- which(is.na(z), arr.ind = TRUE)
    if (ind.NA[1] == m.row & ind.NA[2] != m.col) {
      z[m.row, ] <<- push(z[m.row, ], m.col, ind.NA[2])
      cat("step = ", count <<- count + 1, "\n")
    }
    if (ind.NA[1] != m.row & ind.NA[2] == m.col) {
      z[, m.col] <<- push(z[, m.col], m.row, ind.NA[1])
      cat("step = ", count <<- count + 1, "\n")
    }
    replot(z)
    flag <- z == z.right
    if (all(flag[!is.na(flag)])) {
      paste("You win! Time:", round((proc.time() - ptm)[3], 2), "seconds.")
    }
  }
  ptm <- proc.time()
  replot(z)
  getGraphicsEvent("Game begin!", onMouseDown = mousedown)
})
c("package:fun", "namespace:fun")
c(TRUE, FALSE)
c(FALSE, TRUE)
tag_cloud
list(`package:fun` = function (tagData, htmlOutput = "tagCloud.html", SWFPath = "tagcloud.swf", JSPath = "swfobject.js", divId = "tagCloudId", width = 600, height = 400, transparent = FALSE, tcolor = "333333", tcolor2 = "009900", hicolor = "ff0000", distr = "true", tspeed = 100, version = 9, bgcolor = "ffffff", useXML = FALSE, htmlTitle = "Tag Cloud", noFlashJS, target = NULL, scriptOnly = FALSE, encode = FALSE, reserved = FALSE) 
{
  tagData$tag = htmlspecialchars(tagData$tag)
  if (missing(noFlashJS)) 
    noFlashJS = "This will be shown to users with no Flash or Javascript."
  tagXML = sprintf("<tags>%s</tags>", paste(sprintf("<a href='%s' style='%s'%s%s%s>%s</a>", tagData$link, tagData$count, if (is.null(target)) 
    ""
    else sprintf(" target='%s'", target), if (is.null(tagData$color)) 
      ""
    else {
      ifelse(!is.na(tagData$color), sprintf(" color='0x%s'", tagData$color), "")
    }, if (is.null(tagData$hicolor)) 
      ""
    else {
      ifelse(!is.na(tagData$hicolor), sprintf(" hicolor='0x%s'", tagData$hicolor), "")
    }, tagData$tag), collapse = ""))
  if (encode) 
    tagXML = URLencode(tagXML, reserved)
  if (useXML) {
    cat(tagXML, file = file.path(dirname(htmlOutput), "tagcloud.xml"))
  }
  file.copy(system.file("js", c("tagcloud.swf", "swfobject.js"), package = "fun"), dirname(htmlOutput))
  cat(ifelse(scriptOnly, "", sprintf("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n<title>%s</title>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n</head>\n\n<body>", htmlTitle)), sprintf("\t<script type=\"text/javascript\" src=\"%s\"></script>", JSPath), sprintf("\t<div id=\"%s\">%s</div>", divId, noFlashJS), sprintf("\t<script type=\"text/javascript\">\n\t\tvar so = new SWFObject(\"%s\", \"tagcloud\", \"%d\", \"%d\", \"%d\", \"#%s\");\n%s%s\t\tso.addVariable(\"tcolor\", \"0x%s\");\n\t\tso.addVariable(\"tcolor2\", \"0x%s\");\n\t\tso.addVariable(\"hicolor\", \"0x%s\");\n\t\tso.addVariable(\"tspeed\", \"%d\");\n\t\tso.addVariable(\"distr\", \"%s\");\n%s\n\t\tso.write(\"%s\");\n\t\t</script>\n", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       SWFPath, width, height, version, bgcolor, ifelse(transparent, "\t\tso.addParam(\"wmode\", \"transparent\");\n", ""), ifelse(useXML, "", "\t\tso.addVariable(\"mode\", \"tags\");\n"), tcolor, tcolor2, hicolor, tspeed, distr, ifelse(useXML, "\t\tso.addVariable(\"xmlpath\", \"tagcloud.xml\");", sprintf("\t\tso.addVariable(\"tagcloud\", \"%s\");", tagXML)), divId), ifelse(scriptOnly, "", "</body>\n\n</html>"), file = ifelse(scriptOnly, stdout(), htmlOutput), sep = "\n")
  if (!scriptOnly) 
    cat("HTML file created at ", htmlOutput, "\n")
}, function (tagData, htmlOutput = "tagCloud.html", SWFPath = "tagcloud.swf", JSPath = "swfobject.js", divId = "tagCloudId", width = 600, height = 400, transparent = FALSE, tcolor = "333333", tcolor2 = "009900", hicolor = "ff0000", distr = "true", tspeed = 100, version = 9, bgcolor = "ffffff", useXML = FALSE, htmlTitle = "Tag Cloud", noFlashJS, target = NULL, scriptOnly = FALSE, encode = FALSE, reserved = FALSE) 
{
  tagData$tag = htmlspecialchars(tagData$tag)
  if (missing(noFlashJS)) 
    noFlashJS = "This will be shown to users with no Flash or Javascript."
  tagXML = sprintf("<tags>%s</tags>", paste(sprintf("<a href='%s' style='%s'%s%s%s>%s</a>", tagData$link, tagData$count, if (is.null(target)) 
    ""
    else sprintf(" target='%s'", target), if (is.null(tagData$color)) 
      ""
    else {
      ifelse(!is.na(tagData$color), sprintf(" color='0x%s'", tagData$color), "")
    }, if (is.null(tagData$hicolor)) 
      ""
    else {
      ifelse(!is.na(tagData$hicolor), sprintf(" hicolor='0x%s'", tagData$hicolor), "")
    }, tagData$tag), collapse = ""))
  if (encode) 
    tagXML = URLencode(tagXML, reserved)
  if (useXML) {
    cat(tagXML, file = file.path(dirname(htmlOutput), "tagcloud.xml"))
  }
  file.copy(system.file("js", c("tagcloud.swf", "swfobject.js"), package = "fun"), dirname(htmlOutput))
  cat(ifelse(scriptOnly, "", sprintf("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\">\n<head>\n<title>%s</title>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n</head>\n\n<body>", htmlTitle)), sprintf("\t<script type=\"text/javascript\" src=\"%s\"></script>", JSPath), sprintf("\t<div id=\"%s\">%s</div>", divId, noFlashJS), sprintf("\t<script type=\"text/javascript\">\n\t\tvar so = new SWFObject(\"%s\", \"tagcloud\", \"%d\", \"%d\", \"%d\", \"#%s\");\n%s%s\t\tso.addVariable(\"tcolor\", \"0x%s\");\n\t\tso.addVariable(\"tcolor2\", \"0x%s\");\n\t\tso.addVariable(\"hicolor\", \"0x%s\");\n\t\tso.addVariable(\"tspeed\", \"%d\");\n\t\tso.addVariable(\"distr\", \"%s\");\n%s\n\t\tso.write(\"%s\");\n\t\t</script>\n", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       SWFPath, width, height, version, bgcolor, ifelse(transparent, "\t\tso.addParam(\"wmode\", \"transparent\");\n", ""), ifelse(useXML, "", "\t\tso.addVariable(\"mode\", \"tags\");\n"), tcolor, tcolor2, hicolor, tspeed, distr, ifelse(useXML, "\t\tso.addVariable(\"xmlpath\", \"tagcloud.xml\");", sprintf("\t\tso.addVariable(\"tagcloud\", \"%s\");", tagXML)), divId), ifelse(scriptOnly, "", "</body>\n\n</html>"), file = ifelse(scriptOnly, stdout(), htmlOutput), sep = "\n")
  if (!scriptOnly) 
    cat("HTML file created at ", htmlOutput, "\n")
})
c("package:fun", "namespace:fun")
c(TRUE, FALSE)
c(FALSE, TRUE)
tagData
list(`package:fun` = list(tag = c("2D Kernel Density", "algorithm", "Animation", "AniWiki", "Arcing", "arrows()", "beamer", "Bean machine", "Boadilla", "Book", "Boosting", "Brownian Motion", "Bubble Plot", "Campus", "cluster sampling", "Coin", "Conan Doyle", "Conclusion", "Confidence Interval", "Convergence", "Covariance", "Flash", "Gradient Descent", "Graphics", "Highlight", "Hypothesis Test", "image()", "Interaction", "Joke", "Jokes", "LaTeX", "line break", "Map", "Model", "Mouse", "New Year", 
                                  "parse()", "pdf()", "plugin", "Random Number Generator", "R code", "R Language", "R Package", "Statistical Analysis", "web site"), link = c("http://yihui.name/en/tag/2d-kernel-density/", "http://yihui.name/en/tag/algorithm/", "http://yihui.name/en/tag/animation/", "http://yihui.name/en/tag/aniwiki/", "http://yihui.name/en/tag/arcing/", "http://yihui.name/en/tag/arrows/", "http://yihui.name/en/tag/beamer/", "http://yihui.name/en/tag/bean-machine/", "http://yihui.name/en/tag/boadilla/", "http://yihui.name/en/tag/book/", 
                                                                                                                                                                              "http://yihui.name/en/tag/boosting/", "http://yihui.name/en/tag/brownian-motion/", "http://yihui.name/en/tag/bubble-plot/", "http://yihui.name/en/tag/campus/", "http://yihui.name/en/tag/cluster-sampling/", "http://yihui.name/en/tag/coin/", "http://yihui.name/en/tag/conan-doyle/", "http://yihui.name/en/tag/conclusion/", "http://yihui.name/en/tag/confidence-interval/", "http://yihui.name/en/tag/convergence/", "http://yihui.name/en/tag/covariance/", "http://yihui.name/en/tag/flash/", "http://yihui.name/en/tag/gradient-descent/", 
                                                                                                                                                                              "http://yihui.name/en/tag/graphics/", "http://yihui.name/en/tag/highlight/", "http://yihui.name/en/tag/hypothesis-test/", "http://yihui.name/en/tag/image/", "http://yihui.name/en/tag/interaction/", "http://yihui.name/en/tag/joke/", "http://yihui.name/en/tag/jokes/", "http://yihui.name/en/tag/latex/", "http://yihui.name/en/tag/line-break/", "http://yihui.name/en/tag/map/", "http://yihui.name/en/tag/model/", "http://yihui.name/en/tag/mouse/", "http://yihui.name/en/tag/new-year/", "http://yihui.name/en/tag/parse/", 
                                                                                                                                                                              "http://yihui.name/en/tag/pdf/", "http://yihui.name/en/tag/plugin/", "http://yihui.name/en/tag/random-number-generator/", "http://yihui.name/en/tag/r-code/", "http://yihui.name/en/tag/r-language/", "http://yihui.name/en/tag/r-package/", "http://yihui.name/en/tag/statistical-analysis/", "http://yihui.name/en/tag/web-site/"), count = c(4, 4, 44, 8, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 8, 8, 32, 8, 8, 8, 8, 8, 8, 20, 8, 8, 12, 8, 8, 8, 12, 8, 8, 16, 48, 20, 8, 8), color = c("2163bb", "9f0f38", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "800130", "7ce1df", "df4e4a", "31f5fb", "c2acba", "38daed", "286ec0", "25ec53", "e2c060", "ab4709", "74afe4", "b7eb85", "b4bd58", "a5abcf", "eabeca", "331dc7", "6f0a88", "095f1c", "2880e3", "b3f222", "bdf2d4", "6ea48d", "59508e", "7b88bb", "06ac63", "d8dbce", "a68ed9", "2f18ca", "8cb56a", "dfd52a", "3b64b9", "573218", "c6420f", "3d0906", "1387a6", "6b97b1", "33942f", "a6e5cb", "9f6dca", "6b3da1", "2683f4", "2f1125", "6ca810"), hicolor = c("f0763d", "d825b1", "5b8d6a", "6607b0", "f5cdf2", "19d50d", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "b5339e", "8d8cbe", "e19caf", "bb22df", "a1b2cc", "28e0e1", "79f2fa", "6fbb8c", "2800c7", "d4c594", "4a5814", "a26978", "7b0126", "90568e", "9c530e", "1b76bc", "7cd201", "29a816", "6d5de6", "abc9f8", "92614f", "44f1a4", "4dc5a7", "034e9c", "4b3de1", "b328c7", "13e24e", "f98a0e", "cce772", "e3ff6d", "848ea2", "5855be", "77ab96", "1cdf76", "3af938", "606774", "67bbc4", "aa5142", "2374b4")))
package:fun
TRUE
FALSE
tower_of_hanoi
list(`package:fun` = function (n = 7) 
{
  if (!interactive()) 
    return()
  tower <- list(1:n, NULL, NULL)
  color <- rainbow(n)
  par(mfrow = c(1, 3), mar = rep(0, 4), ann = FALSE)
  bgcolor <- par("bg")
  if (bgcolor == "transparent") 
    bgcolor <- "white"
  draw.hanoi <- function() {
    for (i in 1:3) {
      plot(c(-n, n), c(0, n + 2), type = "n", xlab = "", ylab = "", axes = FALSE)
      rect(-n, 0, n, n + 2, border = bgcolor, col = bgcolor)
      if (length(tower[[i]]) > 0) {
        barplot(rev(tower[[i]]), add = TRUE, horiz = TRUE, col = color[rev(tower[[i]])])
        barplot(-rev(tower[[i]]), add = TRUE, horiz = TRUE, col = color[rev(tower[[i]])])
      }
    }
  }
  move.hanoi <- function(k, from, to, via) {
    if (k > 1) {
      move.hanoi(k - 1, from, via, to)
      move.hanoi(1, from, to, via)
      move.hanoi(k - 1, via, to, from)
    }
    else {
      cat("Move ", tower[[from]][1], " from ", LETTERS[from], " to ", LETTERS[to], "\n")
      tower[[to]] <<- c(tower[[from]][1], tower[[to]])
      tower[[from]] <<- tower[[from]][-1]
      draw.hanoi()
      Sys.sleep(0.5)
    }
  }
  draw.hanoi()
  move.hanoi(n, 1, 2, 3)
}, function (n = 7) 
{
  if (!interactive()) 
    return()
  tower <- list(1:n, NULL, NULL)
  color <- rainbow(n)
  par(mfrow = c(1, 3), mar = rep(0, 4), ann = FALSE)
  bgcolor <- par("bg")
  if (bgcolor == "transparent") 
    bgcolor <- "white"
  draw.hanoi <- function() {
    for (i in 1:3) {
      plot(c(-n, n), c(0, n + 2), type = "n", xlab = "", ylab = "", axes = FALSE)
      rect(-n, 0, n, n + 2, border = bgcolor, col = bgcolor)
      if (length(tower[[i]]) > 0) {
        barplot(rev(tower[[i]]), add = TRUE, horiz = TRUE, col = color[rev(tower[[i]])])
        barplot(-rev(tower[[i]]), add = TRUE, horiz = TRUE, col = color[rev(tower[[i]])])
      }
    }
  }
  move.hanoi <- function(k, from, to, via) {
    if (k > 1) {
      move.hanoi(k - 1, from, via, to)
      move.hanoi(1, from, to, via)
      move.hanoi(k - 1, via, to, from)
    }
    else {
      cat("Move ", tower[[from]][1], " from ", LETTERS[from], " to ", LETTERS[to], "\n")
      tower[[to]] <<- c(tower[[from]][1], tower[[to]])
      tower[[from]] <<- tower[[from]][-1]
      draw.hanoi()
      Sys.sleep(0.5)
    }
  }
  draw.hanoi()
  move.hanoi(n, 1, 2, 3)
})
c("package:fun", "namespace:fun")
c(TRUE, FALSE)
c(FALSE, TRUE)


################################################################################################
library(geosphere)
.__T__$:base
list(`package:geosphere` = <environment>, `package:data.table` = <environment>, `package:methods` = <environment>, <environment>, <environment>, <environment>)
c("package:geosphere", "package:data.table", "package:methods", "namespace:Rcpp", "namespace:methods", "namespace:sp")
c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)
.__T__$<-:base
list(`package:geosphere` = <environment>, `package:data.table` = <environment>, `package:methods` = <environment>, <environment>, <environment>)
c("package:geosphere", "package:data.table", "package:methods", "namespace:methods", "namespace:sp")
c(TRUE, TRUE, TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, TRUE, TRUE)
.__T__[:base
       list(`package:geosphere` = <environment>, `package:data.table` = <environment>, `package:methods` = <environment>, <environment>, <environment>)
       c("package:geosphere", "package:data.table", "package:methods", "namespace:methods", "namespace:sp")
       c(TRUE, TRUE, TRUE, FALSE, FALSE)
       c(FALSE, FALSE, TRUE, TRUE, TRUE)
       .__T__[[:base
               list(`package:geosphere` = <environment>, <environment>)
               c("package:geosphere", "namespace:sp")
               c(TRUE, FALSE)
               c(FALSE, TRUE)
               .__T__[[<-:base
                       list(`package:geosphere` = <environment>, `package:data.table` = <environment>, `package:methods` = <environment>, <environment>, <environment>)
                       c("package:geosphere", "package:data.table", "package:methods", "namespace:methods", "namespace:sp")
                       c(TRUE, TRUE, TRUE, FALSE, FALSE)
                       c(FALSE, FALSE, TRUE, TRUE, TRUE)
                       .__T__[<-:base
                              list(`package:geosphere` = <environment>, `package:data.table` = <environment>, `package:methods` = <environment>, <environment>, <environment>)
                              c("package:geosphere", "package:data.table", "package:methods", "namespace:methods", "namespace:sp")
                              c(TRUE, TRUE, TRUE, FALSE, FALSE)
                              c(FALSE, FALSE, TRUE, TRUE, TRUE)
                              .__T__areaPolygon:geosphere
                              list(`package:geosphere` = <environment>, <environment>)
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              .__T__centroid:geosphere
                              list(`package:geosphere` = <environment>, <environment>)
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              .__T__perimeter:geosphere
                              list(`package:geosphere` = <environment>, <environment>)
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              .__T__span:geosphere
                              list(`package:geosphere` = <environment>, <environment>)
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              alongTrackDistance
                              list(`package:geosphere` = function (p1, p2, p3, r = 6378137) 
                              {
                                toRad <- pi/180
                                p1 <- .pointsToMatrix(p1)
                                p2 <- .pointsToMatrix(p2)
                                p3 <- .pointsToMatrix(p3)
                                p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], p3[, 1], p3[, 2], as.vector(r))
                                p1 <- p[, 1:2, drop = FALSE]
                                p2 <- p[, 3:4, drop = FALSE]
                                p3 <- p[, 5:6, drop = FALSE]
                                r = p[, 7]
                                tc <- bearing(p1, p2) * toRad
                                tcp <- bearing(p1, p3) * toRad
                                dp <- distCosine(p1, p3, r = 1)
                                xtr <- asin(sin(tcp - tc) * sin(dp))
                                bearing <- sign(cos(tc - tcp))
                                dist <- bearing * acos(cos(dp)/cos(xtr)) * r
                                if (is.vector(dist)) {
                                  dist <- matrix(dist)
                                }
                                colnames(dist) <- "distance"
                                return(abs(dist))
                              }, function (p1, p2, p3, r = 6378137) 
                              {
                                toRad <- pi/180
                                p1 <- .pointsToMatrix(p1)
                                p2 <- .pointsToMatrix(p2)
                                p3 <- .pointsToMatrix(p3)
                                p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], p3[, 1], p3[, 2], as.vector(r))
                                p1 <- p[, 1:2, drop = FALSE]
                                p2 <- p[, 3:4, drop = FALSE]
                                p3 <- p[, 5:6, drop = FALSE]
                                r = p[, 7]
                                tc <- bearing(p1, p2) * toRad
                                tcp <- bearing(p1, p3) * toRad
                                dp <- distCosine(p1, p3, r = 1)
                                xtr <- asin(sin(tcp - tc) * sin(dp))
                                bearing <- sign(cos(tc - tcp))
                                dist <- bearing * acos(cos(dp)/cos(xtr)) * r
                                if (is.vector(dist)) {
                                  dist <- matrix(dist)
                                }
                                colnames(dist) <- "distance"
                                return(abs(dist))
                              })
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              antipodal
                              list(`package:geosphere` = function (p1, p2, tol = 1e-09) 
                              {
                                p1 <- .pointsToMatrix(p1)
                                p2 <- .pointsToMatrix(p2)
                                p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
                                p[, c(1, 3)] <- .normalizeLonDeg(p[, c(1, 3)])
                                diflon <- abs(p[, 1] - p[, 3])
                                diflat <- abs(p[, 2] + p[, 4])
                                (diflat < tol) & (cos(p[, 2] * pi/180) * abs(diflon%%360 - 180) < tol)
                              }, function (p1, p2, tol = 1e-09) 
                              {
                                p1 <- .pointsToMatrix(p1)
                                p2 <- .pointsToMatrix(p2)
                                p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
                                p[, c(1, 3)] <- .normalizeLonDeg(p[, c(1, 3)])
                                diflon <- abs(p[, 1] - p[, 3])
                                diflat <- abs(p[, 2] + p[, 4])
                                (diflat < tol) & (cos(p[, 2] * pi/180) * abs(diflon%%360 - 180) < tol)
                              })
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              antipode
                              list(`package:geosphere` = function (p) 
                              {
                                p <- .pointsToMatrix(p)
                                p[, 1] <- .normalizeLonDeg(p[, 1] + 180)
                                p[, 2] <- -p[, 2]
                                return(p)
                              }, function (p) 
                              {
                                p <- .pointsToMatrix(p)
                                p[, 1] <- .normalizeLonDeg(p[, 1] + 180)
                                p[, 2] <- -p[, 2]
                                return(p)
                              })
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              areaPolygon
                              list(`package:geosphere` = new("standardGeneric", .Data = function (x, ...) 
                                standardGeneric("areaPolygon"), generic = "areaPolygon", package = "geosphere", group = list(), valueClass = character(0), signature = "x", default = NULL, skeleton = (function (x, ...) 
                                  stop(gettextf("invalid call in method dispatch to '%s' (no default method)", "areaPolygon"), domain = NA))(x, ...)), new("standardGeneric", .Data = function (x, ...) 
                                    standardGeneric("areaPolygon"), generic = "areaPolygon", package = "geosphere", group = list(), valueClass = character(0), signature = "x", default = NULL, skeleton = (function (x, ...) 
                                      stop(gettextf("invalid call in method dispatch to '%s' (no default method)", "areaPolygon"), domain = NA))(x, ...)))
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              bearing
                              list(`package:geosphere` = function (p1, p2, a = 6378137, f = 1/298.257223563) 
                              {
                                p1 <- .pointsToMatrix(p1)
                                if (missing(p2)) {
                                  if (nrow(p1) < 2) {
                                    return(NA)
                                  }
                                  p2 <- p1[-1, , drop = FALSE]
                                  p1 <- p1[-nrow(p1), , drop = FALSE]
                                  addNA <- TRUE
                                }
                                else {
                                  p2 <- .pointsToMatrix(p2)
                                  addNA <- FALSE
                                }
                                p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
                                r <- .Call("_inversegeodesic", as.double(p[, 1]), as.double(p[, 2]), as.double(p[, 3]), as.double(p[, 4]), as.double(a[1]), as.double(f[1]), PACKAGE = "geosphere")
                                r <- matrix(r, ncol = 3, byrow = TRUE)
                                if (addNA) {
                                  c(r[, 2], NA)
                                }
                                else {
                                  r[, 2]
                                }
                              }, function (p1, p2, a = 6378137, f = 1/298.257223563) 
                              {
                                p1 <- .pointsToMatrix(p1)
                                if (missing(p2)) {
                                  if (nrow(p1) < 2) {
                                    return(NA)
                                  }
                                  p2 <- p1[-1, , drop = FALSE]
                                  p1 <- p1[-nrow(p1), , drop = FALSE]
                                  addNA <- TRUE
                                }
                                else {
                                  p2 <- .pointsToMatrix(p2)
                                  addNA <- FALSE
                                }
                                p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
                                r <- .Call("_inversegeodesic", as.double(p[, 1]), as.double(p[, 2]), as.double(p[, 3]), as.double(p[, 4]), as.double(a[1]), as.double(f[1]), PACKAGE = "geosphere")
                                r <- matrix(r, ncol = 3, byrow = TRUE)
                                if (addNA) {
                                  c(r[, 2], NA)
                                }
                                else {
                                  r[, 2]
                                }
                              })
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              bearingRhumb
                              list(`package:geosphere` = function (p1, p2) 
                              {
                                toRad <- pi/180
                                p1 <- .pointsToMatrix(p1) * toRad
                                p2 <- .pointsToMatrix(p2) * toRad
                                p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
                                p1 <- p[, 1:2, drop = FALSE]
                                p2 <- p[, 3:4, drop = FALSE]
                                keep <- !apply(p1 == p2, 1, sum) == 2
                                res <- rep(NA, length = nrow(p1))
                                if (sum(keep) == 0) {
                                  return(res)
                                }
                                lon1 <- p1[keep, 1, drop = FALSE]
                                lat1 <- p1[keep, 2, drop = FALSE]
                                lon2 <- p2[keep, 1, drop = FALSE]
                                lat2 <- p2[keep, 2, drop = FALSE]
                                dLon <- (lon2 - lon1)
                                dPhi <- log(tan(lat2/2 + pi/4)/tan(lat1/2 + pi/4))
                                i <- (abs(dLon) > pi)
                                j <- i & dLon > 0
                                dLon[j] <- -(2 * pi - dLon[j])
                                j <- i & dLon <= 0
                                dLon[j] <- dLon[j] <- (2 * pi + dLon[j])
                                b <- atan2(dLon, dPhi)
                                b <- b/toRad
                                b <- (b + 360)%%360
                                res[keep] = b
                                return(res)
                              }, function (p1, p2) 
                              {
                                toRad <- pi/180
                                p1 <- .pointsToMatrix(p1) * toRad
                                p2 <- .pointsToMatrix(p2) * toRad
                                p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
                                p1 <- p[, 1:2, drop = FALSE]
                                p2 <- p[, 3:4, drop = FALSE]
                                keep <- !apply(p1 == p2, 1, sum) == 2
                                res <- rep(NA, length = nrow(p1))
                                if (sum(keep) == 0) {
                                  return(res)
                                }
                                lon1 <- p1[keep, 1, drop = FALSE]
                                lat1 <- p1[keep, 2, drop = FALSE]
                                lon2 <- p2[keep, 1, drop = FALSE]
                                lat2 <- p2[keep, 2, drop = FALSE]
                                dLon <- (lon2 - lon1)
                                dPhi <- log(tan(lat2/2 + pi/4)/tan(lat1/2 + pi/4))
                                i <- (abs(dLon) > pi)
                                j <- i & dLon > 0
                                dLon[j] <- -(2 * pi - dLon[j])
                                j <- i & dLon <= 0
                                dLon[j] <- dLon[j] <- (2 * pi + dLon[j])
                                b <- atan2(dLon, dPhi)
                                b <- b/toRad
                                b <- (b + 360)%%360
                                res[keep] = b
                                return(res)
                              })
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              centroid
                              list(`package:geosphere` = new("standardGeneric", .Data = function (x, ...) 
                                standardGeneric("centroid"), generic = "centroid", package = "geosphere", group = list(), valueClass = character(0), signature = "x", default = NULL, skeleton = (function (x, ...) 
                                  stop(gettextf("invalid call in method dispatch to '%s' (no default method)", "centroid"), domain = NA))(x, ...)), new("standardGeneric", .Data = function (x, ...) 
                                    standardGeneric("centroid"), generic = "centroid", package = "geosphere", group = list(), valueClass = character(0), signature = "x", default = NULL, skeleton = (function (x, ...) 
                                      stop(gettextf("invalid call in method dispatch to '%s' (no default method)", "centroid"), domain = NA))(x, ...)))
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              daylength
                              list(`package:geosphere` = function (lat, doy) 
                              {
                                if (class(doy) == "Date" | class(doy) == "character") {
                                  doy <- as.character(doy)
                                  doy <- as.numeric(format(as.Date(doy), "%j"))
                                }
                                else {
                                  doy <- (doy - 1)%%365 + 1
                                }
                                lat[lat > 90 | lat < -90] <- NA
                                P <- asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(0.0086 * (doy - 186)))))
                                a <- (sin(0.8333 * pi/180) + sin(lat * pi/180) * sin(P))/(cos(lat * pi/180) * cos(P))
                                a <- pmin(pmax(a, -1), 1)
                                DL <- 24 - (24/pi) * acos(a)
                                return(DL)
                              }, function (lat, doy) 
                              {
                                if (class(doy) == "Date" | class(doy) == "character") {
                                  doy <- as.character(doy)
                                  doy <- as.numeric(format(as.Date(doy), "%j"))
                                }
                                else {
                                  doy <- (doy - 1)%%365 + 1
                                }
                                lat[lat > 90 | lat < -90] <- NA
                                P <- asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(0.0086 * (doy - 186)))))
                                a <- (sin(0.8333 * pi/180) + sin(lat * pi/180) * sin(P))/(cos(lat * pi/180) * cos(P))
                                a <- pmin(pmax(a, -1), 1)
                                DL <- 24 - (24/pi) * acos(a)
                                return(DL)
                              })
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              destPoint
                              list(`package:geosphere` = function (p, b, d, a = 6378137, f = 1/298.257223563, ...) 
                              {
                                r <- list(...)$r
                                if (!is.null(r)) {
                                  return(.old_destPoint(p, b, d, r = r))
                                }
                                b <- as.vector(b)
                                d <- as.vector(d)
                                p <- .pointsToMatrix(p)
                                p <- cbind(p[, 1], p[, 2], b, d)
                                r <- .Call("_geodesic", as.double(p[, 1]), as.double(p[, 2]), as.double(p[, 3]), as.double(p[, 4]), as.double(a), as.double(f), PACKAGE = "geosphere")
                                r <- matrix(r, ncol = 3, byrow = TRUE)
                                colnames(r) <- c("lon", "lat", "finalbearing")
                                return(r[, 1:2, drop = FALSE])
                              }, function (p, b, d, a = 6378137, f = 1/298.257223563, ...) 
                              {
                                r <- list(...)$r
                                if (!is.null(r)) {
                                  return(.old_destPoint(p, b, d, r = r))
                                }
                                b <- as.vector(b)
                                d <- as.vector(d)
                                p <- .pointsToMatrix(p)
                                p <- cbind(p[, 1], p[, 2], b, d)
                                r <- .Call("_geodesic", as.double(p[, 1]), as.double(p[, 2]), as.double(p[, 3]), as.double(p[, 4]), as.double(a), as.double(f), PACKAGE = "geosphere")
                                r <- matrix(r, ncol = 3, byrow = TRUE)
                                colnames(r) <- c("lon", "lat", "finalbearing")
                                return(r[, 1:2, drop = FALSE])
                              })
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              destPointRhumb
                              list(`package:geosphere` = function (p, b, d, r = 6378137) 
                              {
                                toRad <- pi/180
                                b <- as.vector(b)
                                d <- as.vector(d)
                                r <- as.vector(r)
                                p <- .pointsToMatrix(p)
                                p <- cbind(p[, 1], p[, 2], b, d, r)
                                r <- p[, 5]
                                d <- p[, 4]/r
                                b <- p[, 3] * toRad
                                lon1 <- p[, 1] * toRad
                                lat1 <- p[, 2]
                                lat1[lat1 == 90 | lat1 == -90] <- NA
                                lat1 <- lat1 * toRad
                                lat2 <- lat1 + d * cos(b)
                                dLat <- lat2 - lat1
                                dPhi <- log(tan(lat2/2 + pi/4)/tan(lat1/2 + pi/4))
                                i <- abs(dLat) > 1e-10
                                q <- vector(length = length(i))
                                q[i] <- dLat[i]/dPhi[i]
                                q[!i] <- cos(lat1[!i])
                                dLon <- d * sin(b)/q
                                i <- (abs(lat2) > pi/2) & lat2 > 0
                                lat2[i] <- pi - lat2[i]
                                i <- (abs(lat2) > pi/2) & lat2 <= 0
                                lat2[i] <- (pi - lat2[i])
                                lon2 <- (lon1 + dLon + pi)%%(2 * pi) - pi
                                res <- cbind(lon2, lat2)/toRad
                                colnames(res) <- c("lon", "lat")
                                return(res)
                              }, function (p, b, d, r = 6378137) 
                              {
                                toRad <- pi/180
                                b <- as.vector(b)
                                d <- as.vector(d)
                                r <- as.vector(r)
                                p <- .pointsToMatrix(p)
                                p <- cbind(p[, 1], p[, 2], b, d, r)
                                r <- p[, 5]
                                d <- p[, 4]/r
                                b <- p[, 3] * toRad
                                lon1 <- p[, 1] * toRad
                                lat1 <- p[, 2]
                                lat1[lat1 == 90 | lat1 == -90] <- NA
                                lat1 <- lat1 * toRad
                                lat2 <- lat1 + d * cos(b)
                                dLat <- lat2 - lat1
                                dPhi <- log(tan(lat2/2 + pi/4)/tan(lat1/2 + pi/4))
                                i <- abs(dLat) > 1e-10
                                q <- vector(length = length(i))
                                q[i] <- dLat[i]/dPhi[i]
                                q[!i] <- cos(lat1[!i])
                                dLon <- d * sin(b)/q
                                i <- (abs(lat2) > pi/2) & lat2 > 0
                                lat2[i] <- pi - lat2[i]
                                i <- (abs(lat2) > pi/2) & lat2 <= 0
                                lat2[i] <- (pi - lat2[i])
                                lon2 <- (lon1 + dLon + pi)%%(2 * pi) - pi
                                res <- cbind(lon2, lat2)/toRad
                                colnames(res) <- c("lon", "lat")
                                return(res)
                              })
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              dist2gc
                              list(`package:geosphere` = function (p1, p2, p3, r = 6378137, sign = FALSE) 
                              {
                                toRad <- pi/180
                                p1 <- .pointsToMatrix(p1)
                                p2 <- .pointsToMatrix(p2)
                                p3 <- .pointsToMatrix(p3)
                                r <- as.vector(r)
                                p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], p3[, 1], p3[, 2], r)
                                p1 <- p[, 1:2]
                                p2 <- p[, 3:4]
                                p3 <- p[, 5:6]
                                r <- p[, 7]
                                tc <- bearing(p1, p2, a = r, f = 0) * toRad
                                tcp <- bearing(p1, p3, a = r, f = 0) * toRad
                                dp <- distCosine(p1, p3, r = 1)
                                xtr <- (asin(sin(tcp - tc) * sin(dp)) * r)
                                xtr <- as.vector(xtr)
                                if (!sign) 
                                  xtr <- abs(xtr)
                                xtr
                              }, function (p1, p2, p3, r = 6378137, sign = FALSE) 
                              {
                                toRad <- pi/180
                                p1 <- .pointsToMatrix(p1)
                                p2 <- .pointsToMatrix(p2)
                                p3 <- .pointsToMatrix(p3)
                                r <- as.vector(r)
                                p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], p3[, 1], p3[, 2], r)
                                p1 <- p[, 1:2]
                                p2 <- p[, 3:4]
                                p3 <- p[, 5:6]
                                r <- p[, 7]
                                tc <- bearing(p1, p2, a = r, f = 0) * toRad
                                tcp <- bearing(p1, p3, a = r, f = 0) * toRad
                                dp <- distCosine(p1, p3, r = 1)
                                xtr <- (asin(sin(tcp - tc) * sin(dp)) * r)
                                xtr <- as.vector(xtr)
                                if (!sign) 
                                  xtr <- abs(xtr)
                                xtr
                              })
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              dist2Line
                              list(`package:geosphere` = function (p, line, distfun = distGeo) 
                              {
                                p <- .pointsToMatrix(p)
                                if (inherits(line, "SpatialPolygons")) {
                                  line <- methods::as(line, "SpatialLines")
                                }
                                if (inherits(line, "SpatialLines")) {
                                  return(.spDistPoint2Line(p, line, distfun))
                                }
                                line <- .pointsToMatrix(line)
                                line1 <- line[-nrow(line), , drop = FALSE]
                                line2 <- line[-1, , drop = FALSE]
                                seglength <- distfun(line1, line2)
                                res <- matrix(nrow = nrow(p), ncol = 3)
                                colnames(res) <- c("distance", "lon", "lat")
                                for (i in 1:nrow(p)) {
                                  xy <- p[i, ]
                                  crossdist <- abs(dist2gc(line1, line2, xy))
                                  trackdist1 <- alongTrackDistance(line1, line2, xy)
                                  trackdist2 <- alongTrackDistance(line2, line1, xy)
                                  mintrackdist <- pmin(trackdist1, trackdist2)
                                  maxtrackdist <- pmax(trackdist1, trackdist2)
                                  crossdist[maxtrackdist >= seglength] <- NA
                                  nodedist <- distfun(xy, line)
                                  warnopt = getOption("warn")
                                  options(warn = -1)
                                  distmin1 <- min(nodedist, na.rm = TRUE)
                                  distmin2 <- min(crossdist, na.rm = TRUE)
                                  options(warn = warnopt)
                                  if (distmin1 <= distmin2) {
                                    j <- which.min(nodedist)
                                    res[i, ] <- c(distmin1, line[j, ])
                                  }
                                  else {
                                    j <- which.min(crossdist)
                                    if (trackdist1[j] < trackdist2[j]) {
                                      bear <- bearing(line1[j, ], line2[j, ])
                                      pt <- destPoint(line1[j, ], bear, mintrackdist[j])
                                      res[i, ] <- c(crossdist[j], pt)
                                    }
                                    else {
                                      bear <- bearing(line2[j, ], line1[j, ])
                                      pt <- destPoint(line2[j, ], bear, mintrackdist[j])
                                      res[i, ] <- c(crossdist[j], pt)
                                    }
                                  }
                                }
                                return(res)
                              }, function (p, line, distfun = distGeo) 
                              {
                                p <- .pointsToMatrix(p)
                                if (inherits(line, "SpatialPolygons")) {
                                  line <- methods::as(line, "SpatialLines")
                                }
                                if (inherits(line, "SpatialLines")) {
                                  return(.spDistPoint2Line(p, line, distfun))
                                }
                                line <- .pointsToMatrix(line)
                                line1 <- line[-nrow(line), , drop = FALSE]
                                line2 <- line[-1, , drop = FALSE]
                                seglength <- distfun(line1, line2)
                                res <- matrix(nrow = nrow(p), ncol = 3)
                                colnames(res) <- c("distance", "lon", "lat")
                                for (i in 1:nrow(p)) {
                                  xy <- p[i, ]
                                  crossdist <- abs(dist2gc(line1, line2, xy))
                                  trackdist1 <- alongTrackDistance(line1, line2, xy)
                                  trackdist2 <- alongTrackDistance(line2, line1, xy)
                                  mintrackdist <- pmin(trackdist1, trackdist2)
                                  maxtrackdist <- pmax(trackdist1, trackdist2)
                                  crossdist[maxtrackdist >= seglength] <- NA
                                  nodedist <- distfun(xy, line)
                                  warnopt = getOption("warn")
                                  options(warn = -1)
                                  distmin1 <- min(nodedist, na.rm = TRUE)
                                  distmin2 <- min(crossdist, na.rm = TRUE)
                                  options(warn = warnopt)
                                  if (distmin1 <= distmin2) {
                                    j <- which.min(nodedist)
                                    res[i, ] <- c(distmin1, line[j, ])
                                  }
                                  else {
                                    j <- which.min(crossdist)
                                    if (trackdist1[j] < trackdist2[j]) {
                                      bear <- bearing(line1[j, ], line2[j, ])
                                      pt <- destPoint(line1[j, ], bear, mintrackdist[j])
                                      res[i, ] <- c(crossdist[j], pt)
                                    }
                                    else {
                                      bear <- bearing(line2[j, ], line1[j, ])
                                      pt <- destPoint(line2[j, ], bear, mintrackdist[j])
                                      res[i, ] <- c(crossdist[j], pt)
                                    }
                                  }
                                }
                                return(res)
                              })
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              distCosine
                              list(`package:geosphere` = function (p1, p2, r = 6378137) 
                              {
                                p1 <- .pointsToMatrix(p1)
                                if (missing(p2)) {
                                  p2 <- p1[-1, , drop = FALSE]
                                  p1 <- p1[-nrow(p1), , drop = FALSE]
                                }
                                else {
                                  p2 <- .pointsToMatrix(p2)
                                }
                                pp <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(r))
                                i <- rowSums(abs(pp[, 1:2, drop = FALSE] - pp[, 3:4, drop = FALSE]) < .Machine$double.eps^0.5) < 2
                                p <- pp[i, , drop = FALSE]
                                r <- rep(0, nrow(pp))
                                if (nrow(p) > 0) {
                                  p[, 1:4] <- p[, 1:4] * pi/180
                                  r[i] <- acos(sin(p[, 2]) * sin(p[, 4]) + cos(p[, 2]) * cos(p[, 4]) * cos(p[, 1] - p[, 3])) * p[, 5]
                                }
                                r
                              }, function (p1, p2, r = 6378137) 
                              {
                                p1 <- .pointsToMatrix(p1)
                                if (missing(p2)) {
                                  p2 <- p1[-1, , drop = FALSE]
                                  p1 <- p1[-nrow(p1), , drop = FALSE]
                                }
                                else {
                                  p2 <- .pointsToMatrix(p2)
                                }
                                pp <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(r))
                                i <- rowSums(abs(pp[, 1:2, drop = FALSE] - pp[, 3:4, drop = FALSE]) < .Machine$double.eps^0.5) < 2
                                p <- pp[i, , drop = FALSE]
                                r <- rep(0, nrow(pp))
                                if (nrow(p) > 0) {
                                  p[, 1:4] <- p[, 1:4] * pi/180
                                  r[i] <- acos(sin(p[, 2]) * sin(p[, 4]) + cos(p[, 2]) * cos(p[, 4]) * cos(p[, 1] - p[, 3])) * p[, 5]
                                }
                                r
                              })
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              distGeo
                              list(`package:geosphere` = function (p1, p2, a = 6378137, f = 1/298.257223563) 
                              {
                                p1 <- .pointsToMatrix(p1)
                                if (missing(p2)) {
                                  if (nrow(p1) == 1) 
                                    return(0)
                                  if (nrow(p1) == 0) 
                                    return(NULL)
                                  p2 <- p1[-1, , drop = FALSE]
                                  p1 <- p1[-nrow(p1), , drop = FALSE]
                                  addNA <- TRUE
                                }
                                else {
                                  p2 <- .pointsToMatrix(p2)
                                  addNA <- FALSE
                                }
                                p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
                                r <- .Call("_inversegeodesic", as.double(p[, 1]), as.double(p[, 2]), as.double(p[, 3]), as.double(p[, 4]), as.double(a), as.double(f), PACKAGE = "geosphere")
                                r <- matrix(r, ncol = 3, byrow = TRUE)
                                if (addNA) {
                                  c(r[, 1], NA)
                                }
                                else {
                                  r[, 1]
                                }
                              }, function (p1, p2, a = 6378137, f = 1/298.257223563) 
                              {
                                p1 <- .pointsToMatrix(p1)
                                if (missing(p2)) {
                                  if (nrow(p1) == 1) 
                                    return(0)
                                  if (nrow(p1) == 0) 
                                    return(NULL)
                                  p2 <- p1[-1, , drop = FALSE]
                                  p1 <- p1[-nrow(p1), , drop = FALSE]
                                  addNA <- TRUE
                                }
                                else {
                                  p2 <- .pointsToMatrix(p2)
                                  addNA <- FALSE
                                }
                                p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
                                r <- .Call("_inversegeodesic", as.double(p[, 1]), as.double(p[, 2]), as.double(p[, 3]), as.double(p[, 4]), as.double(a), as.double(f), PACKAGE = "geosphere")
                                r <- matrix(r, ncol = 3, byrow = TRUE)
                                if (addNA) {
                                  c(r[, 1], NA)
                                }
                                else {
                                  r[, 1]
                                }
                              })
                              c("package:geosphere", "namespace:geosphere")
                              c(TRUE, FALSE)
                              c(FALSE, TRUE)
                              distHaversine
                              list(`package:geosphere` = function (p1, p2, r = 6378137) 
                              {
                                toRad <- pi/180
                                p1 <- .pointsToMatrix(p1) * toRad
                                if (missing(p2)) {
                                  p2 <- p1[-1, , drop = FALSE]
                                  p1 <- p1[-nrow(p1), , drop = FALSE]
                                }
                                else {
                                  p2 <- .pointsToMatrix(p2) * toRad
                                }
                                p = cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(r))
                                dLat <- p[, 4] - p[, 2]
                                dLon <- p[, 3] - p[, 1]
                                a <- (sin(dLat/2))^2 + cos(p[, 2]) * cos(p[, 4]) * (sin(dLon/2))^2
                                a <- pmin(a, 1)
                                dist <- 2 * atan2(sqrt(a), sqrt(1 - a)) * p[, 5]
                                return(as.vector(dist))
                              }, function (p1, p2, r = 6378137) 
                              {
                                toRad <- pi/180
                                p1 <- .pointsToMatrix(p1) * toRad
                                if (missing(p2)) {
                                  p2 <- p1[-1, , drop = FALSE]
                                  p1 <- p1[-nrow(p1), , drop = FALSE]
                                }
                                else {
                                  p2 <- .pointsToMatrix(p2) * toRad
                                }
                                p = cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(r))
                                dLat <- p[, 4] - p[, 2]
                                dLon <- p[, 3] - p[, 1]
                                a <- (sin(dLat/2))^2 + cos(p[, 2]) * cos(p[, 4]) * (sin(dLon/2))^2
                                a <- pmin(a, 1)
                                dist <- 2 * atan2(sqrt(a), sqrt(1 - a)) * p[, 5]
                                return(as.vector(dist))
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
distm
list(`package:geosphere` = function (x, y, fun = distGeo) 
{
  x <- .pointsToMatrix(x)
  if (missing(y)) {
    return(.distm1(x, fun))
  }
  y <- .pointsToMatrix(y)
  n = nrow(x)
  m = nrow(y)
  dm = matrix(ncol = m, nrow = n)
  for (i in 1:n) {
    dm[i, ] = fun(x[i, ], y)
  }
  return(dm)
}, function (x, y, fun = distGeo) 
{
  x <- .pointsToMatrix(x)
  if (missing(y)) {
    return(.distm1(x, fun))
  }
  y <- .pointsToMatrix(y)
  n = nrow(x)
  m = nrow(y)
  dm = matrix(ncol = m, nrow = n)
  for (i in 1:n) {
    dm[i, ] = fun(x[i, ], y)
  }
  return(dm)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
distMeeus
list(`package:geosphere` = function (p1, p2, a = 6378137, f = 1/298.257223563) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1) * toRad
  if (missing(p2)) {
    p2 <- p1[-1, , drop = FALSE]
    p1 <- p1[-nrow(p1), , drop = FALSE]
  }
  else {
    p2 <- .pointsToMatrix(p2) * toRad
  }
  F <- (p1[, 2] + p2[, 2])/2
  G <- (p1[, 2] - p2[, 2])/2
  L <- (p1[, 1] - p2[, 1])/2
  sinG2 <- (sin(G))^2
  cosG2 <- (cos(G))^2
  sinF2 <- (sin(F))^2
  cosF2 <- (cos(F))^2
  sinL2 <- (sin(L))^2
  cosL2 <- (cos(L))^2
  S <- sinG2 * cosL2 + cosF2 * sinL2
  C <- cosG2 * cosL2 + sinF2 * sinL2
  w <- atan(sqrt(S/C))
  R <- sqrt(S * C)/w
  D <- 2 * w * a
  H1 <- (3 * R - 1)/(2 * C)
  H2 <- (3 * R + 1)/(2 * S)
  dst <- D * (1 + f * H1 * sinF2 * cosG2 - f * H2 * cosF2 * sinG2)
  dst[which(w == 0)] <- 0
  return(as.vector(dst))
}, function (p1, p2, a = 6378137, f = 1/298.257223563) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1) * toRad
  if (missing(p2)) {
    p2 <- p1[-1, , drop = FALSE]
    p1 <- p1[-nrow(p1), , drop = FALSE]
  }
  else {
    p2 <- .pointsToMatrix(p2) * toRad
  }
  F <- (p1[, 2] + p2[, 2])/2
  G <- (p1[, 2] - p2[, 2])/2
  L <- (p1[, 1] - p2[, 1])/2
  sinG2 <- (sin(G))^2
  cosG2 <- (cos(G))^2
  sinF2 <- (sin(F))^2
  cosF2 <- (cos(F))^2
  sinL2 <- (sin(L))^2
  cosL2 <- (cos(L))^2
  S <- sinG2 * cosL2 + cosF2 * sinL2
  C <- cosG2 * cosL2 + sinF2 * sinL2
  w <- atan(sqrt(S/C))
  R <- sqrt(S * C)/w
  D <- 2 * w * a
  H1 <- (3 * R - 1)/(2 * C)
  H2 <- (3 * R + 1)/(2 * S)
  dst <- D * (1 + f * H1 * sinF2 * cosG2 - f * H2 * cosF2 * sinG2)
  dst[which(w == 0)] <- 0
  return(as.vector(dst))
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
distRhumb
list(`package:geosphere` = function (p1, p2, r = 6378137) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1) * toRad
  if (missing(p2)) {
    p2 <- p1[-1, , drop = FALSE]
    p1 <- p1[-nrow(p1), , drop = FALSE]
  }
  else {
    p2 <- .pointsToMatrix(p2) * toRad
  }
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(r))
  lon1 <- p[, 1]
  lat1 <- p[, 2]
  lon2 <- p[, 3]
  lat2 <- p[, 4]
  r <- p[, 5]
  dLat <- (lat2 - lat1)
  dLon <- abs(lon2 - lon1)
  dPhi <- log(tan(lat2/2 + pi/4)/tan(lat1/2 + pi/4))
  i <- abs(dLat) > 1e-10
  q <- vector(length = length(i))
  q[i] <- dLat[i]/dPhi[i]
  q[!i] <- cos(lat1[!i])
  dLon[dLon > pi] <- 2 * pi - dLon[dLon > pi]
  d <- sqrt(dLat * dLat + q * q * dLon * dLon)
  return(d * r)
}, function (p1, p2, r = 6378137) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1) * toRad
  if (missing(p2)) {
    p2 <- p1[-1, , drop = FALSE]
    p1 <- p1[-nrow(p1), , drop = FALSE]
  }
  else {
    p2 <- .pointsToMatrix(p2) * toRad
  }
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(r))
  lon1 <- p[, 1]
  lat1 <- p[, 2]
  lon2 <- p[, 3]
  lat2 <- p[, 4]
  r <- p[, 5]
  dLat <- (lat2 - lat1)
  dLon <- abs(lon2 - lon1)
  dPhi <- log(tan(lat2/2 + pi/4)/tan(lat1/2 + pi/4))
  i <- abs(dLat) > 1e-10
  q <- vector(length = length(i))
  q[i] <- dLat[i]/dPhi[i]
  q[!i] <- cos(lat1[!i])
  dLon[dLon > pi] <- 2 * pi - dLon[dLon > pi]
  d <- sqrt(dLat * dLat + q * q * dLon * dLon)
  return(d * r)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
distVincentyEllipsoid
list(`package:geosphere` = function (p1, p2, a = 6378137, b = 6356752.3142, f = 1/298.257223563) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1) * toRad
  if (missing(p2)) {
    p2 <- p1[-1, , drop = FALSE]
    p1 <- p1[-nrow(p1), , drop = FALSE]
  }
  else {
    p2 <- .pointsToMatrix(p2) * toRad
  }
  p = cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(a), as.vector(b), as.vector(f))
  p1 = p[, 1:2, drop = FALSE]
  p2 = p[, 3:4, drop = FALSE]
  res <- vector(length = nrow(p1))
  for (i in 1:dim(p1)[1]) {
    if (any(is.na(c(p1[i, ], p2[i, ])))) {
      res[i] <- NA
    }
    else if (isTRUE(all.equal(p1[i, ], p2[i, ]))) {
      res[i] <- 0
    }
    else {
      lon1 <- p1[i, 1]
      lat1 <- p1[i, 2]
      lon2 <- p2[i, 1]
      lat2 <- p2[i, 2]
      a = p[i, 5]
      b = p[i, 6]
      f = p[i, 7]
      L <- (lon2 - lon1)
      U1 <- atan((1 - f) * tan(lat1))
      U2 <- atan((1 - f) * tan(lat2))
      sinU1 <- sin(U1)
      cosU1 <- cos(U1)
      sinU2 <- sin(U2)
      cosU2 <- cos(U2)
      lambda <- L
      iterLimit <- 100
      continue <- TRUE
      while (continue) {
        sinLambda <- sin(lambda)
        cosLambda <- cos(lambda)
        sinSigma <- sqrt((cosU2 * sinLambda) * (cosU2 * sinLambda) + (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda) * (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda))
        cosSigma <- sinU1 * sinU2 + cosU1 * cosU2 * cosLambda
        sigma <- atan2(sinSigma, cosSigma)
        sinAlpha <- cosU1 * cosU2 * sinLambda/sinSigma
        cosSqAlpha <- 1 - sinAlpha * sinAlpha
        cos2SigmaM <- cosSigma - 2 * sinU1 * sinU2/cosSqAlpha
        if (is.nan(cos2SigmaM)) 
          cos2SigmaM <- 0
        C <- f/16 * cosSqAlpha * (4 + f * (4 - 3 * cosSqAlpha))
        lambdaP <- lambda
        lambda <- L + (1 - C) * f * sinAlpha * (sigma + C * sinSigma * (cos2SigmaM + C * cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM)))
        iterLimit <- iterLimit - 1
        continue <- (abs(lambda - lambdaP) > 1e-12 && iterLimit > 0)
      }
      if (iterLimit == 0) {
        res[i] <- NA
      }
      else {
        uSq <- cosSqAlpha * (a * a - b * b)/(b * b)
        A <- 1 + uSq/16384 * (4096 + uSq * (-768 + uSq * (320 - 175 * uSq)))
        B <- uSq/1024 * (256 + uSq * (-128 + uSq * (74 - 47 * uSq)))
        deltaSigma <- B * sinSigma * (cos2SigmaM + B/4 * (cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM) - B/6 * cos2SigmaM * (-3 + 4 * sinSigma * sinSigma) * (-3 + 4 * cos2SigmaM * cos2SigmaM)))
        res[i] <- b * A * (sigma - deltaSigma)
      }
    }
  }
  return(as.vector(res))
}, function (p1, p2, a = 6378137, b = 6356752.3142, f = 1/298.257223563) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1) * toRad
  if (missing(p2)) {
    p2 <- p1[-1, , drop = FALSE]
    p1 <- p1[-nrow(p1), , drop = FALSE]
  }
  else {
    p2 <- .pointsToMatrix(p2) * toRad
  }
  p = cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(a), as.vector(b), as.vector(f))
  p1 = p[, 1:2, drop = FALSE]
  p2 = p[, 3:4, drop = FALSE]
  res <- vector(length = nrow(p1))
  for (i in 1:dim(p1)[1]) {
    if (any(is.na(c(p1[i, ], p2[i, ])))) {
      res[i] <- NA
    }
    else if (isTRUE(all.equal(p1[i, ], p2[i, ]))) {
      res[i] <- 0
    }
    else {
      lon1 <- p1[i, 1]
      lat1 <- p1[i, 2]
      lon2 <- p2[i, 1]
      lat2 <- p2[i, 2]
      a = p[i, 5]
      b = p[i, 6]
      f = p[i, 7]
      L <- (lon2 - lon1)
      U1 <- atan((1 - f) * tan(lat1))
      U2 <- atan((1 - f) * tan(lat2))
      sinU1 <- sin(U1)
      cosU1 <- cos(U1)
      sinU2 <- sin(U2)
      cosU2 <- cos(U2)
      lambda <- L
      iterLimit <- 100
      continue <- TRUE
      while (continue) {
        sinLambda <- sin(lambda)
        cosLambda <- cos(lambda)
        sinSigma <- sqrt((cosU2 * sinLambda) * (cosU2 * sinLambda) + (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda) * (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda))
        cosSigma <- sinU1 * sinU2 + cosU1 * cosU2 * cosLambda
        sigma <- atan2(sinSigma, cosSigma)
        sinAlpha <- cosU1 * cosU2 * sinLambda/sinSigma
        cosSqAlpha <- 1 - sinAlpha * sinAlpha
        cos2SigmaM <- cosSigma - 2 * sinU1 * sinU2/cosSqAlpha
        if (is.nan(cos2SigmaM)) 
          cos2SigmaM <- 0
        C <- f/16 * cosSqAlpha * (4 + f * (4 - 3 * cosSqAlpha))
        lambdaP <- lambda
        lambda <- L + (1 - C) * f * sinAlpha * (sigma + C * sinSigma * (cos2SigmaM + C * cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM)))
        iterLimit <- iterLimit - 1
        continue <- (abs(lambda - lambdaP) > 1e-12 && iterLimit > 0)
      }
      if (iterLimit == 0) {
        res[i] <- NA
      }
      else {
        uSq <- cosSqAlpha * (a * a - b * b)/(b * b)
        A <- 1 + uSq/16384 * (4096 + uSq * (-768 + uSq * (320 - 175 * uSq)))
        B <- uSq/1024 * (256 + uSq * (-128 + uSq * (74 - 47 * uSq)))
        deltaSigma <- B * sinSigma * (cos2SigmaM + B/4 * (cosSigma * (-1 + 2 * cos2SigmaM * cos2SigmaM) - B/6 * cos2SigmaM * (-3 + 4 * sinSigma * sinSigma) * (-3 + 4 * cos2SigmaM * cos2SigmaM)))
        res[i] <- b * A * (sigma - deltaSigma)
      }
    }
  }
  return(as.vector(res))
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
distVincentySphere
list(`package:geosphere` = function (p1, p2, r = 6378137) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1) * toRad
  if (missing(p2)) {
    p2 <- p1[-1, , drop = FALSE]
    p1 <- p1[-nrow(p1), , drop = FALSE]
  }
  else {
    p2 <- .pointsToMatrix(p2) * toRad
  }
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(r))
  lon1 <- p[, 1]
  lat1 <- p[, 2]
  lon2 <- p[, 3]
  lat2 <- p[, 4]
  x <- sqrt((cos(lat2) * sin(lon1 - lon2))^2 + (cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(lon1 - lon2))^2)
  y <- sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon1 - lon2)
  dist <- p[, 5] * atan2(x, y)
  return(as.vector(dist))
}, function (p1, p2, r = 6378137) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1) * toRad
  if (missing(p2)) {
    p2 <- p1[-1, , drop = FALSE]
    p1 <- p1[-nrow(p1), , drop = FALSE]
  }
  else {
    p2 <- .pointsToMatrix(p2) * toRad
  }
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(r))
  lon1 <- p[, 1]
  lat1 <- p[, 2]
  lon2 <- p[, 3]
  lat2 <- p[, 4]
  x <- sqrt((cos(lat2) * sin(lon1 - lon2))^2 + (cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(lon1 - lon2))^2)
  y <- sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon1 - lon2)
  dist <- p[, 5] * atan2(x, y)
  return(as.vector(dist))
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
finalBearing
list(`package:geosphere` = function (p1, p2, a = 6378137, f = 1/298.257223563, sphere = FALSE) 
{
  if (sphere) {
    return(.old_bearing(p2, p1))
  }
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
  r <- .Call("_inversegeodesic", as.double(p[, 1]), as.double(p[, 2]), as.double(p[, 3]), as.double(p[, 4]), as.double(a[1]), as.double(f[1]), PACKAGE = "geosphere")
  r <- matrix(r, ncol = 3, byrow = TRUE)
  return(r[, 3])
}, function (p1, p2, a = 6378137, f = 1/298.257223563, sphere = FALSE) 
{
  if (sphere) {
    return(.old_bearing(p2, p1))
  }
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
  r <- .Call("_inversegeodesic", as.double(p[, 1]), as.double(p[, 2]), as.double(p[, 3]), as.double(p[, 4]), as.double(a[1]), as.double(f[1]), PACKAGE = "geosphere")
  r <- matrix(r, ncol = 3, byrow = TRUE)
  return(r[, 3])
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
gcIntermediate
list(`package:geosphere` = function (p1, p2, n = 50, breakAtDateLine = FALSE, addStartEnd = FALSE, sp = FALSE, sepNA = FALSE) 
{
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(n))
  res <- list()
  for (i in 1:nrow(p)) {
    x <- .interm(p[i, 1:2, drop = FALSE], p[i, 3:4, drop = FALSE], p[i, 5])
    if (addStartEnd) {
      x <- rbind(p[i, 1:2, drop = FALSE], x, p[i, 3:4, drop = FALSE])
    }
    if (breakAtDateLine) {
      res[[i]] <- .breakAtDateLine(x)
    }
    else {
      res[[i]] <- x
    }
  }
  if (sp) {
    for (i in 1:length(res)) {
      if (!is.list(res[[i]])) {
        res[[i]] <- Lines(list(Line(res[[i]])), ID = as.character(i))
      }
      else {
        res[[i]] <- Lines(list(Line(res[[i]][[1]]), Line(res[[i]][[2]])), ID = as.character(i))
      }
    }
    res <- SpatialLines(res, CRS("+proj=longlat +ellps=WGS84"))
  }
  else if (nrow(p) == 1) {
    res <- res[[1]]
  }
  else if (sepNA) {
    r <- res[[1]]
    for (i in 2:length(res)) {
      r <- rbind(r, c(NA, NA), res[[i]])
    }
    return(r)
  }
  return(res)
}, function (p1, p2, n = 50, breakAtDateLine = FALSE, addStartEnd = FALSE, sp = FALSE, sepNA = FALSE) 
{
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(n))
  res <- list()
  for (i in 1:nrow(p)) {
    x <- .interm(p[i, 1:2, drop = FALSE], p[i, 3:4, drop = FALSE], p[i, 5])
    if (addStartEnd) {
      x <- rbind(p[i, 1:2, drop = FALSE], x, p[i, 3:4, drop = FALSE])
    }
    if (breakAtDateLine) {
      res[[i]] <- .breakAtDateLine(x)
    }
    else {
      res[[i]] <- x
    }
  }
  if (sp) {
    for (i in 1:length(res)) {
      if (!is.list(res[[i]])) {
        res[[i]] <- Lines(list(Line(res[[i]])), ID = as.character(i))
      }
      else {
        res[[i]] <- Lines(list(Line(res[[i]][[1]]), Line(res[[i]][[2]])), ID = as.character(i))
      }
    }
    res <- SpatialLines(res, CRS("+proj=longlat +ellps=WGS84"))
  }
  else if (nrow(p) == 1) {
    res <- res[[1]]
  }
  else if (sepNA) {
    r <- res[[1]]
    for (i in 2:length(res)) {
      r <- rbind(r, c(NA, NA), res[[i]])
    }
    return(r)
  }
  return(res)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
gcIntersect
list(`package:geosphere` = function (p1, p2, p3, p4) 
{
  einv <- function(e) {
    lat <- atan2(e[, 3], sqrt(e[, 1]^2 + e[, 2]^2))
    lon <- atan2(-e[, 2], e[, 1])
    return(cbind(lon, lat))
  }
  eXe5 <- function(lon1, lat1, lon2, lat2) {
    ex <- sin(lat1 - lat2) * sin((lon1 + lon2)/2) * cos((lon1 - lon2)/2) - sin(lat1 + lat2) * cos((lon1 + lon2)/2) * sin((lon1 - lon2)/2)
    ey <- sin(lat1 - lat2) * cos((lon1 + lon2)/2) * cos((lon1 - lon2)/2) + sin(lat1 + lat2) * sin((lon1 + lon2)/2) * sin((lon1 - lon2)/2)
    ez <- cos(lat1) * cos(lat2) * sin(lon1 - lon2)
    return(cbind(ex, ey, ez))
  }
  eXe3 <- function(e1, e2) {
    x <- e1[, 2] * e2[, 3] - e2[, 2] * e1[, 3]
    y <- e1[, 3] * e2[, 1] - e2[, 3] * e1[, 1]
    z <- e1[, 1] * e2[, 2] - e1[, 2] * e2[, 1]
    return(cbind(x, y, z))
  }
  eSQRT <- function(e) {
    return(sqrt(e[, 1]^2 + e[, 2]^2 + e[, 3]^2))
  }
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p3 <- .pointsToMatrix(p3)
  p4 <- .pointsToMatrix(p4)
  p1 <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
  p3 <- cbind(p3[, 1], p3[, 2], p4[, 1], p4[, 2])
  p <- cbind(p1[, 1], p1[, 2], p1[, 3], p1[, 4], p3[, 1], p3[, 2], p3[, 3], p3[, 4])
  p1 <- p[, 1:2, drop = FALSE]
  p2 <- p[, 3:4, drop = FALSE]
  p3 <- p[, 5:6, drop = FALSE]
  p4 <- p[, 7:8, drop = FALSE]
  res <- matrix(NA, nrow = nrow(p1), ncol = 4)
  colnames(res) <- c("lon1", "lat1", "lon2", "lat2")
  keep <- !antipodal(p1, p2) | antipodal(p3, p4)
  keep <- keep & !apply(p1 == p2, 1, sum) == 2
  if (sum(keep) == 0) {
    return(res)
  }
  toRad <- pi/180
  p1 <- p1[keep, , drop = FALSE] * toRad
  p2 <- p2[keep, , drop = FALSE] * toRad
  p3 <- p3[keep, , drop = FALSE] * toRad
  p4 <- p4[keep, , drop = FALSE] * toRad
  e1Xe2 <- eXe5(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
  e3Xe4 <- eXe5(p3[, 1], p3[, 2], p4[, 1], p4[, 2])
  ea <- e1Xe2/eSQRT(e1Xe2)
  eb <- e3Xe4/eSQRT(e3Xe4)
  eaXeb <- eXe3(ea, eb)
  ll <- einv(eaXeb)
  ll2 <- cbind(ll[, 1] + pi, -ll[, 2])
  pts <- cbind(ll, ll2)
  pts[, 1] <- .normalizeLonRad(pts[, 1])
  pts[, 3] <- .normalizeLonRad(pts[, 3])
  res[keep, ] <- pts/toRad
  return(res)
}, function (p1, p2, p3, p4) 
{
  einv <- function(e) {
    lat <- atan2(e[, 3], sqrt(e[, 1]^2 + e[, 2]^2))
    lon <- atan2(-e[, 2], e[, 1])
    return(cbind(lon, lat))
  }
  eXe5 <- function(lon1, lat1, lon2, lat2) {
    ex <- sin(lat1 - lat2) * sin((lon1 + lon2)/2) * cos((lon1 - lon2)/2) - sin(lat1 + lat2) * cos((lon1 + lon2)/2) * sin((lon1 - lon2)/2)
    ey <- sin(lat1 - lat2) * cos((lon1 + lon2)/2) * cos((lon1 - lon2)/2) + sin(lat1 + lat2) * sin((lon1 + lon2)/2) * sin((lon1 - lon2)/2)
    ez <- cos(lat1) * cos(lat2) * sin(lon1 - lon2)
    return(cbind(ex, ey, ez))
  }
  eXe3 <- function(e1, e2) {
    x <- e1[, 2] * e2[, 3] - e2[, 2] * e1[, 3]
    y <- e1[, 3] * e2[, 1] - e2[, 3] * e1[, 1]
    z <- e1[, 1] * e2[, 2] - e1[, 2] * e2[, 1]
    return(cbind(x, y, z))
  }
  eSQRT <- function(e) {
    return(sqrt(e[, 1]^2 + e[, 2]^2 + e[, 3]^2))
  }
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p3 <- .pointsToMatrix(p3)
  p4 <- .pointsToMatrix(p4)
  p1 <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
  p3 <- cbind(p3[, 1], p3[, 2], p4[, 1], p4[, 2])
  p <- cbind(p1[, 1], p1[, 2], p1[, 3], p1[, 4], p3[, 1], p3[, 2], p3[, 3], p3[, 4])
  p1 <- p[, 1:2, drop = FALSE]
  p2 <- p[, 3:4, drop = FALSE]
  p3 <- p[, 5:6, drop = FALSE]
  p4 <- p[, 7:8, drop = FALSE]
  res <- matrix(NA, nrow = nrow(p1), ncol = 4)
  colnames(res) <- c("lon1", "lat1", "lon2", "lat2")
  keep <- !antipodal(p1, p2) | antipodal(p3, p4)
  keep <- keep & !apply(p1 == p2, 1, sum) == 2
  if (sum(keep) == 0) {
    return(res)
  }
  toRad <- pi/180
  p1 <- p1[keep, , drop = FALSE] * toRad
  p2 <- p2[keep, , drop = FALSE] * toRad
  p3 <- p3[keep, , drop = FALSE] * toRad
  p4 <- p4[keep, , drop = FALSE] * toRad
  e1Xe2 <- eXe5(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
  e3Xe4 <- eXe5(p3[, 1], p3[, 2], p4[, 1], p4[, 2])
  ea <- e1Xe2/eSQRT(e1Xe2)
  eb <- e3Xe4/eSQRT(e3Xe4)
  eaXeb <- eXe3(ea, eb)
  ll <- einv(eaXeb)
  ll2 <- cbind(ll[, 1] + pi, -ll[, 2])
  pts <- cbind(ll, ll2)
  pts[, 1] <- .normalizeLonRad(pts[, 1])
  pts[, 3] <- .normalizeLonRad(pts[, 3])
  res[keep, ] <- pts/toRad
  return(res)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
gcIntersectBearing
list(`package:geosphere` = function (p1, brng1, p2, brng2) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1) * toRad
  p2 <- .pointsToMatrix(p2) * toRad
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(brng1), as.vector(brng2))
  lon1 <- p[, 1]
  lat1 <- p[, 2]
  lon2 <- p[, 3]
  lat2 <- p[, 4]
  lat1[lat1 == 90 | lat1 == -90] <- NA
  lat2[lat2 == 90 | lat2 == -90] <- NA
  brng13 <- p[, 5] * toRad
  brng23 <- p[, 6] * toRad
  dLat <- lat2 - lat1
  dLon <- lon2 - lon1
  dist12 <- 2 * asin(sqrt(sin(dLat/2) * sin(dLat/2) + cos(lat1) * cos(lat2) * sin(dLon/2) * sin(dLon/2)))
  lat3 <- lon3 <- vector(length = length(nrow(lon1)))
  i <- rep(TRUE, length(dist12))
  i[dist12 == 0] <- FALSE
  brngA <- acos((sin(lat2) - sin(lat1) * cos(dist12))/(sin(dist12) * cos(lat1)))
  brngA[is.na(brngA)] <- 0
  brngB <- acos((sin(lat1) - sin(lat2) * cos(dist12))/(sin(dist12) * cos(lat2)))
  g <- (sin(lon2 - lon1) > 0)
  brng12 <- vector(length = length(g))
  brng21 <- brng12
  brng12[g] <- brngA[g]
  brng21[g] <- 2 * pi - brngB[g]
  brng12[!g] <- 2 * pi - brngA[!g]
  brng21[!g] <- brngB[!g]
  alpha1 <- (brng13 - brng12 + pi)%%(2 * pi) - pi
  alpha2 <- (brng21 - brng23 + pi)%%(2 * pi) - pi
  g <- sin(alpha1) == 0 & sin(alpha2) == 0
  h <- (sin(alpha1) * sin(alpha2)) < 0
  i <- !(g | h) & i
  lon3[!i] <- lat3[!i] <- NA
  alpha1 <- abs(alpha1)
  alpha2 <- abs(alpha2)
  alpha3 <- acos(-cos(alpha1) * cos(alpha2) + sin(alpha1) * sin(alpha2) * cos(dist12))
  dist13 <- atan2(sin(dist12) * sin(alpha1) * sin(alpha2), cos(alpha2) + cos(alpha1) * cos(alpha3))
  lat3[i] <- asin(sin(lat1[i]) * cos(dist13[i]) + cos(lat1[i]) * sin(dist13[i]) * cos(brng13[i]))
  dLon13 <- atan2(sin(brng13) * sin(dist13) * cos(lat1), cos(dist13) - sin(lat1) * sin(lat3))
  lon3[i] <- lon1[i] + dLon13[i]
  lon3 <- (lon3 + pi)%%(2 * pi) - pi
  int <- cbind(lon3, lat3)/toRad
  colnames(int) <- c("lon", "lat")
  int <- cbind(int, antipode(int))
  rownames(int) <- NULL
  return(int)
}, function (p1, brng1, p2, brng2) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1) * toRad
  p2 <- .pointsToMatrix(p2) * toRad
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(brng1), as.vector(brng2))
  lon1 <- p[, 1]
  lat1 <- p[, 2]
  lon2 <- p[, 3]
  lat2 <- p[, 4]
  lat1[lat1 == 90 | lat1 == -90] <- NA
  lat2[lat2 == 90 | lat2 == -90] <- NA
  brng13 <- p[, 5] * toRad
  brng23 <- p[, 6] * toRad
  dLat <- lat2 - lat1
  dLon <- lon2 - lon1
  dist12 <- 2 * asin(sqrt(sin(dLat/2) * sin(dLat/2) + cos(lat1) * cos(lat2) * sin(dLon/2) * sin(dLon/2)))
  lat3 <- lon3 <- vector(length = length(nrow(lon1)))
  i <- rep(TRUE, length(dist12))
  i[dist12 == 0] <- FALSE
  brngA <- acos((sin(lat2) - sin(lat1) * cos(dist12))/(sin(dist12) * cos(lat1)))
  brngA[is.na(brngA)] <- 0
  brngB <- acos((sin(lat1) - sin(lat2) * cos(dist12))/(sin(dist12) * cos(lat2)))
  g <- (sin(lon2 - lon1) > 0)
  brng12 <- vector(length = length(g))
  brng21 <- brng12
  brng12[g] <- brngA[g]
  brng21[g] <- 2 * pi - brngB[g]
  brng12[!g] <- 2 * pi - brngA[!g]
  brng21[!g] <- brngB[!g]
  alpha1 <- (brng13 - brng12 + pi)%%(2 * pi) - pi
  alpha2 <- (brng21 - brng23 + pi)%%(2 * pi) - pi
  g <- sin(alpha1) == 0 & sin(alpha2) == 0
  h <- (sin(alpha1) * sin(alpha2)) < 0
  i <- !(g | h) & i
  lon3[!i] <- lat3[!i] <- NA
  alpha1 <- abs(alpha1)
  alpha2 <- abs(alpha2)
  alpha3 <- acos(-cos(alpha1) * cos(alpha2) + sin(alpha1) * sin(alpha2) * cos(dist12))
  dist13 <- atan2(sin(dist12) * sin(alpha1) * sin(alpha2), cos(alpha2) + cos(alpha1) * cos(alpha3))
  lat3[i] <- asin(sin(lat1[i]) * cos(dist13[i]) + cos(lat1[i]) * sin(dist13[i]) * cos(brng13[i]))
  dLon13 <- atan2(sin(brng13) * sin(dist13) * cos(lat1), cos(dist13) - sin(lat1) * sin(lat3))
  lon3[i] <- lon1[i] + dLon13[i]
  lon3 <- (lon3 + pi)%%(2 * pi) - pi
  int <- cbind(lon3, lat3)/toRad
  colnames(int) <- c("lon", "lat")
  int <- cbind(int, antipode(int))
  rownames(int) <- NULL
  return(int)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
gcLat
list(`package:geosphere` = function (p1, p2, lon) 
{
  toRad <- pi/180
  d <- distCosine(p1, p2)
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(lon))
  p1 <- p[, 1:2, drop = FALSE]
  p2 <- p[, 3:4, drop = FALSE]
  lon <- p[, 5]
  res <- rep(NA, nrow(p))
  notanti <- !antipodal(p1, p2)
  lon1 <- p1[, 1] * toRad
  lat1 <- p1[, 2] * toRad
  lon2 <- p2[, 1] * toRad
  lat2 <- p2[, 2] * toRad
  lon <- lon * toRad
  notmeridians <- !sin(lon1 - lon2) == 0
  keep <- notanti & notmeridians
  if (sum(keep) == 0) {
    return(res)
  }
  lon1 <- lon1[keep]
  lat1 <- lat1[keep]
  lon2 <- lon2[keep]
  lat2 <- lat2[keep]
  lon <- lon[keep]
  res[keep] <- atan((sin(lat1) * cos(lat2) * sin(lon - lon2) - sin(lat2) * cos(lat1) * sin(lon - lon1))/(cos(lat1) * cos(lat2) * sin(lon1 - lon2)))
  return(res/toRad)
}, function (p1, p2, lon) 
{
  toRad <- pi/180
  d <- distCosine(p1, p2)
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(lon))
  p1 <- p[, 1:2, drop = FALSE]
  p2 <- p[, 3:4, drop = FALSE]
  lon <- p[, 5]
  res <- rep(NA, nrow(p))
  notanti <- !antipodal(p1, p2)
  lon1 <- p1[, 1] * toRad
  lat1 <- p1[, 2] * toRad
  lon2 <- p2[, 1] * toRad
  lat2 <- p2[, 2] * toRad
  lon <- lon * toRad
  notmeridians <- !sin(lon1 - lon2) == 0
  keep <- notanti & notmeridians
  if (sum(keep) == 0) {
    return(res)
  }
  lon1 <- lon1[keep]
  lat1 <- lat1[keep]
  lon2 <- lon2[keep]
  lat2 <- lat2[keep]
  lon <- lon[keep]
  res[keep] <- atan((sin(lat1) * cos(lat2) * sin(lon - lon2) - sin(lat2) * cos(lat1) * sin(lon - lon1))/(cos(lat1) * cos(lat2) * sin(lon1 - lon2)))
  return(res/toRad)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
gcLon
list(`package:geosphere` = function (p1, p2, lat) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], lat)
  p1 <- p[, 1:2, drop = FALSE]
  p2 <- p[, 3:4, drop = FALSE]
  lat <- p[, 5]
  res <- matrix(NA, nrow = nrow(p1), ncol = 2)
  colnames(res) <- c("lon1", "lon2")
  anti <- !antipodal(p1, p2)
  if (sum(anti) == 0) {
    return(res)
  }
  p1 <- p1[anti, , drop = FALSE] * toRad
  p2 <- p2[anti, , drop = FALSE] * toRad
  lon1 <- p1[, 1] * -1
  lat1 <- p1[, 2]
  lon2 <- p2[, 1] * -1
  lat2 <- p2[, 2]
  lat3 <- lat * toRad
  l12 <- lon1 - lon2
  A <- sin(lat1) * cos(lat2) * cos(lat3) * sin(l12)
  B <- sin(lat1) * cos(lat2) * cos(lat3) * cos(l12) - cos(lat1) * sin(lat2) * cos(lat3)
  C <- cos(lat1) * cos(lat2) * sin(lat3) * sin(l12)
  lon <- atan2(B, A)
  lon3 <- matrix(NA, nrow = length(lon1), ncol = 2)
  i <- (abs(C) > sqrt(A^2 + B^2)) | (sqrt(A^2 + B^2) == 0)
  lon3[i, ] <- NA
  i <- !i
  dlon <- rep(NA, length(A))
  dlon[i] <- acos(C[i]/sqrt(A[i]^2 + B[i]^2))
  lon3[i, 1] <- .normalizeLonRad(lon1[i] + dlon[i] + lon[i])
  lon3[i, 2] <- .normalizeLonRad(lon1[i] - dlon[i] + lon[i])
  res[anti, ] <- -1 * lon3/toRad
  return(res)
}, function (p1, p2, lat) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], lat)
  p1 <- p[, 1:2, drop = FALSE]
  p2 <- p[, 3:4, drop = FALSE]
  lat <- p[, 5]
  res <- matrix(NA, nrow = nrow(p1), ncol = 2)
  colnames(res) <- c("lon1", "lon2")
  anti <- !antipodal(p1, p2)
  if (sum(anti) == 0) {
    return(res)
  }
  p1 <- p1[anti, , drop = FALSE] * toRad
  p2 <- p2[anti, , drop = FALSE] * toRad
  lon1 <- p1[, 1] * -1
  lat1 <- p1[, 2]
  lon2 <- p2[, 1] * -1
  lat2 <- p2[, 2]
  lat3 <- lat * toRad
  l12 <- lon1 - lon2
  A <- sin(lat1) * cos(lat2) * cos(lat3) * sin(l12)
  B <- sin(lat1) * cos(lat2) * cos(lat3) * cos(l12) - cos(lat1) * sin(lat2) * cos(lat3)
  C <- cos(lat1) * cos(lat2) * sin(lat3) * sin(l12)
  lon <- atan2(B, A)
  lon3 <- matrix(NA, nrow = length(lon1), ncol = 2)
  i <- (abs(C) > sqrt(A^2 + B^2)) | (sqrt(A^2 + B^2) == 0)
  lon3[i, ] <- NA
  i <- !i
  dlon <- rep(NA, length(A))
  dlon[i] <- acos(C[i]/sqrt(A[i]^2 + B[i]^2))
  lon3[i, 1] <- .normalizeLonRad(lon1[i] + dlon[i] + lon[i])
  lon3[i, 2] <- .normalizeLonRad(lon1[i] - dlon[i] + lon[i])
  res[anti, ] <- -1 * lon3/toRad
  return(res)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
gcMaxLat
list(`package:geosphere` = function (p1, p2) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
  p1 <- p[, 1:2, drop = FALSE]
  p2 <- p[, 3:4, drop = FALSE]
  anti <- antipodal(p1, p2)
  same <- apply(p1 == p2, 1, sum) == 2
  use <- !(anti | same)
  res <- matrix(rep(NA, nrow(p1) * 2), ncol = 2)
  colnames(res) <- c("lon", "lat")
  if (length(use) == 0) {
    return(res)
  }
  pp1 <- p1[use, , drop = FALSE]
  pp2 <- p2[use, , drop = FALSE]
  b <- .old_bearing(pp1, pp2) * toRad
  lat <- pp1[, 2] * toRad
  maxlat <- acos(abs(sin(b) * cos(lat)))/toRad
  ml <- maxlat - 1e-12
  maxlon <- mean(gcLon(pp1, pp2, ml))
  res[use, ] <- cbind(maxlon, maxlat)
  return(res)
}, function (p1, p2) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
  p1 <- p[, 1:2, drop = FALSE]
  p2 <- p[, 3:4, drop = FALSE]
  anti <- antipodal(p1, p2)
  same <- apply(p1 == p2, 1, sum) == 2
  use <- !(anti | same)
  res <- matrix(rep(NA, nrow(p1) * 2), ncol = 2)
  colnames(res) <- c("lon", "lat")
  if (length(use) == 0) {
    return(res)
  }
  pp1 <- p1[use, , drop = FALSE]
  pp2 <- p2[use, , drop = FALSE]
  b <- .old_bearing(pp1, pp2) * toRad
  lat <- pp1[, 2] * toRad
  maxlat <- acos(abs(sin(b) * cos(lat)))/toRad
  ml <- maxlat - 1e-12
  maxlon <- mean(gcLon(pp1, pp2, ml))
  res[use, ] <- cbind(maxlon, maxlat)
  return(res)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
geodesic
list(`package:geosphere` = function (p, azi, d, a = 6378137, f = 1/298.257223563, ...) 
{
  p <- .pointsToMatrix(p)
  p <- cbind(p[, 1], p[, 2], azi, d)
  r <- .Call("_geodesic", as.double(p[, 1]), as.double(p[, 2]), as.double(p[, 3]), as.double(p[, 4]), as.double(a), as.double(f), PACKAGE = "geosphere")
  r <- matrix(r, ncol = 3, byrow = TRUE)
  colnames(r) <- c("longitude", "latitude", "azimuth")
  r
}, function (p, azi, d, a = 6378137, f = 1/298.257223563, ...) 
{
  p <- .pointsToMatrix(p)
  p <- cbind(p[, 1], p[, 2], azi, d)
  r <- .Call("_geodesic", as.double(p[, 1]), as.double(p[, 2]), as.double(p[, 3]), as.double(p[, 4]), as.double(a), as.double(f), PACKAGE = "geosphere")
  r <- matrix(r, ncol = 3, byrow = TRUE)
  colnames(r) <- c("longitude", "latitude", "azimuth")
  r
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
geodesic_inverse
list(`package:geosphere` = function (p1, p2, a = 6378137, f = 1/298.257223563, ...) 
{
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
  r <- .Call("_inversegeodesic", as.double(p[, 1]), as.double(p[, 2]), as.double(p[, 3]), as.double(p[, 4]), as.double(a), as.double(f), PACKAGE = "geosphere")
  r <- matrix(r, ncol = 3, byrow = TRUE)
  colnames(r) <- c("distance", "azimuth1", "azimuth2")
  r
}, function (p1, p2, a = 6378137, f = 1/298.257223563, ...) 
{
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2])
  r <- .Call("_inversegeodesic", as.double(p[, 1]), as.double(p[, 2]), as.double(p[, 3]), as.double(p[, 4]), as.double(a), as.double(f), PACKAGE = "geosphere")
  r <- matrix(r, ncol = 3, byrow = TRUE)
  colnames(r) <- c("distance", "azimuth1", "azimuth2")
  r
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
geomean
list(`package:geosphere` = function (xy, w = NULL) 
{
  xy <- .pointsToMatrix(xy)
  if (is.null(w)) {
    w <- 1
  }
  else if (length(w) != nrow(xy)) {
    stop("length of weights not correct. It should be: ", nrow(xy))
  }
  w <- w/sum(w)
  xyw <- cbind(xy, w)
  xy <- stats::na.omit(xyw)
  xy <- xyw[, 1:2]
  w <- xyw[, 3]
  xy[, 1] <- xy[, 1] + 180
  xy <- xy * pi/180
  Sx <- mean(sin(xy[, 1]) * w)
  Cx <- mean(cos(xy[, 1]) * w)
  x <- atan2(Sx, Cx)
  x <- x%%(2 * pi) - pi
  Sy <- mean(sin(xy[, 2]) * w)
  Cy <- mean(cos(xy[, 2]) * w)
  y <- atan2(Sy, Cy)
  cbind(x, y) * 180/pi
}, function (xy, w = NULL) 
{
  xy <- .pointsToMatrix(xy)
  if (is.null(w)) {
    w <- 1
  }
  else if (length(w) != nrow(xy)) {
    stop("length of weights not correct. It should be: ", nrow(xy))
  }
  w <- w/sum(w)
  xyw <- cbind(xy, w)
  xy <- stats::na.omit(xyw)
  xy <- xyw[, 1:2]
  w <- xyw[, 3]
  xy[, 1] <- xy[, 1] + 180
  xy <- xy * pi/180
  Sx <- mean(sin(xy[, 1]) * w)
  Cx <- mean(cos(xy[, 1]) * w)
  x <- atan2(Sx, Cx)
  x <- x%%(2 * pi) - pi
  Sy <- mean(sin(xy[, 2]) * w)
  Cy <- mean(cos(xy[, 2]) * w)
  y <- atan2(Sy, Cy)
  cbind(x, y) * 180/pi
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
greatCircle
list(`package:geosphere` = function (p1, p2, n = 360, sp = FALSE) 
{
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], n)
  p1 <- p[, 1:2]
  p2 <- p[, 3:4]
  n <- pmax(round(p[, 5]), 1)
  if (nrow(p) == 1) {
    lon <- (1:n * 360/n) - 180
    lat <- gcLat(p1, p2, lon)
    res <- cbind(lon, lat)
    if (sp) {
      lat <- gcLat(p1, p2, 180)
      res <- list(rbind(cbind(-180, lat), res))
      res <- SpatialLines(list(Lines(list(Line(res)), ID = as.character(1))), CRS("+proj=longlat +ellps=WGS84"))
    }
  }
  else {
    res <- list()
    for (i in 1:nrow(p1)) {
      lon <- (1:n[i] * 360/n[i]) - 180
      lat <- gcLat(p1[i, ], p2[i, ], lon)
      res[[i]] <- cbind(lon, lat)
    }
    if (sp) {
      for (i in 1:length(res)) {
        lat <- gcLat(p1[i, ], p2[i, ], 180)
        res[[i]] <- rbind(cbind(-180, lat), res[[i]])
        res[[i]] <- Lines(list(Line(res[[i]])), ID = as.character(i))
      }
      res <- SpatialLines(res, CRS("+proj=longlat +ellps=WGS84"))
    }
  }
  return(res)
}, function (p1, p2, n = 360, sp = FALSE) 
{
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], n)
  p1 <- p[, 1:2]
  p2 <- p[, 3:4]
  n <- pmax(round(p[, 5]), 1)
  if (nrow(p) == 1) {
    lon <- (1:n * 360/n) - 180
    lat <- gcLat(p1, p2, lon)
    res <- cbind(lon, lat)
    if (sp) {
      lat <- gcLat(p1, p2, 180)
      res <- list(rbind(cbind(-180, lat), res))
      res <- SpatialLines(list(Lines(list(Line(res)), ID = as.character(1))), CRS("+proj=longlat +ellps=WGS84"))
    }
  }
  else {
    res <- list()
    for (i in 1:nrow(p1)) {
      lon <- (1:n[i] * 360/n[i]) - 180
      lat <- gcLat(p1[i, ], p2[i, ], lon)
      res[[i]] <- cbind(lon, lat)
    }
    if (sp) {
      for (i in 1:length(res)) {
        lat <- gcLat(p1[i, ], p2[i, ], 180)
        res[[i]] <- rbind(cbind(-180, lat), res[[i]])
        res[[i]] <- Lines(list(Line(res[[i]])), ID = as.character(i))
      }
      res <- SpatialLines(res, CRS("+proj=longlat +ellps=WGS84"))
    }
  }
  return(res)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
greatCircleBearing
list(`package:geosphere` = function (p, brng, n = 360) 
{
  p <- .pointsToMatrix(p)
  p <- cbind(p[, 1], p[, 2], as.vector(brng), n)
  p2 <- destPoint(p[, 1:2], p[, 3], 1e+07)
  return(greatCircle(p[, 1:2], p2, n = p[, 4]))
}, function (p, brng, n = 360) 
{
  p <- .pointsToMatrix(p)
  p <- cbind(p[, 1], p[, 2], as.vector(brng), n)
  p2 <- destPoint(p[, 1:2], p[, 3], 1e+07)
  return(greatCircle(p[, 1:2], p2, n = p[, 4]))
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
horizon
list(`package:geosphere` = function (h, r = 6378137) 
{
  x = cbind(as.vector(h), as.vector(r))
  h = x[, 1]
  r = x[, 2]
  b = 0.8279
  sqrt(2 * r * h/b)
}, function (h, r = 6378137) 
{
  x = cbind(as.vector(h), as.vector(r))
  h = x[, 1]
  r = x[, 2]
  b = 0.8279
  sqrt(2 * r * h/b)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
lengthLine
list(`package:geosphere` = function (line) 
{
  if (inherits(line, "SpatialPolygons")) {
    requireNamespace("raster")
    line <- raster::geom(methods::as(line, "SpatialLines"))
  }
  else if (inherits(line, "SpatialLines")) {
    requireNamespace("raster")
    line <- raster::geom(line)
  }
  else {
    line <- cbind(object = 1, part = 1, cump = 1, line[, 1:2])
    colnames(line)[4:5] <- c("x", "y")
  }
  ids <- unique(line[, 1])
  len <- rep(0, length(ids))
  for (i in 1:length(ids)) {
    d <- line[line[, 1] == ids[i], ]
    parts <- unique(d[, 2])
    for (p in parts) {
      dd <- d[d[, 2] == p, , drop = FALSE]
      for (j in 1:(nrow(dd) - 1)) {
        len[i] <- len[i] + distGeo(dd[j, c("x", "y"), drop = FALSE], dd[j + 1, c("x", "y"), drop = FALSE])
      }
    }
  }
  return(len)
}, function (line) 
{
  if (inherits(line, "SpatialPolygons")) {
    requireNamespace("raster")
    line <- raster::geom(methods::as(line, "SpatialLines"))
  }
  else if (inherits(line, "SpatialLines")) {
    requireNamespace("raster")
    line <- raster::geom(line)
  }
  else {
    line <- cbind(object = 1, part = 1, cump = 1, line[, 1:2])
    colnames(line)[4:5] <- c("x", "y")
  }
  ids <- unique(line[, 1])
  len <- rep(0, length(ids))
  for (i in 1:length(ids)) {
    d <- line[line[, 1] == ids[i], ]
    parts <- unique(d[, 2])
    for (p in parts) {
      dd <- d[d[, 2] == p, , drop = FALSE]
      for (j in 1:(nrow(dd) - 1)) {
        len[i] <- len[i] + distGeo(dd[j, c("x", "y"), drop = FALSE], dd[j + 1, c("x", "y"), drop = FALSE])
      }
    }
  }
  return(len)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
makeLine
list(`package:geosphere` = function (p, interval = 10000, sp = FALSE, ...) 
{
  if (inherits(p, "SpatialLines")) {
    test <- !is.projected(p)
    if (!isTRUE(test)) {
      if (is.na(test)) {
        warning("Coordinate reference system of SpatialPolygons object is not set. Assuming it is degrees (longitude/latitude)!")
      }
      else {
        stop("Points are projected. They should be in degrees (longitude/latitude)")
      }
    }
    x = p@lines
    n = length(x)
    lines = list()
    for (i in 1:n) {
      parts = length(x[[i]]@Lines)
      partlist = list()
      for (j in 1:parts) {
        crd = x[[i]]@Lines[[j]]@coords
        crd = .makeSingleLine(crd, interval = interval, ...)
        partlist[[j]] = Line(crd)
      }
      lines[[i]] = Lines(partlist, i)
    }
    lines <- SpatialLines(lines)
    if (inherits(p, "SpatialLinesDataFrame")) {
      lines <- SpatialLinesDataFrame(lines, p@data)
    }
    lines@proj4string <- p@proj4string
    return(lines)
  }
  else {
    p <- .pointsToMatrix(p)
    if (nrow(p) < 3) {
      stop("cannot make a polygon (insufficent number of vertices)")
    }
    res <- .makeSingleLine(p, interval = interval, ...)
    if (sp) {
      res <- SpatialLines(list(Lines(list(Line(res)), 1)))
      res@proj4string <- CRS("+proj=longlat +datum=WGS84")
    }
    return(res)
  }
}, function (p, interval = 10000, sp = FALSE, ...) 
{
  if (inherits(p, "SpatialLines")) {
    test <- !is.projected(p)
    if (!isTRUE(test)) {
      if (is.na(test)) {
        warning("Coordinate reference system of SpatialPolygons object is not set. Assuming it is degrees (longitude/latitude)!")
      }
      else {
        stop("Points are projected. They should be in degrees (longitude/latitude)")
      }
    }
    x = p@lines
    n = length(x)
    lines = list()
    for (i in 1:n) {
      parts = length(x[[i]]@Lines)
      partlist = list()
      for (j in 1:parts) {
        crd = x[[i]]@Lines[[j]]@coords
        crd = .makeSingleLine(crd, interval = interval, ...)
        partlist[[j]] = Line(crd)
      }
      lines[[i]] = Lines(partlist, i)
    }
    lines <- SpatialLines(lines)
    if (inherits(p, "SpatialLinesDataFrame")) {
      lines <- SpatialLinesDataFrame(lines, p@data)
    }
    lines@proj4string <- p@proj4string
    return(lines)
  }
  else {
    p <- .pointsToMatrix(p)
    if (nrow(p) < 3) {
      stop("cannot make a polygon (insufficent number of vertices)")
    }
    res <- .makeSingleLine(p, interval = interval, ...)
    if (sp) {
      res <- SpatialLines(list(Lines(list(Line(res)), 1)))
      res@proj4string <- CRS("+proj=longlat +datum=WGS84")
    }
    return(res)
  }
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
makePoly
list(`package:geosphere` = function (p, interval = 10000, sp = FALSE, ...) 
{
  if (inherits(p, "SpatialPolygons")) {
    test <- !is.projected(p)
    if (!isTRUE(test)) {
      if (is.na(test)) {
        warning("Coordinate reference system of SpatialPolygons object is not set. Assuming it is degrees (longitude/latitude)!")
      }
      else {
        stop("Points are projected. They should be in degrees (longitude/latitude)")
      }
    }
    x <- p@polygons
    n <- length(x)
    polys = list()
    for (i in 1:n) {
      parts <- length(x[[i]]@Polygons)
      partlist <- list()
      for (j in 1:parts) {
        crd <- x[[i]]@Polygons[[j]]@coords
        crd <- .makeSinglePoly(crd, interval = interval, ...)
        partlist[[j]] <- Polygon(crd)
      }
      polys[[i]] <- Polygons(partlist, i)
    }
    polys <- SpatialPolygons(polys)
    if (inherits(p, "SpatialPolygonsDataFrame")) {
      rownames(p@data) <- 1:nrow(p@data)
      polys <- SpatialPolygonsDataFrame(polys, p@data)
    }
    polys@proj4string <- p@proj4string
    return(polys)
  }
  else {
    p <- .pointsToMatrix(p)
    if (nrow(p) < 3) {
      stop("cannot make a polygon (insufficent number of vertices)")
    }
    if (!isTRUE(all.equal(p[1, ], p[nrow(p), ]))) {
      p <- rbind(p, p[1, ])
    }
    res <- .makeSinglePoly(p, interval = interval, ...)
    if (sp) {
      res <- SpatialPolygons(list(Polygons(list(Polygon(res)), 1)))
      res@proj4string <- CRS("+proj=longlat +datum=WGS84")
    }
    return(res)
  }
}, function (p, interval = 10000, sp = FALSE, ...) 
{
  if (inherits(p, "SpatialPolygons")) {
    test <- !is.projected(p)
    if (!isTRUE(test)) {
      if (is.na(test)) {
        warning("Coordinate reference system of SpatialPolygons object is not set. Assuming it is degrees (longitude/latitude)!")
      }
      else {
        stop("Points are projected. They should be in degrees (longitude/latitude)")
      }
    }
    x <- p@polygons
    n <- length(x)
    polys = list()
    for (i in 1:n) {
      parts <- length(x[[i]]@Polygons)
      partlist <- list()
      for (j in 1:parts) {
        crd <- x[[i]]@Polygons[[j]]@coords
        crd <- .makeSinglePoly(crd, interval = interval, ...)
        partlist[[j]] <- Polygon(crd)
      }
      polys[[i]] <- Polygons(partlist, i)
    }
    polys <- SpatialPolygons(polys)
    if (inherits(p, "SpatialPolygonsDataFrame")) {
      rownames(p@data) <- 1:nrow(p@data)
      polys <- SpatialPolygonsDataFrame(polys, p@data)
    }
    polys@proj4string <- p@proj4string
    return(polys)
  }
  else {
    p <- .pointsToMatrix(p)
    if (nrow(p) < 3) {
      stop("cannot make a polygon (insufficent number of vertices)")
    }
    if (!isTRUE(all.equal(p[1, ], p[nrow(p), ]))) {
      p <- rbind(p, p[1, ])
    }
    res <- .makeSinglePoly(p, interval = interval, ...)
    if (sp) {
      res <- SpatialPolygons(list(Polygons(list(Polygon(res)), 1)))
      res@proj4string <- CRS("+proj=longlat +datum=WGS84")
    }
    return(res)
  }
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
mercator
list(`package:geosphere` = function (p, inverse = FALSE, r = 6378137) 
{
  toRad <- pi/180
  if (inverse) {
    p <- .pointsToMatrix(p, checkLonLat = FALSE)
    p[, 2] <- pi/2 - 2 * atan(exp(-p[, 2]/r))
    p[, 1] <- p[, 1]/r
    colnames(p) <- c("lon", "lat")
    return(p/toRad)
  }
  else {
    p <- .pointsToMatrix(p) * toRad
    p[, 2] <- log(tan(p[, 2]) + (1/cos(p[, 2])))
    p <- p * r
    colnames(p) <- c("x", "y")
    return(p)
  }
}, function (p, inverse = FALSE, r = 6378137) 
{
  toRad <- pi/180
  if (inverse) {
    p <- .pointsToMatrix(p, checkLonLat = FALSE)
    p[, 2] <- pi/2 - 2 * atan(exp(-p[, 2]/r))
    p[, 1] <- p[, 1]/r
    colnames(p) <- c("lon", "lat")
    return(p/toRad)
  }
  else {
    p <- .pointsToMatrix(p) * toRad
    p[, 2] <- log(tan(p[, 2]) + (1/cos(p[, 2])))
    p <- p * r
    colnames(p) <- c("x", "y")
    return(p)
  }
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
midPoint
list(`package:geosphere` = function (p1, p2, a = 6378137, f = 1/298.257223563) 
{
  gi <- geodesic_inverse(p1, p2, a = a, f = f)
  destPoint(p1, gi[, "azimuth1"], gi[, "distance"]/2, a = a, f = f)
}, function (p1, p2, a = 6378137, f = 1/298.257223563) 
{
  gi <- geodesic_inverse(p1, p2, a = a, f = f)
  destPoint(p1, gi[, "azimuth1"], gi[, "distance"]/2, a = a, f = f)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
onGreatCircle
list(`package:geosphere` = function (p1, p2, p3, tol = 1e-04) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p3 <- .pointsToMatrix(p3)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], p3[, 1], p3[, 2])
  p1 <- p[, 1:2, drop = FALSE] * toRad
  p2 <- p[, 3:4, drop = FALSE] * toRad
  p3 <- p[, 5:6, drop = FALSE] * toRad
  lon1 <- p1[, 1]
  lat1 <- p1[, 2]
  lon2 <- p2[, 1]
  lat2 <- p2[, 2]
  lon <- p3[, 1]
  lat <- p3[, 2]
  newlat <- atan((sin(lat1) * cos(lat2) * sin(lon - lon2) - sin(lat2) * cos(lat1) * sin(lon - lon1))/(cos(lat1) * cos(lat2) * sin(lon1 - lon2)))
  res <- abs(newlat - lat) < tol
  meridian <- p1[, 1] == p2[, 1] & p1[, 1] == p3[, 1]
  res[meridian] <- TRUE
  return(as.vector(res))
}, function (p1, p2, p3, tol = 1e-04) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1)
  p2 <- .pointsToMatrix(p2)
  p3 <- .pointsToMatrix(p3)
  p <- cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], p3[, 1], p3[, 2])
  p1 <- p[, 1:2, drop = FALSE] * toRad
  p2 <- p[, 3:4, drop = FALSE] * toRad
  p3 <- p[, 5:6, drop = FALSE] * toRad
  lon1 <- p1[, 1]
  lat1 <- p1[, 2]
  lon2 <- p2[, 1]
  lat2 <- p2[, 2]
  lon <- p3[, 1]
  lat <- p3[, 2]
  newlat <- atan((sin(lat1) * cos(lat2) * sin(lon - lon2) - sin(lat2) * cos(lat1) * sin(lon - lon1))/(cos(lat1) * cos(lat2) * sin(lon1 - lon2)))
  res <- abs(newlat - lat) < tol
  meridian <- p1[, 1] == p2[, 1] & p1[, 1] == p3[, 1]
  res[meridian] <- TRUE
  return(as.vector(res))
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
perimeter
list(`package:geosphere` = new("standardGeneric", .Data = function (x, ...) 
  standardGeneric("perimeter"), generic = "perimeter", package = "geosphere", group = list(), valueClass = character(0), signature = "x", default = NULL, skeleton = (function (x, ...) 
    stop(gettextf("invalid call in method dispatch to '%s' (no default method)", "perimeter"), domain = NA))(x, ...)), new("standardGeneric", .Data = function (x, ...) 
      standardGeneric("perimeter"), generic = "perimeter", package = "geosphere", group = list(), valueClass = character(0), signature = "x", default = NULL, skeleton = (function (x, ...) 
        stop(gettextf("invalid call in method dispatch to '%s' (no default method)", "perimeter"), domain = NA))(x, ...)))
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
plotArrows
list(`package:geosphere` = function (p, fraction = 0.9, length = 0.15, first = "", add = FALSE, ...) 
{
  asp = 1
  if (inherits(p, "Spatial")) {
    bb = t(bbox(p))
    interval = distm(bb)[2][1]/1000
    if (!add) {
      plot(bb, asp = asp, type = "n")
    }
    p = p@polygons
    n = length(p)
    for (i in 1:n) {
      parts = length(p[[i]]@Polygons)
      sumarea = 0
      for (j in 1:parts) {
        pp = p[[i]]@Polygons[[j]]@coords
        line = .makeSinglePoly(pp, interval = interval)
        .doArrows(pp, line, fraction, length, interval = interval, ...)
      }
      graphics::points(pp[1, 1], pp[1, 2], pch = first, cex = 2)
    }
  }
  else {
    interval = max(distm(p), na.rm = TRUE)/1000
    line = .makeSinglePoly(p, interval = interval)
    if (!add) {
      plot(line, asp = asp, type = "n")
    }
    .doArrows(p, line = line, fraction, length, interval = interval, ...)
    graphics::points(p[1, 1], p[1, 2], pch = first, cex = 2)
  }
}, function (p, fraction = 0.9, length = 0.15, first = "", add = FALSE, ...) 
{
  asp = 1
  if (inherits(p, "Spatial")) {
    bb = t(bbox(p))
    interval = distm(bb)[2][1]/1000
    if (!add) {
      plot(bb, asp = asp, type = "n")
    }
    p = p@polygons
    n = length(p)
    for (i in 1:n) {
      parts = length(p[[i]]@Polygons)
      sumarea = 0
      for (j in 1:parts) {
        pp = p[[i]]@Polygons[[j]]@coords
        line = .makeSinglePoly(pp, interval = interval)
        .doArrows(pp, line, fraction, length, interval = interval, ...)
      }
      graphics::points(pp[1, 1], pp[1, 2], pch = first, cex = 2)
    }
  }
  else {
    interval = max(distm(p), na.rm = TRUE)/1000
    line = .makeSinglePoly(p, interval = interval)
    if (!add) {
      plot(line, asp = asp, type = "n")
    }
    .doArrows(p, line = line, fraction, length, interval = interval, ...)
    graphics::points(p[1, 1], p[1, 2], pch = first, cex = 2)
  }
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
randomCoordinates
list(`package:geosphere` = function (n) 
{
  z <- stats::runif(n) * 2 - 1
  t <- stats::runif(n) * 2 * pi
  r <- sqrt(1 - z^2)
  x <- r * cos(t)
  y <- r * sin(t)
  r <- sqrt(x^2 + y^2 + z^2)
  theta <- acos(z/r)
  phi <- atan2(y, x)
  lat <- theta * 180/pi - 90
  lon <- phi * 180/pi
  return(cbind(lon, lat))
}, function (n) 
{
  z <- stats::runif(n) * 2 - 1
  t <- stats::runif(n) * 2 * pi
  r <- sqrt(1 - z^2)
  x <- r * cos(t)
  y <- r * sin(t)
  r <- sqrt(x^2 + y^2 + z^2)
  theta <- acos(z/r)
  phi <- atan2(y, x)
  lat <- theta * 180/pi - 90
  lon <- phi * 180/pi
  return(cbind(lon, lat))
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
refEllipsoids
list(`package:geosphere` = function () 
{
  data.frame(ellipsoid = c("Airy (1930)", "Australian National", "Bessel 1841", "Ethiopia,  Indonesia,  Japan,  Korea", "Namibia", "Clarke 1866", "Clarke 1880", "Everest - Brunei & E. Malasia (Sabah & Sarawak)", "Everest - India 1830", "Everest - India 1956", "Everest - Pakistan", "Everest - W. Malasia and Singapore 1948", "Everest - W. Malasia 1969", "Geodetic Reference System 1980 (GRS 80)", "Helmert 1906", "Hough 1960", "Indonesian 1974", "International 1924", "Krassovsky 1940", "Modified Airy", 
                           "Modified Fischer 1960 (South Asia)", "South American 1969", "World Geodetic System 1972 (WGS 72)", "World Geodetic System 1984 (WGS 84)"), code = c("AA", "AN", "??", "BR", "BN", "CC", "CD", "EB", "EA", "EC", "EF", "EE", "ED", "RF", "HE", "HO", "ID", "IN", "KA", "AM", "FA", "SA", "WD", "WE"), invf = c(299.3249646, 298.25, 299.1528434, 299.1528128, 299.1528128, 294.9786982, 293.465, 300.8017, 300.8017, 300.8017, 300.8017, 300.8017, 300.8017, 298.2572221, 298.3, 297, 298.247, 297, 298.3, 299.3249646, 
                                                                                                                                                                                                                                                                                                                                          298.3, 298.25, 298.26, 298.2572236), a = c(6377563.396, 6378160, 6377397.155, 6377397.155, 6377483.865, 6378206.4, 6378249.145, 6377298.556, 6377276.345, 6377301.243, 6377309.613, 6377304.063, 6377295.664, 6378137, 6378200, 6378270, 6378160, 6378388, 6378245, 6377340.189, 6378155, 6378160, 6378135, 6378137), stringsAsFactors = FALSE)
}, function () 
{
  data.frame(ellipsoid = c("Airy (1930)", "Australian National", "Bessel 1841", "Ethiopia,  Indonesia,  Japan,  Korea", "Namibia", "Clarke 1866", "Clarke 1880", "Everest - Brunei & E. Malasia (Sabah & Sarawak)", "Everest - India 1830", "Everest - India 1956", "Everest - Pakistan", "Everest - W. Malasia and Singapore 1948", "Everest - W. Malasia 1969", "Geodetic Reference System 1980 (GRS 80)", "Helmert 1906", "Hough 1960", "Indonesian 1974", "International 1924", "Krassovsky 1940", "Modified Airy", 
                           "Modified Fischer 1960 (South Asia)", "South American 1969", "World Geodetic System 1972 (WGS 72)", "World Geodetic System 1984 (WGS 84)"), code = c("AA", "AN", "??", "BR", "BN", "CC", "CD", "EB", "EA", "EC", "EF", "EE", "ED", "RF", "HE", "HO", "ID", "IN", "KA", "AM", "FA", "SA", "WD", "WE"), invf = c(299.3249646, 298.25, 299.1528434, 299.1528128, 299.1528128, 294.9786982, 293.465, 300.8017, 300.8017, 300.8017, 300.8017, 300.8017, 300.8017, 298.2572221, 298.3, 297, 298.247, 297, 298.3, 299.3249646, 
                                                                                                                                                                                                                                                                                                                                          298.3, 298.25, 298.26, 298.2572236), a = c(6377563.396, 6378160, 6377397.155, 6377397.155, 6377483.865, 6378206.4, 6378249.145, 6377298.556, 6377276.345, 6377301.243, 6377309.613, 6377304.063, 6377295.664, 6378137, 6378200, 6378270, 6378160, 6378388, 6378245, 6377340.189, 6378155, 6378160, 6378135, 6378137), stringsAsFactors = FALSE)
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
regularCoordinates
list(`package:geosphere` = function (N) 
{
  N <- round(N)
  if (N < 1) {
    stop("N should be >= 1")
  }
  beta <- 0.5 * pi/N
  A <- 2 * sin(beta/2)
  points <- rbind(c(0, 0, 1), c(0, 0, -1))
  R <- sin(1:N * beta)
  Z <- cos(1:N * beta)
  M <- round(R * 2 * pi/A)
  for (i in 1:N) {
    j <- 0:(M[i] - 1)
    Alpha <- j/M[i] * 2 * pi
    X <- cos(Alpha) * R[i]
    Y <- sin(Alpha) * R[i]
    points <- rbind(points, cbind(X, Y, Z[i]))
    if (i != N) {
      points <- rbind(points, cbind(X, Y, -Z[i]))
    }
  }
  r <- sqrt(points[, 1]^2 + points[, 2]^2 + points[, 3]^2)
  theta <- acos(points[, 3]/r)
  phi <- atan2(points[, 2], points[, 1])
  lat <- theta * 180/pi - 90
  lon <- phi * 180/pi
  return(cbind(lon, lat))
}, function (N) 
{
  N <- round(N)
  if (N < 1) {
    stop("N should be >= 1")
  }
  beta <- 0.5 * pi/N
  A <- 2 * sin(beta/2)
  points <- rbind(c(0, 0, 1), c(0, 0, -1))
  R <- sin(1:N * beta)
  Z <- cos(1:N * beta)
  M <- round(R * 2 * pi/A)
  for (i in 1:N) {
    j <- 0:(M[i] - 1)
    Alpha <- j/M[i] * 2 * pi
    X <- cos(Alpha) * R[i]
    Y <- sin(Alpha) * R[i]
    points <- rbind(points, cbind(X, Y, Z[i]))
    if (i != N) {
      points <- rbind(points, cbind(X, Y, -Z[i]))
    }
  }
  r <- sqrt(points[, 1]^2 + points[, 2]^2 + points[, 3]^2)
  theta <- acos(points[, 3]/r)
  phi <- atan2(points[, 2], points[, 1])
  lat <- theta * 180/pi - 90
  lon <- phi * 180/pi
  return(cbind(lon, lat))
})
c("package:geosphere", "namespace:geosphere")
c(TRUE, FALSE)
c(FALSE, TRUE)
span
list(`package:geosphere` = new("standardGeneric", .Data = function (x, ...) 
  standardGeneric("span"), generic = "span", package = "geosphere", group = list(), valueClass = character(0), signature = "x", default = NULL, skeleton = (function (x, ...) 
    stop(gettextf("invalid call in method dispatch to '%s' (no default method)", "span"), domain = NA))(x, ...)), function (..., .noWS = NULL, .renderHook = NULL) 
    {
      validateNoWS(.noWS)
      contents <- dots_list(...)
      tag("span", contents, .noWS = .noWS, .renderHook = .renderHook)
    }, new("standardGeneric", .Data = function (x, ...) 
      standardGeneric("span"), generic = "span", package = "geosphere", group = list(), valueClass = character(0), signature = "x", default = NULL, skeleton = (function (x, ...) 
        stop(gettextf("invalid call in method dispatch to '%s' (no default method)", "span"), domain = NA))(x, ...)))
c("package:geosphere", "namespace:htmltools", "namespace:geosphere")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
