library(rmarkdown)
library(readr)
library(readxl)
library(RODBC)
library(shiny)

library(rmarkdown)
all_output_formats
list(`package:rmarkdown` = function (input, output_yaml = NULL) 
{
  enumerate_output_formats(input, output_yaml = output_yaml)
}, function (input, output_yaml = NULL) 
{
  enumerate_output_formats(input, output_yaml = output_yaml)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
available_templates
list(`package:rmarkdown` = function (package = "rmarkdown", full_path = FALSE) 
{
  template_folder <- pkg_file("rmarkdown", "templates", package = package)
  if (!dir_exists(template_folder)) 
    return(invisible(character()))
  templates <- list.files(template_folder, full.names = TRUE)
  templates <- templates[utils::file_test("-d", templates)]
  if (full_path) 
    return(templates)
  basename(templates)
}, function (package = "rmarkdown", full_path = FALSE) 
{
  template_folder <- pkg_file("rmarkdown", "templates", package = package)
  if (!dir_exists(template_folder)) 
    return(invisible(character()))
  templates <- list.files(template_folder, full.names = TRUE)
  templates <- templates[utils::file_test("-d", templates)]
  if (full_path) 
    return(templates)
  basename(templates)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
beamer_presentation
list(`package:rmarkdown` = function (toc = FALSE, slide_level = NULL, number_sections = FALSE, incremental = FALSE, fig_width = 10, fig_height = 7, fig_crop = "auto", fig_caption = TRUE, dev = "pdf", df_print = "default", theme = "default", colortheme = "default", fonttheme = "default", highlight = "default", template = "default", keep_tex = FALSE, keep_md = FALSE, latex_engine = "pdflatex", citation_package = c("default", "natbib", "biblatex"), self_contained = TRUE, includes = NULL, md_extensions = NULL, 
                                     pandoc_args = NULL) 
{
  args <- c()
  if (!is.null(template)) {
    if (identical(template, "default")) 
      template <- patch_beamer_template()
    if (!is.null(template)) 
      args <- c(args, "--template", pandoc_path_arg(template))
  }
  if (toc) 
    args <- c(args, "--table-of-contents")
  if (!is.null(slide_level)) 
    args <- c(args, "--slide-level", as.character(slide_level))
  if (number_sections) 
    args <- c(args, "--number-sections")
  if (incremental) 
    args <- c(args, "--incremental")
  if (!identical(theme, "default")) 
    args <- c(args, pandoc_variable_arg("theme", theme))
  if (!identical(colortheme, "default")) 
    args <- c(args, pandoc_variable_arg("colortheme", colortheme))
  if (!identical(fonttheme, "default")) 
    args <- c(args, pandoc_variable_arg("fonttheme", fonttheme))
  if (!is.null(highlight)) 
    highlight <- resolve_highlight(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))
  latex_engine <- match.arg(latex_engine, c("pdflatex", "lualatex", "xelatex", "tectonic"))
  args <- c(args, pandoc_latex_engine_args(latex_engine))
  args <- c(args, citation_package_arg(citation_package))
  if (self_contained) 
    args <- c(args, "--self-contained")
  args <- c(args, includes_to_pandoc_args(includes))
  if (identical(template, "default")) 
    args <- c(args, "--variable", "graphics=yes")
  args <- c(args, pandoc_args)
  saved_files_dir <- NULL
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    saved_files_dir <<- files_dir
    args <- append_in_header(process_header_includes(metadata))
    args
  }
  intermediates_generator <- function(...) {
    general_intermediates_generator(saved_files_dir, ...)
  }
  output_format(knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev), pandoc = pandoc_options(to = "beamer", from = from_rmarkdown(fig_caption, md_extensions), args = args, latex_engine = latex_engine, keep_tex = keep_tex, lua_filters = pkg_file_lua(c("pagebreak.lua", "latex-div.lua"))), pre_processor = pre_processor, intermediates_generator = intermediates_generator, clean_supporting = !keep_tex, keep_md = keep_md, df_print = df_print)
}, function (toc = FALSE, slide_level = NULL, number_sections = FALSE, incremental = FALSE, fig_width = 10, fig_height = 7, fig_crop = "auto", fig_caption = TRUE, dev = "pdf", df_print = "default", theme = "default", colortheme = "default", fonttheme = "default", highlight = "default", template = "default", keep_tex = FALSE, keep_md = FALSE, latex_engine = "pdflatex", citation_package = c("default", "natbib", "biblatex"), self_contained = TRUE, includes = NULL, md_extensions = NULL, pandoc_args = NULL) 
{
  args <- c()
  if (!is.null(template)) {
    if (identical(template, "default")) 
      template <- patch_beamer_template()
    if (!is.null(template)) 
      args <- c(args, "--template", pandoc_path_arg(template))
  }
  if (toc) 
    args <- c(args, "--table-of-contents")
  if (!is.null(slide_level)) 
    args <- c(args, "--slide-level", as.character(slide_level))
  if (number_sections) 
    args <- c(args, "--number-sections")
  if (incremental) 
    args <- c(args, "--incremental")
  if (!identical(theme, "default")) 
    args <- c(args, pandoc_variable_arg("theme", theme))
  if (!identical(colortheme, "default")) 
    args <- c(args, pandoc_variable_arg("colortheme", colortheme))
  if (!identical(fonttheme, "default")) 
    args <- c(args, pandoc_variable_arg("fonttheme", fonttheme))
  if (!is.null(highlight)) 
    highlight <- resolve_highlight(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))
  latex_engine <- match.arg(latex_engine, c("pdflatex", "lualatex", "xelatex", "tectonic"))
  args <- c(args, pandoc_latex_engine_args(latex_engine))
  args <- c(args, citation_package_arg(citation_package))
  if (self_contained) 
    args <- c(args, "--self-contained")
  args <- c(args, includes_to_pandoc_args(includes))
  if (identical(template, "default")) 
    args <- c(args, "--variable", "graphics=yes")
  args <- c(args, pandoc_args)
  saved_files_dir <- NULL
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    saved_files_dir <<- files_dir
    args <- append_in_header(process_header_includes(metadata))
    args
  }
  intermediates_generator <- function(...) {
    general_intermediates_generator(saved_files_dir, ...)
  }
  output_format(knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev), pandoc = pandoc_options(to = "beamer", from = from_rmarkdown(fig_caption, md_extensions), args = args, latex_engine = latex_engine, keep_tex = keep_tex, lua_filters = pkg_file_lua(c("pagebreak.lua", "latex-div.lua"))), pre_processor = pre_processor, intermediates_generator = intermediates_generator, clean_supporting = !keep_tex, keep_md = keep_md, df_print = df_print)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
clean_site
list(`package:rmarkdown` = function (input = ".", preview = TRUE, quiet = FALSE, encoding = "UTF-8") 
{
  input <- input_as_dir(input)
  generator <- site_generator(input, output_format = NULL)
  if (is.null(generator)) 
    stop("No site generator found.")
  files <- generator$clean()
  if (length(files) == 0) {
    if (preview || !quiet) 
      cat("Nothing to removed. All clean !\n")
    return(invisible(NULL))
  }
  if (preview) {
    cat("These files and folders can probably be removed:\n", paste0("* ", xfun::mark_dirs(files)), "\nUse rmarkdown::clean_site(preview = FALSE) to remove them.", sep = "\n")
  }
  else {
    if (!quiet) {
      cat("Removing files: \n", paste0("* ", xfun::mark_dirs(files)), sep = "\n")
    }
    unlink(file.path(input, files), recursive = TRUE)
  }
}, function (input = ".", preview = TRUE, quiet = FALSE, encoding = "UTF-8") 
{
  input <- input_as_dir(input)
  generator <- site_generator(input, output_format = NULL)
  if (is.null(generator)) 
    stop("No site generator found.")
  files <- generator$clean()
  if (length(files) == 0) {
    if (preview || !quiet) 
      cat("Nothing to removed. All clean !\n")
    return(invisible(NULL))
  }
  if (preview) {
    cat("These files and folders can probably be removed:\n", paste0("* ", xfun::mark_dirs(files)), "\nUse rmarkdown::clean_site(preview = FALSE) to remove them.", sep = "\n")
  }
  else {
    if (!quiet) {
      cat("Removing files: \n", paste0("* ", xfun::mark_dirs(files)), sep = "\n")
    }
    unlink(file.path(input, files), recursive = TRUE)
  }
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
context_document
list(`package:rmarkdown` = function (toc = FALSE, toc_depth = 2, number_sections = FALSE, fig_width = 6.5, fig_height = 4.5, fig_crop = "auto", fig_caption = TRUE, dev = "pdf", df_print = "default", template = NULL, keep_tex = FALSE, keep_md = FALSE, citation_package = c("default", "natbib", "biblatex"), includes = NULL, md_extensions = NULL, output_extensions = NULL, pandoc_args = NULL, context_path = NULL, context_args = NULL, ext = c(".pdf", ".tex")) 
{
  if (!is.null(context_path)) {
    context_path <- normalizePath(context_path, mustWork = TRUE)
  }
  sys_context <- if (is.null(context_path)) 
    find_program("context")
  else context_path
  ext <- match.arg(ext)
  if (identical(ext, ".pdf") && !nzchar(sys_context)) 
    stop2("Cannot find ConTeXt.\n", "Please, check that ConTeXt is installed.\n", "For more information, see the help page '?context_document'.")
  args <- c("--standalone")
  if (!is.null(context_path)) {
    args <- c(args, pandoc_latex_engine_args(context_path))
  }
  if (length(context_args)) 
    args <- c(args, paste("--pdf-engine-opt", context_args, sep = "="))
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  if (number_sections) 
    args <- c(args, "--number-sections")
  if (!is.null(template)) 
    args <- c(args, "--template", pandoc_path_arg(template))
  args <- c(args, citation_package_arg(citation_package))
  args <- c(args, includes_to_pandoc_args(includes))
  args <- c(args, pandoc_args)
  clean_supporting <- identical(ext, ".pdf") && !isTRUE(keep_tex)
  post_processor <- NULL
  if (identical(ext, ".pdf") && isTRUE(keep_tex)) {
    ext <- ".tex"
    post_processor <- function(metadata, input_file, output_file, clean, verbose) {
      context_args <- unique(c(context_args, "--purgeall", "--batchmode"))
      is_pandoc_verbose <- !is.na(match("--verbose", pandoc_args))
      stdout <- if (is_pandoc_verbose) 
        ""
      else FALSE
      system2(sys_context, c(output_file, context_args), stdout = stdout)
      xfun::with_ext(output_file, "pdf")
    }
  }
  output_format(knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev), pandoc = pandoc_options(to = paste(c("context", output_extensions), collapse = ""), from = from_rmarkdown(fig_caption, md_extensions), args = args, keep_tex = FALSE, ext = ext, lua_filters = pkg_file_lua("pagebreak.lua")), clean_supporting = !isTRUE(keep_tex), keep_md = keep_md, df_print = df_print, intermediates_generator = general_intermediates_generator, post_processor = post_processor)
}, function (toc = FALSE, toc_depth = 2, number_sections = FALSE, fig_width = 6.5, fig_height = 4.5, fig_crop = "auto", fig_caption = TRUE, dev = "pdf", df_print = "default", template = NULL, keep_tex = FALSE, keep_md = FALSE, citation_package = c("default", "natbib", "biblatex"), includes = NULL, md_extensions = NULL, output_extensions = NULL, pandoc_args = NULL, context_path = NULL, context_args = NULL, ext = c(".pdf", ".tex")) 
{
  if (!is.null(context_path)) {
    context_path <- normalizePath(context_path, mustWork = TRUE)
  }
  sys_context <- if (is.null(context_path)) 
    find_program("context")
  else context_path
  ext <- match.arg(ext)
  if (identical(ext, ".pdf") && !nzchar(sys_context)) 
    stop2("Cannot find ConTeXt.\n", "Please, check that ConTeXt is installed.\n", "For more information, see the help page '?context_document'.")
  args <- c("--standalone")
  if (!is.null(context_path)) {
    args <- c(args, pandoc_latex_engine_args(context_path))
  }
  if (length(context_args)) 
    args <- c(args, paste("--pdf-engine-opt", context_args, sep = "="))
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  if (number_sections) 
    args <- c(args, "--number-sections")
  if (!is.null(template)) 
    args <- c(args, "--template", pandoc_path_arg(template))
  args <- c(args, citation_package_arg(citation_package))
  args <- c(args, includes_to_pandoc_args(includes))
  args <- c(args, pandoc_args)
  clean_supporting <- identical(ext, ".pdf") && !isTRUE(keep_tex)
  post_processor <- NULL
  if (identical(ext, ".pdf") && isTRUE(keep_tex)) {
    ext <- ".tex"
    post_processor <- function(metadata, input_file, output_file, clean, verbose) {
      context_args <- unique(c(context_args, "--purgeall", "--batchmode"))
      is_pandoc_verbose <- !is.na(match("--verbose", pandoc_args))
      stdout <- if (is_pandoc_verbose) 
        ""
      else FALSE
      system2(sys_context, c(output_file, context_args), stdout = stdout)
      xfun::with_ext(output_file, "pdf")
    }
  }
  output_format(knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev), pandoc = pandoc_options(to = paste(c("context", output_extensions), collapse = ""), from = from_rmarkdown(fig_caption, md_extensions), args = args, keep_tex = FALSE, ext = ext, lua_filters = pkg_file_lua("pagebreak.lua")), clean_supporting = !isTRUE(keep_tex), keep_md = keep_md, df_print = df_print, intermediates_generator = general_intermediates_generator, post_processor = post_processor)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
convert_ipynb
list(`package:rmarkdown` = function (input, output = xfun::with_ext(input, "Rmd")) 
{
  json <- jsonlite::fromJSON(input, simplifyDataFrame = FALSE)
  spec <- json$metadata$kernelspec
  lang <- spec$language
  if (is.null(lang) && identical(tolower(spec$name), "ir")) 
    lang <- "r"
  if (is.null(lang)) 
    lang <- "python"
  res <- character()
  for (cell in json$cells) {
    if (length(src <- unlist(cell$source)) == 0) 
      next
    src <- gsub("\n$", "", src)
    src <- switch(cell$cell_type, code = cell_chunk(src, lang, cell$metadata), raw = cell_raw(src, cell$metadata$format), src)
    res <- c(res, src, "")
  }
  res <- c("---", ipynb_yaml(json$metadata, input), "---\n", res)
  xfun::write_utf8(res, output)
  invisible(output)
}, function (input, output = xfun::with_ext(input, "Rmd")) 
{
  json <- jsonlite::fromJSON(input, simplifyDataFrame = FALSE)
  spec <- json$metadata$kernelspec
  lang <- spec$language
  if (is.null(lang) && identical(tolower(spec$name), "ir")) 
    lang <- "r"
  if (is.null(lang)) 
    lang <- "python"
  res <- character()
  for (cell in json$cells) {
    if (length(src <- unlist(cell$source)) == 0) 
      next
    src <- gsub("\n$", "", src)
    src <- switch(cell$cell_type, code = cell_chunk(src, lang, cell$metadata), raw = cell_raw(src, cell$metadata$format), src)
    res <- c(res, src, "")
  }
  res <- c("---", ipynb_yaml(json$metadata, input), "---\n", res)
  xfun::write_utf8(res, output)
  invisible(output)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
default_output_format
list(`package:rmarkdown` = function (input, output_yaml = NULL) 
{
  oldwd <- setwd(dirname(abs_path(input)))
  on.exit(setwd(oldwd), add = TRUE)
  input <- basename(input)
  input_lines <- read_utf8(input)
  format <- output_format_from_yaml_front_matter(input_lines, output_yaml = output_yaml)
  format_function <- eval(xfun::parse_only(format$name))
  format$options <- merge_lists(as.list(formals(format_function)), format$options, recursive = FALSE)
  format
}, function (input, output_yaml = NULL) 
{
  oldwd <- setwd(dirname(abs_path(input)))
  on.exit(setwd(oldwd), add = TRUE)
  input <- basename(input)
  input_lines <- read_utf8(input)
  format <- output_format_from_yaml_front_matter(input_lines, output_yaml = output_yaml)
  format_function <- eval(xfun::parse_only(format$name))
  format$options <- merge_lists(as.list(formals(format_function)), format$options, recursive = FALSE)
  format
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
default_site_generator
list(`package:rmarkdown` = function (input, output_format_filter = NULL, ...) 
{
  config <- site_config(input)
  if (is.null(config)) 
    stop("No site configuration (_site.yml) file found.")
  input_files <- function() {
    pattern <- sprintf("^[^_].*\\.%s$", if (isTRUE(config$autospin)) {
      "([Rr]|[Rr]?md)"
    }
    else {
      "[Rr]?md"
    })
    files <- list.files(input, pattern)
    if (is.character(config$autospin)) 
      files <- c(files, config$autospin)
    files[!grepl("^README\\.R?md$", files)]
  }
  render <- function(input_file, output_format, envir, quiet, ...) {
    outputs <- c()
    incremental <- !is.null(input_file)
    if (incremental) 
      files <- input_file
    else {
      files <- file.path(input, input_files())
    }
    sapply(files, function(x) {
      render_one <- if (isTRUE(config$new_session)) {
        render_new_session
      }
      else {
        render_current_session
      }
      if (!quiet) 
        message("\nRendering: ", x)
      file_output_format <- output_format
      if (is.function(output_format_filter)) {
        file_output_format <- output_format_filter(x, output_format)
      }
      output <- render_one(input = x, output_format = file_output_format, output_options = list(lib_dir = "site_libs", self_contained = FALSE), envir = envir, quiet = quiet)
      outputs <<- c(outputs, output)
      sidecar_files_dir <- knitr_files_dir(output)
      files_dir_info <- file.info(sidecar_files_dir)
      if (isTRUE(files_dir_info$isdir)) 
        outputs <<- c(outputs, sidecar_files_dir)
    })
    if (config$output_dir != ".") {
      output_dir <- file.path(input, config$output_dir)
      if (file.exists(output_dir)) {
        if (!incremental) {
          unlink(output_dir, recursive = TRUE)
          dir.create(output_dir)
        }
      }
      else {
        dir.create(output_dir)
      }
      for (output in outputs) {
        if (grepl("^.*_files$", output)) {
          cache_dir <- gsub("_files$", "_cache", output)
          if (dir_exists(cache_dir)) 
            next
        }
        output_dest <- file.path(output_dir, basename(output))
        if (dir_exists(output_dest)) 
          unlink(output_dest, recursive = TRUE)
        file.rename(output, output_dest)
      }
      lib_dir <- file.path(input, "site_libs")
      output_lib_dir <- file.path(output_dir, "site_libs")
      if (!file.exists(output_lib_dir)) 
        dir.create(output_lib_dir)
      libs <- list.files(lib_dir)
      for (lib in libs) file.copy(file.path(lib_dir, lib), output_lib_dir, recursive = TRUE)
      unlink(lib_dir, recursive = TRUE)
      copy_site_resources(input)
    }
    if (!quiet) {
      output_file <- ifelse(is.null(input_file), "index.html", file_with_ext(basename(input_file), "html"))
      if (config$output_dir != ".") 
        output_file <- file.path(config$output_dir, output_file)
      message("\nOutput created: ", output_file)
    }
  }
  clean <- function() {
    generated <- c()
    files <- input_files()
    html_files <- file_with_ext(files, "html")
    html_supporting <- paste0(knitr_files_dir(html_files), "/")
    generated <- c(generated, html_supporting)
    html_cache <- paste0(knitr_root_cache_dir(html_files), "/")
    generated <- c(generated, html_cache)
    if (config$output_dir == ".") {
      generated <- c(generated, html_files)
      generated <- c(generated, "site_libs/")
    }
    else {
      generated <- c(generated, paste0(config$output_dir, "/"))
    }
    generated[file.exists(file.path(input, generated))]
  }
  list(name = config$name, output_dir = config$output_dir, render = render, clean = clean)
}, function (input, output_format_filter = NULL, ...) 
{
  config <- site_config(input)
  if (is.null(config)) 
    stop("No site configuration (_site.yml) file found.")
  input_files <- function() {
    pattern <- sprintf("^[^_].*\\.%s$", if (isTRUE(config$autospin)) {
      "([Rr]|[Rr]?md)"
    }
    else {
      "[Rr]?md"
    })
    files <- list.files(input, pattern)
    if (is.character(config$autospin)) 
      files <- c(files, config$autospin)
    files[!grepl("^README\\.R?md$", files)]
  }
  render <- function(input_file, output_format, envir, quiet, ...) {
    outputs <- c()
    incremental <- !is.null(input_file)
    if (incremental) 
      files <- input_file
    else {
      files <- file.path(input, input_files())
    }
    sapply(files, function(x) {
      render_one <- if (isTRUE(config$new_session)) {
        render_new_session
      }
      else {
        render_current_session
      }
      if (!quiet) 
        message("\nRendering: ", x)
      file_output_format <- output_format
      if (is.function(output_format_filter)) {
        file_output_format <- output_format_filter(x, output_format)
      }
      output <- render_one(input = x, output_format = file_output_format, output_options = list(lib_dir = "site_libs", self_contained = FALSE), envir = envir, quiet = quiet)
      outputs <<- c(outputs, output)
      sidecar_files_dir <- knitr_files_dir(output)
      files_dir_info <- file.info(sidecar_files_dir)
      if (isTRUE(files_dir_info$isdir)) 
        outputs <<- c(outputs, sidecar_files_dir)
    })
    if (config$output_dir != ".") {
      output_dir <- file.path(input, config$output_dir)
      if (file.exists(output_dir)) {
        if (!incremental) {
          unlink(output_dir, recursive = TRUE)
          dir.create(output_dir)
        }
      }
      else {
        dir.create(output_dir)
      }
      for (output in outputs) {
        if (grepl("^.*_files$", output)) {
          cache_dir <- gsub("_files$", "_cache", output)
          if (dir_exists(cache_dir)) 
            next
        }
        output_dest <- file.path(output_dir, basename(output))
        if (dir_exists(output_dest)) 
          unlink(output_dest, recursive = TRUE)
        file.rename(output, output_dest)
      }
      lib_dir <- file.path(input, "site_libs")
      output_lib_dir <- file.path(output_dir, "site_libs")
      if (!file.exists(output_lib_dir)) 
        dir.create(output_lib_dir)
      libs <- list.files(lib_dir)
      for (lib in libs) file.copy(file.path(lib_dir, lib), output_lib_dir, recursive = TRUE)
      unlink(lib_dir, recursive = TRUE)
      copy_site_resources(input)
    }
    if (!quiet) {
      output_file <- ifelse(is.null(input_file), "index.html", file_with_ext(basename(input_file), "html"))
      if (config$output_dir != ".") 
        output_file <- file.path(config$output_dir, output_file)
      message("\nOutput created: ", output_file)
    }
  }
  clean <- function() {
    generated <- c()
    files <- input_files()
    html_files <- file_with_ext(files, "html")
    html_supporting <- paste0(knitr_files_dir(html_files), "/")
    generated <- c(generated, html_supporting)
    html_cache <- paste0(knitr_root_cache_dir(html_files), "/")
    generated <- c(generated, html_cache)
    if (config$output_dir == ".") {
      generated <- c(generated, html_files)
      generated <- c(generated, "site_libs/")
    }
    else {
      generated <- c(generated, paste0(config$output_dir, "/"))
    }
    generated[file.exists(file.path(input, generated))]
  }
  list(name = config$name, output_dir = config$output_dir, render = render, clean = clean)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
draft
list(`package:rmarkdown` = function (file, template, package = NULL, create_dir = "default", edit = TRUE) 
{
  if (!is.null(package)) {
    template_path = pkg_file("rmarkdown", "templates", template, package = package)
    if (!nzchar(template_path)) {
      stop("The template '", template, "' was not found in the ", package, " package")
    }
  }
  else {
    template_path <- template
  }
  template_yaml <- file.path(template_path, "template.yaml")
  if (!file.exists(template_yaml)) {
    stop("No template.yaml file found for template '", template, "'")
  }
  template_meta <- yaml_load_file(template_yaml)
  if (is.null(template_meta$name) || is.null(template_meta$description)) {
    stop("template.yaml must contain name and description fields")
  }
  if (identical(create_dir, "default")) 
    create_dir <- isTRUE(template_meta$create_dir)
  if (create_dir) {
    file <- xfun::sans_ext(file)
    if (dir_exists(file)) 
      stop("The directory '", file, "' already exists.")
    dir.create(file)
    file <- file.path(file, basename(file))
  }
  if (!identical(tolower(xfun::file_ext(file)), "rmd")) 
    file <- paste(file, ".Rmd", sep = "")
  if (file.exists(file)) 
    stop("The file '", file, "' already exists.")
  skeleton_files <- list.files(file.path(template_path, "skeleton"), full.names = TRUE)
  to <- dirname(file)
  for (f in skeleton_files) {
    if (file.exists(file.path(to, basename(f)))) 
      stop("The file '", basename(f), "' already exists")
    file.copy(from = f, to = to, overwrite = FALSE, recursive = TRUE)
  }
  file.rename(file.path(dirname(file), "skeleton.Rmd"), file)
  if (edit) 
    utils::file.edit(normalizePath(file))
  invisible(file)
}, function (file, template, package = NULL, create_dir = "default", edit = TRUE) 
{
  if (!is.null(package)) {
    template_path = pkg_file("rmarkdown", "templates", template, package = package)
    if (!nzchar(template_path)) {
      stop("The template '", template, "' was not found in the ", package, " package")
    }
  }
  else {
    template_path <- template
  }
  template_yaml <- file.path(template_path, "template.yaml")
  if (!file.exists(template_yaml)) {
    stop("No template.yaml file found for template '", template, "'")
  }
  template_meta <- yaml_load_file(template_yaml)
  if (is.null(template_meta$name) || is.null(template_meta$description)) {
    stop("template.yaml must contain name and description fields")
  }
  if (identical(create_dir, "default")) 
    create_dir <- isTRUE(template_meta$create_dir)
  if (create_dir) {
    file <- xfun::sans_ext(file)
    if (dir_exists(file)) 
      stop("The directory '", file, "' already exists.")
    dir.create(file)
    file <- file.path(file, basename(file))
  }
  if (!identical(tolower(xfun::file_ext(file)), "rmd")) 
    file <- paste(file, ".Rmd", sep = "")
  if (file.exists(file)) 
    stop("The file '", file, "' already exists.")
  skeleton_files <- list.files(file.path(template_path, "skeleton"), full.names = TRUE)
  to <- dirname(file)
  for (f in skeleton_files) {
    if (file.exists(file.path(to, basename(f)))) 
      stop("The file '", basename(f), "' already exists")
    file.copy(from = f, to = to, overwrite = FALSE, recursive = TRUE)
  }
  file.rename(file.path(dirname(file), "skeleton.Rmd"), file)
  if (edit) 
    utils::file.edit(normalizePath(file))
  invisible(file)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
find_external_resources
list(`package:rmarkdown` = function (input_file, encoding = "UTF-8") 
{
  ext <- tolower(xfun::file_ext(input_file))
  if (!(ext %in% c("md", "rmd", "qmd", "html", "htm", "r", "css"))) {
    stop("Resource discovery is only supported for R Markdown files or HTML files.")
  }
  if (!file.exists(input_file)) {
    stop("The input file file '", input_file, "' does not exist.")
  }
  discovered_resources <- data.frame(path = character(0), explicit = logical(0), web = logical(0))
  input_dir <- dirname(normalize_path(input_file))
  discover_single_resource <- function(path, explicit, web) {
    if (!(is.character(path) && length(path) == 1 && path != "." && path != ".." && file.exists(file.path(input_dir, path)))) 
      return(FALSE)
    ext <- tolower(xfun::file_ext(file.path(input_dir, path)))
    if (identical(ext, "r")) {
      discover_r_resources(file.path(input_dir, path), discover_single_resource)
    }
    else if (identical(ext, "css")) {
      discover_css_resources(file.path(input_dir, path), discover_single_resource)
    }
    if (!explicit && dir_exists(file.path(input_dir, path))) {
      return(FALSE)
    }
    discovered_resources <<- rbind(discovered_resources, data.frame(path = path, explicit = explicit, web = web, stringsAsFactors = FALSE))
    TRUE
  }
  if (ext %in% c("md", "rmd", "qmd")) {
    discover_rmd_resources(input_file, discover_single_resource)
  }
  else if (ext %in% c("htm", "html")) {
    discover_html_resources(input_file, discover_single_resource)
    sidecar_files_dir <- knitr_files_dir(input_file)
    files_dir_info <- file.info(sidecar_files_dir)
    if (isTRUE(files_dir_info$isdir)) {
      files_dir_prefix <- file.path(basename(sidecar_files_dir), "")
      files_dir_matches <- substr(discovered_resources$path, 1, nchar(files_dir_prefix)) == files_dir_prefix
      discovered_resources <- discovered_resources[!files_dir_matches, , drop = FALSE]
      discovered_resources <- rbind(discovered_resources, data.frame(path = files_dir_prefix, explicit = FALSE, web = TRUE, stringsAsFactors = FALSE))
    }
  }
  else if (ext == "r") {
    discover_r_resources(input_file, discover_single_resource)
  }
  else if (ext == "css") {
    discover_css_resources(input_file, discover_single_resource)
  }
  rownames(discovered_resources) <- NULL
  discovered_resources$path <- as.character(discovered_resources$path)
  has_prefix <- grepl("^\\./", discovered_resources$path)
  discovered_resources$path[has_prefix] <- substring(discovered_resources$path[has_prefix], 3)
  discovered_resources
}, function (input_file, encoding = "UTF-8") 
{
  ext <- tolower(xfun::file_ext(input_file))
  if (!(ext %in% c("md", "rmd", "qmd", "html", "htm", "r", "css"))) {
    stop("Resource discovery is only supported for R Markdown files or HTML files.")
  }
  if (!file.exists(input_file)) {
    stop("The input file file '", input_file, "' does not exist.")
  }
  discovered_resources <- data.frame(path = character(0), explicit = logical(0), web = logical(0))
  input_dir <- dirname(normalize_path(input_file))
  discover_single_resource <- function(path, explicit, web) {
    if (!(is.character(path) && length(path) == 1 && path != "." && path != ".." && file.exists(file.path(input_dir, path)))) 
      return(FALSE)
    ext <- tolower(xfun::file_ext(file.path(input_dir, path)))
    if (identical(ext, "r")) {
      discover_r_resources(file.path(input_dir, path), discover_single_resource)
    }
    else if (identical(ext, "css")) {
      discover_css_resources(file.path(input_dir, path), discover_single_resource)
    }
    if (!explicit && dir_exists(file.path(input_dir, path))) {
      return(FALSE)
    }
    discovered_resources <<- rbind(discovered_resources, data.frame(path = path, explicit = explicit, web = web, stringsAsFactors = FALSE))
    TRUE
  }
  if (ext %in% c("md", "rmd", "qmd")) {
    discover_rmd_resources(input_file, discover_single_resource)
  }
  else if (ext %in% c("htm", "html")) {
    discover_html_resources(input_file, discover_single_resource)
    sidecar_files_dir <- knitr_files_dir(input_file)
    files_dir_info <- file.info(sidecar_files_dir)
    if (isTRUE(files_dir_info$isdir)) {
      files_dir_prefix <- file.path(basename(sidecar_files_dir), "")
      files_dir_matches <- substr(discovered_resources$path, 1, nchar(files_dir_prefix)) == files_dir_prefix
      discovered_resources <- discovered_resources[!files_dir_matches, , drop = FALSE]
      discovered_resources <- rbind(discovered_resources, data.frame(path = files_dir_prefix, explicit = FALSE, web = TRUE, stringsAsFactors = FALSE))
    }
  }
  else if (ext == "r") {
    discover_r_resources(input_file, discover_single_resource)
  }
  else if (ext == "css") {
    discover_css_resources(input_file, discover_single_resource)
  }
  rownames(discovered_resources) <- NULL
  discovered_resources$path <- as.character(discovered_resources$path)
  has_prefix <- grepl("^\\./", discovered_resources$path)
  discovered_resources$path[has_prefix] <- substring(discovered_resources$path[has_prefix], 3)
  discovered_resources
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
find_pandoc
list(`package:rmarkdown` = function (cache = TRUE, dir = NULL, version = NULL) 
{
  if (!cache) 
    set_pandoc_info(NULL)
  if (!is.null(.pandoc$dir) && (is.null(version) || version == .pandoc$version)) 
    return(as.list(.pandoc))
  sources <- if (length(dir) == 0) 
    c(Sys.getenv("RSTUDIO_PANDOC"), dirname(find_program("pandoc")), "~/opt/pandoc")
  else dir
  sources <- path.expand(sources)
  versions <- lapply(sources, function(src) {
    if (dir_exists(src)) 
      get_pandoc_version(src)
    else numeric_version("0")
  })
  found_src <- NULL
  found_ver <- numeric_version("0")
  for (i in seq_along(sources)) {
    ver <- versions[[i]]
    if ((!is.null(version) && ver == version) || (is.null(version) && ver > found_ver)) {
      found_ver <- ver
      found_src <- sources[[i]]
    }
  }
  set_pandoc_info(found_src, found_ver)
  as.list(.pandoc)
}, function (cache = TRUE, dir = NULL, version = NULL) 
{
  if (!cache) 
    set_pandoc_info(NULL)
  if (!is.null(.pandoc$dir) && (is.null(version) || version == .pandoc$version)) 
    return(as.list(.pandoc))
  sources <- if (length(dir) == 0) 
    c(Sys.getenv("RSTUDIO_PANDOC"), dirname(find_program("pandoc")), "~/opt/pandoc")
  else dir
  sources <- path.expand(sources)
  versions <- lapply(sources, function(src) {
    if (dir_exists(src)) 
      get_pandoc_version(src)
    else numeric_version("0")
  })
  found_src <- NULL
  found_ver <- numeric_version("0")
  for (i in seq_along(sources)) {
    ver <- versions[[i]]
    if ((!is.null(version) && ver == version) || (is.null(version) && ver > found_ver)) {
      found_ver <- ver
      found_src <- sources[[i]]
    }
  }
  set_pandoc_info(found_src, found_ver)
  as.list(.pandoc)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
from_rmarkdown
list(`package:rmarkdown` = function (implicit_figures = TRUE, extensions = NULL) 
{
  extensions <- paste0(extensions, collapse = "")
  extensions <- gsub(" ", "", extensions)
  if (!implicit_figures && !grepl("implicit_figures", extensions)) 
    extensions <- paste0("-implicit_figures", extensions)
  rmarkdown_format(extensions)
}, function (implicit_figures = TRUE, extensions = NULL) 
{
  extensions <- paste0(extensions, collapse = "")
  extensions <- gsub(" ", "", extensions)
  if (!implicit_figures && !grepl("implicit_figures", extensions)) 
    extensions <- paste0("-implicit_figures", extensions)
  rmarkdown_format(extensions)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
github_document
list(`package:rmarkdown` = function (toc = FALSE, toc_depth = 3, number_sections = FALSE, math_method = "webtex", preserve_yaml = FALSE, fig_width = 7, fig_height = 5, dev = "png", df_print = "default", includes = NULL, md_extensions = NULL, hard_line_breaks = TRUE, pandoc_args = NULL, html_preview = TRUE, keep_html = FALSE) 
{
  pandoc_args <- c(pandoc_args, "--template", pkg_file_arg("rmarkdown/templates/github_document/resources/default.md"))
  pandoc2 <- pandoc2.0()
  if (pandoc2) {
    variant <- "gfm"
  }
  else {
    variant <- "markdown_github"
  }
  if (!hard_line_breaks) 
    variant <- paste0(variant, "-hard_line_breaks")
  if (!pandoc_available("2.11.2")) 
    pandoc_args <- c("--atx-headers", pandoc_args)
  if ((toc || number_sections) && !isTRUE(grepl("gfm_auto_identifiers", md_extensions))) {
    md_extensions <- c(md_extensions, "+gfm_auto_identifiers")
  }
  if (!is.null(math_method)) {
    math <- check_math_argument(math_method)
    if (math$engine != "webtex") {
      stop("Markdown output format only support 'webtex' for math engine")
    }
    if (is.null(math$url)) {
      math$url <- "https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;"
    }
    math <- add_math_support(math, NULL, NULL, NULL)
    pandoc_args <- c(pandoc_args, math$args)
  }
  format <- md_document(variant = variant, toc = toc, toc_depth = toc_depth, number_sections = number_sections, preserve_yaml = preserve_yaml, fig_width = fig_width, fig_height = fig_height, dev = dev, df_print = df_print, includes = includes, md_extensions = md_extensions, pandoc_args = pandoc_args)
  post <- format$post_processor
  format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    if (is.function(post)) 
      output_file <- post(metadata, input_file, output_file, clean, verbose)
    if (html_preview) {
      css <- pkg_file_arg("rmarkdown/templates/github_document/resources/github.css")
      args <- c("--standalone", "--self-contained", "--highlight-style", "pygments", "--template", pkg_file_arg("rmarkdown/templates/github_document/resources/preview.html"), "--variable", paste0("github-markdown-css:", css), if (pandoc2) c("--metadata", "pagetitle=PREVIEW"), if (!is.null(math_method)) math$args)
      preview_file <- file_with_ext(output_file, "html")
      pandoc_convert(input = output_file, to = "html", from = variant, output = preview_file, options = args, verbose = verbose)
      if (!keep_html) {
        preview_dir <- Sys.getenv("RMARKDOWN_PREVIEW_DIR", unset = NA)
        if (!is.na(preview_dir)) {
          relocated_preview_file <- tempfile("preview-", preview_dir, ".html")
          file.copy(preview_file, relocated_preview_file)
          file.remove(preview_file)
          preview_file <- relocated_preview_file
        }
      }
      if (verbose) 
        message("\nPreview created: ", preview_file)
    }
    output_file
  }
  format
}, function (toc = FALSE, toc_depth = 3, number_sections = FALSE, math_method = "webtex", preserve_yaml = FALSE, fig_width = 7, fig_height = 5, dev = "png", df_print = "default", includes = NULL, md_extensions = NULL, hard_line_breaks = TRUE, pandoc_args = NULL, html_preview = TRUE, keep_html = FALSE) 
{
  pandoc_args <- c(pandoc_args, "--template", pkg_file_arg("rmarkdown/templates/github_document/resources/default.md"))
  pandoc2 <- pandoc2.0()
  if (pandoc2) {
    variant <- "gfm"
  }
  else {
    variant <- "markdown_github"
  }
  if (!hard_line_breaks) 
    variant <- paste0(variant, "-hard_line_breaks")
  if (!pandoc_available("2.11.2")) 
    pandoc_args <- c("--atx-headers", pandoc_args)
  if ((toc || number_sections) && !isTRUE(grepl("gfm_auto_identifiers", md_extensions))) {
    md_extensions <- c(md_extensions, "+gfm_auto_identifiers")
  }
  if (!is.null(math_method)) {
    math <- check_math_argument(math_method)
    if (math$engine != "webtex") {
      stop("Markdown output format only support 'webtex' for math engine")
    }
    if (is.null(math$url)) {
      math$url <- "https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;"
    }
    math <- add_math_support(math, NULL, NULL, NULL)
    pandoc_args <- c(pandoc_args, math$args)
  }
  format <- md_document(variant = variant, toc = toc, toc_depth = toc_depth, number_sections = number_sections, preserve_yaml = preserve_yaml, fig_width = fig_width, fig_height = fig_height, dev = dev, df_print = df_print, includes = includes, md_extensions = md_extensions, pandoc_args = pandoc_args)
  post <- format$post_processor
  format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    if (is.function(post)) 
      output_file <- post(metadata, input_file, output_file, clean, verbose)
    if (html_preview) {
      css <- pkg_file_arg("rmarkdown/templates/github_document/resources/github.css")
      args <- c("--standalone", "--self-contained", "--highlight-style", "pygments", "--template", pkg_file_arg("rmarkdown/templates/github_document/resources/preview.html"), "--variable", paste0("github-markdown-css:", css), if (pandoc2) c("--metadata", "pagetitle=PREVIEW"), if (!is.null(math_method)) math$args)
      preview_file <- file_with_ext(output_file, "html")
      pandoc_convert(input = output_file, to = "html", from = variant, output = preview_file, options = args, verbose = verbose)
      if (!keep_html) {
        preview_dir <- Sys.getenv("RMARKDOWN_PREVIEW_DIR", unset = NA)
        if (!is.na(preview_dir)) {
          relocated_preview_file <- tempfile("preview-", preview_dir, ".html")
          file.copy(preview_file, relocated_preview_file)
          file.remove(preview_file)
          preview_file <- relocated_preview_file
        }
      }
      if (verbose) 
        message("\nPreview created: ", preview_file)
    }
    output_file
  }
  format
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_dependency_bootstrap
list(`package:rmarkdown` = function (theme) 
{
  theme <- resolve_theme(theme)
  if (is_bs_theme(theme)) {
    h1_size <- if ("3" %in% theme_version(theme)) 
      "font-size-h1"
    else "h1-font-size"
    theme <- bslib::bs_add_rules(theme, c(paste0("h1.title {margin-top: 1.25rem; font-size: 1.15 * $", h1_size, "}"), "pre:not([class]) { background-color: $body-bg }"))
    return(bslib::bs_theme_dependencies(theme))
  }
  htmlDependency(name = "bootstrap", version = "3.3.5", src = pkg_file("rmd/h/bootstrap"), meta = list(viewport = "width=device-width, initial-scale=1"), script = c("js/bootstrap.min.js", "shim/html5shiv.min.js", "shim/respond.min.js"), stylesheet = paste0("css/", theme, ".min.css"), head = format(tags$style(HTML("h1 {font-size: 34px;}\n       h1.title {font-size: 38px;}\n       h2 {font-size: 30px;}\n       h3 {font-size: 24px;}\n       h4 {font-size: 18px;}\n       h5 {font-size: 16px;}\n       h6 {font-size: 12px;}\n       code {color: inherit; background-color: rgba(0, 0, 0, 0.04);}\n       pre:not([class]) { background-color: white }"))))
}, function (theme) 
{
  theme <- resolve_theme(theme)
  if (is_bs_theme(theme)) {
    h1_size <- if ("3" %in% theme_version(theme)) 
      "font-size-h1"
    else "h1-font-size"
    theme <- bslib::bs_add_rules(theme, c(paste0("h1.title {margin-top: 1.25rem; font-size: 1.15 * $", h1_size, "}"), "pre:not([class]) { background-color: $body-bg }"))
    return(bslib::bs_theme_dependencies(theme))
  }
  htmlDependency(name = "bootstrap", version = "3.3.5", src = pkg_file("rmd/h/bootstrap"), meta = list(viewport = "width=device-width, initial-scale=1"), script = c("js/bootstrap.min.js", "shim/html5shiv.min.js", "shim/respond.min.js"), stylesheet = paste0("css/", theme, ".min.css"), head = format(tags$style(HTML("h1 {font-size: 34px;}\n       h1.title {font-size: 38px;}\n       h2 {font-size: 30px;}\n       h3 {font-size: 24px;}\n       h4 {font-size: 18px;}\n       h5 {font-size: 16px;}\n       h6 {font-size: 12px;}\n       code {color: inherit; background-color: rgba(0, 0, 0, 0.04);}\n       pre:not([class]) { background-color: white }"))))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_dependency_codefolding_lua
list(`package:rmarkdown` = function () 
{
  htmlDependency("codefolding-lua", version = "1.1", src = pkg_file("rmd/h/navigation-1.1"), stylesheet = "codefolding-lua.css", all_files = FALSE)
}, function () 
{
  htmlDependency("codefolding-lua", version = "1.1", src = pkg_file("rmd/h/navigation-1.1"), stylesheet = "codefolding-lua.css", all_files = FALSE)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_dependency_font_awesome
list(`package:rmarkdown` = function () 
{
  htmlDependency("font-awesome", "5.1.0", src = pkg_file("rmd/h/fontawesome"), stylesheet = c("css/all.css", "css/v4-shims.css"))
}, function () 
{
  htmlDependency("font-awesome", "5.1.0", src = pkg_file("rmd/h/fontawesome"), stylesheet = c("css/all.css", "css/v4-shims.css"))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_dependency_highlightjs
list(`package:rmarkdown` = function (highlight) 
{
  htmlDependency("highlightjs", version = "9.12.0", src = pkg_file("rmd/h/highlightjs"), script = "highlight.js", stylesheet = paste0(highlight, ".css"))
}, function (highlight) 
{
  htmlDependency("highlightjs", version = "9.12.0", src = pkg_file("rmd/h/highlightjs"), script = "highlight.js", stylesheet = paste0(highlight, ".css"))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_dependency_ionicons
list(`package:rmarkdown` = function () 
{
  htmlDependency(name = "ionicons", version = "2.0.1", src = pkg_file("rmd/h/ionicons"), stylesheet = "css/ionicons.min.css")
}, function () 
{
  htmlDependency(name = "ionicons", version = "2.0.1", src = pkg_file("rmd/h/ionicons"), stylesheet = "css/ionicons.min.css")
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_dependency_jquery
list(`package:rmarkdown` = function () 
{
  jquerylib::jquery_core(major_version = getOption("rmarkdown.jquery.version", 3))
}, function () 
{
  jquerylib::jquery_core(major_version = getOption("rmarkdown.jquery.version", 3))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_dependency_jqueryui
list(`package:rmarkdown` = function () 
{
  htmlDependency(name = "jqueryui", version = "1.11.4", src = pkg_file("rmd/h/jqueryui"), script = "jquery-ui.min.js")
}, function () 
{
  htmlDependency(name = "jqueryui", version = "1.11.4", src = pkg_file("rmd/h/jqueryui"), script = "jquery-ui.min.js")
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_dependency_pagedtable
list(`package:rmarkdown` = function () 
{
  htmlDependency("pagedtable", version = "1.1", src = pkg_file("rmd/h/pagedtable-1.1"), script = "js/pagedtable.js", stylesheet = "css/pagedtable.css")
}, function () 
{
  htmlDependency("pagedtable", version = "1.1", src = pkg_file("rmd/h/pagedtable-1.1"), script = "js/pagedtable.js", stylesheet = "css/pagedtable.css")
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_dependency_tabset
list(`package:rmarkdown` = function () 
{
  htmlDependency("tabset", version = "1.0", src = pkg_file("rmd/h/tabset"), script = "tabset.js", stylesheet = "tabset.css", all_files = FALSE)
}, function () 
{
  htmlDependency("tabset", version = "1.0", src = pkg_file("rmd/h/tabset"), script = "tabset.js", stylesheet = "tabset.css", all_files = FALSE)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_dependency_tocify
list(`package:rmarkdown` = function () 
{
  htmlDependency(name = "tocify", version = "1.9.1", src = pkg_file("rmd/h/tocify"), script = "jquery.tocify.js", stylesheet = "jquery.tocify.css")
}, function () 
{
  htmlDependency(name = "tocify", version = "1.9.1", src = pkg_file("rmd/h/tocify"), script = "jquery.tocify.js", stylesheet = "jquery.tocify.css")
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_document
list(`package:rmarkdown` = function (toc = FALSE, toc_depth = 3, toc_float = FALSE, number_sections = FALSE, anchor_sections = FALSE, section_divs = TRUE, fig_width = 7, fig_height = 5, fig_retina = 2, fig_caption = TRUE, dev = "png", df_print = "default", code_folding = c("none", "show", "hide"), code_download = FALSE, self_contained = TRUE, theme = "default", highlight = "default", highlight_downlit = FALSE, math_method = "default", mathjax = "default", template = "default", extra_dependencies = NULL, 
                                     css = NULL, includes = NULL, keep_md = FALSE, lib_dir = NULL, md_extensions = NULL, pandoc_args = NULL, ...) 
{
  args <- c("--standalone")
  lua_filters <- c()
  if (section_divs) 
    args <- c(args, "--section-divs")
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  theme <- resolve_theme(theme)
  if (toc && !identical(toc_float, FALSE)) {
    if (is.null(theme)) 
      stop("You must use a theme when specifying the 'toc_float' option")
    toc_float_options <- list(collapsed = TRUE, smooth_scroll = TRUE, print = TRUE)
    if (is.list(toc_float)) {
      toc_float_options <- merge_lists(toc_float_options, toc_float)
      toc_float <- TRUE
    }
    else if (!isTRUE(toc_float)) {
      stop("toc_float must be a logical or a list with options")
    }
    extra_dependencies <- append(extra_dependencies, list(html_dependency_jquery(), html_dependency_jqueryui(), html_dependency_tocify()))
    args <- c(args, pandoc_variable_arg("toc_float", "1"))
    selectors <- paste0("h", seq(1, toc_depth), collapse = ",")
    args <- c(args, pandoc_variable_arg("toc_selectors", selectors))
    if (toc_float_options$collapsed) 
      args <- c(args, pandoc_variable_arg("toc_collapsed", "1"))
    if (toc_float_options$smooth_scroll) 
      args <- c(args, pandoc_variable_arg("toc_smooth_scroll", "1"))
    if (toc_float_options$print) 
      args <- c(args, pandoc_variable_arg("toc_print", "1"))
  }
  template_file <- if (identical(template, "default")) {
    pkg_file("rmd/h/default.html")
  }
  else template
  if (!is.null(template_file)) 
    args <- c(args, "--template", pandoc_path_arg(template_file))
  code_folding <- match.arg(code_folding)
  if (!is.null(theme)) {
    code_menu <- !identical(code_folding, "none") || code_download
    source_embed <- code_download
    extra_dependencies <- append(extra_dependencies, list(html_dependency_jquery(), html_dependency_navigation(code_menu = code_menu, source_embed = source_embed)))
  }
  if (highlight_downlit && !xfun::loadable("downlit")) {
    stop("highlight_downlit=TRUE requires the downlit package to be installed.", call. = FALSE)
  }
  args <- c(args, pandoc_html_highlight_args(template, highlight, highlight_downlit))
  extra_dependencies <- append(extra_dependencies, if (identical(template, "default") && is_highlightjs(highlight)) {
    list(html_dependency_highlightjs(highlight))
  }
  else if (!is.null(highlight)) {
    list(html_dependency_accessible_code_block())
  })
  if (number_sections) 
    args <- c(args, "--number-sections")
  exit_actions <- list()
  on_exit <- function() {
    for (action in exit_actions) try(action())
  }
  source_code <- NULL
  source_file <- NULL
  pre_knit <- function(input, ...) {
    if (code_download) {
      source_file <<- basename(input)
      source_code <<- paste0("<div id=\"rmd-source-code\">", xfun::base64_encode(input), "</div>")
    }
  }
  if (identical(df_print, "paged")) {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_pagedtable()))
  }
  components <- add_anchor_sections(anchor_sections, section_divs)
  args <- c(args, components$args)
  lua_filters <- c(lua_filters, components$lua_filters)
  extra_dependencies <- append(extra_dependencies, components$extra_dependencies)
  post_knit <- function(metadata, input_file, runtime, ...) {
    args <- c()
    if (!is.null(theme)) {
      navbar <- file.path(normalize_path(dirname(input_file)), "_navbar.html")
      if (!file.exists(navbar)) {
        navbar_yaml <- file.path(dirname(navbar), "_navbar.yml")
        if (file.exists(navbar_yaml)) 
          navbar <- navbar_html_from_yaml(navbar_yaml)
        config <- site_config(input_file)
        if (!is.null(config) && !is.null(config$navbar)) 
          navbar <- navbar_html(config$navbar)
      }
      if (file.exists(navbar)) {
        includes <- list(before_body = navbar)
        args <- c(args, includes_to_pandoc_args(includes, filter = if (is_shiny_classic(runtime)) function(x) normalize_path(x, mustWork = FALSE) else identity))
        args <- c(args, pandoc_variable_arg("navbar", "1"))
        iconDeps <- navbar_icon_dependencies(navbar)
        if (length(iconDeps) > 0) 
          knitr::knit_meta_add(list(iconDeps))
      }
    }
    args
  }
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    if (is.null(lib_dir)) 
      lib_dir <- files_dir
    args <- c()
    code_menu <- FALSE
    if (code_folding %in% c("show", "hide")) {
      if (is.null(theme)) 
        stop("You must use a theme when specifying the 'code_folding' option")
      args <- c(args, pandoc_variable_arg("code_folding", code_folding))
      code_menu <- TRUE
    }
    if (code_download) {
      if (is.null(theme)) 
        stop("You must use a theme when specifying the 'code_download' option")
      args <- c(args, pandoc_variable_arg("source_embed", source_file))
      sourceCodeFile <- tempfile(fileext = ".html")
      write_utf8(source_code, sourceCodeFile)
      args <- c(args, pandoc_include_args(after_body = sourceCodeFile))
      code_menu <- TRUE
    }
    if (code_menu) 
      args <- c(args, pandoc_variable_arg("code_menu", "1"))
    args <- c(args, includes_to_pandoc_args(includes, filter = if (is_shiny_classic(runtime)) function(x) normalize_path(x, mustWork = FALSE) else identity))
    args
  }
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    if (highlight_downlit) {
      output_file <- downlit::downlit_html_path(output_file, output_file, classes = downlit::classes_pandoc())
    }
    output_file
  }
  output_format(knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md, dev), pandoc = pandoc_options(to = "html", from = from_rmarkdown(fig_caption, md_extensions), args = args, lua_filters = lua_filters), keep_md = keep_md, clean_supporting = self_contained, df_print = df_print, pre_knit = pre_knit, post_knit = post_knit, pre_processor = pre_processor, post_processor = post_processor, on_exit = on_exit, base_format = html_document_base(theme = theme, self_contained = self_contained, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         lib_dir = lib_dir, math_method = math_method, mathjax = mathjax, template = template, pandoc_args = pandoc_args, extra_dependencies = extra_dependencies, css = css, ...))
}, function (toc = FALSE, toc_depth = 3, toc_float = FALSE, number_sections = FALSE, anchor_sections = FALSE, section_divs = TRUE, fig_width = 7, fig_height = 5, fig_retina = 2, fig_caption = TRUE, dev = "png", df_print = "default", code_folding = c("none", "show", "hide"), code_download = FALSE, self_contained = TRUE, theme = "default", highlight = "default", highlight_downlit = FALSE, math_method = "default", mathjax = "default", template = "default", extra_dependencies = NULL, css = NULL, includes = NULL, 
             keep_md = FALSE, lib_dir = NULL, md_extensions = NULL, pandoc_args = NULL, ...) 
{
  args <- c("--standalone")
  lua_filters <- c()
  if (section_divs) 
    args <- c(args, "--section-divs")
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  theme <- resolve_theme(theme)
  if (toc && !identical(toc_float, FALSE)) {
    if (is.null(theme)) 
      stop("You must use a theme when specifying the 'toc_float' option")
    toc_float_options <- list(collapsed = TRUE, smooth_scroll = TRUE, print = TRUE)
    if (is.list(toc_float)) {
      toc_float_options <- merge_lists(toc_float_options, toc_float)
      toc_float <- TRUE
    }
    else if (!isTRUE(toc_float)) {
      stop("toc_float must be a logical or a list with options")
    }
    extra_dependencies <- append(extra_dependencies, list(html_dependency_jquery(), html_dependency_jqueryui(), html_dependency_tocify()))
    args <- c(args, pandoc_variable_arg("toc_float", "1"))
    selectors <- paste0("h", seq(1, toc_depth), collapse = ",")
    args <- c(args, pandoc_variable_arg("toc_selectors", selectors))
    if (toc_float_options$collapsed) 
      args <- c(args, pandoc_variable_arg("toc_collapsed", "1"))
    if (toc_float_options$smooth_scroll) 
      args <- c(args, pandoc_variable_arg("toc_smooth_scroll", "1"))
    if (toc_float_options$print) 
      args <- c(args, pandoc_variable_arg("toc_print", "1"))
  }
  template_file <- if (identical(template, "default")) {
    pkg_file("rmd/h/default.html")
  }
  else template
  if (!is.null(template_file)) 
    args <- c(args, "--template", pandoc_path_arg(template_file))
  code_folding <- match.arg(code_folding)
  if (!is.null(theme)) {
    code_menu <- !identical(code_folding, "none") || code_download
    source_embed <- code_download
    extra_dependencies <- append(extra_dependencies, list(html_dependency_jquery(), html_dependency_navigation(code_menu = code_menu, source_embed = source_embed)))
  }
  if (highlight_downlit && !xfun::loadable("downlit")) {
    stop("highlight_downlit=TRUE requires the downlit package to be installed.", call. = FALSE)
  }
  args <- c(args, pandoc_html_highlight_args(template, highlight, highlight_downlit))
  extra_dependencies <- append(extra_dependencies, if (identical(template, "default") && is_highlightjs(highlight)) {
    list(html_dependency_highlightjs(highlight))
  }
  else if (!is.null(highlight)) {
    list(html_dependency_accessible_code_block())
  })
  if (number_sections) 
    args <- c(args, "--number-sections")
  exit_actions <- list()
  on_exit <- function() {
    for (action in exit_actions) try(action())
  }
  source_code <- NULL
  source_file <- NULL
  pre_knit <- function(input, ...) {
    if (code_download) {
      source_file <<- basename(input)
      source_code <<- paste0("<div id=\"rmd-source-code\">", xfun::base64_encode(input), "</div>")
    }
  }
  if (identical(df_print, "paged")) {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_pagedtable()))
  }
  components <- add_anchor_sections(anchor_sections, section_divs)
  args <- c(args, components$args)
  lua_filters <- c(lua_filters, components$lua_filters)
  extra_dependencies <- append(extra_dependencies, components$extra_dependencies)
  post_knit <- function(metadata, input_file, runtime, ...) {
    args <- c()
    if (!is.null(theme)) {
      navbar <- file.path(normalize_path(dirname(input_file)), "_navbar.html")
      if (!file.exists(navbar)) {
        navbar_yaml <- file.path(dirname(navbar), "_navbar.yml")
        if (file.exists(navbar_yaml)) 
          navbar <- navbar_html_from_yaml(navbar_yaml)
        config <- site_config(input_file)
        if (!is.null(config) && !is.null(config$navbar)) 
          navbar <- navbar_html(config$navbar)
      }
      if (file.exists(navbar)) {
        includes <- list(before_body = navbar)
        args <- c(args, includes_to_pandoc_args(includes, filter = if (is_shiny_classic(runtime)) function(x) normalize_path(x, mustWork = FALSE) else identity))
        args <- c(args, pandoc_variable_arg("navbar", "1"))
        iconDeps <- navbar_icon_dependencies(navbar)
        if (length(iconDeps) > 0) 
          knitr::knit_meta_add(list(iconDeps))
      }
    }
    args
  }
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    if (is.null(lib_dir)) 
      lib_dir <- files_dir
    args <- c()
    code_menu <- FALSE
    if (code_folding %in% c("show", "hide")) {
      if (is.null(theme)) 
        stop("You must use a theme when specifying the 'code_folding' option")
      args <- c(args, pandoc_variable_arg("code_folding", code_folding))
      code_menu <- TRUE
    }
    if (code_download) {
      if (is.null(theme)) 
        stop("You must use a theme when specifying the 'code_download' option")
      args <- c(args, pandoc_variable_arg("source_embed", source_file))
      sourceCodeFile <- tempfile(fileext = ".html")
      write_utf8(source_code, sourceCodeFile)
      args <- c(args, pandoc_include_args(after_body = sourceCodeFile))
      code_menu <- TRUE
    }
    if (code_menu) 
      args <- c(args, pandoc_variable_arg("code_menu", "1"))
    args <- c(args, includes_to_pandoc_args(includes, filter = if (is_shiny_classic(runtime)) function(x) normalize_path(x, mustWork = FALSE) else identity))
    args
  }
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    if (highlight_downlit) {
      output_file <- downlit::downlit_html_path(output_file, output_file, classes = downlit::classes_pandoc())
    }
    output_file
  }
  output_format(knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md, dev), pandoc = pandoc_options(to = "html", from = from_rmarkdown(fig_caption, md_extensions), args = args, lua_filters = lua_filters), keep_md = keep_md, clean_supporting = self_contained, df_print = df_print, pre_knit = pre_knit, post_knit = post_knit, pre_processor = pre_processor, post_processor = post_processor, on_exit = on_exit, base_format = html_document_base(theme = theme, self_contained = self_contained, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         lib_dir = lib_dir, math_method = math_method, mathjax = mathjax, template = template, pandoc_args = pandoc_args, extra_dependencies = extra_dependencies, css = css, ...))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_document_base
list(`package:rmarkdown` = function (theme = NULL, self_contained = TRUE, lib_dir = NULL, math_method = "default", mathjax = "default", pandoc_args = NULL, template = "default", dependency_resolver = NULL, copy_resources = FALSE, extra_dependencies = NULL, css = NULL, bootstrap_compatible = FALSE, ...) 
{
  if (is.null(dependency_resolver)) 
    dependency_resolver <- html_dependency_resolver
  args <- c()
  math <- mathjax_to_math(mathjax, math_method)
  math <- check_math_argument(math)
  if (self_contained) {
    if (copy_resources) 
      stop("Local resource copying is incompatible with self-contained documents.")
    validate_self_contained(math)
    args <- c(args, "--self-contained")
  }
  args <- c(args, pandoc_args)
  preserved_chunks <- character()
  output_dir <- ""
  theme <- resolve_theme(theme)
  old_theme <- NULL
  pre_knit <- function(input, ...) {
    if (is_bs_theme(theme)) {
      for (f in css) theme <<- bslib::bs_add_rules(theme, xfun::read_utf8(f))
      css <<- NULL
      old_theme <<- bslib::bs_global_set(theme)
    }
  }
  post_knit <- function(metadata, input_file, runtime, ...) {
  }
  on_exit <- function() {
    if (is_bs_theme(theme)) 
      bslib::bs_global_set(old_theme)
  }
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    args <- c()
    if (is.null(lib_dir)) 
      lib_dir <<- files_dir
    output_dir <<- output_dir
    if (!is.null(theme)) {
      theme_arg <- if (is.list(theme)) 
        "bootstrap"
      else theme
      args <- c(args, pandoc_variable_arg("theme", theme_arg))
    }
    for (f in css) {
      if (grepl("\\.s[ac]ss$", f)) {
        if (!xfun::loadable("sass")) {
          stop2("Using `.sass` or `.scss` file in `css` argument requires the sass package.")
        }
        f <- sass::sass(sass::sass_file(f), output = sass::output_template(basename = xfun::sans_ext(basename(f)), dirname = "sass", path = lib_dir), options = sass::sass_options(output_style = "compressed"))
        f <- normalized_relative_to(output_dir, f)
      }
      args <- c(args, "--css", pandoc_path_arg(f, backslash = FALSE))
    }
    math_support <- add_math_support(math, template, lib_dir, output_dir)
    args <- c(args, math_support$args)
    extra_dependencies <- c(extra_dependencies, math_support$extra_dependencies)
    format_deps <- list()
    format_deps <- append(format_deps, html_dependency_header_attrs())
    if (!is.null(theme)) {
      format_deps <- append(format_deps, list(html_dependency_jquery()))
      if (is_bs_theme(theme)) {
        theme <- bslib::bs_global_get()
      }
      bootstrap_deps <- if (is_bs_theme(theme) && is_shiny(runtime, metadata[["server"]])) {
        list(shiny_bootstrap_lib(theme))
      }
      else {
        bootstrap_dependencies(theme)
      }
      format_deps <- append(format_deps, htmltools::resolveDependencies(bootstrap_deps))
    }
    else if (isTRUE(bootstrap_compatible) && is_shiny(runtime, metadata[["server"]])) {
      format_deps <- append(format_deps, bootstrap_dependencies("bootstrap"))
    }
    format_deps <- append(format_deps, extra_dependencies)
    extras <- html_extras_for_document(knit_meta, runtime, dependency_resolver, format_deps)
    args <- c(args, pandoc_html_extras_args(extras, self_contained, lib_dir, output_dir))
    preserved_chunks <<- extract_preserve_chunks(input_file)
    args
  }
  intermediates_generator <- function(original_input, intermediates_dir) {
    copy_render_intermediates(original_input, intermediates_dir, !self_contained)
  }
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    if (identical(math_method, "r-katex") && xfun::pkg_available("katex", "1.4.0")) {
      katex::render_math_in_html(output_file, output = output_file)
    }
    output_str <- read_utf8(output_file)
    s1 <- "<span class=\"sc\">|</span><span class=\"er\">&gt;</span>"
    s2 <- "<span class=\"ot\">=</span><span class=\"er\">&gt;</span>"
    if ((length(preserved_chunks) == 0 && !isTRUE(copy_resources) && self_contained) && !length(c(grep(s1, output_str, fixed = TRUE), grep(s2, output_str, fixed = TRUE)))) 
      return(output_file)
    if (length(preserved_chunks) > 0) {
      for (i in names(preserved_chunks)) {
        output_str <- gsub(paste0("<p>", i, "</p>"), i, output_str, fixed = TRUE, useBytes = TRUE)
        output_str <- gsub(paste0(" id=\"[^\"]*?", i, "[^\"]*?\" "), " ", output_str, useBytes = TRUE)
      }
      output_str <- restorePreserveChunks(output_str, preserved_chunks)
    }
    if (copy_resources) {
      output_str <- copy_html_resources(one_string(output_str), lib_dir, output_dir)
    }
    else if (!self_contained) {
      image_relative <- function(img_src, src) {
        in_file <- utils::URLdecode(src)
        if (grepl("^[.][.]", in_file)) 
          return(img_src)
        if (length(in_file) && file.exists(in_file)) {
          img_src <- sub(src, utils::URLencode(normalized_relative_to(output_dir, in_file)), img_src, fixed = TRUE)
        }
        img_src
      }
      output_str <- process_images(output_str, image_relative)
    }
    output_str <- gsub(s1, "<span class=\"sc\">|&gt;</span>", output_str, fixed = TRUE)
    output_str <- gsub(s2, "<span class=\"ot\">=&gt;</span>", output_str, fixed = TRUE)
    write_utf8(output_str, output_file)
    output_file
  }
  if (!is.null(theme)) {
    bs3 <- identical("3", theme_version(theme))
    args <- c(args, pandoc_variable_arg("bs3", bs3))
  }
  output_format(knitr = NULL, pandoc = pandoc_options(to = "html", from = NULL, args = args, lua_filters = pkg_file_lua(c("pagebreak.lua", "latex-div.lua"))), keep_md = FALSE, clean_supporting = FALSE, pre_knit = pre_knit, post_knit = post_knit, on_exit = on_exit, pre_processor = pre_processor, intermediates_generator = intermediates_generator, post_processor = post_processor)
}, function (theme = NULL, self_contained = TRUE, lib_dir = NULL, math_method = "default", mathjax = "default", pandoc_args = NULL, template = "default", dependency_resolver = NULL, copy_resources = FALSE, extra_dependencies = NULL, css = NULL, bootstrap_compatible = FALSE, ...) 
{
  if (is.null(dependency_resolver)) 
    dependency_resolver <- html_dependency_resolver
  args <- c()
  math <- mathjax_to_math(mathjax, math_method)
  math <- check_math_argument(math)
  if (self_contained) {
    if (copy_resources) 
      stop("Local resource copying is incompatible with self-contained documents.")
    validate_self_contained(math)
    args <- c(args, "--self-contained")
  }
  args <- c(args, pandoc_args)
  preserved_chunks <- character()
  output_dir <- ""
  theme <- resolve_theme(theme)
  old_theme <- NULL
  pre_knit <- function(input, ...) {
    if (is_bs_theme(theme)) {
      for (f in css) theme <<- bslib::bs_add_rules(theme, xfun::read_utf8(f))
      css <<- NULL
      old_theme <<- bslib::bs_global_set(theme)
    }
  }
  post_knit <- function(metadata, input_file, runtime, ...) {
  }
  on_exit <- function() {
    if (is_bs_theme(theme)) 
      bslib::bs_global_set(old_theme)
  }
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    args <- c()
    if (is.null(lib_dir)) 
      lib_dir <<- files_dir
    output_dir <<- output_dir
    if (!is.null(theme)) {
      theme_arg <- if (is.list(theme)) 
        "bootstrap"
      else theme
      args <- c(args, pandoc_variable_arg("theme", theme_arg))
    }
    for (f in css) {
      if (grepl("\\.s[ac]ss$", f)) {
        if (!xfun::loadable("sass")) {
          stop2("Using `.sass` or `.scss` file in `css` argument requires the sass package.")
        }
        f <- sass::sass(sass::sass_file(f), output = sass::output_template(basename = xfun::sans_ext(basename(f)), dirname = "sass", path = lib_dir), options = sass::sass_options(output_style = "compressed"))
        f <- normalized_relative_to(output_dir, f)
      }
      args <- c(args, "--css", pandoc_path_arg(f, backslash = FALSE))
    }
    math_support <- add_math_support(math, template, lib_dir, output_dir)
    args <- c(args, math_support$args)
    extra_dependencies <- c(extra_dependencies, math_support$extra_dependencies)
    format_deps <- list()
    format_deps <- append(format_deps, html_dependency_header_attrs())
    if (!is.null(theme)) {
      format_deps <- append(format_deps, list(html_dependency_jquery()))
      if (is_bs_theme(theme)) {
        theme <- bslib::bs_global_get()
      }
      bootstrap_deps <- if (is_bs_theme(theme) && is_shiny(runtime, metadata[["server"]])) {
        list(shiny_bootstrap_lib(theme))
      }
      else {
        bootstrap_dependencies(theme)
      }
      format_deps <- append(format_deps, htmltools::resolveDependencies(bootstrap_deps))
    }
    else if (isTRUE(bootstrap_compatible) && is_shiny(runtime, metadata[["server"]])) {
      format_deps <- append(format_deps, bootstrap_dependencies("bootstrap"))
    }
    format_deps <- append(format_deps, extra_dependencies)
    extras <- html_extras_for_document(knit_meta, runtime, dependency_resolver, format_deps)
    args <- c(args, pandoc_html_extras_args(extras, self_contained, lib_dir, output_dir))
    preserved_chunks <<- extract_preserve_chunks(input_file)
    args
  }
  intermediates_generator <- function(original_input, intermediates_dir) {
    copy_render_intermediates(original_input, intermediates_dir, !self_contained)
  }
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    if (identical(math_method, "r-katex") && xfun::pkg_available("katex", "1.4.0")) {
      katex::render_math_in_html(output_file, output = output_file)
    }
    output_str <- read_utf8(output_file)
    s1 <- "<span class=\"sc\">|</span><span class=\"er\">&gt;</span>"
    s2 <- "<span class=\"ot\">=</span><span class=\"er\">&gt;</span>"
    if ((length(preserved_chunks) == 0 && !isTRUE(copy_resources) && self_contained) && !length(c(grep(s1, output_str, fixed = TRUE), grep(s2, output_str, fixed = TRUE)))) 
      return(output_file)
    if (length(preserved_chunks) > 0) {
      for (i in names(preserved_chunks)) {
        output_str <- gsub(paste0("<p>", i, "</p>"), i, output_str, fixed = TRUE, useBytes = TRUE)
        output_str <- gsub(paste0(" id=\"[^\"]*?", i, "[^\"]*?\" "), " ", output_str, useBytes = TRUE)
      }
      output_str <- restorePreserveChunks(output_str, preserved_chunks)
    }
    if (copy_resources) {
      output_str <- copy_html_resources(one_string(output_str), lib_dir, output_dir)
    }
    else if (!self_contained) {
      image_relative <- function(img_src, src) {
        in_file <- utils::URLdecode(src)
        if (grepl("^[.][.]", in_file)) 
          return(img_src)
        if (length(in_file) && file.exists(in_file)) {
          img_src <- sub(src, utils::URLencode(normalized_relative_to(output_dir, in_file)), img_src, fixed = TRUE)
        }
        img_src
      }
      output_str <- process_images(output_str, image_relative)
    }
    output_str <- gsub(s1, "<span class=\"sc\">|&gt;</span>", output_str, fixed = TRUE)
    output_str <- gsub(s2, "<span class=\"ot\">=&gt;</span>", output_str, fixed = TRUE)
    write_utf8(output_str, output_file)
    output_file
  }
  if (!is.null(theme)) {
    bs3 <- identical("3", theme_version(theme))
    args <- c(args, pandoc_variable_arg("bs3", bs3))
  }
  output_format(knitr = NULL, pandoc = pandoc_options(to = "html", from = NULL, args = args, lua_filters = pkg_file_lua(c("pagebreak.lua", "latex-div.lua"))), keep_md = FALSE, clean_supporting = FALSE, pre_knit = pre_knit, post_knit = post_knit, on_exit = on_exit, pre_processor = pre_processor, intermediates_generator = intermediates_generator, post_processor = post_processor)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_fragment
list(`package:rmarkdown` = function (number_sections = FALSE, section_divs = TRUE, fig_width = 7, fig_height = 5, fig_retina = 2, fig_caption = TRUE, dev = "png", df_print = "default", mathjax = TRUE, includes = NULL, keep_md = FALSE, md_extensions = NULL, pandoc_args = NULL, ...) 
{
  if (mathjax) 
    pandoc_args <- c(pandoc_args, "--mathjax")
  html_document(number_sections = number_sections, fig_width = fig_width, fig_height = fig_height, fig_retina = fig_retina, fig_caption = fig_caption, dev = dev, df_print = df_print, keep_md = keep_md, md_extensions = md_extensions, pandoc_args = pandoc_args, includes = includes, mathjax = NULL, section_divs = section_divs, highlight = NULL, theme = NULL, ..., template = pkg_file("rmd/fragment/default.html"))
}, function (number_sections = FALSE, section_divs = TRUE, fig_width = 7, fig_height = 5, fig_retina = 2, fig_caption = TRUE, dev = "png", df_print = "default", mathjax = TRUE, includes = NULL, keep_md = FALSE, md_extensions = NULL, pandoc_args = NULL, ...) 
{
  if (mathjax) 
    pandoc_args <- c(pandoc_args, "--mathjax")
  html_document(number_sections = number_sections, fig_width = fig_width, fig_height = fig_height, fig_retina = fig_retina, fig_caption = fig_caption, dev = dev, df_print = df_print, keep_md = keep_md, md_extensions = md_extensions, pandoc_args = pandoc_args, includes = includes, mathjax = NULL, section_divs = section_divs, highlight = NULL, theme = NULL, ..., template = pkg_file("rmd/fragment/default.html"))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_notebook
list(`package:rmarkdown` = function (toc = FALSE, toc_depth = 3, toc_float = FALSE, number_sections = FALSE, fig_width = 7, fig_height = 5, fig_retina = 2, fig_caption = TRUE, code_folding = "show", theme = "default", highlight = "textmate", highlight_downlit = FALSE, math_method = "default", mathjax = "default", extra_dependencies = NULL, css = NULL, includes = NULL, md_extensions = NULL, pandoc_args = NULL, output_source = NULL, self_contained = TRUE, ...) 
{
  exit_actions <- list()
  on_exit <- function() {
    for (action in exit_actions) try(action())
  }
  paged_table_html_asis = function(x) {
    knitr::asis_output(paged_table_html(x))
  }
  pre_knit <- function(input, ...) {
    if (is.function(output_source)) {
      validate_output_source(output_source)
      knitr.duplicate.label <- getOption("knitr.duplicate.label")
      if (identical(knitr.duplicate.label, "allow")) {
        warning("unsetting 'knitr.duplicate.label' for duration of render")
        options(knitr.duplicate.label = "deny")
        exit_actions <<- c(exit_actions, function() {
          options(knitr.duplicate.label = knitr.duplicate.label)
        })
      }
      unnamed.chunk.label <- knitr::opts_knit$get("unnamed.chunk.label")
      if (!identical(unnamed.chunk.label, "unnamed-chunk")) {
        warning("reverting 'unnamed.chunk.label' to 'unnamed-chunk' for duration of render")
        knitr::opts_knit$set(unnamed.chunk.label = "unnamed-chunk")
        exit_actions <<- c(exit_actions, function() {
          knitr::opts_knit$set(unnamed.chunk.label = unnamed.chunk.label)
        })
      }
      output_source_pre_knit <- attr(output_source, "pre_knit", exact = TRUE)
      if (is.function(output_source_pre_knit)) 
        try(output_source_pre_knit())
      chunk_options <- list()
      include_hook <- knitr::opts_hooks$get("include")
      exit_actions <<- c(exit_actions, function() {
        knitr::opts_hooks$set(include = if (is.null(include_hook)) {
          function(options) options
        } else {
          include_hook
        })
      })
      knitr::opts_hooks$set(include = function(options) {
        chunk_options <<- options
        options$engine <- "R"
        options$engine.opts <- NULL
        options$cache <- FALSE
        if (is.function(include_hook)) 
          include_hook(options)
        else options
      })
      evaluate_hook <- knitr::knit_hooks$get("evaluate")
      exit_actions <<- c(exit_actions, function() {
        knitr::knit_hooks$set(evaluate = evaluate_hook)
      })
      knitr::knit_hooks$set(evaluate = function(code, ...) {
        chunk_options <- merge_render_context(chunk_options)
        context <- list2env(chunk_options)
        output <- output_source(code, context, ...)
        as_evaluate_output(output, context, ...)
      })
    }
    knit_sql_max_print <- knitr::opts_knit$get("sql.max.print")
    if (is.null(knit_sql_max_print)) {
      knitr::opts_knit$set(sql.max.print = 1000)
      exit_actions <<- c(exit_actions, function() {
        knitr::opts_knit$set(sql.max.print = knit_sql_max_print)
      })
    }
    knit_sql_print <- knitr::opts_knit$get("sql.print")
    if (is.null(knit_sql_print)) {
      knitr::opts_knit$set(sql.print = paged_table_html)
      exit_actions <<- c(exit_actions, function() {
        knitr::opts_knit$set(sql.print = knit_sql_print)
      })
    }
  }
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    args <- c()
    args <- c(args, pandoc_variable_arg("kable-scroll", "1"))
    args
  }
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    nb_output_file <- output_file
    if (!ends_with_bytes(output_file, ".nb.html")) {
      nb_output_file <- gsub("\\.html$", ".nb.html", output_file)
      file.rename(output_file, nb_output_file)
    }
    nb_output_file
  }
  fixed_args <- c("keep_md", "template", "lib_dir", "dev")
  forwarded_args <- names(list(...))
  for (arg in forwarded_args) {
    if (arg %in% fixed_args) 
      stop("The ", arg, " option is not valid for the html_notebook format.")
  }
  extra_dependencies <- append(extra_dependencies, list(html_dependency_pagedtable()))
  if (highlight_downlit && missing(highlight)) 
    highlight <- "default"
  base_format <- html_document(toc = toc, toc_depth = toc_depth, toc_float = toc_float, number_sections = number_sections, fig_width = fig_width, fig_height = fig_height, fig_retina = fig_retina, fig_caption = fig_caption, code_folding = code_folding, theme = theme, highlight = highlight, highlight_downlit = highlight_downlit, math_method = math_method, mathjax = mathjax, extra_dependencies = extra_dependencies, css = css, includes = includes, md_extensions = md_extensions, pandoc_args = pandoc_args, 
                               self_contained = self_contained, dev = "png", code_download = TRUE, keep_md = FALSE, template = "default", lib_dir = NULL, ...)
  rmarkdown::output_format(knitr = html_notebook_knitr_options(), pandoc = NULL, df_print = paged_table_html_asis, pre_knit = pre_knit, pre_processor = pre_processor, post_processor = post_processor, base_format = base_format, on_exit = on_exit)
}, function (toc = FALSE, toc_depth = 3, toc_float = FALSE, number_sections = FALSE, fig_width = 7, fig_height = 5, fig_retina = 2, fig_caption = TRUE, code_folding = "show", theme = "default", highlight = "textmate", highlight_downlit = FALSE, math_method = "default", mathjax = "default", extra_dependencies = NULL, css = NULL, includes = NULL, md_extensions = NULL, pandoc_args = NULL, output_source = NULL, self_contained = TRUE, ...) 
{
  exit_actions <- list()
  on_exit <- function() {
    for (action in exit_actions) try(action())
  }
  paged_table_html_asis = function(x) {
    knitr::asis_output(paged_table_html(x))
  }
  pre_knit <- function(input, ...) {
    if (is.function(output_source)) {
      validate_output_source(output_source)
      knitr.duplicate.label <- getOption("knitr.duplicate.label")
      if (identical(knitr.duplicate.label, "allow")) {
        warning("unsetting 'knitr.duplicate.label' for duration of render")
        options(knitr.duplicate.label = "deny")
        exit_actions <<- c(exit_actions, function() {
          options(knitr.duplicate.label = knitr.duplicate.label)
        })
      }
      unnamed.chunk.label <- knitr::opts_knit$get("unnamed.chunk.label")
      if (!identical(unnamed.chunk.label, "unnamed-chunk")) {
        warning("reverting 'unnamed.chunk.label' to 'unnamed-chunk' for duration of render")
        knitr::opts_knit$set(unnamed.chunk.label = "unnamed-chunk")
        exit_actions <<- c(exit_actions, function() {
          knitr::opts_knit$set(unnamed.chunk.label = unnamed.chunk.label)
        })
      }
      output_source_pre_knit <- attr(output_source, "pre_knit", exact = TRUE)
      if (is.function(output_source_pre_knit)) 
        try(output_source_pre_knit())
      chunk_options <- list()
      include_hook <- knitr::opts_hooks$get("include")
      exit_actions <<- c(exit_actions, function() {
        knitr::opts_hooks$set(include = if (is.null(include_hook)) {
          function(options) options
        } else {
          include_hook
        })
      })
      knitr::opts_hooks$set(include = function(options) {
        chunk_options <<- options
        options$engine <- "R"
        options$engine.opts <- NULL
        options$cache <- FALSE
        if (is.function(include_hook)) 
          include_hook(options)
        else options
      })
      evaluate_hook <- knitr::knit_hooks$get("evaluate")
      exit_actions <<- c(exit_actions, function() {
        knitr::knit_hooks$set(evaluate = evaluate_hook)
      })
      knitr::knit_hooks$set(evaluate = function(code, ...) {
        chunk_options <- merge_render_context(chunk_options)
        context <- list2env(chunk_options)
        output <- output_source(code, context, ...)
        as_evaluate_output(output, context, ...)
      })
    }
    knit_sql_max_print <- knitr::opts_knit$get("sql.max.print")
    if (is.null(knit_sql_max_print)) {
      knitr::opts_knit$set(sql.max.print = 1000)
      exit_actions <<- c(exit_actions, function() {
        knitr::opts_knit$set(sql.max.print = knit_sql_max_print)
      })
    }
    knit_sql_print <- knitr::opts_knit$get("sql.print")
    if (is.null(knit_sql_print)) {
      knitr::opts_knit$set(sql.print = paged_table_html)
      exit_actions <<- c(exit_actions, function() {
        knitr::opts_knit$set(sql.print = knit_sql_print)
      })
    }
  }
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    args <- c()
    args <- c(args, pandoc_variable_arg("kable-scroll", "1"))
    args
  }
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    nb_output_file <- output_file
    if (!ends_with_bytes(output_file, ".nb.html")) {
      nb_output_file <- gsub("\\.html$", ".nb.html", output_file)
      file.rename(output_file, nb_output_file)
    }
    nb_output_file
  }
  fixed_args <- c("keep_md", "template", "lib_dir", "dev")
  forwarded_args <- names(list(...))
  for (arg in forwarded_args) {
    if (arg %in% fixed_args) 
      stop("The ", arg, " option is not valid for the html_notebook format.")
  }
  extra_dependencies <- append(extra_dependencies, list(html_dependency_pagedtable()))
  if (highlight_downlit && missing(highlight)) 
    highlight <- "default"
  base_format <- html_document(toc = toc, toc_depth = toc_depth, toc_float = toc_float, number_sections = number_sections, fig_width = fig_width, fig_height = fig_height, fig_retina = fig_retina, fig_caption = fig_caption, code_folding = code_folding, theme = theme, highlight = highlight, highlight_downlit = highlight_downlit, math_method = math_method, mathjax = mathjax, extra_dependencies = extra_dependencies, css = css, includes = includes, md_extensions = md_extensions, pandoc_args = pandoc_args, 
                               self_contained = self_contained, dev = "png", code_download = TRUE, keep_md = FALSE, template = "default", lib_dir = NULL, ...)
  rmarkdown::output_format(knitr = html_notebook_knitr_options(), pandoc = NULL, df_print = paged_table_html_asis, pre_knit = pre_knit, pre_processor = pre_processor, post_processor = post_processor, base_format = base_format, on_exit = on_exit)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_notebook_metadata
list(`package:rmarkdown` = function (iframe = TRUE) 
{
  list(iframe = iframe)
}, function (iframe = TRUE) 
{
  list(iframe = iframe)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_notebook_output_code
list(`package:rmarkdown` = function (code, attributes = list(class = "r"), meta = NULL) 
{
  code <- sprintf("```%s\n%s\n```", attributes$class, one_string(code))
  meta$data <- code
  html_notebook_annotated_output(code, "source", meta)
}, function (code, attributes = list(class = "r"), meta = NULL) 
{
  code <- sprintf("```%s\n%s\n```", attributes$class, one_string(code))
  meta$data <- code
  html_notebook_annotated_output(code, "source", meta)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_notebook_output_html
list(`package:rmarkdown` = function (html, meta = NULL) 
{
  html_notebook_annotated_output(one_string(html), "html", meta)
}, function (html, meta = NULL) 
{
  html_notebook_annotated_output(one_string(html), "html", meta)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_notebook_output_img
list(`package:rmarkdown` = function (path = NULL, bytes = NULL, attributes = NULL, meta = NULL, format = c("png", "jpeg")) 
{
  template <- paste0("<img%s src=\"data:image/", match.arg(format), ";base64,%s\" />")
  html <- html_notebook_render_base64_data(path, bytes, attributes, template)
  html_notebook_annotated_output(html, "plot", meta)
}, function (path = NULL, bytes = NULL, attributes = NULL, meta = NULL, format = c("png", "jpeg")) 
{
  template <- paste0("<img%s src=\"data:image/", match.arg(format), ";base64,%s\" />")
  html <- html_notebook_render_base64_data(path, bytes, attributes, template)
  html_notebook_annotated_output(html, "plot", meta)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_notebook_output_png
list(`package:rmarkdown` = function (path = NULL, bytes = NULL, attributes = NULL, meta = NULL, format = c("png", "jpeg")) 
{
  template <- paste0("<img%s src=\"data:image/", match.arg(format), ";base64,%s\" />")
  html <- html_notebook_render_base64_data(path, bytes, attributes, template)
  html_notebook_annotated_output(html, "plot", meta)
}, function (path = NULL, bytes = NULL, attributes = NULL, meta = NULL, format = c("png", "jpeg")) 
{
  template <- paste0("<img%s src=\"data:image/", match.arg(format), ";base64,%s\" />")
  html <- html_notebook_render_base64_data(path, bytes, attributes, template)
  html_notebook_annotated_output(html, "plot", meta)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
html_vignette
list(`package:rmarkdown` = function (fig_width = 3, fig_height = 3, dev = "png", df_print = "default", css = NULL, highlight = "pygments", keep_md = FALSE, readme = FALSE, self_contained = TRUE, tabset = FALSE, code_folding = c("none", "show", "hide"), extra_dependencies = NULL, pandoc_args = NULL, ...) 
{
  lua_filters <- c()
  if (is.null(css)) {
    css <- system.file("rmarkdown", "templates", "html_vignette", "resources", "vignette.css", package = "rmarkdown")
  }
  if (tabset) {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_tabset()))
  }
  code_folding <- match.arg(code_folding)
  if (code_folding != "none") {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_codefolding_lua()))
    pandoc_args <- c(pandoc_args, pandoc_metadata_arg("rmd_codefolding_lua", code_folding))
    lua_filters <- c(lua_filters, pkg_file_lua("codefolding.lua"))
  }
  pre_knit <- function(input, ...) {
    if (readme) {
      rmarkdown::render(input, output_format = "github_document", output_options = list(html_preview = FALSE), output_file = "README.md", output_dir = dirname(dirname(input)), quiet = TRUE)
    }
  }
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    vignette_pre_processor(input_file, metadata)
  }
  base_format <- html_document(fig_width = fig_width, fig_height = fig_height, dev = dev, fig_retina = NULL, css = css, theme = NULL, highlight = highlight, self_contained = self_contained, extra_dependencies = extra_dependencies, pandoc_args = pandoc_args, ...)
  base_format$pandoc$lua_filters <- append(base_format$pandoc$lua_filters, lua_filters)
  output_format(knitr = NULL, pandoc = NULL, df_print = df_print, pre_knit = pre_knit, keep_md = keep_md, clean_supporting = self_contained, pre_processor = pre_processor, base_format = base_format)
}, function (fig_width = 3, fig_height = 3, dev = "png", df_print = "default", css = NULL, highlight = "pygments", keep_md = FALSE, readme = FALSE, self_contained = TRUE, tabset = FALSE, code_folding = c("none", "show", "hide"), extra_dependencies = NULL, pandoc_args = NULL, ...) 
{
  lua_filters <- c()
  if (is.null(css)) {
    css <- system.file("rmarkdown", "templates", "html_vignette", "resources", "vignette.css", package = "rmarkdown")
  }
  if (tabset) {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_tabset()))
  }
  code_folding <- match.arg(code_folding)
  if (code_folding != "none") {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_codefolding_lua()))
    pandoc_args <- c(pandoc_args, pandoc_metadata_arg("rmd_codefolding_lua", code_folding))
    lua_filters <- c(lua_filters, pkg_file_lua("codefolding.lua"))
  }
  pre_knit <- function(input, ...) {
    if (readme) {
      rmarkdown::render(input, output_format = "github_document", output_options = list(html_preview = FALSE), output_file = "README.md", output_dir = dirname(dirname(input)), quiet = TRUE)
    }
  }
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    vignette_pre_processor(input_file, metadata)
  }
  base_format <- html_document(fig_width = fig_width, fig_height = fig_height, dev = dev, fig_retina = NULL, css = css, theme = NULL, highlight = highlight, self_contained = self_contained, extra_dependencies = extra_dependencies, pandoc_args = pandoc_args, ...)
  base_format$pandoc$lua_filters <- append(base_format$pandoc$lua_filters, lua_filters)
  output_format(knitr = NULL, pandoc = NULL, df_print = df_print, pre_knit = pre_knit, keep_md = keep_md, clean_supporting = self_contained, pre_processor = pre_processor, base_format = base_format)
}, function (..., fig_caption = TRUE, theme = NULL, highlight = "pygments", css = system.file("misc", "vignette.css", package = "knitr"), includes = list(after_body = system.file("misc", "vignette.html", package = "knitr"))) 
{
  rmarkdown::html_document(..., fig_caption = fig_caption, theme = theme, highlight = highlight, css = css, includes = includes)
})
c("package:rmarkdown", "namespace:rmarkdown", "namespace:knitr")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
includes
list(`package:rmarkdown` = function (in_header = NULL, before_body = NULL, after_body = NULL) 
{
  list(in_header = in_header, before_body = before_body, after_body = after_body)
}, function (in_header = NULL, before_body = NULL, after_body = NULL) 
{
  list(in_header = in_header, before_body = before_body, after_body = after_body)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
includes_to_pandoc_args
list(`package:rmarkdown` = function (includes, filter = identity) 
{
  if (!is.null(includes)) 
    pandoc_include_args(in_header = filter(includes$in_header), before_body = filter(includes$before_body), after_body = filter(includes$after_body))
}, function (includes, filter = identity) 
{
  if (!is.null(includes)) 
    pandoc_include_args(in_header = filter(includes$in_header), before_body = filter(includes$before_body), after_body = filter(includes$after_body))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
ioslides_presentation
list(`package:rmarkdown` = function (number_sections = FALSE, logo = NULL, slide_level = 2, incremental = FALSE, fig_width = 7.5, fig_height = 4.5, fig_retina = 2, fig_caption = TRUE, dev = "png", df_print = "default", smart = TRUE, self_contained = TRUE, widescreen = FALSE, smaller = FALSE, transition = "default", math_method = "mathjax", mathjax = "default", analytics = NULL, template = NULL, css = NULL, includes = NULL, keep_md = FALSE, lib_dir = NULL, md_extensions = NULL, pandoc_args = NULL, 
                                     extra_dependencies = NULL, ...) 
{
  args <- c()
  math <- mathjax_to_math(mathjax, math_method)
  math <- check_math_argument(math)
  if (!identical(math$engine, "mathjax")) {
    stop2("Only mathjax is supported for `ioslide_presentation()` for 'math'.")
  }
  if (widescreen) 
    args <- c(args, "--variable", "widescreen")
  if (identical(df_print, "paged")) {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_pagedtable()))
  }
  if (is.numeric(transition)) 
    transition <- as.character(transition)
  else if (transition %in% c("default", "faster", "slower")) 
    transition <- switch(transition, default = "0.4", faster = "0.2", slower = "0.6")
  else stop2("transition must be \"default\", \"faster\", \"slower\" or a ", "numeric value (representing seconds)")
  args <- c(args, pandoc_variable_arg("transition", transition))
  for (css_file in css) args <- c(args, "--css", pandoc_path_arg(css_file, backslash = FALSE))
  args <- c(args, includes_to_pandoc_args(includes))
  if (is.null(template) || !file.exists(template)) 
    template <- pkg_file("rmd/ioslides/default.html")
  args <- c(args, "--template", pandoc_path_arg(template))
  extra_dependencies <- append(extra_dependencies, list(html_dependency_ioslides()))
  if (!is.null(analytics)) 
    args <- c(args, pandoc_variable_arg("analytics", analytics))
  if (!length(grep("--wrap", pandoc_args))) 
    pandoc_args <- c("--wrap", "none", pandoc_args)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    if (is.null(lib_dir)) 
      lib_dir <- files_dir
    args <- c()
    if (!dir_exists(files_dir)) 
      dir.create(files_dir)
    if (!is.null(logo)) {
      logo_path <- logo
      if (!self_contained) {
        logo_ext <- xfun::file_ext(logo)
        if (nchar(logo_ext) < 1) 
          logo_ext <- "png"
        logo_path <- file.path(files_dir, paste("logo", logo_ext, sep = "."))
        file.copy(from = logo, to = logo_path)
        logo_path <- normalized_relative_to(output_dir, logo_path)
      }
      else {
        logo_path <- pandoc_path_arg(logo_path)
      }
      args <- c(args, "--variable", paste("logo=", logo_path, sep = ""))
    }
    args
  }
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    args <- c()
    args <- c(args, pandoc_args)
    if (number_sections) 
      args <- c(args, pandoc_lua_filter_args(pkg_file_lua("number-sections.lua")))
    lua_writer <- file.path(dirname(input_file), "ioslides_presentation.lua")
    if (!file.create(lua_writer, showWarnings = FALSE)) 
      lua_writer <- file.path(dirname(output_file), basename(lua_writer))
    on.exit(unlink(lua_writer), add = TRUE)
    run_citeproc <- citeproc_required(metadata, read_utf8(input_file))
    settings <- c()
    add_setting <- function(name, value) {
      settings <<- c(settings, paste("local", name, "=", ifelse(value, "true", "false")))
    }
    add_setting("fig_caption", fig_caption)
    add_setting("incremental", incremental)
    add_setting("smaller", smaller)
    add_setting("smart", smart)
    add_setting("mathjax", !is.null(mathjax))
    settings <- c(settings, sprintf("local slide_level = %s", slide_level))
    write_utf8(settings, lua_writer)
    args <- c(args, "--slide-level", as.character(slide_level))
    file.append(lua_writer, pkg_file("rmd/ioslides/ioslides_presentation.lua"))
    output_tmpfile <- tempfile("ioslides-output", fileext = ".html")
    on.exit(unlink(output_tmpfile), add = TRUE)
    if (is_windows()) {
      codepage <- as.numeric(gsub("\\D", "", system2("chcp", stdout = TRUE)))
      if (!is.na(codepage)) {
        on.exit(system2("chcp", args = codepage, stdout = TRUE), add = TRUE)
        system2("chcp", args = 65001, stdout = TRUE)
      }
    }
    pandoc_convert(input = input_file, to = relative_to(dirname(input_file), lua_writer), from = from_rmarkdown(fig_caption), output = output_tmpfile, options = args, citeproc = run_citeproc, verbose = verbose)
    slides_lines <- read_utf8(output_tmpfile)
    if (self_contained) {
      slides_lines <- base64_encode_images(slides_lines)
    }
    output_lines <- read_utf8(output_file)
    sentinel_line <- grep("^RENDERED_SLIDES$", output_lines)
    if (length(sentinel_line) == 1) {
      preface_lines <- c(output_lines[1:sentinel_line[1] - 1])
      suffix_lines <- c(output_lines[-(1:sentinel_line[1])])
      output_lines <- c(preface_lines, slides_lines, suffix_lines)
      write_utf8(output_lines, output_file)
    }
    else {
      stop2("Slides placeholder not found in slides HTML")
    }
    output_file
  }
  output_format(knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md, dev), pandoc = pandoc_options(to = "html", from = from_rmarkdown(fig_caption, md_extensions), args = args), keep_md = keep_md, clean_supporting = self_contained, df_print = df_print, pre_processor = pre_processor, post_processor = post_processor, base_format = html_document_base(lib_dir = lib_dir, self_contained = self_contained, mathjax = mathjax, pandoc_args = pandoc_args, extra_dependencies = extra_dependencies, 
                                                                                                                                                                                                                                                                                                                                                                               bootstrap_compatible = TRUE, ...))
}, function (number_sections = FALSE, logo = NULL, slide_level = 2, incremental = FALSE, fig_width = 7.5, fig_height = 4.5, fig_retina = 2, fig_caption = TRUE, dev = "png", df_print = "default", smart = TRUE, self_contained = TRUE, widescreen = FALSE, smaller = FALSE, transition = "default", math_method = "mathjax", mathjax = "default", analytics = NULL, template = NULL, css = NULL, includes = NULL, keep_md = FALSE, lib_dir = NULL, md_extensions = NULL, pandoc_args = NULL, extra_dependencies = NULL, 
             ...) 
{
  args <- c()
  math <- mathjax_to_math(mathjax, math_method)
  math <- check_math_argument(math)
  if (!identical(math$engine, "mathjax")) {
    stop2("Only mathjax is supported for `ioslide_presentation()` for 'math'.")
  }
  if (widescreen) 
    args <- c(args, "--variable", "widescreen")
  if (identical(df_print, "paged")) {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_pagedtable()))
  }
  if (is.numeric(transition)) 
    transition <- as.character(transition)
  else if (transition %in% c("default", "faster", "slower")) 
    transition <- switch(transition, default = "0.4", faster = "0.2", slower = "0.6")
  else stop2("transition must be \"default\", \"faster\", \"slower\" or a ", "numeric value (representing seconds)")
  args <- c(args, pandoc_variable_arg("transition", transition))
  for (css_file in css) args <- c(args, "--css", pandoc_path_arg(css_file, backslash = FALSE))
  args <- c(args, includes_to_pandoc_args(includes))
  if (is.null(template) || !file.exists(template)) 
    template <- pkg_file("rmd/ioslides/default.html")
  args <- c(args, "--template", pandoc_path_arg(template))
  extra_dependencies <- append(extra_dependencies, list(html_dependency_ioslides()))
  if (!is.null(analytics)) 
    args <- c(args, pandoc_variable_arg("analytics", analytics))
  if (!length(grep("--wrap", pandoc_args))) 
    pandoc_args <- c("--wrap", "none", pandoc_args)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    if (is.null(lib_dir)) 
      lib_dir <- files_dir
    args <- c()
    if (!dir_exists(files_dir)) 
      dir.create(files_dir)
    if (!is.null(logo)) {
      logo_path <- logo
      if (!self_contained) {
        logo_ext <- xfun::file_ext(logo)
        if (nchar(logo_ext) < 1) 
          logo_ext <- "png"
        logo_path <- file.path(files_dir, paste("logo", logo_ext, sep = "."))
        file.copy(from = logo, to = logo_path)
        logo_path <- normalized_relative_to(output_dir, logo_path)
      }
      else {
        logo_path <- pandoc_path_arg(logo_path)
      }
      args <- c(args, "--variable", paste("logo=", logo_path, sep = ""))
    }
    args
  }
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    args <- c()
    args <- c(args, pandoc_args)
    if (number_sections) 
      args <- c(args, pandoc_lua_filter_args(pkg_file_lua("number-sections.lua")))
    lua_writer <- file.path(dirname(input_file), "ioslides_presentation.lua")
    if (!file.create(lua_writer, showWarnings = FALSE)) 
      lua_writer <- file.path(dirname(output_file), basename(lua_writer))
    on.exit(unlink(lua_writer), add = TRUE)
    run_citeproc <- citeproc_required(metadata, read_utf8(input_file))
    settings <- c()
    add_setting <- function(name, value) {
      settings <<- c(settings, paste("local", name, "=", ifelse(value, "true", "false")))
    }
    add_setting("fig_caption", fig_caption)
    add_setting("incremental", incremental)
    add_setting("smaller", smaller)
    add_setting("smart", smart)
    add_setting("mathjax", !is.null(mathjax))
    settings <- c(settings, sprintf("local slide_level = %s", slide_level))
    write_utf8(settings, lua_writer)
    args <- c(args, "--slide-level", as.character(slide_level))
    file.append(lua_writer, pkg_file("rmd/ioslides/ioslides_presentation.lua"))
    output_tmpfile <- tempfile("ioslides-output", fileext = ".html")
    on.exit(unlink(output_tmpfile), add = TRUE)
    if (is_windows()) {
      codepage <- as.numeric(gsub("\\D", "", system2("chcp", stdout = TRUE)))
      if (!is.na(codepage)) {
        on.exit(system2("chcp", args = codepage, stdout = TRUE), add = TRUE)
        system2("chcp", args = 65001, stdout = TRUE)
      }
    }
    pandoc_convert(input = input_file, to = relative_to(dirname(input_file), lua_writer), from = from_rmarkdown(fig_caption), output = output_tmpfile, options = args, citeproc = run_citeproc, verbose = verbose)
    slides_lines <- read_utf8(output_tmpfile)
    if (self_contained) {
      slides_lines <- base64_encode_images(slides_lines)
    }
    output_lines <- read_utf8(output_file)
    sentinel_line <- grep("^RENDERED_SLIDES$", output_lines)
    if (length(sentinel_line) == 1) {
      preface_lines <- c(output_lines[1:sentinel_line[1] - 1])
      suffix_lines <- c(output_lines[-(1:sentinel_line[1])])
      output_lines <- c(preface_lines, slides_lines, suffix_lines)
      write_utf8(output_lines, output_file)
    }
    else {
      stop2("Slides placeholder not found in slides HTML")
    }
    output_file
  }
  output_format(knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md, dev), pandoc = pandoc_options(to = "html", from = from_rmarkdown(fig_caption, md_extensions), args = args), keep_md = keep_md, clean_supporting = self_contained, df_print = df_print, pre_processor = pre_processor, post_processor = post_processor, base_format = html_document_base(lib_dir = lib_dir, self_contained = self_contained, mathjax = mathjax, pandoc_args = pandoc_args, extra_dependencies = extra_dependencies, 
                                                                                                                                                                                                                                                                                                                                                                               bootstrap_compatible = TRUE, ...))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
knit_params_ask
list(`package:rmarkdown` = function (file = NULL, input_lines = NULL, params = NULL, shiny_args = NULL, save_caption = "Save", encoding = "UTF-8") 
{
  if (is.null(input_lines)) {
    if (is.null(file)) {
      stop("knit_params_ask must have a non-NULL file or input_lines parameter")
    }
    input_lines <- read_utf8(file)
  }
  knit_params <- knitr::knit_params(input_lines)
  if (!is.null(params)) {
    if (!is.list(params) || (length(names(params)) != length(params))) {
      stop("knit_params_ask params argument must be a named list")
    }
  }
  if (length(knit_params) == 0) {
    return(params_namedList())
  }
  configurable <- Filter(params_configurable, knit_params)
  unconfigurable <- Filter(Negate(params_configurable), knit_params)
  values <- params_namedList()
  server <- function(input, output, session) {
    param.ui <- function(param) {
      inputControlFn <- params_get_control(param)
      inputControlFnFormals <- names(formals(inputControlFn))
      inputId <- param$name
      label <- params_label(inputControlFn, param)
      arguments = list(inputId = inputId, label = label)
      attrib_names <- unique(c(names(param), "value"))
      lapply(attrib_names, function(name) {
        if (name %in% c("name", "input", "expr")) {
        }
        else if (name == "label") {
          arguments$label <<- label
        }
        else if (name == "value") {
          current_value <- param$value
          if (!is.null(params)) {
            override <- params[[param$name]]
            if (!is.null(override)) {
              current_value <- override
            }
          }
          current_value <- params_value_to_ui(inputControlFn, current_value, param$show_default)
          if ("value" %in% inputControlFnFormals) {
            arguments$value <<- current_value
          }
          else if ("selected" %in% inputControlFnFormals) {
            arguments$selected <<- current_value
          }
        }
        else if (name == "show_default") {
        }
        else {
          arguments[[name]] <<- if (inherits(param[[name]], "knit_param_expr")) {
            param[[name]][["value"]]
          }
          else param[[name]]
        }
      })
      uidefault <- params_value_to_ui(inputControlFn, param$value, param$show_default)
      hasDefaultValue <- function(value) {
        identical(uidefault, value)
      }
      inputControl <- NULL
      unsupported <- setdiff(names(arguments), inputControlFnFormals)
      if (length(unsupported) > 0) {
        inputControl <- shiny::div(class = "form-group", tags$label(class = "control-label", param$name), shiny::div(paste("Cannot customize the parameter \"", param$name, "\" ", "because the \"", params_get_input(param), "\" ", "Shiny control does not support: ", paste(unsupported, collapse = ", "), sep = "")))
      }
      else {
        inputControl <- do.call(inputControlFn, arguments)
      }
      showSelectControl <- NULL
      selectControl <- NULL
      selectInputId <- paste0("select_", param$name)
      makeSelectControl <- function(default_name, custom_name) {
        showSelectControl <<- function(current) {
          (is.null(current) || identical(current, "default"))
        }
        hasDefaultValue <<- function(value) {
          FALSE
        }
        choices <- list()
        choices[[default_name]] <- "default"
        choices[[custom_name]] <- "custom"
        selectControl <<- shiny::selectInput(inputId = selectInputId, label = label, choices = choices)
      }
      if (is.null(params[[param$name]])) {
        if (identical("Sys.time()", param$expr)) {
          makeSelectControl(paste0("now (", param$value, ")"), "Use a custom time")
        }
        else if (identical("Sys.Date()", param$expr)) {
          makeSelectControl(paste0("today (", param$value, ")"), "Use a custom date")
        }
        else if (is.null(param$value)) {
          if (!identical(inputControlFn, shiny::fileInput)) {
            makeSelectControl("Unspecified (NULL)", "Use a custom value")
          }
        }
      }
      output[[paste0("ui_", param$name)]] <- shiny::renderUI({
        if (!is.null(showSelectControl) && showSelectControl(input[[selectInputId]])) {
          selectControl
        }
        else {
          inputControl
        }
      })
      shiny::observe({
        uivalue <- input[[param$name]]
        if (is.null(uivalue) || hasDefaultValue(uivalue)) {
          values[[param$name]] <<- NULL
        }
        else {
          values[[param$name]] <<- params_value_from_ui(inputControlFn, param$value, uivalue)
        }
      })
    }
    lapply(configurable, function(param) {
      param.ui(param)
    })
    shiny::observeEvent(input$save, {
      session$onFlushed(function() {
        session$close()
        shiny::stopApp(values)
      })
    })
    shiny::observeEvent(input$cancel, {
      session$onFlushed(function() {
        session$close()
        shiny::stopApp(NULL)
      })
    })
  }
  contents <- tags$div(shiny::fluidRow(shiny::column(12, lapply(configurable, function(param) {
    shiny::uiOutput(paste0("ui_", param$name))
  }))), class = "container-fluid")
  if (length(unconfigurable) > 0) {
    skipped <- tags$div(tags$strong("Note:"), "The following parameters cannot be customized:", paste(lapply(unconfigurable, function(param) param$name), collapse = ", "))
    contents <- shiny::tagAppendChildren(contents, shiny::fluidRow(shiny::column(12, skipped)))
  }
  footer <- tags$div(tags$div(shiny::fluidRow(shiny::column(12, shiny::actionButton("save", save_caption, class = "btn-primary navbar-btn pull-right"), shiny::actionButton("cancel", "Cancel", class = "navbar-btn pull-right"))), class = "container-fluid"), class = "navbar navbar-default navbar-fixed-bottom")
  style <- tags$style(".container-fluid .shiny-input-container { width: auto; }", ".navbar button { margin-left: 10px; }", "body { padding-bottom: 70px; }")
  script <- tags$script(HTML("$(document).keyup(function(e) {\n", "if (e.which == 13) { $('#save').click(); } // enter\n", "if (e.which == 27) { $('#cancel').click(); } // esc\n", "});"))
  ui <- shiny::bootstrapPage(tags$head(style, script), contents, footer)
  shiny_app <- shiny::shinyApp(ui = ui, server = server)
  shiny_args <- merge_lists(list(appDir = shiny_app), shiny_args)
  do.call(shiny::runApp, shiny_args)
}, function (file = NULL, input_lines = NULL, params = NULL, shiny_args = NULL, save_caption = "Save", encoding = "UTF-8") 
{
  if (is.null(input_lines)) {
    if (is.null(file)) {
      stop("knit_params_ask must have a non-NULL file or input_lines parameter")
    }
    input_lines <- read_utf8(file)
  }
  knit_params <- knitr::knit_params(input_lines)
  if (!is.null(params)) {
    if (!is.list(params) || (length(names(params)) != length(params))) {
      stop("knit_params_ask params argument must be a named list")
    }
  }
  if (length(knit_params) == 0) {
    return(params_namedList())
  }
  configurable <- Filter(params_configurable, knit_params)
  unconfigurable <- Filter(Negate(params_configurable), knit_params)
  values <- params_namedList()
  server <- function(input, output, session) {
    param.ui <- function(param) {
      inputControlFn <- params_get_control(param)
      inputControlFnFormals <- names(formals(inputControlFn))
      inputId <- param$name
      label <- params_label(inputControlFn, param)
      arguments = list(inputId = inputId, label = label)
      attrib_names <- unique(c(names(param), "value"))
      lapply(attrib_names, function(name) {
        if (name %in% c("name", "input", "expr")) {
        }
        else if (name == "label") {
          arguments$label <<- label
        }
        else if (name == "value") {
          current_value <- param$value
          if (!is.null(params)) {
            override <- params[[param$name]]
            if (!is.null(override)) {
              current_value <- override
            }
          }
          current_value <- params_value_to_ui(inputControlFn, current_value, param$show_default)
          if ("value" %in% inputControlFnFormals) {
            arguments$value <<- current_value
          }
          else if ("selected" %in% inputControlFnFormals) {
            arguments$selected <<- current_value
          }
        }
        else if (name == "show_default") {
        }
        else {
          arguments[[name]] <<- if (inherits(param[[name]], "knit_param_expr")) {
            param[[name]][["value"]]
          }
          else param[[name]]
        }
      })
      uidefault <- params_value_to_ui(inputControlFn, param$value, param$show_default)
      hasDefaultValue <- function(value) {
        identical(uidefault, value)
      }
      inputControl <- NULL
      unsupported <- setdiff(names(arguments), inputControlFnFormals)
      if (length(unsupported) > 0) {
        inputControl <- shiny::div(class = "form-group", tags$label(class = "control-label", param$name), shiny::div(paste("Cannot customize the parameter \"", param$name, "\" ", "because the \"", params_get_input(param), "\" ", "Shiny control does not support: ", paste(unsupported, collapse = ", "), sep = "")))
      }
      else {
        inputControl <- do.call(inputControlFn, arguments)
      }
      showSelectControl <- NULL
      selectControl <- NULL
      selectInputId <- paste0("select_", param$name)
      makeSelectControl <- function(default_name, custom_name) {
        showSelectControl <<- function(current) {
          (is.null(current) || identical(current, "default"))
        }
        hasDefaultValue <<- function(value) {
          FALSE
        }
        choices <- list()
        choices[[default_name]] <- "default"
        choices[[custom_name]] <- "custom"
        selectControl <<- shiny::selectInput(inputId = selectInputId, label = label, choices = choices)
      }
      if (is.null(params[[param$name]])) {
        if (identical("Sys.time()", param$expr)) {
          makeSelectControl(paste0("now (", param$value, ")"), "Use a custom time")
        }
        else if (identical("Sys.Date()", param$expr)) {
          makeSelectControl(paste0("today (", param$value, ")"), "Use a custom date")
        }
        else if (is.null(param$value)) {
          if (!identical(inputControlFn, shiny::fileInput)) {
            makeSelectControl("Unspecified (NULL)", "Use a custom value")
          }
        }
      }
      output[[paste0("ui_", param$name)]] <- shiny::renderUI({
        if (!is.null(showSelectControl) && showSelectControl(input[[selectInputId]])) {
          selectControl
        }
        else {
          inputControl
        }
      })
      shiny::observe({
        uivalue <- input[[param$name]]
        if (is.null(uivalue) || hasDefaultValue(uivalue)) {
          values[[param$name]] <<- NULL
        }
        else {
          values[[param$name]] <<- params_value_from_ui(inputControlFn, param$value, uivalue)
        }
      })
    }
    lapply(configurable, function(param) {
      param.ui(param)
    })
    shiny::observeEvent(input$save, {
      session$onFlushed(function() {
        session$close()
        shiny::stopApp(values)
      })
    })
    shiny::observeEvent(input$cancel, {
      session$onFlushed(function() {
        session$close()
        shiny::stopApp(NULL)
      })
    })
  }
  contents <- tags$div(shiny::fluidRow(shiny::column(12, lapply(configurable, function(param) {
    shiny::uiOutput(paste0("ui_", param$name))
  }))), class = "container-fluid")
  if (length(unconfigurable) > 0) {
    skipped <- tags$div(tags$strong("Note:"), "The following parameters cannot be customized:", paste(lapply(unconfigurable, function(param) param$name), collapse = ", "))
    contents <- shiny::tagAppendChildren(contents, shiny::fluidRow(shiny::column(12, skipped)))
  }
  footer <- tags$div(tags$div(shiny::fluidRow(shiny::column(12, shiny::actionButton("save", save_caption, class = "btn-primary navbar-btn pull-right"), shiny::actionButton("cancel", "Cancel", class = "navbar-btn pull-right"))), class = "container-fluid"), class = "navbar navbar-default navbar-fixed-bottom")
  style <- tags$style(".container-fluid .shiny-input-container { width: auto; }", ".navbar button { margin-left: 10px; }", "body { padding-bottom: 70px; }")
  script <- tags$script(HTML("$(document).keyup(function(e) {\n", "if (e.which == 13) { $('#save').click(); } // enter\n", "if (e.which == 27) { $('#cancel').click(); } // esc\n", "});"))
  ui <- shiny::bootstrapPage(tags$head(style, script), contents, footer)
  shiny_app <- shiny::shinyApp(ui = ui, server = server)
  shiny_args <- merge_lists(list(appDir = shiny_app), shiny_args)
  do.call(shiny::runApp, shiny_args)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
knitr_options
list(`package:rmarkdown` = function (opts_knit = NULL, opts_chunk = NULL, knit_hooks = NULL, opts_hooks = NULL, opts_template = NULL) 
{
  list(opts_knit = opts_knit, opts_chunk = opts_chunk, knit_hooks = knit_hooks, opts_hooks = opts_hooks, opts_template = opts_template)
}, function (opts_knit = NULL, opts_chunk = NULL, knit_hooks = NULL, opts_hooks = NULL, opts_template = NULL) 
{
  list(opts_knit = opts_knit, opts_chunk = opts_chunk, knit_hooks = knit_hooks, opts_hooks = opts_hooks, opts_template = opts_template)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
knitr_options_html
list(`package:rmarkdown` = function (fig_width, fig_height, fig_retina, keep_md, dev = "png") 
{
  opts_chunk <- list(dev = dev, dpi = 96, fig.width = fig_width, fig.height = fig_height, fig.retina = fig_retina)
  if (keep_md) 
    opts_chunk$fig.retina <- NULL
  knitr_options(opts_chunk = opts_chunk)
}, function (fig_width, fig_height, fig_retina, keep_md, dev = "png") 
{
  opts_chunk <- list(dev = dev, dpi = 96, fig.width = fig_width, fig.height = fig_height, fig.retina = fig_retina)
  if (keep_md) 
    opts_chunk$fig.retina <- NULL
  knitr_options(opts_chunk = opts_chunk)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
knitr_options_pdf
list(`package:rmarkdown` = function (fig_width, fig_height, fig_crop, dev = "pdf") 
{
  opts_knit <- NULL
  opts_chunk <- list(dev = dev, fig.width = fig_width, fig.height = fig_height)
  if (dev == "pdf") 
    opts_chunk$dev.args <- list(pdf = list(useDingbats = FALSE))
  knit_hooks <- NULL
  if (identical(fig_crop, "auto")) 
    fig_crop <- has_crop_tools(FALSE)
  else {
    if (fig_crop && !has_crop_tools()) 
      fig_crop <- FALSE
  }
  if (fig_crop) {
    knit_hooks <- list(crop = knitr::hook_pdfcrop)
    opts_chunk$crop <- TRUE
  }
  knitr_options(opts_knit = opts_knit, opts_chunk = opts_chunk, knit_hooks = knit_hooks)
}, function (fig_width, fig_height, fig_crop, dev = "pdf") 
{
  opts_knit <- NULL
  opts_chunk <- list(dev = dev, fig.width = fig_width, fig.height = fig_height)
  if (dev == "pdf") 
    opts_chunk$dev.args <- list(pdf = list(useDingbats = FALSE))
  knit_hooks <- NULL
  if (identical(fig_crop, "auto")) 
    fig_crop <- has_crop_tools(FALSE)
  else {
    if (fig_crop && !has_crop_tools()) 
      fig_crop <- FALSE
  }
  if (fig_crop) {
    knit_hooks <- list(crop = knitr::hook_pdfcrop)
    opts_chunk$crop <- TRUE
  }
  knitr_options(opts_knit = opts_knit, opts_chunk = opts_chunk, knit_hooks = knit_hooks)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
latex_dependency
list(`package:rmarkdown` = function (name, options = NULL, extra_lines = NULL) 
{
  output <- list(name = name, options = options, extra_lines = extra_lines)
  class(output) <- "latex_dependency"
  validate_latex_dependency(output)
}, function (name, options = NULL, extra_lines = NULL) 
{
  output <- list(name = name, options = options, extra_lines = extra_lines)
  class(output) <- "latex_dependency"
  validate_latex_dependency(output)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
latex_dependency_tikz
list(`package:rmarkdown` = function (libraries, options = NULL, extra_lines = NULL) 
{
  libraries <- sprintf("\\usetikzlibrary{%s}", paste(libraries, collapse = ", "))
  latex_dependency("tikz", options = options, extra_lines = c(libraries, extra_lines))
}, function (libraries, options = NULL, extra_lines = NULL) 
{
  libraries <- sprintf("\\usetikzlibrary{%s}", paste(libraries, collapse = ", "))
  latex_dependency("tikz", options = options, extra_lines = c(libraries, extra_lines))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
latex_document
list(`package:rmarkdown` = function (...) 
{
  merge_lists(pdf_document(..., keep_tex = TRUE), list(pandoc = list(ext = ".tex")))
}, function (...) 
{
  merge_lists(pdf_document(..., keep_tex = TRUE), list(pandoc = list(ext = ".tex")))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
latex_fragment
list(`package:rmarkdown` = function (...) 
{
  latex_document(..., template = pkg_file("rmd/fragment/default.tex"))
}, function (...) 
{
  latex_document(..., template = pkg_file("rmd/fragment/default.tex"))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
md_document
list(`package:rmarkdown` = function (variant = "markdown_strict", preserve_yaml = FALSE, toc = FALSE, toc_depth = 3, number_sections = FALSE, standalone = FALSE, fig_width = 7, fig_height = 5, fig_retina = NULL, dev = "png", df_print = "default", includes = NULL, md_extensions = NULL, pandoc_args = NULL, ext = ".md") 
{
  if (toc) 
    standalone <- TRUE
  args <- c(if (standalone) "--standalone")
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  args <- c(args, includes_to_pandoc_args(includes))
  args <- c(args, pandoc_args)
  if (number_sections && !pandoc_available("2.1")) {
    warning("`number_sections = TRUE` requires at least Pandoc 2.1. The feature will be deactivated", call. = FALSE)
    number_sections <- FALSE
  }
  pre_processor <- if (number_sections && grepl("^(commonmark|gfm|markdown)", variant) && any(grepl("+gfm_auto_identifiers", md_extensions, fixed = TRUE))) {
    function(metadata, input_file, ...) {
      input_lines <- read_utf8(input_file)
      pandoc_convert(input_file, to = "markdown", output = input_file, options = c("--lua-filter", pkg_file_lua("number-sections.lua"), "--metadata", "preprocess_number_sections=true"))
      input_lines2 <- read_utf8(input_file)
      write_utf8(.preserve_yaml(input_lines, input_lines2), input_file)
      return(character(0))
    }
  }
  variant <- adapt_md_variant(variant)
  post_processor <- if (preserve_yaml) {
    function(metadata, input_file, output_file, clean, verbose) {
      input_lines <- read_utf8(input_file)
      output_lines <- read_utf8(output_file)
      write_utf8(.preserve_yaml(input_lines, output_lines), output_file)
      output_file
    }
  }
  output_format(knitr = knitr_options_html(fig_width, fig_height, fig_retina, FALSE, dev), pandoc = pandoc_options(to = variant, from = from_rmarkdown(extensions = md_extensions), args = args, lua_filters = if (number_sections) 
    pkg_file_lua("number-sections.lua"), ext = ext), clean_supporting = FALSE, df_print = df_print, pre_processor = pre_processor, post_processor = post_processor)
}, function (variant = "markdown_strict", preserve_yaml = FALSE, toc = FALSE, toc_depth = 3, number_sections = FALSE, standalone = FALSE, fig_width = 7, fig_height = 5, fig_retina = NULL, dev = "png", df_print = "default", includes = NULL, md_extensions = NULL, pandoc_args = NULL, ext = ".md") 
{
  if (toc) 
    standalone <- TRUE
  args <- c(if (standalone) "--standalone")
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  args <- c(args, includes_to_pandoc_args(includes))
  args <- c(args, pandoc_args)
  if (number_sections && !pandoc_available("2.1")) {
    warning("`number_sections = TRUE` requires at least Pandoc 2.1. The feature will be deactivated", call. = FALSE)
    number_sections <- FALSE
  }
  pre_processor <- if (number_sections && grepl("^(commonmark|gfm|markdown)", variant) && any(grepl("+gfm_auto_identifiers", md_extensions, fixed = TRUE))) {
    function(metadata, input_file, ...) {
      input_lines <- read_utf8(input_file)
      pandoc_convert(input_file, to = "markdown", output = input_file, options = c("--lua-filter", pkg_file_lua("number-sections.lua"), "--metadata", "preprocess_number_sections=true"))
      input_lines2 <- read_utf8(input_file)
      write_utf8(.preserve_yaml(input_lines, input_lines2), input_file)
      return(character(0))
    }
  }
  variant <- adapt_md_variant(variant)
  post_processor <- if (preserve_yaml) {
    function(metadata, input_file, output_file, clean, verbose) {
      input_lines <- read_utf8(input_file)
      output_lines <- read_utf8(output_file)
      write_utf8(.preserve_yaml(input_lines, output_lines), output_file)
      output_file
    }
  }
  output_format(knitr = knitr_options_html(fig_width, fig_height, fig_retina, FALSE, dev), pandoc = pandoc_options(to = variant, from = from_rmarkdown(extensions = md_extensions), args = args, lua_filters = if (number_sections) 
    pkg_file_lua("number-sections.lua"), ext = ext), clean_supporting = FALSE, df_print = df_print, pre_processor = pre_processor, post_processor = post_processor)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
metadata
list(`package:rmarkdown` = list(), list())
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
navbar_html
list(`package:rmarkdown` = function (navbar) 
{
  if (is.null(navbar$title)) 
    navbar$title <- ""
  if (is.null(navbar$type)) 
    navbar$type <- "default"
  left <- navbar_links_html(navbar$left)
  right <- navbar_links_html(navbar$right)
  template <- file_string(pkg_file("rmd/h/_navbar.html"))
  navbar_html <- sprintf(template, navbar$type, navbar$title, left, right)
  as_tmpfile(navbar_html)
}, function (navbar) 
{
  if (is.null(navbar$title)) 
    navbar$title <- ""
  if (is.null(navbar$type)) 
    navbar$type <- "default"
  left <- navbar_links_html(navbar$left)
  right <- navbar_links_html(navbar$right)
  template <- file_string(pkg_file("rmd/h/_navbar.html"))
  navbar_html <- sprintf(template, navbar$type, navbar$title, left, right)
  as_tmpfile(navbar_html)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
navbar_links_html
list(`package:rmarkdown` = function (links) 
{
  as.character(navbar_links_tags(links))
}, function (links) 
{
  as.character(navbar_links_tags(links))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
odt_document
list(`package:rmarkdown` = function (number_sections = FALSE, fig_width = 5, fig_height = 4, fig_caption = TRUE, template = "default", reference_odt = "default", includes = NULL, keep_md = FALSE, md_extensions = NULL, pandoc_args = NULL) 
{
  knitr <- knitr_options(opts_chunk = list(dev = "png", dpi = 96, fig.width = fig_width, fig.height = fig_height))
  args <- c()
  if (!is.null(template) && !identical(template, "default")) 
    args <- c(args, "--template", pandoc_path_arg(template))
  args <- c(args, includes_to_pandoc_args(includes))
  args <- c(args, reference_doc_args("odt", reference_odt))
  args <- c(args, pandoc_args)
  saved_files_dir <- NULL
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    saved_files_dir <<- files_dir
    NULL
  }
  intermediates_generator <- function(...) {
    reference_intermediates_generator(saved_files_dir, ..., reference_odt)
  }
  output_format(knitr = knitr, pandoc = pandoc_options(to = "odt", from = from_rmarkdown(fig_caption, md_extensions), args = args, lua_filters = pkg_file_lua(c("pagebreak.lua", if (number_sections) "number-sections.lua"))), keep_md = keep_md, pre_processor = pre_processor, intermediates_generator = intermediates_generator)
}, function (number_sections = FALSE, fig_width = 5, fig_height = 4, fig_caption = TRUE, template = "default", reference_odt = "default", includes = NULL, keep_md = FALSE, md_extensions = NULL, pandoc_args = NULL) 
{
  knitr <- knitr_options(opts_chunk = list(dev = "png", dpi = 96, fig.width = fig_width, fig.height = fig_height))
  args <- c()
  if (!is.null(template) && !identical(template, "default")) 
    args <- c(args, "--template", pandoc_path_arg(template))
  args <- c(args, includes_to_pandoc_args(includes))
  args <- c(args, reference_doc_args("odt", reference_odt))
  args <- c(args, pandoc_args)
  saved_files_dir <- NULL
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    saved_files_dir <<- files_dir
    NULL
  }
  intermediates_generator <- function(...) {
    reference_intermediates_generator(saved_files_dir, ..., reference_odt)
  }
  output_format(knitr = knitr, pandoc = pandoc_options(to = "odt", from = from_rmarkdown(fig_caption, md_extensions), args = args, lua_filters = pkg_file_lua(c("pagebreak.lua", if (number_sections) "number-sections.lua"))), keep_md = keep_md, pre_processor = pre_processor, intermediates_generator = intermediates_generator)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
output_format
list(`package:rmarkdown` = function (knitr, pandoc, keep_md = FALSE, clean_supporting = TRUE, df_print = NULL, pre_knit = NULL, post_knit = NULL, pre_processor = NULL, intermediates_generator = NULL, post_processor = NULL, on_exit = NULL, file_scope = NULL, base_format = NULL) 
{
  format <- list(knitr = knitr, pandoc = pandoc, keep_md = keep_md, clean_supporting = if (isTRUE(keep_md)) FALSE else clean_supporting, df_print = df_print, pre_knit = pre_knit, post_knit = post_knit, pre_processor = pre_processor, intermediates_generator = intermediates_generator, post_processor = post_processor, file_scope = file_scope, on_exit = on_exit)
  class(format) <- "rmarkdown_output_format"
  if (!is.null(base_format)) 
    merge_output_formats(base_format, format)
  else format
}, function (knitr, pandoc, keep_md = FALSE, clean_supporting = TRUE, df_print = NULL, pre_knit = NULL, post_knit = NULL, pre_processor = NULL, intermediates_generator = NULL, post_processor = NULL, on_exit = NULL, file_scope = NULL, base_format = NULL) 
{
  format <- list(knitr = knitr, pandoc = pandoc, keep_md = keep_md, clean_supporting = if (isTRUE(keep_md)) FALSE else clean_supporting, df_print = df_print, pre_knit = pre_knit, post_knit = post_knit, pre_processor = pre_processor, intermediates_generator = intermediates_generator, post_processor = post_processor, file_scope = file_scope, on_exit = on_exit)
  class(format) <- "rmarkdown_output_format"
  if (!is.null(base_format)) 
    merge_output_formats(base_format, format)
  else format
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
output_metadata
list(`package:rmarkdown` = list(get = function (name, default = FALSE, drop = TRUE) 
{
  if (default) 
    defaults = value
  if (missing(name)) 
    defaults
  else {
    if (drop && length(name) == 1) 
      defaults[[name]]
    else {
      setNames(defaults[name], name)
    }
  }
}, set = function (...) 
{
  set2(resolve(...))
}, delete = function (keys) 
{
  for (k in keys) defaults[[k]] <<- NULL
}, append = function (...) 
{
  dots = resolve(...)
  for (i in names(dots)) dots[[i]] <- c(defaults[[i]], dots[[i]])
  set2(dots)
}, merge = function (values) 
  merge_list(defaults, values), restore = function (target = value) 
    defaults <<- target), list(get = function (name, default = FALSE, drop = TRUE) 
    {
      if (default) 
        defaults = value
      if (missing(name)) 
        defaults
      else {
        if (drop && length(name) == 1) 
          defaults[[name]]
        else {
          setNames(defaults[name], name)
        }
      }
    }, set = function (...) 
    {
      set2(resolve(...))
    }, delete = function (keys) 
    {
      for (k in keys) defaults[[k]] <<- NULL
    }, append = function (...) 
    {
      dots = resolve(...)
      for (i in names(dots)) dots[[i]] <- c(defaults[[i]], dots[[i]])
      set2(dots)
    }, merge = function (values) 
      merge_list(defaults, values), restore = function (target = value) 
        defaults <<- target))
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
paged_table
list(`package:rmarkdown` = function (x, options = NULL) 
{
  if (!is.data.frame(x)) {
    stop("Only data frames can be used to create a paged_table.")
  }
  class(x) <- c("paged_df", "data.frame")
  attr(x, "options") <- options
  x
}, function (x, options = NULL) 
{
  if (!is.data.frame(x)) {
    stop("Only data frames can be used to create a paged_table.")
  }
  class(x) <- c("paged_df", "data.frame")
  attr(x, "options") <- options
  x
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_available
list(`package:rmarkdown` = function (version = NULL, error = FALSE) 
{
  find_pandoc()
  found <- !is.null(.pandoc$dir) && (is.null(version) || .pandoc$version >= version)
  msg <- c("pandoc", if (!is.null(version)) c("version", version, "or higher"), "is required and was not found (see the help page ?rmarkdown::pandoc_available).")
  if (error && !found) 
    stop2(paste(msg, collapse = " "))
  found
}, function (version = NULL, error = FALSE) 
{
  find_pandoc()
  found <- !is.null(.pandoc$dir) && (is.null(version) || .pandoc$version >= version)
  msg <- c("pandoc", if (!is.null(version)) c("version", version, "or higher"), "is required and was not found (see the help page ?rmarkdown::pandoc_available).")
  if (error && !found) 
    stop2(paste(msg, collapse = " "))
  found
}, function () 
{
  rmarkdown::pandoc_available("1.12.3")
})
c("package:rmarkdown", "namespace:rmarkdown", "namespace:knitr")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
pandoc_citeproc_args
list(`package:rmarkdown` = function () 
{
  if (pandoc_available("2.11")) 
    "--citeproc"
  else c("--filter", pandoc_citeproc())
}, function () 
{
  if (pandoc_available("2.11")) 
    "--citeproc"
  else c("--filter", pandoc_citeproc())
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_citeproc_convert
list(`package:rmarkdown` = function (file, type = c("list", "json", "yaml")) 
{
  find_pandoc()
  type <- match.arg(type)
  if (pandoc_available("2.11")) {
    bin <- pandoc()
    to <- switch(type, list = "csljson", json = "csljson", yaml = "markdown")
    args <- c(file, "-s", "-t", to)
  }
  else {
    bin <- pandoc_citeproc()
    conversion <- switch(type, list = "--bib2json", json = "--bib2json", yaml = "--bib2yaml")
    args <- c(conversion, file)
  }
  command <- paste(quoted(bin), paste(quoted(args), collapse = " "))
  with_pandoc_safe_environment({
    result <- system(command, intern = TRUE)
  })
  status <- attr(result, "status")
  if (!is.null(status)) {
    cat(result, sep = "\n")
    stop("Error ", status, " occurred building shared library.")
  }
  Encoding(result) <- "UTF-8"
  if (type == "list") {
    jsonlite::fromJSON(result, simplifyVector = FALSE)
  }
  else {
    result
  }
}, function (file, type = c("list", "json", "yaml")) 
{
  find_pandoc()
  type <- match.arg(type)
  if (pandoc_available("2.11")) {
    bin <- pandoc()
    to <- switch(type, list = "csljson", json = "csljson", yaml = "markdown")
    args <- c(file, "-s", "-t", to)
  }
  else {
    bin <- pandoc_citeproc()
    conversion <- switch(type, list = "--bib2json", json = "--bib2json", yaml = "--bib2yaml")
    args <- c(conversion, file)
  }
  command <- paste(quoted(bin), paste(quoted(args), collapse = " "))
  with_pandoc_safe_environment({
    result <- system(command, intern = TRUE)
  })
  status <- attr(result, "status")
  if (!is.null(status)) {
    cat(result, sep = "\n")
    stop("Error ", status, " occurred building shared library.")
  }
  Encoding(result) <- "UTF-8"
  if (type == "list") {
    jsonlite::fromJSON(result, simplifyVector = FALSE)
  }
  else {
    result
  }
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_convert
list(`package:rmarkdown` = function (input, to = NULL, from = NULL, output = NULL, citeproc = FALSE, options = NULL, verbose = FALSE, wd = NULL) 
{
  find_pandoc()
  force(output)
  if (is.null(wd)) {
    wd <- base_dir(input)
  }
  oldwd <- setwd(wd)
  on.exit(setwd(oldwd), add = TRUE)
  args <- c(input)
  if (!is.null(to)) {
    if (to == "html") 
      to <- "html4"
    if (to == "pdf") 
      to <- "latex"
    args <- c(args, "--to", to)
  }
  if (!is.null(from)) 
    args <- c(args, "--from", from)
  if (!is.null(output)) 
    args <- c(args, "--output", output)
  args <- prepend_pandoc_stack_size(args)
  args <- c(args, options)
  if (citeproc) {
    args <- c(args[!args %in% c("--natbib", "--biblatex")], pandoc_citeproc_args())
  }
  command <- paste(quoted(pandoc()), paste(quoted(args), collapse = " "))
  if (verbose) 
    cat(command, "\n")
  with_pandoc_safe_environment({
    result <- system(command)
  })
  if (result != 0) 
    stop2("pandoc document conversion failed with error ", result)
  invisible(NULL)
}, function (input, to = NULL, from = NULL, output = NULL, citeproc = FALSE, options = NULL, verbose = FALSE, wd = NULL) 
{
  find_pandoc()
  force(output)
  if (is.null(wd)) {
    wd <- base_dir(input)
  }
  oldwd <- setwd(wd)
  on.exit(setwd(oldwd), add = TRUE)
  args <- c(input)
  if (!is.null(to)) {
    if (to == "html") 
      to <- "html4"
    if (to == "pdf") 
      to <- "latex"
    args <- c(args, "--to", to)
  }
  if (!is.null(from)) 
    args <- c(args, "--from", from)
  if (!is.null(output)) 
    args <- c(args, "--output", output)
  args <- prepend_pandoc_stack_size(args)
  args <- c(args, options)
  if (citeproc) {
    args <- c(args[!args %in% c("--natbib", "--biblatex")], pandoc_citeproc_args())
  }
  command <- paste(quoted(pandoc()), paste(quoted(args), collapse = " "))
  if (verbose) 
    cat(command, "\n")
  with_pandoc_safe_environment({
    result <- system(command)
  })
  if (result != 0) 
    stop2("pandoc document conversion failed with error ", result)
  invisible(NULL)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_exec
list(`package:rmarkdown` = function () 
{
  find_pandoc()
  file.path(.pandoc$dir, "pandoc")
}, function () 
{
  find_pandoc()
  file.path(.pandoc$dir, "pandoc")
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_highlight_args
list(`package:rmarkdown` = function (highlight, default = "tango") 
{
  args <- c()
  if (is.null(highlight)) 
    args <- c(args, "--no-highlight")
  else {
    if (identical(highlight, "default")) 
      highlight <- default
    args <- c(args, "--highlight-style", highlight)
  }
  args
}, function (highlight, default = "tango") 
{
  args <- c()
  if (is.null(highlight)) 
    args <- c(args, "--no-highlight")
  else {
    if (identical(highlight, "default")) 
      highlight <- default
    args <- c(args, "--highlight-style", highlight)
  }
  args
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_include_args
list(`package:rmarkdown` = function (in_header = NULL, before_body = NULL, after_body = NULL) 
{
  args <- c()
  for (file in in_header) args <- c(args, "--include-in-header", pandoc_path_arg(file))
  for (file in before_body) args <- c(args, "--include-before-body", pandoc_path_arg(file))
  for (file in after_body) args <- c(args, "--include-after-body", pandoc_path_arg(file))
  args
}, function (in_header = NULL, before_body = NULL, after_body = NULL) 
{
  args <- c()
  for (file in in_header) args <- c(args, "--include-in-header", pandoc_path_arg(file))
  for (file in before_body) args <- c(args, "--include-before-body", pandoc_path_arg(file))
  for (file in after_body) args <- c(args, "--include-after-body", pandoc_path_arg(file))
  args
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_latex_engine_args
list(`package:rmarkdown` = function (latex_engine) 
{
  c(if (pandoc2.0()) "--pdf-engine" else "--latex-engine", find_latex_engine(latex_engine))
}, function (latex_engine) 
{
  c(if (pandoc2.0()) "--pdf-engine" else "--latex-engine", find_latex_engine(latex_engine))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_lua_filter_args
list(`package:rmarkdown` = function (lua_files) 
{
  if (pandoc2.0()) 
    c(rbind("--lua-filter", pandoc_path_arg(lua_files)))
}, function (lua_files) 
{
  if (pandoc2.0()) 
    c(rbind("--lua-filter", pandoc_path_arg(lua_files)))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_metadata_arg
list(`package:rmarkdown` = function (name, value) 
{
  c("--metadata", if (missing(value)) name else paste(name, "=", value, sep = ""))
}, function (name, value) 
{
  c("--metadata", if (missing(value)) name else paste(name, "=", value, sep = ""))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_options
list(`package:rmarkdown` = function (to, from = rmarkdown_format(), args = NULL, keep_tex = FALSE, latex_engine = c("pdflatex", "lualatex", "xelatex", "tectonic"), ext = NULL, lua_filters = NULL) 
{
  list(to = to, from = from, args = args, keep_tex = keep_tex, latex_engine = match.arg(latex_engine), ext = ext, lua_filters = lua_filters)
}, function (to, from = rmarkdown_format(), args = NULL, keep_tex = FALSE, latex_engine = c("pdflatex", "lualatex", "xelatex", "tectonic"), ext = NULL, lua_filters = NULL) 
{
  list(to = to, from = from, args = args, keep_tex = keep_tex, latex_engine = match.arg(latex_engine), ext = ext, lua_filters = lua_filters)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_path_arg
list(`package:rmarkdown` = function (path, backslash = TRUE) 
{
  path <- path.expand(path)
  path <- sub("^[.]/", "", path)
  if (is_windows()) {
    i <- grep(" ", path)
    if (length(i)) 
      path[i] <- utils::shortPathName(path[i])
    if (backslash) 
      path <- gsub("/", "\\\\", path)
  }
  path
}, function (path, backslash = TRUE) 
{
  path <- path.expand(path)
  path <- sub("^[.]/", "", path)
  if (is_windows()) {
    i <- grep(" ", path)
    if (length(i)) 
      path[i] <- utils::shortPathName(path[i])
    if (backslash) 
      path <- gsub("/", "\\\\", path)
  }
  path
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_self_contained_html
list(`package:rmarkdown` = function (input, output) 
{
  input <- normalizePath(input)
  if (!file.exists(output)) 
    file.create(output)
  output <- normalizePath(output)
  template <- tempfile(fileext = ".html")
  on.exit(unlink(template), add = TRUE)
  write_utf8("$body$", template)
  from <- if (pandoc_available("1.17")) 
    "markdown_strict"
  else "markdown"
  pandoc_convert(input = input, from = from, output = output, options = c("--self-contained", "--template", template))
  invisible(output)
}, function (input, output) 
{
  input <- normalizePath(input)
  if (!file.exists(output)) 
    file.create(output)
  output <- normalizePath(output)
  template <- tempfile(fileext = ".html")
  on.exit(unlink(template), add = TRUE)
  write_utf8("$body$", template)
  from <- if (pandoc_available("1.17")) 
    "markdown_strict"
  else "markdown"
  pandoc_convert(input = input, from = from, output = output, options = c("--self-contained", "--template", template))
  invisible(output)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_template
list(`package:rmarkdown` = function (metadata, template, output, verbose = FALSE) 
{
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp))
  cat("---\n", file = tmp)
  cat(yaml::as.yaml(metadata), file = tmp, append = TRUE)
  cat("---\n", file = tmp, append = TRUE)
  cat("\n", file = tmp, append = TRUE)
  pandoc_convert(tmp, "markdown", output = output, options = paste0("--template=", template), verbose = verbose)
  invisible(output)
}, function (metadata, template, output, verbose = FALSE) 
{
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp))
  cat("---\n", file = tmp)
  cat(yaml::as.yaml(metadata), file = tmp, append = TRUE)
  cat("---\n", file = tmp, append = TRUE)
  cat("\n", file = tmp, append = TRUE)
  pandoc_convert(tmp, "markdown", output = output, options = paste0("--template=", template), verbose = verbose)
  invisible(output)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_toc_args
list(`package:rmarkdown` = function (toc, toc_depth = 3) 
{
  args <- c()
  if (toc) {
    args <- c(args, "--table-of-contents")
    args <- c(args, "--toc-depth", toc_depth)
  }
  args
}, function (toc, toc_depth = 3) 
{
  args <- c()
  if (toc) {
    args <- c(args, "--table-of-contents")
    args <- c(args, "--toc-depth", toc_depth)
  }
  args
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_variable_arg
list(`package:rmarkdown` = function (name, value) 
{
  c("--variable", if (missing(value)) name else paste(name, "=", value, sep = ""))
}, function (name, value) 
{
  c("--variable", if (missing(value)) name else paste(name, "=", value, sep = ""))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pandoc_version
list(`package:rmarkdown` = function () 
{
  find_pandoc()
  .pandoc$version
}, function () 
{
  find_pandoc()
  .pandoc$version
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
parse_html_notebook
list(`package:rmarkdown` = function (path) 
{
  contents <- read_utf8(path)
  re_comment <- "^\\s*<!--\\s*rnb-([^-]+)-(begin|end)\\s*([^\\s-]+)?\\s*-->\\s*$"
  re_document <- "^<div id=\"rmd-source-code\">([^<]+)<\\/div>$"
  rmd_contents <- NULL
  builder <- list_builder()
  for (row in seq_along(contents)) {
    line <- contents[[row]]
    matches <- gregexpr(re_document, line, perl = TRUE)[[1]]
    if (!identical(c(matches), -1)) {
      start <- c(attr(matches, "capture.start"))
      end <- start + c(attr(matches, "capture.length")) - 1
      decoded <- rawToChar(xfun::base64_decode(substring(line, start, end)))
      rmd_contents <- strsplit(decoded, "\\r?\\n", perl = TRUE)[[1]]
      next
    }
    matches <- gregexpr(re_comment, line, perl = TRUE)[[1]]
    if (identical(c(matches), -1)) 
      next
    starts <- c(attr(matches, "capture.start"))
    ends <- starts + c(attr(matches, "capture.length")) - 1
    strings <- substring(line, starts, ends)
    n <- length(strings)
    if (n < 2) 
      stop("invalid rnb comment")
    data <- list(row = row, label = strings[[1]], state = strings[[2]])
    if (n >= 3 && nzchar(strings[[3]])) 
      data[["meta"]] <- base64_decode_object(strings[[3]])
    else data["meta"] <- list(NULL)
    builder$append(data)
  }
  annotations <- builder$data()
  head_start <- grep("^\\s*<head>\\s*$", contents, perl = TRUE)[[1]]
  head_end <- grep("^\\s*</head>\\s*$", contents, perl = TRUE)[[1]]
  list(source = contents, rmd = rmd_contents, header = contents[head_start:head_end], annotations = annotations)
}, function (path) 
{
  contents <- read_utf8(path)
  re_comment <- "^\\s*<!--\\s*rnb-([^-]+)-(begin|end)\\s*([^\\s-]+)?\\s*-->\\s*$"
  re_document <- "^<div id=\"rmd-source-code\">([^<]+)<\\/div>$"
  rmd_contents <- NULL
  builder <- list_builder()
  for (row in seq_along(contents)) {
    line <- contents[[row]]
    matches <- gregexpr(re_document, line, perl = TRUE)[[1]]
    if (!identical(c(matches), -1)) {
      start <- c(attr(matches, "capture.start"))
      end <- start + c(attr(matches, "capture.length")) - 1
      decoded <- rawToChar(xfun::base64_decode(substring(line, start, end)))
      rmd_contents <- strsplit(decoded, "\\r?\\n", perl = TRUE)[[1]]
      next
    }
    matches <- gregexpr(re_comment, line, perl = TRUE)[[1]]
    if (identical(c(matches), -1)) 
      next
    starts <- c(attr(matches, "capture.start"))
    ends <- starts + c(attr(matches, "capture.length")) - 1
    strings <- substring(line, starts, ends)
    n <- length(strings)
    if (n < 2) 
      stop("invalid rnb comment")
    data <- list(row = row, label = strings[[1]], state = strings[[2]])
    if (n >= 3 && nzchar(strings[[3]])) 
      data[["meta"]] <- base64_decode_object(strings[[3]])
    else data["meta"] <- list(NULL)
    builder$append(data)
  }
  annotations <- builder$data()
  head_start <- grep("^\\s*<head>\\s*$", contents, perl = TRUE)[[1]]
  head_end <- grep("^\\s*</head>\\s*$", contents, perl = TRUE)[[1]]
  list(source = contents, rmd = rmd_contents, header = contents[head_start:head_end], annotations = annotations)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pdf_document
list(`package:rmarkdown` = function (toc = FALSE, toc_depth = 2, number_sections = FALSE, fig_width = 6.5, fig_height = 4.5, fig_crop = "auto", fig_caption = TRUE, dev = "pdf", df_print = "default", highlight = "default", template = "default", keep_tex = FALSE, keep_md = FALSE, latex_engine = "pdflatex", citation_package = c("default", "natbib", "biblatex"), includes = NULL, md_extensions = NULL, output_extensions = NULL, pandoc_args = NULL, extra_dependencies = NULL) 
{
  args <- c("--self-contained")
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  if (!is.null(template) && !identical(template, "default")) {
    args <- c(args, "--template", pandoc_path_arg(template))
  }
  if (number_sections) 
    args <- c(args, "--number-sections")
  if (!is.null(highlight)) 
    highlight <- resolve_highlight(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))
  latex_engine <- match.arg(latex_engine, c("pdflatex", "lualatex", "xelatex", "tectonic"))
  args <- c(args, pandoc_latex_engine_args(latex_engine))
  args <- c(args, citation_package_arg(citation_package))
  args <- c(args, includes_to_pandoc_args(includes))
  if (identical(template, "default")) 
    args <- c(args, "--variable", "graphics")
  args <- c(args, pandoc_args)
  saved_files_dir <- NULL
  pdf_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    args <- append_in_header(process_header_includes(metadata))
    if (identical(template, "default")) {
      if (default_geometry(names(metadata), pandoc_args)) 
        args <- c(args, "--variable", "geometry:margin=1in")
      if (("subtitle" %in% names(metadata)) && !pandoc_available("2.6")) 
        args <- c(args, append_in_header(file = pkg_file("rmd/latex/subtitle.tex")))
    }
    if (length(extra_dependencies) || has_latex_dependencies(knit_meta)) {
      extra_dependencies <- latex_dependencies(extra_dependencies)
      all_dependencies <- append(extra_dependencies, flatten_latex_dependencies(knit_meta))
      args <- c(args, append_in_header(latex_dependencies_as_string(all_dependencies)))
    }
    args
  }
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    saved_files_dir <<- files_dir
    pdf_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir)
  }
  intermediates_generator <- function(...) {
    general_intermediates_generator(saved_files_dir, ...)
  }
  output_format(knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev), pandoc = pandoc_options(to = paste(c("latex", output_extensions), collapse = ""), from = from_rmarkdown(fig_caption, md_extensions), args = args, latex_engine = latex_engine, keep_tex = keep_tex, lua_filters = pkg_file_lua(c("pagebreak.lua", "latex-div.lua"))), clean_supporting = !keep_tex, keep_md = keep_md, df_print = df_print, pre_processor = pre_processor, intermediates_generator = intermediates_generator)
}, function (toc = FALSE, toc_depth = 2, number_sections = FALSE, fig_width = 6.5, fig_height = 4.5, fig_crop = "auto", fig_caption = TRUE, dev = "pdf", df_print = "default", highlight = "default", template = "default", keep_tex = FALSE, keep_md = FALSE, latex_engine = "pdflatex", citation_package = c("default", "natbib", "biblatex"), includes = NULL, md_extensions = NULL, output_extensions = NULL, pandoc_args = NULL, extra_dependencies = NULL) 
{
  args <- c("--self-contained")
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  if (!is.null(template) && !identical(template, "default")) {
    args <- c(args, "--template", pandoc_path_arg(template))
  }
  if (number_sections) 
    args <- c(args, "--number-sections")
  if (!is.null(highlight)) 
    highlight <- resolve_highlight(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))
  latex_engine <- match.arg(latex_engine, c("pdflatex", "lualatex", "xelatex", "tectonic"))
  args <- c(args, pandoc_latex_engine_args(latex_engine))
  args <- c(args, citation_package_arg(citation_package))
  args <- c(args, includes_to_pandoc_args(includes))
  if (identical(template, "default")) 
    args <- c(args, "--variable", "graphics")
  args <- c(args, pandoc_args)
  saved_files_dir <- NULL
  pdf_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    args <- append_in_header(process_header_includes(metadata))
    if (identical(template, "default")) {
      if (default_geometry(names(metadata), pandoc_args)) 
        args <- c(args, "--variable", "geometry:margin=1in")
      if (("subtitle" %in% names(metadata)) && !pandoc_available("2.6")) 
        args <- c(args, append_in_header(file = pkg_file("rmd/latex/subtitle.tex")))
    }
    if (length(extra_dependencies) || has_latex_dependencies(knit_meta)) {
      extra_dependencies <- latex_dependencies(extra_dependencies)
      all_dependencies <- append(extra_dependencies, flatten_latex_dependencies(knit_meta))
      args <- c(args, append_in_header(latex_dependencies_as_string(all_dependencies)))
    }
    args
  }
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    saved_files_dir <<- files_dir
    pdf_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir)
  }
  intermediates_generator <- function(...) {
    general_intermediates_generator(saved_files_dir, ...)
  }
  output_format(knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev), pandoc = pandoc_options(to = paste(c("latex", output_extensions), collapse = ""), from = from_rmarkdown(fig_caption, md_extensions), args = args, latex_engine = latex_engine, keep_tex = keep_tex, lua_filters = pkg_file_lua(c("pagebreak.lua", "latex-div.lua"))), clean_supporting = !keep_tex, keep_md = keep_md, df_print = df_print, pre_processor = pre_processor, intermediates_generator = intermediates_generator)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
pkg_file_lua
list(`package:rmarkdown` = function (filters = NULL, package = "rmarkdown") 
{
  files <- pkg_file("rmarkdown", "lua", if (is.null(filters)) 
    "."
    else filters, package = package, mustWork = TRUE)
  if (is.null(filters)) {
    files <- list.files(dirname(files), "[.]lua$", full.names = TRUE)
  }
  pandoc_path_arg(files)
}, function (filters = NULL, package = "rmarkdown") 
{
  files <- pkg_file("rmarkdown", "lua", if (is.null(filters)) 
    "."
    else filters, package = package, mustWork = TRUE)
  if (is.null(filters)) {
    files <- list.files(dirname(files), "[.]lua$", full.names = TRUE)
  }
  pandoc_path_arg(files)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
powerpoint_presentation
list(`package:rmarkdown` = function (toc = FALSE, toc_depth = 2, number_sections = FALSE, incremental = FALSE, fig_width = 5, fig_height = 4, fig_caption = TRUE, df_print = "default", keep_md = FALSE, md_extensions = NULL, slide_level = NULL, reference_doc = "default", pandoc_args = NULL) 
{
  pandoc_available("2.0.5", error = TRUE)
  knitr <- knitr_options(opts_chunk = list(dev = "png", dpi = 96, fig.width = fig_width, fig.height = fig_height))
  args <- c()
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  args <- c(args, reference_doc_args("doc", reference_doc))
  if (incremental) {
    if (!pandoc_available("2.15")) {
      warning("`incremental = TRUE` for powerpoint presentation is supported since Pandoc 2.15.\n", " It will have no effect with current Pandoc version used: ", pandoc_version(), ".", call. = FALSE)
    }
    else {
      args <- c(args, "--incremental")
    }
  }
  if (!is.null(slide_level)) 
    args <- c(args, "--slide-level", as.character(slide_level))
  args <- c(args, pandoc_args)
  saved_files_dir <- NULL
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    saved_files_dir <<- files_dir
    NULL
  }
  intermediates_generator <- function(...) {
    reference_intermediates_generator(saved_files_dir, ..., reference_doc)
  }
  output_format(knitr = knitr, pandoc = pandoc_options(to = "pptx", from = from_rmarkdown(fig_caption, md_extensions), args = args, lua_filters = if (number_sections) 
    pkg_file_lua("number-sections.lua")), keep_md = keep_md, df_print = df_print, pre_processor = pre_processor, intermediates_generator = intermediates_generator)
}, function (toc = FALSE, toc_depth = 2, number_sections = FALSE, incremental = FALSE, fig_width = 5, fig_height = 4, fig_caption = TRUE, df_print = "default", keep_md = FALSE, md_extensions = NULL, slide_level = NULL, reference_doc = "default", pandoc_args = NULL) 
{
  pandoc_available("2.0.5", error = TRUE)
  knitr <- knitr_options(opts_chunk = list(dev = "png", dpi = 96, fig.width = fig_width, fig.height = fig_height))
  args <- c()
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  args <- c(args, reference_doc_args("doc", reference_doc))
  if (incremental) {
    if (!pandoc_available("2.15")) {
      warning("`incremental = TRUE` for powerpoint presentation is supported since Pandoc 2.15.\n", " It will have no effect with current Pandoc version used: ", pandoc_version(), ".", call. = FALSE)
    }
    else {
      args <- c(args, "--incremental")
    }
  }
  if (!is.null(slide_level)) 
    args <- c(args, "--slide-level", as.character(slide_level))
  args <- c(args, pandoc_args)
  saved_files_dir <- NULL
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    saved_files_dir <<- files_dir
    NULL
  }
  intermediates_generator <- function(...) {
    reference_intermediates_generator(saved_files_dir, ..., reference_doc)
  }
  output_format(knitr = knitr, pandoc = pandoc_options(to = "pptx", from = from_rmarkdown(fig_caption, md_extensions), args = args, lua_filters = if (number_sections) 
    pkg_file_lua("number-sections.lua")), keep_md = keep_md, df_print = df_print, pre_processor = pre_processor, intermediates_generator = intermediates_generator)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
publish_site
list(`package:rmarkdown` = function (site_dir = ".", site_name = NULL, method = c("rsconnect"), server = NULL, account = NULL, render = TRUE, launch_browser = interactive()) 
{
  method <- match.arg(method)
  if (identical(method, "rsconnect")) {
    if (!requireNamespace("rsconnect", quietly = FALSE)) {
      stop("The rsconnect package is required to publish websites. ", "Please install rsconnect with install.packages(\"rsconnect\")")
    }
    accounts <- rsconnect::accounts()
    accounts <- subset(accounts, server != "shinyapps.io")
    if (is.null(server) && is.null(account)) {
      if (is.null(accounts) || nrow(accounts) == 0) 
        stop("You must specify a server to publish the website to")
      else if (nrow(accounts) == 1) {
        account <- accounts$name
        server <- accounts$server
      }
    }
    if (!is.null(server) && is.null(account)) {
      if (!grepl("^https?://", server)) 
        server_with_protocol <- paste0("https://", server)
      else server_with_protocol <- server
      server_with_protocol <- sub("/+$", "", server_with_protocol)
      server <- sub("^https?://", "", server_with_protocol)
      server_name <- server
      accounts <- rsconnect::accounts()
      accounts <- subset(accounts, server == server_name)
      if (nrow(accounts) == 0) {
        message(sprintf("You do not currently have a %s publishing account ", server), "configured on this system.")
        result = readline("Would you like to configure one now? [Y/n]: ")
        if (tolower(result) == "n") 
          return(invisible())
        servers <- rsconnect::servers()
        if (nrow(subset(servers, servers$name == server)) == 0) {
          rsconnect::addServer(sprintf("%s/__api__", server_with_protocol), server)
        }
        rsconnect::connectUser(server = server)
      }
      else if (nrow(accounts) == 1) {
        account <- accounts$name
      }
      else {
        stop("There is more than one account registered for ", server, "\nPlease specify which account you want to publish to.")
      }
    }
    rsconnect::deploySite(siteDir = site_dir, siteName = site_name, account = account, server = server, render = if (render) 
      "local"
      else "none", launch.browser = launch_browser)
  }
}, function (site_dir = ".", site_name = NULL, method = c("rsconnect"), server = NULL, account = NULL, render = TRUE, launch_browser = interactive()) 
{
  method <- match.arg(method)
  if (identical(method, "rsconnect")) {
    if (!requireNamespace("rsconnect", quietly = FALSE)) {
      stop("The rsconnect package is required to publish websites. ", "Please install rsconnect with install.packages(\"rsconnect\")")
    }
    accounts <- rsconnect::accounts()
    accounts <- subset(accounts, server != "shinyapps.io")
    if (is.null(server) && is.null(account)) {
      if (is.null(accounts) || nrow(accounts) == 0) 
        stop("You must specify a server to publish the website to")
      else if (nrow(accounts) == 1) {
        account <- accounts$name
        server <- accounts$server
      }
    }
    if (!is.null(server) && is.null(account)) {
      if (!grepl("^https?://", server)) 
        server_with_protocol <- paste0("https://", server)
      else server_with_protocol <- server
      server_with_protocol <- sub("/+$", "", server_with_protocol)
      server <- sub("^https?://", "", server_with_protocol)
      server_name <- server
      accounts <- rsconnect::accounts()
      accounts <- subset(accounts, server == server_name)
      if (nrow(accounts) == 0) {
        message(sprintf("You do not currently have a %s publishing account ", server), "configured on this system.")
        result = readline("Would you like to configure one now? [Y/n]: ")
        if (tolower(result) == "n") 
          return(invisible())
        servers <- rsconnect::servers()
        if (nrow(subset(servers, servers$name == server)) == 0) {
          rsconnect::addServer(sprintf("%s/__api__", server_with_protocol), server)
        }
        rsconnect::connectUser(server = server)
      }
      else if (nrow(accounts) == 1) {
        account <- accounts$name
      }
      else {
        stop("There is more than one account registered for ", server, "\nPlease specify which account you want to publish to.")
      }
    }
    rsconnect::deploySite(siteDir = site_dir, siteName = site_name, account = account, server = server, render = if (render) 
      "local"
      else "none", launch.browser = launch_browser)
  }
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
relative_to
list(`package:rmarkdown` = function (dir, file) 
{
  dir <- sub("/+$", "/", paste0(dir, "/"))
  if (identical(substr(file, 1, nchar(dir)), dir)) 
    file <- substr(file, nchar(dir) + 1, nchar(file))
  sub("^[.]/", "", file)
}, function (dir, file) 
{
  dir <- sub("/+$", "/", paste0(dir, "/"))
  if (identical(substr(file, 1, nchar(dir)), dir)) 
    file <- substr(file, nchar(dir) + 1, nchar(file))
  sub("^[.]/", "", file)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
render
list(`package:rmarkdown` = function (input, output_format = NULL, output_file = NULL, output_dir = NULL, output_options = NULL, output_yaml = NULL, intermediates_dir = NULL, knit_root_dir = NULL, runtime = c("auto", "static", "shiny", "shinyrmd", "shiny_prerendered"), clean = TRUE, params = NULL, knit_meta = NULL, envir = parent.frame(), run_pandoc = TRUE, quiet = FALSE, encoding = "UTF-8") 
{
  perf_timer_start("render")
  init_render_context()
  on.exit(clear_render_context(), add = TRUE)
  .globals$level <- .globals$level + 1
  on.exit({
    .globals$level <- .globals$level - 1
    if (.globals$level == 0) clean_tmpfiles()
  }, add = TRUE)
  if (identical(output_format, "all")) {
    output_format <- enumerate_output_formats(input)
    if (is.null(output_format)) 
      output_format <- "html_document"
  }
  if (is.character(output_format) && length(output_format) > 1) {
    outputs <- character()
    for (i in seq_along(output_format)) {
      output <- render(input = input, output_format = output_format[i], output_file = output_file[i], output_dir = output_dir, output_options = output_options, intermediates_dir = intermediates_dir, knit_root_dir = knit_root_dir, runtime = runtime, clean = clean, params = params, knit_meta = knit_meta, envir = envir, run_pandoc = run_pandoc, quiet = quiet)
      outputs <- c(outputs, output)
    }
    return(invisible(outputs))
  }
  if (run_pandoc) {
    required_pandoc <- "1.12.3"
    pandoc_available(required_pandoc, error = TRUE)
  }
  intermediates <- c()
  on.exit(if (clean) unlink(intermediates, recursive = TRUE), add = TRUE)
  if (!is.null(intermediates_dir)) {
    if (!dir_exists(intermediates_dir)) 
      dir.create(intermediates_dir, recursive = TRUE)
    intermediates_dir <- normalize_path(intermediates_dir)
  }
  intermediates_loc <- function(file) {
    if (is.null(intermediates_dir)) 
      file
    else file.path(intermediates_dir, file)
  }
  if (!is.null(output_dir)) {
    if (!dir_exists(output_dir)) 
      dir.create(output_dir, recursive = TRUE)
    output_dir <- normalize_path(output_dir)
  }
  requires_knit <- tolower(xfun::file_ext(input)) %in% c("r", "rmd", "rmarkdown", "qmd")
  original_input <- normalize_path(input)
  if (grepl(.shell_chars_regex, basename(input))) {
    input_no_shell_chars <- intermediates_loc(file_name_without_shell_chars(basename(input)))
    if (file.exists(input_no_shell_chars)) {
      stop2("The name of the input file cannot contain the special shell ", "characters: ", .shell_chars_regex, " (attempted to copy to a ", "version without those characters '", input_no_shell_chars, "' ", "however that file already exists)")
    }
    file.copy(input, input_no_shell_chars, overwrite = TRUE)
    intermediates <- c(intermediates, input_no_shell_chars)
    input <- input_no_shell_chars
    if (is.null(intermediates_dir)) {
      intermediates_dir <- dirname(normalize_path(input_no_shell_chars))
    }
  }
  if (!is.null(intermediates_dir) && same_path(intermediates_dir, dirname(original_input))) 
    intermediates_dir <- NULL
  force(knit_root_dir)
  oldwd <- setwd(dirname(abs_path(input)))
  on.exit(setwd(oldwd), add = TRUE)
  input <- basename(input)
  knit_input <- input
  knit_output <- intermediates_loc(file_with_meta_ext(input, "knit", getOption("rmarkdown.knit.ext", "md")))
  intermediates <- c(intermediates, knit_output)
  md_input <- identical(tolower(xfun::file_ext(input)), "md")
  if (identical(tolower(xfun::file_ext(input)), "r")) {
    spin_input <- intermediates_loc(file_with_meta_ext(input, "spin", "R"))
    file.copy(input, spin_input, overwrite = TRUE)
    intermediates <- c(intermediates, spin_input)
    spin_rmd <- knitr::spin(spin_input, knit = FALSE, envir = envir, format = "Rmd")
    intermediates <- c(intermediates, spin_rmd)
    knit_input <- spin_rmd
    meta1 <- yaml_front_matter(knit_input)
    meta2 <- list(title = input, author = Sys.info()[["user"]], date = as.character(Sys.Date()))
    for (i in names(meta2)) if (!is.null(meta1[[i]])) 
      meta2[[i]] <- NULL
    if (length(meta2)) {
      input_lines <- read_utf8(knit_input)
      write_utf8(c(input_lines, "\n\n---", yaml::as.yaml(meta2), "---"), knit_input)
    }
  }
  input_lines <- read_utf8(knit_input)
  front_matter <- parse_yaml_front_matter(input_lines)
  old_output_metadata <- output_metadata$get()
  on.exit(output_metadata$restore(old_output_metadata), add = TRUE)
  output_metadata$restore(as.list(front_matter[["rmd_output_metadata"]]))
  shiny_prerendered_dependencies <- list()
  if (requires_knit && is_shiny_prerendered(front_matter$runtime, front_matter$server)) {
    if (requireNamespace("shiny")) {
      if (!"package:shiny" %in% search()) 
        attachNamespace("shiny")
    }
    else stop("The shiny package is required for shiny documents")
    global_r <- file.path.ci(".", "global.R")
    if (file.exists(global_r)) {
      source(global_r, local = envir)
    }
    output_options$self_contained <- FALSE
    output_options$dependency_resolver <- function(deps) {
      shiny_prerendered_dependencies <<- list(deps = deps, packages = get_loaded_packages())
      list()
    }
  }
  if (!is_output_format(output_format)) {
    output_format <- output_format_from_yaml_front_matter(input_lines, output_options, output_format, output_yaml, output_file)
    output_format <- create_output_format(output_format$name, output_format$options)
  }
  pandoc_to <- output_format$pandoc$to
  output_auto <- pandoc_output_file(input, output_format$pandoc)
  if (is.null(output_file) || is.na(output_file)) 
    output_file <- output_auto
  else {
    if (!inherits(output_file, "AsIs") && xfun::file_ext(output_file) == "") 
      output_file <- paste(output_file, xfun::file_ext(output_auto), sep = ".")
  }
  if (!is.null(output_dir)) {
    output_file <- file.path(output_dir, basename(output_file))
  }
  output_dir <- dirname(output_file)
  if (!dir_exists(output_dir)) {
    stop2("The directory '", output_dir, "' does not not exist.")
  }
  files_dir_slash <- file.path(output_dir, knitr_files_dir(basename(output_file)))
  files_dir <- pandoc_path_arg(files_dir_slash)
  cache_dir <- NULL
  if (!is.null(intermediates_dir) && !is.null(output_format$intermediates_generator)) {
    intermediates <- c(intermediates, output_format$intermediates_generator(original_input, intermediates_dir))
  }
  old_knit_meta <- knit_meta_reset()
  on.exit({
    knit_meta_reset()
    if (length(old_knit_meta)) {
      knitr::knit_meta_add(old_knit_meta, attr(old_knit_meta, "knit_meta_id"))
    }
  }, add = TRUE)
  runtime <- match.arg(runtime)
  if (identical(runtime, "auto")) {
    if (is_shiny_prerendered(front_matter$runtime, front_matter$server)) {
      runtime <- "shiny_prerendered"
    }
    else {
      runtime <- front_matter$runtime %||% "static"
    }
  }
  context <- render_context()
  context$df_print <- resolve_df_print(output_format$df_print)
  env <- environment(render)
  metadata_this <- env$metadata
  do.call("unlockBinding", list("metadata", env))
  on.exit({
    if (bindingIsLocked("metadata", env)) {
      do.call("unlockBinding", list("metadata", env))
    }
    env$metadata <- metadata_this
    lockBinding("metadata", env)
  }, add = TRUE)
  env$metadata <- front_matter
  if (!is.null(output_format$pre_knit)) {
    output_format$pre_knit(input = original_input)
  }
  call_post_knit_handler <- function() {
    if (!is.null(output_format$post_knit)) {
      post_knit_extra_args <- output_format$post_knit(front_matter, knit_input, runtime, encoding = "UTF-8")
    }
    else {
      post_knit_extra_args <- NULL
    }
    c(output_format$pandoc$args, post_knit_extra_args)
  }
  id_prefix <- id_prefix_from_args(output_format$pandoc$args)
  if (!nzchar(id_prefix) && is_shiny(runtime, front_matter[["server"]])) {
    id_prefix <- "section-"
    output_format$pandoc$args <- c(output_format$pandoc$args, rbind("--id-prefix", id_prefix))
  }
  if (requires_knit) {
    optk <- knitr::opts_knit$get()
    on.exit(knitr::opts_knit$restore(optk), add = TRUE)
    optc <- knitr::opts_chunk$get()
    on.exit(knitr::opts_chunk$restore(optc), add = TRUE)
    hooks <- knitr::knit_hooks$get()
    on.exit(knitr::knit_hooks$restore(hooks), add = TRUE)
    ohooks <- knitr::opts_hooks$get()
    on.exit(knitr::opts_hooks$restore(ohooks), add = TRUE)
    templates <- knitr::opts_template$get()
    on.exit(knitr::opts_template$restore(templates), add = TRUE)
    if (pandoc2.0() && packageVersion("htmltools") >= "0.5.1") {
      if (is.null(prev <- getOption("htmltools.preserve.raw"))) {
        options(htmltools.preserve.raw = TRUE)
        on.exit(options(htmltools.preserve.raw = prev), add = TRUE)
      }
    }
    if (is.function(output_format$on_exit)) 
      on.exit(output_format$on_exit(), add = TRUE)
    knitr::render_markdown()
    knitr::opts_chunk$set(tidy = FALSE, error = FALSE)
    if (!grepl("[.]html$", output_file)) 
      knitr::opts_chunk$set(fig.retina = NULL)
    knitr::opts_knit$set(rmarkdown.pandoc.from = output_format$pandoc$from, rmarkdown.pandoc.to = pandoc_to, rmarkdown.pandoc.args = output_format$pandoc$args, rmarkdown.pandoc.id_prefix = id_prefix, rmarkdown.keep_md = output_format$keep_md, rmarkdown.df_print = output_format$df_print, rmarkdown.version = 2, rmarkdown.runtime = runtime)
    root_dir <- knit_root_dir
    if (is.null(root_dir)) 
      root_dir <- front_matter$knit_root_dir
    if (!is.null(root_dir)) 
      knitr::opts_knit$set(root.dir = root_dir)
    base_pandoc_to <- gsub("[-+].*", "", pandoc_to)
    if (base_pandoc_to == "html4") 
      base_pandoc_to <- "html"
    knitr::opts_chunk$set(fig.path = paste0(pandoc_path_arg(files_dir_slash, backslash = FALSE), "/figure-", base_pandoc_to, "/"))
    cache_dir <- knitr_cache_dir(input, base_pandoc_to)
    knitr::opts_chunk$set(cache.path = cache_dir)
    cache_dir <- gsub("/$", "", cache_dir)
    if (!is.null(output_format$knitr)) {
      knitr::opts_knit$set(as.list(output_format$knitr$opts_knit))
      knitr::opts_chunk$set(adjust_dev(as.list(output_format$knitr$opts_chunk)))
      knitr::opts_template$set(as.list(output_format$knitr$opts_template))
      knitr::knit_hooks$set(as.list(output_format$knitr$knit_hooks))
      knitr::opts_hooks$set(as.list(output_format$knitr$opts_hooks))
    }
    knitr::opts_knit$set(rmarkdown.runtime = runtime)
    if (is_shiny_prerendered(runtime)) {
      shiny_prerendered_remove_uncached_data(original_input)
      knitr::opts_hooks$set(label = shiny_prerendered_option_hook(original_input))
      knitr::knit_hooks$set(evaluate = shiny_prerendered_evaluate_hook(original_input))
    }
    if (is_shiny_classic(runtime) && !is.null(shiny::getDefaultReactiveDomain())) {
      knitr::knit_hooks$set(evaluate = function(code, envir, ...) {
        if (identical(knitr::opts_current$get("label"), "global")) {
          code_string <- one_string(code)
          if (!code_string %in% .globals$evaluated_global_chunks) {
            .globals$evaluated_global_chunks <- c(.globals$evaluated_global_chunks, code_string)
            shiny::withReactiveDomain(NULL, {
              evaluate::evaluate(code, envir = globalenv(), ...)
            })
          }
          else {
            list()
          }
        }
        else {
          evaluate::evaluate(code, envir, ...)
        }
      })
    }
    if (!is.null(front_matter$params)) {
      params <- knit_params_get(input_lines, params)
      hasParams <- exists("params", envir = envir, inherits = FALSE)
      envirParams <- NULL
      if (hasParams) {
        envirParams <- get("params", envir = envir, inherits = FALSE)
        isKnownParamsObject <- inherits(envirParams, "knit_param_list") || inherits(envirParams, "knit_param")
        if (!isKnownParamsObject) {
          stop2("params object already exists in knit environment ", "so can't be overwritten by render params")
        }
      }
      assign("params", params, envir = envir)
      lockBinding("params", envir)
      on.exit({
        if (exists("params", envir = envir, inherits = FALSE)) {
          do.call("unlockBinding", list("params", envir))
          if (hasParams) assign("params", envirParams, envir = envir) else remove("params", envir = envir)
        }
      }, add = TRUE)
    }
    sapply(as.list(getHook("rmarkdown.onKnit")), function(hook) {
      tryCatch(hook(input = original_input), error = function(e) NULL)
    })
    on.exit({
      sapply(as.list(getHook("rmarkdown.onKnitCompleted")), function(hook) {
        tryCatch(hook(input = original_input), error = function(e) NULL)
      })
    }, add = TRUE)
    perf_timer_start("knitr")
    input <- knitr::knit(knit_input, knit_output, envir = envir, quiet = quiet)
    perf_timer_stop("knitr")
    front_matter <- yaml_front_matter(input)
    output_format$pandoc$args <- call_post_knit_handler()
    rmd_warnings <- knit_meta_reset(class = "rmd_warning")
    for (rmd_warning in rmd_warnings) {
      message("Warning: ", rmd_warning)
    }
    shiny_prerendered_append_contexts(runtime, input)
    knit_meta <- knit_meta_reset()
  }
  else {
    output_format$pandoc$args <- call_post_knit_handler()
  }
  if (!(is_pandoc_to_html(output_format$pandoc) || identical(tolower(xfun::file_ext(output_file)), "html"))) {
    if (has_html_dependencies(knit_meta)) {
      if (!isTRUE(front_matter$always_allow_html)) {
        stop2("Functions that produce HTML output found in document targeting ", pandoc_to, " output.\nPlease change the output type ", "of this document to HTML. Alternatively, you can allow\n", "HTML output in non-HTML formats by adding this option to the YAML front", "-matter of\nyour rmarkdown file:\n\n", "  always_allow_html: true\n\n", "Note however that the HTML output will not be visible in non-HTML formats.\n\n")
      }
    }
    if (!identical(runtime, "static")) {
      stop2("Runtime '", runtime, "' is not supported for ", pandoc_to, " output.\nPlease change the output type ", "of this document to HTML.")
    }
  }
  intermediates_fig <- if (output_format$clean_supporting && !dir_exists(cache_dir)) {
    fig_path <- gsub("/$", "", knitr::opts_chunk$get("fig.path"))
    files_dir_fig <- list.files(files_dir, "^figure-.+")
    if (length(files_dir_fig) < 1 || identical(files_dir_fig, basename(fig_path))) {
      files_dir
    }
    else {
      fig_path
    }
  }
  intermediates <- c(intermediates, intermediates_fig)
  if (run_pandoc) {
    lua_env_vars <- xfun::set_envvar(c(RMARKDOWN_LUA_SHARED = pkg_file_lua("shared.lua")))
    on.exit(xfun::set_envvar(lua_env_vars), add = TRUE)
    perf_timer_start("pre-processor")
    if (!is.null(output_format$pre_processor)) {
      extra_args <- output_format$pre_processor(front_matter, input, runtime, knit_meta, files_dir, output_dir)
      output_format$pandoc$args <- c(output_format$pandoc$args, extra_args)
    }
    if (is_shiny_prerendered(runtime)) {
      shiny_prerendered_append_dependencies(input, shiny_prerendered_dependencies, files_dir, output_dir)
      output_format$pandoc$args <- c(output_format$pandoc$args, pandoc_include_args(in_header = pkg_file("rmd/h/shiny-header.html")))
    }
    perf_timer_stop("pre-processor")
    need_bibtex <- grepl("[.](pdf|tex)$", output_file) && any(c("--natbib", "--biblatex") %in% output_format$pandoc$args)
    perf_timer_start("pandoc")
    convert <- function(output, citeproc = FALSE) {
      if (!is.null(intermediates_dir)) {
        figures_dir <- gsub("/$", "", knitr::opts_chunk$get("fig.path"))
        files <- list.files(figures_dir, full.names = TRUE, recursive = TRUE)
        if (citeproc) 
          files <- c(files, front_matter[["bibliography"]])
        for (f in files) {
          intermediates <<- c(intermediates, copy_file_with_dir(f, intermediates_dir))
        }
      }
      input <- path.expand(input)
      output <- path.expand(output)
      pandoc_args <- output_format$pandoc$args
      if (!is.null(lua_filters <- output_format$pandoc$lua_filters)) {
        lua_filters <- pandoc_lua_filter_args(lua_filters)
      }
      pandoc_args <- c(lua_filters, pandoc_args)
      input_files <- input
      if (is.function(output_format$file_scope)) {
        input_files <- file_scope_split(input, output_format$file_scope)
        if (length(input_files) > 1) {
          pandoc_args <- c(pandoc_args, "--file-scope")
          on.exit(unlink(input_files), add = TRUE)
        }
      }
      if (!grepl(.shell_chars_regex, output) && !grepl(.shell_chars_regex, input)) {
        return(pandoc_convert(input_files, pandoc_to, output_format$pandoc$from, output, citeproc, pandoc_args, !quiet))
      }
      ext <- xfun::file_ext(output)
      if (ext != "") 
        ext <- paste0(".", ext)
      pandoc_output_tmp <- basename(tempfile("pandoc", getwd(), ext))
      on.exit(unlink(pandoc_output_tmp), add = TRUE)
      status <- pandoc_convert(input_files, pandoc_to, output_format$pandoc$from, pandoc_output_tmp, citeproc, pandoc_args, !quiet)
      pandoc_output_tmp_path <- file.path(dirname(input), pandoc_output_tmp)
      renamed <- suppressWarnings(file.rename(pandoc_output_tmp_path, output))
      if (!renamed) {
        copied <- file.copy(pandoc_output_tmp_path, output, overwrite = TRUE)
        if (!copied) {
          stop("failed to copy rendered pandoc artefact to '", output, "'")
        }
      }
      status
    }
    texfile <- file_with_ext(output_file, "tex")
    run_citeproc <- citeproc_required(front_matter, input_lines)
    if (output_format$pandoc$keep_tex || pandoc_to %in% c("latex", "beamer")) {
      convert(texfile, run_citeproc && !need_bibtex)
      if (!("--template" %in% output_format$pandoc$args)) 
        patch_tex_output(texfile)
      fix_horiz_rule(texfile)
      if (!grepl("[.]tex$", output_file)) {
        latexmk(texfile, output_format$pandoc$latex_engine, "--biblatex" %in% output_format$pandoc$args)
        file.rename(file_with_ext(texfile, "pdf"), output_file)
        if (!output_format$pandoc$keep_tex) {
          texfile <- normalize_path(texfile)
          on.exit(unlink(texfile), add = TRUE)
        }
      }
    }
    else {
      convert(output_file, run_citeproc)
    }
    if (!is.null(intermediates_dir)) {
      intermediate_output <- file.path(intermediates_dir, basename(output_file))
      if (file.exists(intermediate_output)) {
        move_dir(intermediate_output, output_file)
      }
    }
    perf_timer_stop("pandoc")
    perf_timer_start("post-processor")
    if (!is.null(output_format$post_processor)) 
      output_file <- output_format$post_processor(front_matter, input, output_file, clean, !quiet)
    if (!quiet && getOption("rmarkdown.render.message", TRUE)) {
      message("\nOutput created: ", relative_to(oldwd, output_file))
    }
    perf_timer_stop("post-processor")
  }
  perf_timer_stop("render")
  if (output_format$keep_md && !md_input) {
    file.copy(input, file_with_ext(output_file, "md"), overwrite = TRUE)
  }
  if (run_pandoc) {
    output_file <- abs_path(output_file)
    if (length(output_meta <- output_metadata$get())) 
      attr(output_file, "rmd_output_metadata") <- output_meta
    invisible(output_file)
  }
  else {
    intermediates <- setdiff(intermediates, c(input, intermediates_fig))
    structure(input, knit_meta = knit_meta, files_dir = files_dir, intermediates_dir = intermediates_fig, intermediates = intermediates)
  }
}, function (input, output_format = NULL, output_file = NULL, output_dir = NULL, output_options = NULL, output_yaml = NULL, intermediates_dir = NULL, knit_root_dir = NULL, runtime = c("auto", "static", "shiny", "shinyrmd", "shiny_prerendered"), clean = TRUE, params = NULL, knit_meta = NULL, envir = parent.frame(), run_pandoc = TRUE, quiet = FALSE, encoding = "UTF-8") 
{
  perf_timer_start("render")
  init_render_context()
  on.exit(clear_render_context(), add = TRUE)
  .globals$level <- .globals$level + 1
  on.exit({
    .globals$level <- .globals$level - 1
    if (.globals$level == 0) clean_tmpfiles()
  }, add = TRUE)
  if (identical(output_format, "all")) {
    output_format <- enumerate_output_formats(input)
    if (is.null(output_format)) 
      output_format <- "html_document"
  }
  if (is.character(output_format) && length(output_format) > 1) {
    outputs <- character()
    for (i in seq_along(output_format)) {
      output <- render(input = input, output_format = output_format[i], output_file = output_file[i], output_dir = output_dir, output_options = output_options, intermediates_dir = intermediates_dir, knit_root_dir = knit_root_dir, runtime = runtime, clean = clean, params = params, knit_meta = knit_meta, envir = envir, run_pandoc = run_pandoc, quiet = quiet)
      outputs <- c(outputs, output)
    }
    return(invisible(outputs))
  }
  if (run_pandoc) {
    required_pandoc <- "1.12.3"
    pandoc_available(required_pandoc, error = TRUE)
  }
  intermediates <- c()
  on.exit(if (clean) unlink(intermediates, recursive = TRUE), add = TRUE)
  if (!is.null(intermediates_dir)) {
    if (!dir_exists(intermediates_dir)) 
      dir.create(intermediates_dir, recursive = TRUE)
    intermediates_dir <- normalize_path(intermediates_dir)
  }
  intermediates_loc <- function(file) {
    if (is.null(intermediates_dir)) 
      file
    else file.path(intermediates_dir, file)
  }
  if (!is.null(output_dir)) {
    if (!dir_exists(output_dir)) 
      dir.create(output_dir, recursive = TRUE)
    output_dir <- normalize_path(output_dir)
  }
  requires_knit <- tolower(xfun::file_ext(input)) %in% c("r", "rmd", "rmarkdown", "qmd")
  original_input <- normalize_path(input)
  if (grepl(.shell_chars_regex, basename(input))) {
    input_no_shell_chars <- intermediates_loc(file_name_without_shell_chars(basename(input)))
    if (file.exists(input_no_shell_chars)) {
      stop2("The name of the input file cannot contain the special shell ", "characters: ", .shell_chars_regex, " (attempted to copy to a ", "version without those characters '", input_no_shell_chars, "' ", "however that file already exists)")
    }
    file.copy(input, input_no_shell_chars, overwrite = TRUE)
    intermediates <- c(intermediates, input_no_shell_chars)
    input <- input_no_shell_chars
    if (is.null(intermediates_dir)) {
      intermediates_dir <- dirname(normalize_path(input_no_shell_chars))
    }
  }
  if (!is.null(intermediates_dir) && same_path(intermediates_dir, dirname(original_input))) 
    intermediates_dir <- NULL
  force(knit_root_dir)
  oldwd <- setwd(dirname(abs_path(input)))
  on.exit(setwd(oldwd), add = TRUE)
  input <- basename(input)
  knit_input <- input
  knit_output <- intermediates_loc(file_with_meta_ext(input, "knit", getOption("rmarkdown.knit.ext", "md")))
  intermediates <- c(intermediates, knit_output)
  md_input <- identical(tolower(xfun::file_ext(input)), "md")
  if (identical(tolower(xfun::file_ext(input)), "r")) {
    spin_input <- intermediates_loc(file_with_meta_ext(input, "spin", "R"))
    file.copy(input, spin_input, overwrite = TRUE)
    intermediates <- c(intermediates, spin_input)
    spin_rmd <- knitr::spin(spin_input, knit = FALSE, envir = envir, format = "Rmd")
    intermediates <- c(intermediates, spin_rmd)
    knit_input <- spin_rmd
    meta1 <- yaml_front_matter(knit_input)
    meta2 <- list(title = input, author = Sys.info()[["user"]], date = as.character(Sys.Date()))
    for (i in names(meta2)) if (!is.null(meta1[[i]])) 
      meta2[[i]] <- NULL
    if (length(meta2)) {
      input_lines <- read_utf8(knit_input)
      write_utf8(c(input_lines, "\n\n---", yaml::as.yaml(meta2), "---"), knit_input)
    }
  }
  input_lines <- read_utf8(knit_input)
  front_matter <- parse_yaml_front_matter(input_lines)
  old_output_metadata <- output_metadata$get()
  on.exit(output_metadata$restore(old_output_metadata), add = TRUE)
  output_metadata$restore(as.list(front_matter[["rmd_output_metadata"]]))
  shiny_prerendered_dependencies <- list()
  if (requires_knit && is_shiny_prerendered(front_matter$runtime, front_matter$server)) {
    if (requireNamespace("shiny")) {
      if (!"package:shiny" %in% search()) 
        attachNamespace("shiny")
    }
    else stop("The shiny package is required for shiny documents")
    global_r <- file.path.ci(".", "global.R")
    if (file.exists(global_r)) {
      source(global_r, local = envir)
    }
    output_options$self_contained <- FALSE
    output_options$dependency_resolver <- function(deps) {
      shiny_prerendered_dependencies <<- list(deps = deps, packages = get_loaded_packages())
      list()
    }
  }
  if (!is_output_format(output_format)) {
    output_format <- output_format_from_yaml_front_matter(input_lines, output_options, output_format, output_yaml, output_file)
    output_format <- create_output_format(output_format$name, output_format$options)
  }
  pandoc_to <- output_format$pandoc$to
  output_auto <- pandoc_output_file(input, output_format$pandoc)
  if (is.null(output_file) || is.na(output_file)) 
    output_file <- output_auto
  else {
    if (!inherits(output_file, "AsIs") && xfun::file_ext(output_file) == "") 
      output_file <- paste(output_file, xfun::file_ext(output_auto), sep = ".")
  }
  if (!is.null(output_dir)) {
    output_file <- file.path(output_dir, basename(output_file))
  }
  output_dir <- dirname(output_file)
  if (!dir_exists(output_dir)) {
    stop2("The directory '", output_dir, "' does not not exist.")
  }
  files_dir_slash <- file.path(output_dir, knitr_files_dir(basename(output_file)))
  files_dir <- pandoc_path_arg(files_dir_slash)
  cache_dir <- NULL
  if (!is.null(intermediates_dir) && !is.null(output_format$intermediates_generator)) {
    intermediates <- c(intermediates, output_format$intermediates_generator(original_input, intermediates_dir))
  }
  old_knit_meta <- knit_meta_reset()
  on.exit({
    knit_meta_reset()
    if (length(old_knit_meta)) {
      knitr::knit_meta_add(old_knit_meta, attr(old_knit_meta, "knit_meta_id"))
    }
  }, add = TRUE)
  runtime <- match.arg(runtime)
  if (identical(runtime, "auto")) {
    if (is_shiny_prerendered(front_matter$runtime, front_matter$server)) {
      runtime <- "shiny_prerendered"
    }
    else {
      runtime <- front_matter$runtime %||% "static"
    }
  }
  context <- render_context()
  context$df_print <- resolve_df_print(output_format$df_print)
  env <- environment(render)
  metadata_this <- env$metadata
  do.call("unlockBinding", list("metadata", env))
  on.exit({
    if (bindingIsLocked("metadata", env)) {
      do.call("unlockBinding", list("metadata", env))
    }
    env$metadata <- metadata_this
    lockBinding("metadata", env)
  }, add = TRUE)
  env$metadata <- front_matter
  if (!is.null(output_format$pre_knit)) {
    output_format$pre_knit(input = original_input)
  }
  call_post_knit_handler <- function() {
    if (!is.null(output_format$post_knit)) {
      post_knit_extra_args <- output_format$post_knit(front_matter, knit_input, runtime, encoding = "UTF-8")
    }
    else {
      post_knit_extra_args <- NULL
    }
    c(output_format$pandoc$args, post_knit_extra_args)
  }
  id_prefix <- id_prefix_from_args(output_format$pandoc$args)
  if (!nzchar(id_prefix) && is_shiny(runtime, front_matter[["server"]])) {
    id_prefix <- "section-"
    output_format$pandoc$args <- c(output_format$pandoc$args, rbind("--id-prefix", id_prefix))
  }
  if (requires_knit) {
    optk <- knitr::opts_knit$get()
    on.exit(knitr::opts_knit$restore(optk), add = TRUE)
    optc <- knitr::opts_chunk$get()
    on.exit(knitr::opts_chunk$restore(optc), add = TRUE)
    hooks <- knitr::knit_hooks$get()
    on.exit(knitr::knit_hooks$restore(hooks), add = TRUE)
    ohooks <- knitr::opts_hooks$get()
    on.exit(knitr::opts_hooks$restore(ohooks), add = TRUE)
    templates <- knitr::opts_template$get()
    on.exit(knitr::opts_template$restore(templates), add = TRUE)
    if (pandoc2.0() && packageVersion("htmltools") >= "0.5.1") {
      if (is.null(prev <- getOption("htmltools.preserve.raw"))) {
        options(htmltools.preserve.raw = TRUE)
        on.exit(options(htmltools.preserve.raw = prev), add = TRUE)
      }
    }
    if (is.function(output_format$on_exit)) 
      on.exit(output_format$on_exit(), add = TRUE)
    knitr::render_markdown()
    knitr::opts_chunk$set(tidy = FALSE, error = FALSE)
    if (!grepl("[.]html$", output_file)) 
      knitr::opts_chunk$set(fig.retina = NULL)
    knitr::opts_knit$set(rmarkdown.pandoc.from = output_format$pandoc$from, rmarkdown.pandoc.to = pandoc_to, rmarkdown.pandoc.args = output_format$pandoc$args, rmarkdown.pandoc.id_prefix = id_prefix, rmarkdown.keep_md = output_format$keep_md, rmarkdown.df_print = output_format$df_print, rmarkdown.version = 2, rmarkdown.runtime = runtime)
    root_dir <- knit_root_dir
    if (is.null(root_dir)) 
      root_dir <- front_matter$knit_root_dir
    if (!is.null(root_dir)) 
      knitr::opts_knit$set(root.dir = root_dir)
    base_pandoc_to <- gsub("[-+].*", "", pandoc_to)
    if (base_pandoc_to == "html4") 
      base_pandoc_to <- "html"
    knitr::opts_chunk$set(fig.path = paste0(pandoc_path_arg(files_dir_slash, backslash = FALSE), "/figure-", base_pandoc_to, "/"))
    cache_dir <- knitr_cache_dir(input, base_pandoc_to)
    knitr::opts_chunk$set(cache.path = cache_dir)
    cache_dir <- gsub("/$", "", cache_dir)
    if (!is.null(output_format$knitr)) {
      knitr::opts_knit$set(as.list(output_format$knitr$opts_knit))
      knitr::opts_chunk$set(adjust_dev(as.list(output_format$knitr$opts_chunk)))
      knitr::opts_template$set(as.list(output_format$knitr$opts_template))
      knitr::knit_hooks$set(as.list(output_format$knitr$knit_hooks))
      knitr::opts_hooks$set(as.list(output_format$knitr$opts_hooks))
    }
    knitr::opts_knit$set(rmarkdown.runtime = runtime)
    if (is_shiny_prerendered(runtime)) {
      shiny_prerendered_remove_uncached_data(original_input)
      knitr::opts_hooks$set(label = shiny_prerendered_option_hook(original_input))
      knitr::knit_hooks$set(evaluate = shiny_prerendered_evaluate_hook(original_input))
    }
    if (is_shiny_classic(runtime) && !is.null(shiny::getDefaultReactiveDomain())) {
      knitr::knit_hooks$set(evaluate = function(code, envir, ...) {
        if (identical(knitr::opts_current$get("label"), "global")) {
          code_string <- one_string(code)
          if (!code_string %in% .globals$evaluated_global_chunks) {
            .globals$evaluated_global_chunks <- c(.globals$evaluated_global_chunks, code_string)
            shiny::withReactiveDomain(NULL, {
              evaluate::evaluate(code, envir = globalenv(), ...)
            })
          }
          else {
            list()
          }
        }
        else {
          evaluate::evaluate(code, envir, ...)
        }
      })
    }
    if (!is.null(front_matter$params)) {
      params <- knit_params_get(input_lines, params)
      hasParams <- exists("params", envir = envir, inherits = FALSE)
      envirParams <- NULL
      if (hasParams) {
        envirParams <- get("params", envir = envir, inherits = FALSE)
        isKnownParamsObject <- inherits(envirParams, "knit_param_list") || inherits(envirParams, "knit_param")
        if (!isKnownParamsObject) {
          stop2("params object already exists in knit environment ", "so can't be overwritten by render params")
        }
      }
      assign("params", params, envir = envir)
      lockBinding("params", envir)
      on.exit({
        if (exists("params", envir = envir, inherits = FALSE)) {
          do.call("unlockBinding", list("params", envir))
          if (hasParams) assign("params", envirParams, envir = envir) else remove("params", envir = envir)
        }
      }, add = TRUE)
    }
    sapply(as.list(getHook("rmarkdown.onKnit")), function(hook) {
      tryCatch(hook(input = original_input), error = function(e) NULL)
    })
    on.exit({
      sapply(as.list(getHook("rmarkdown.onKnitCompleted")), function(hook) {
        tryCatch(hook(input = original_input), error = function(e) NULL)
      })
    }, add = TRUE)
    perf_timer_start("knitr")
    input <- knitr::knit(knit_input, knit_output, envir = envir, quiet = quiet)
    perf_timer_stop("knitr")
    front_matter <- yaml_front_matter(input)
    output_format$pandoc$args <- call_post_knit_handler()
    rmd_warnings <- knit_meta_reset(class = "rmd_warning")
    for (rmd_warning in rmd_warnings) {
      message("Warning: ", rmd_warning)
    }
    shiny_prerendered_append_contexts(runtime, input)
    knit_meta <- knit_meta_reset()
  }
  else {
    output_format$pandoc$args <- call_post_knit_handler()
  }
  if (!(is_pandoc_to_html(output_format$pandoc) || identical(tolower(xfun::file_ext(output_file)), "html"))) {
    if (has_html_dependencies(knit_meta)) {
      if (!isTRUE(front_matter$always_allow_html)) {
        stop2("Functions that produce HTML output found in document targeting ", pandoc_to, " output.\nPlease change the output type ", "of this document to HTML. Alternatively, you can allow\n", "HTML output in non-HTML formats by adding this option to the YAML front", "-matter of\nyour rmarkdown file:\n\n", "  always_allow_html: true\n\n", "Note however that the HTML output will not be visible in non-HTML formats.\n\n")
      }
    }
    if (!identical(runtime, "static")) {
      stop2("Runtime '", runtime, "' is not supported for ", pandoc_to, " output.\nPlease change the output type ", "of this document to HTML.")
    }
  }
  intermediates_fig <- if (output_format$clean_supporting && !dir_exists(cache_dir)) {
    fig_path <- gsub("/$", "", knitr::opts_chunk$get("fig.path"))
    files_dir_fig <- list.files(files_dir, "^figure-.+")
    if (length(files_dir_fig) < 1 || identical(files_dir_fig, basename(fig_path))) {
      files_dir
    }
    else {
      fig_path
    }
  }
  intermediates <- c(intermediates, intermediates_fig)
  if (run_pandoc) {
    lua_env_vars <- xfun::set_envvar(c(RMARKDOWN_LUA_SHARED = pkg_file_lua("shared.lua")))
    on.exit(xfun::set_envvar(lua_env_vars), add = TRUE)
    perf_timer_start("pre-processor")
    if (!is.null(output_format$pre_processor)) {
      extra_args <- output_format$pre_processor(front_matter, input, runtime, knit_meta, files_dir, output_dir)
      output_format$pandoc$args <- c(output_format$pandoc$args, extra_args)
    }
    if (is_shiny_prerendered(runtime)) {
      shiny_prerendered_append_dependencies(input, shiny_prerendered_dependencies, files_dir, output_dir)
      output_format$pandoc$args <- c(output_format$pandoc$args, pandoc_include_args(in_header = pkg_file("rmd/h/shiny-header.html")))
    }
    perf_timer_stop("pre-processor")
    need_bibtex <- grepl("[.](pdf|tex)$", output_file) && any(c("--natbib", "--biblatex") %in% output_format$pandoc$args)
    perf_timer_start("pandoc")
    convert <- function(output, citeproc = FALSE) {
      if (!is.null(intermediates_dir)) {
        figures_dir <- gsub("/$", "", knitr::opts_chunk$get("fig.path"))
        files <- list.files(figures_dir, full.names = TRUE, recursive = TRUE)
        if (citeproc) 
          files <- c(files, front_matter[["bibliography"]])
        for (f in files) {
          intermediates <<- c(intermediates, copy_file_with_dir(f, intermediates_dir))
        }
      }
      input <- path.expand(input)
      output <- path.expand(output)
      pandoc_args <- output_format$pandoc$args
      if (!is.null(lua_filters <- output_format$pandoc$lua_filters)) {
        lua_filters <- pandoc_lua_filter_args(lua_filters)
      }
      pandoc_args <- c(lua_filters, pandoc_args)
      input_files <- input
      if (is.function(output_format$file_scope)) {
        input_files <- file_scope_split(input, output_format$file_scope)
        if (length(input_files) > 1) {
          pandoc_args <- c(pandoc_args, "--file-scope")
          on.exit(unlink(input_files), add = TRUE)
        }
      }
      if (!grepl(.shell_chars_regex, output) && !grepl(.shell_chars_regex, input)) {
        return(pandoc_convert(input_files, pandoc_to, output_format$pandoc$from, output, citeproc, pandoc_args, !quiet))
      }
      ext <- xfun::file_ext(output)
      if (ext != "") 
        ext <- paste0(".", ext)
      pandoc_output_tmp <- basename(tempfile("pandoc", getwd(), ext))
      on.exit(unlink(pandoc_output_tmp), add = TRUE)
      status <- pandoc_convert(input_files, pandoc_to, output_format$pandoc$from, pandoc_output_tmp, citeproc, pandoc_args, !quiet)
      pandoc_output_tmp_path <- file.path(dirname(input), pandoc_output_tmp)
      renamed <- suppressWarnings(file.rename(pandoc_output_tmp_path, output))
      if (!renamed) {
        copied <- file.copy(pandoc_output_tmp_path, output, overwrite = TRUE)
        if (!copied) {
          stop("failed to copy rendered pandoc artefact to '", output, "'")
        }
      }
      status
    }
    texfile <- file_with_ext(output_file, "tex")
    run_citeproc <- citeproc_required(front_matter, input_lines)
    if (output_format$pandoc$keep_tex || pandoc_to %in% c("latex", "beamer")) {
      convert(texfile, run_citeproc && !need_bibtex)
      if (!("--template" %in% output_format$pandoc$args)) 
        patch_tex_output(texfile)
      fix_horiz_rule(texfile)
      if (!grepl("[.]tex$", output_file)) {
        latexmk(texfile, output_format$pandoc$latex_engine, "--biblatex" %in% output_format$pandoc$args)
        file.rename(file_with_ext(texfile, "pdf"), output_file)
        if (!output_format$pandoc$keep_tex) {
          texfile <- normalize_path(texfile)
          on.exit(unlink(texfile), add = TRUE)
        }
      }
    }
    else {
      convert(output_file, run_citeproc)
    }
    if (!is.null(intermediates_dir)) {
      intermediate_output <- file.path(intermediates_dir, basename(output_file))
      if (file.exists(intermediate_output)) {
        move_dir(intermediate_output, output_file)
      }
    }
    perf_timer_stop("pandoc")
    perf_timer_start("post-processor")
    if (!is.null(output_format$post_processor)) 
      output_file <- output_format$post_processor(front_matter, input, output_file, clean, !quiet)
    if (!quiet && getOption("rmarkdown.render.message", TRUE)) {
      message("\nOutput created: ", relative_to(oldwd, output_file))
    }
    perf_timer_stop("post-processor")
  }
  perf_timer_stop("render")
  if (output_format$keep_md && !md_input) {
    file.copy(input, file_with_ext(output_file, "md"), overwrite = TRUE)
  }
  if (run_pandoc) {
    output_file <- abs_path(output_file)
    if (length(output_meta <- output_metadata$get())) 
      attr(output_file, "rmd_output_metadata") <- output_meta
    invisible(output_file)
  }
  else {
    intermediates <- setdiff(intermediates, c(input, intermediates_fig))
    structure(input, knit_meta = knit_meta, files_dir = files_dir, intermediates_dir = intermediates_fig, intermediates = intermediates)
  }
}, function (x) 
  if (isS4(x)) methods::show(x) else print(x))
c("package:rmarkdown", "namespace:rmarkdown", "namespace:evaluate")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
render_delayed
list(`package:rmarkdown` = function (expr) 
{
  env <- parent.frame()
  env_snapshot <- new.env(parent = parent.env(env))
  for (var in ls(env, all.names = TRUE)) assign(var, get(var, env), env_snapshot)
  assign("knitr_cached_chunk_opts", knitr::opts_current$get(), env_snapshot)
  assign("knitr_cached_knit_opts", knitr::opts_knit$get(), env_snapshot)
  assign("knitr_orig_expr", substitute(expr), env_snapshot)
  shiny::renderUI(quote({
    knitr::opts_current$restore(knitr_cached_chunk_opts)
    knitr::opts_knit$restore(knitr_cached_knit_opts)
    HTML(knitr::knit_print(eval(knitr_orig_expr), knitr::opts_current$get()))
  }), env = env_snapshot, quoted = TRUE)
}, function (expr) 
{
  env <- parent.frame()
  env_snapshot <- new.env(parent = parent.env(env))
  for (var in ls(env, all.names = TRUE)) assign(var, get(var, env), env_snapshot)
  assign("knitr_cached_chunk_opts", knitr::opts_current$get(), env_snapshot)
  assign("knitr_cached_knit_opts", knitr::opts_knit$get(), env_snapshot)
  assign("knitr_orig_expr", substitute(expr), env_snapshot)
  shiny::renderUI(quote({
    knitr::opts_current$restore(knitr_cached_chunk_opts)
    knitr::opts_knit$restore(knitr_cached_knit_opts)
    HTML(knitr::knit_print(eval(knitr_orig_expr), knitr::opts_current$get()))
  }), env = env_snapshot, quoted = TRUE)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
render_site
list(`package:rmarkdown` = function (input = ".", output_format = "all", envir = parent.frame(), quiet = FALSE, encoding = "UTF-8") 
{
  original_input <- input
  input <- input_as_dir(input)
  input_file <- NULL
  if (!dir_exists(original_input)) {
    input_file <- original_input
    if (output_format == "all") 
      output_format <- NULL
  }
  generator <- site_generator(input, output_format)
  if (is.null(generator)) 
    stop("No site generator found.")
  generator$render(input_file = input_file, output_format = output_format, envir = envir, quiet = quiet)
  if (!dir_exists(original_input)) 
    output <- file_with_ext(basename(original_input), "html")
  else output <- "index.html"
  output <- file.path(input, generator$output_dir, output)
  output <- normalized_relative_to(input, output)
  invisible(output)
}, function (input = ".", output_format = "all", envir = parent.frame(), quiet = FALSE, encoding = "UTF-8") 
{
  original_input <- input
  input <- input_as_dir(input)
  input_file <- NULL
  if (!dir_exists(original_input)) {
    input_file <- original_input
    if (output_format == "all") 
      output_format <- NULL
  }
  generator <- site_generator(input, output_format)
  if (is.null(generator)) 
    stop("No site generator found.")
  generator$render(input_file = input_file, output_format = output_format, envir = envir, quiet = quiet)
  if (!dir_exists(original_input)) 
    output <- file_with_ext(basename(original_input), "html")
  else output <- "index.html"
  output <- file.path(input, generator$output_dir, output)
  output <- normalized_relative_to(input, output)
  invisible(output)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
render_supporting_files
list(`package:rmarkdown` = function (from, files_dir, rename_to = NULL) 
{
  if (!dir_exists(files_dir)) 
    dir.create(files_dir)
  target_stage_dir <- file.path(files_dir, basename(from))
  target_dir <- file.path(files_dir, ifelse(is.null(rename_to), basename(from), rename_to))
  if (!dir_exists(target_dir) && !dir_exists(target_stage_dir)) {
    file.copy(from = from, to = files_dir, recursive = TRUE, copy.mode = FALSE)
    if (!is.null(rename_to)) {
      file.rename(from = target_stage_dir, to = target_dir)
    }
  }
  target_dir
}, function (from, files_dir, rename_to = NULL) 
{
  if (!dir_exists(files_dir)) 
    dir.create(files_dir)
  target_stage_dir <- file.path(files_dir, basename(from))
  target_dir <- file.path(files_dir, ifelse(is.null(rename_to), basename(from), rename_to))
  if (!dir_exists(target_dir) && !dir_exists(target_stage_dir)) {
    file.copy(from = from, to = files_dir, recursive = TRUE, copy.mode = FALSE)
    if (!is.null(rename_to)) {
      file.rename(from = target_stage_dir, to = target_dir)
    }
  }
  target_dir
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
resolve_output_format
list(`package:rmarkdown` = function (input, output_format = NULL, output_options = NULL, output_yaml = NULL) 
{
  input_lines <- read_utf8(input)
  if (!is.null(output_format) && !is.character(output_format)) 
    stop("output_format must be a character vector")
  output_format <- output_format_from_yaml_front_matter(input_lines, output_options, output_format, output_yaml)
  create_output_format(output_format$name, output_format$options)
}, function (input, output_format = NULL, output_options = NULL, output_yaml = NULL) 
{
  input_lines <- read_utf8(input)
  if (!is.null(output_format) && !is.character(output_format)) 
    stop("output_format must be a character vector")
  output_format <- output_format_from_yaml_front_matter(input_lines, output_options, output_format, output_yaml)
  create_output_format(output_format$name, output_format$options)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
rmarkdown_format
list(`package:rmarkdown` = function (extensions = NULL) 
{
  format <- c("markdown")
  addExtension <- function(extension) {
    if (length(grep(extension, extensions)) == 0) 
      format <<- c(format, paste0("+", extension))
  }
  addExtension("autolink_bare_uris")
  addExtension("tex_math_single_backslash")
  format <- c(format, extensions, recursive = TRUE)
  paste(format, collapse = "")
}, function (extensions = NULL) 
{
  format <- c("markdown")
  addExtension <- function(extension) {
    if (length(grep(extension, extensions)) == 0) 
      format <<- c(format, paste0("+", extension))
  }
  addExtension("autolink_bare_uris")
  addExtension("tex_math_single_backslash")
  format <- c(format, extensions, recursive = TRUE)
  paste(format, collapse = "")
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
rtf_document
list(`package:rmarkdown` = function (toc = FALSE, toc_depth = 3, number_sections = FALSE, fig_width = 5, fig_height = 4, keep_md = FALSE, md_extensions = NULL, pandoc_args = NULL) 
{
  knitr <- knitr_options(opts_chunk = list(dev = "png", dpi = 96, fig.width = fig_width, fig.height = fig_height))
  args <- c("--standalone")
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  args <- c(args, pandoc_args)
  preserved_chunks <- character()
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    preserved_chunks <<- extract_preserve_chunks(input_file, knitr::extract_raw_output)
    NULL
  }
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    output_str <- read_utf8(output_file)
    output_res <- knitr::restore_raw_output(output_str, preserved_chunks)
    if (!identical(output_str, output_res)) 
      write_utf8(output_res, output_file)
    output_file
  }
  output_format(knitr = knitr, pandoc = pandoc_options(to = "rtf", from = from_rmarkdown(extensions = md_extensions), args = args, lua_filters = if (number_sections) 
    pkg_file_lua("number-sections.lua")), keep_md = keep_md, pre_processor = pre_processor, post_processor = post_processor)
}, function (toc = FALSE, toc_depth = 3, number_sections = FALSE, fig_width = 5, fig_height = 4, keep_md = FALSE, md_extensions = NULL, pandoc_args = NULL) 
{
  knitr <- knitr_options(opts_chunk = list(dev = "png", dpi = 96, fig.width = fig_width, fig.height = fig_height))
  args <- c("--standalone")
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  args <- c(args, pandoc_args)
  preserved_chunks <- character()
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    preserved_chunks <<- extract_preserve_chunks(input_file, knitr::extract_raw_output)
    NULL
  }
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    output_str <- read_utf8(output_file)
    output_res <- knitr::restore_raw_output(output_str, preserved_chunks)
    if (!identical(output_str, output_res)) 
      write_utf8(output_res, output_file)
    output_file
  }
  output_format(knitr = knitr, pandoc = pandoc_options(to = "rtf", from = from_rmarkdown(extensions = md_extensions), args = args, lua_filters = if (number_sections) 
    pkg_file_lua("number-sections.lua")), keep_md = keep_md, pre_processor = pre_processor, post_processor = post_processor)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
run
list(`package:rmarkdown` = function (file = "index.Rmd", dir = dirname(file), default_file = NULL, auto_reload = TRUE, shiny_args = NULL, render_args = NULL) 
{
  if (missing(file) && !file.exists("index.Rmd") && file.exists("index.qmd")) 
    file <- "index.qmd"
  if (missing(file) && missing(default_file)) {
    if (!any(file.exists(file.path(dir, paste0("index.", c("qmd", "Rmd"))))) && any(file.exists(file.path(dir, paste0("ui.", c("qmd", "Rmd")))))) {
      uiRmd = file.path(dir, "ui.Rmd")
      file <- ifelse(file.exists(uiRmd), uiRmd, file.path(dir, "ui.qmd"))
    }
  }
  if (is.null(default_file)) {
    allRmds <- list.files(path = dir, pattern = "^[^_].*\\.[Rrq][Mm][Dd]$")
    if (length(allRmds) == 1) {
      default_file <- allRmds
    }
    else {
      index <- which(tolower(allRmds) %in% c("index.rmd", "index.qmd", "ui.rmd", "ui.qmd"))
      if (length(index) > 0) {
        default_file <- allRmds[index[1]]
      }
      else {
        for (rmd in allRmds) {
          yaml <- yaml_front_matter(file.path(dir, rmd))
          if (is_shiny(yaml$runtime, yaml$server)) {
            default_file <- rmd
            break
          }
        }
      }
    }
  }
  if (is.null(default_file)) {
    indexHtml <- list.files(dir, "(index|ui).html?", ignore.case = TRUE)
    if (length(indexHtml) > 0) 
      default_file <- indexHtml[1]
  }
  dir <- normalize_path(dir)
  if (!dir_exists(dir)) 
    stop("The directory '", dir, "' does not exist")
  if (!is.null(file)) {
    file_rel <- normalize_path(file)
    if (identical(substr(file_rel, 1, nchar(dir)), dir)) 
      file_rel <- substr(file_rel, nchar(dir) + 2, nchar(file_rel))
    if (is.null(default_file)) {
      resolved <- resolve_relative(dir, file_rel)
      if (is.null(resolved) || !file.exists(resolved)) 
        stop("The file '", file, "' does not exist in the directory '", dir, "'")
    }
  }
  if (is.null(render_args$envir)) 
    render_args$envir <- parent.frame()
  target_file <- file %||% file.path(dir, default_file)
  yaml_front <- if (length(target_file)) 
    yaml_front_matter(target_file)
  runtime <- yaml_front$runtime
  server <- yaml_front$server
  theme <- render_args$output_options$theme
  if (length(target_file)) {
    format <- output_format_from_yaml_front_matter(read_utf8(target_file))
    old_theme <- shiny::getCurrentTheme()
    on.exit(set_current_theme(old_theme), add = TRUE)
    set_current_theme(resolve_theme(format$options$theme))
  }
  if (is_shiny_prerendered(runtime, server)) {
    app <- shiny_prerendered_app(target_file, render_args = render_args)
  }
  else {
    onStart <- function() {
      global_r <- file.path.ci(dir, "global.R")
      if (file.exists(global_r)) {
        source(global_r, local = FALSE)
      }
      shiny::addResourcePath("rmd_resources", pkg_file("rmd/h/rmarkdown"))
    }
    app <- shiny::shinyApp(ui = rmarkdown_shiny_ui(dir, default_file), uiPattern = "^/$|^/index\\.html?$|^(/.*\\.[Rrq][Mm][Dd])$", onStart = onStart, server = rmarkdown_shiny_server(dir, default_file, auto_reload, render_args))
    on.exit({
      .globals$evaluated_global_chunks <- character()
    }, add = TRUE)
  }
  launch_browser <- shiny_args$launch.browser %||% (!is.null(file) && interactive())
  if (isTRUE(launch_browser)) {
    launch_browser <- function(url) {
      url <- paste(url, file_rel, sep = "/")
      browser <- getOption("shiny.launch.browser")
      if (is.function(browser)) {
        browser(url)
      }
      else {
        utils::browseURL(url)
      }
    }
  }
  shiny_args <- merge_lists(list(appDir = app, launch.browser = launch_browser), shiny_args)
  ret <- do.call(shiny::runApp, shiny_args)
  invisible(ret)
}, function (command = NULL, args = character(), error_on_status = TRUE, wd = NULL, echo_cmd = FALSE, echo = FALSE, spinner = FALSE, timeout = Inf, stdout = "|", stderr = "|", stdout_line_callback = NULL, stdout_callback = NULL, stderr_line_callback = NULL, stderr_callback = NULL, stderr_to_stdout = FALSE, env = NULL, windows_verbatim_args = FALSE, windows_hide_window = FALSE, encoding = "", cleanup_tree = FALSE, ...) 
{
  assert_that(is_flag(error_on_status))
  assert_that(is_time_interval(timeout))
  assert_that(is_flag(spinner))
  assert_that(is_string_or_null(stdout))
  assert_that(is_string_or_null(stderr))
  assert_that(is.null(stdout_line_callback) || is.function(stdout_line_callback))
  assert_that(is.null(stderr_line_callback) || is.function(stderr_line_callback))
  assert_that(is.null(stdout_callback) || is.function(stdout_callback))
  assert_that(is.null(stderr_callback) || is.function(stderr_callback))
  assert_that(is_flag(cleanup_tree))
  assert_that(is_flag(stderr_to_stdout))
  "!DEBUG run() Checked arguments"
  if (!interactive()) 
    spinner <- FALSE
  if (stderr_to_stdout) 
    stderr <- "2>&1"
  pr <- process$new(command, args, echo_cmd = echo_cmd, wd = wd, windows_verbatim_args = windows_verbatim_args, windows_hide_window = windows_hide_window, stdout = stdout, stderr = stderr, env = env, encoding = encoding, cleanup_tree = cleanup_tree, ...)
  "#!DEBUG run() Started the process: `pr$get_pid()`"
  if (cleanup_tree) {
    on.exit(pr$kill_tree(), add = TRUE)
  }
  else {
    on.exit(pr$kill(), add = TRUE)
  }
  if (echo) {
    stdout_callback <- echo_callback(stdout_callback, "stdout")
    stderr_callback <- echo_callback(stderr_callback, "stderr")
  }
  runcall <- sys.call()
  resenv <- new.env(parent = emptyenv())
  has_stdout <- !is.null(stdout) && stdout == "|"
  has_stderr <- !is.null(stderr) && stderr == "|"
  if (has_stdout) {
    resenv$outbuf <- make_buffer()
    on.exit(resenv$outbuf$done(), add = TRUE)
  }
  if (has_stderr) {
    resenv$errbuf <- make_buffer()
    on.exit(resenv$errbuf$done(), add = TRUE)
  }
  res <- tryCatch(run_manage(pr, timeout, spinner, stdout, stderr, stdout_line_callback, stdout_callback, stderr_line_callback, stderr_callback, resenv), interrupt = function(e) {
    "!DEBUG run() process `pr$get_pid()` killed on interrupt"
    out <- if (has_stdout) {
      resenv$outbuf$push(pr$read_output())
      resenv$outbuf$push(pr$read_output())
      resenv$outbuf$read()
    }
    err <- if (has_stderr) {
      resenv$errbuf$push(pr$read_error())
      resenv$errbuf$push(pr$read_error())
      resenv$errbuf$read()
    }
    tryCatch(pr$kill(), error = function(e) NULL)
    signalCondition(new_process_interrupt_cond(list(interrupt = TRUE, stderr = err, stdout = out, command = command, args = args), runcall, echo = echo, stderr_to_stdout = stderr_to_stdout))
    cat("\n")
    invokeRestart("abort")
  })
  if (error_on_status && (is.na(res$status) || res$status != 0)) {
    "!DEBUG run() error on status `res$status` for process `pr$get_pid()`"
    throw(new_process_error(res, call = sys.call(), echo = echo, stderr_to_stdout, res$status, command = command, args = args))
  }
  res
}, function (file = "index.Rmd", dir = dirname(file), default_file = NULL, auto_reload = TRUE, shiny_args = NULL, render_args = NULL) 
{
  if (missing(file) && !file.exists("index.Rmd") && file.exists("index.qmd")) 
    file <- "index.qmd"
  if (missing(file) && missing(default_file)) {
    if (!any(file.exists(file.path(dir, paste0("index.", c("qmd", "Rmd"))))) && any(file.exists(file.path(dir, paste0("ui.", c("qmd", "Rmd")))))) {
      uiRmd = file.path(dir, "ui.Rmd")
      file <- ifelse(file.exists(uiRmd), uiRmd, file.path(dir, "ui.qmd"))
    }
  }
  if (is.null(default_file)) {
    allRmds <- list.files(path = dir, pattern = "^[^_].*\\.[Rrq][Mm][Dd]$")
    if (length(allRmds) == 1) {
      default_file <- allRmds
    }
    else {
      index <- which(tolower(allRmds) %in% c("index.rmd", "index.qmd", "ui.rmd", "ui.qmd"))
      if (length(index) > 0) {
        default_file <- allRmds[index[1]]
      }
      else {
        for (rmd in allRmds) {
          yaml <- yaml_front_matter(file.path(dir, rmd))
          if (is_shiny(yaml$runtime, yaml$server)) {
            default_file <- rmd
            break
          }
        }
      }
    }
  }
  if (is.null(default_file)) {
    indexHtml <- list.files(dir, "(index|ui).html?", ignore.case = TRUE)
    if (length(indexHtml) > 0) 
      default_file <- indexHtml[1]
  }
  dir <- normalize_path(dir)
  if (!dir_exists(dir)) 
    stop("The directory '", dir, "' does not exist")
  if (!is.null(file)) {
    file_rel <- normalize_path(file)
    if (identical(substr(file_rel, 1, nchar(dir)), dir)) 
      file_rel <- substr(file_rel, nchar(dir) + 2, nchar(file_rel))
    if (is.null(default_file)) {
      resolved <- resolve_relative(dir, file_rel)
      if (is.null(resolved) || !file.exists(resolved)) 
        stop("The file '", file, "' does not exist in the directory '", dir, "'")
    }
  }
  if (is.null(render_args$envir)) 
    render_args$envir <- parent.frame()
  target_file <- file %||% file.path(dir, default_file)
  yaml_front <- if (length(target_file)) 
    yaml_front_matter(target_file)
  runtime <- yaml_front$runtime
  server <- yaml_front$server
  theme <- render_args$output_options$theme
  if (length(target_file)) {
    format <- output_format_from_yaml_front_matter(read_utf8(target_file))
    old_theme <- shiny::getCurrentTheme()
    on.exit(set_current_theme(old_theme), add = TRUE)
    set_current_theme(resolve_theme(format$options$theme))
  }
  if (is_shiny_prerendered(runtime, server)) {
    app <- shiny_prerendered_app(target_file, render_args = render_args)
  }
  else {
    onStart <- function() {
      global_r <- file.path.ci(dir, "global.R")
      if (file.exists(global_r)) {
        source(global_r, local = FALSE)
      }
      shiny::addResourcePath("rmd_resources", pkg_file("rmd/h/rmarkdown"))
    }
    app <- shiny::shinyApp(ui = rmarkdown_shiny_ui(dir, default_file), uiPattern = "^/$|^/index\\.html?$|^(/.*\\.[Rrq][Mm][Dd])$", onStart = onStart, server = rmarkdown_shiny_server(dir, default_file, auto_reload, render_args))
    on.exit({
      .globals$evaluated_global_chunks <- character()
    }, add = TRUE)
  }
  launch_browser <- shiny_args$launch.browser %||% (!is.null(file) && interactive())
  if (isTRUE(launch_browser)) {
    launch_browser <- function(url) {
      url <- paste(url, file_rel, sep = "/")
      browser <- getOption("shiny.launch.browser")
      if (is.function(browser)) {
        browser(url)
      }
      else {
        utils::browseURL(url)
      }
    }
  }
  shiny_args <- merge_lists(list(appDir = app, launch.browser = launch_browser), shiny_args)
  ret <- do.call(shiny::runApp, shiny_args)
  invisible(ret)
}, function (...) 
  UseMethod("run"))
c("package:rmarkdown", "namespace:processx", "namespace:rmarkdown", "namespace:future")
c(TRUE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, FALSE)
shiny_prerendered_chunk
list(`package:rmarkdown` = function (context, code, singleton = FALSE) 
{
  if (!is_shiny_prerendered(knitr::opts_knit$get("rmarkdown.runtime"))) 
    stop2("The shiny_prerendered_chunk function can only be called from ", "within a shiny server compatible document")
  knitr::knit_meta_add(list(structure(class = "shiny_prerendered", list(name = context, code = code, singleton = singleton))))
  invisible(NULL)
}, function (context, code, singleton = FALSE) 
{
  if (!is_shiny_prerendered(knitr::opts_knit$get("rmarkdown.runtime"))) 
    stop2("The shiny_prerendered_chunk function can only be called from ", "within a shiny server compatible document")
  knitr::knit_meta_add(list(structure(class = "shiny_prerendered", list(name = context, code = code, singleton = singleton))))
  invisible(NULL)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
shiny_prerendered_clean
list(`package:rmarkdown` = function (input) 
{
  html_file <- file_with_ext(input, "html")
  if (file.exists(html_file)) 
    file.remove(html_file)
  cache_dir <- knitr_root_cache_dir(input)
  if (dir_exists(cache_dir)) 
    unlink(cache_dir, recursive = TRUE)
  files_dir <- knitr_files_dir(input)
  if (dir_exists(files_dir)) 
    unlink(files_dir, recursive = TRUE)
  data_dir <- shiny_prerendered_data_dir(input)
  if (dir_exists(data_dir)) 
    unlink(data_dir, recursive = TRUE)
}, function (input) 
{
  html_file <- file_with_ext(input, "html")
  if (file.exists(html_file)) 
    file.remove(html_file)
  cache_dir <- knitr_root_cache_dir(input)
  if (dir_exists(cache_dir)) 
    unlink(cache_dir, recursive = TRUE)
  files_dir <- knitr_files_dir(input)
  if (dir_exists(files_dir)) 
    unlink(files_dir, recursive = TRUE)
  data_dir <- shiny_prerendered_data_dir(input)
  if (dir_exists(data_dir)) 
    unlink(data_dir, recursive = TRUE)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
shiny_prerendered_server_start_code
list(`package:rmarkdown` = function (server_envir) 
{
  if (exists(".shiny_prerendered_server_start_code", envir = server_envir)) 
    get(".shiny_prerendered_server_start_code", envir = server_envir)
  else ""
}, function (server_envir) 
{
  if (exists(".shiny_prerendered_server_start_code", envir = server_envir)) 
    get(".shiny_prerendered_server_start_code", envir = server_envir)
  else ""
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
site_config
list(`package:rmarkdown` = function (input = ".", encoding = "UTF-8") 
{
  input <- input_as_dir(input)
  config_file <- site_config_file(input)
  if (file.exists(config_file)) {
    config_lines <- read_utf8(config_file)
    config <- yaml_load(config_lines)
    if (!is.list(config)) 
      config <- list()
    if (is.null(config$name)) 
      config$name <- basename(normalize_path(input))
    if (is.null(config$output_dir)) 
      config$output_dir <- "_site"
    if (is.null(config$new_session)) 
      config$new_session <- FALSE
    config
  }
  else {
    NULL
  }
}, function (input = ".", encoding = "UTF-8") 
{
  input <- input_as_dir(input)
  config_file <- site_config_file(input)
  if (file.exists(config_file)) {
    config_lines <- read_utf8(config_file)
    config <- yaml_load(config_lines)
    if (!is.list(config)) 
      config <- list()
    if (is.null(config$name)) 
      config$name <- basename(normalize_path(input))
    if (is.null(config$output_dir)) 
      config$output_dir <- "_site"
    if (is.null(config$new_session)) 
      config$new_session <- FALSE
    config
  }
  else {
    NULL
  }
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
site_generator
list(`package:rmarkdown` = function (input = ".", output_format = NULL) 
{
  has_rproj <- function(dir) {
    length(list.files(dir, "[.]Rproj$")) != 0
  }
  root <- tryCatch(proj_root(input, "^index.R?md$", "^\\s*site:.*::.*$", has_rproj), error = function(e) NULL)
  input <- input_as_dir(input)
  if (is.null(root)) {
    if (file.exists(site_config_file(input))) {
      return(default_site(input))
    }
    else {
      return(NULL)
    }
  }
  index <- file.path(root, "index.Rmd")
  if (!file.exists(index)) 
    index <- file.path(root, "index.md")
  in_subdir <- !same_path(input, root)
  front_matter <- yaml_front_matter(index)
  create_site_generator <- eval(xfun::parse_only(front_matter$site))
  if (!is.function(create_site_generator)) 
    stop("Cannot find the site generator from the 'site' field in YAML frontmatter ", "of '", index, "'.")
  generator <- create_site_generator(root)
  if (in_subdir) {
    if (isTRUE(generator$subdirs)) {
      generator
    }
    else {
      NULL
    }
  }
  else {
    generator
  }
}, function (input = ".", output_format = NULL) 
{
  has_rproj <- function(dir) {
    length(list.files(dir, "[.]Rproj$")) != 0
  }
  root <- tryCatch(proj_root(input, "^index.R?md$", "^\\s*site:.*::.*$", has_rproj), error = function(e) NULL)
  input <- input_as_dir(input)
  if (is.null(root)) {
    if (file.exists(site_config_file(input))) {
      return(default_site(input))
    }
    else {
      return(NULL)
    }
  }
  index <- file.path(root, "index.Rmd")
  if (!file.exists(index)) 
    index <- file.path(root, "index.md")
  in_subdir <- !same_path(input, root)
  front_matter <- yaml_front_matter(index)
  create_site_generator <- eval(xfun::parse_only(front_matter$site))
  if (!is.function(create_site_generator)) 
    stop("Cannot find the site generator from the 'site' field in YAML frontmatter ", "of '", index, "'.")
  generator <- create_site_generator(root)
  if (in_subdir) {
    if (isTRUE(generator$subdirs)) {
      generator
    }
    else {
      NULL
    }
  }
  else {
    generator
  }
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
site_resources
list(`package:rmarkdown` = function (site_dir, include = NULL, exclude = NULL, recursive = FALSE) 
{
  all_files <- list.files(site_dir, all.files = TRUE)
  extensions <- c("R", "r", "S", "s", "Rmd", "rmd", "md", "Rmarkdown", "rmarkdown", "Rproj", "rproj", "RData", "rdata", "rds")
  extensions_regex <- utils::glob2rx(paste0("*.", extensions))
  excludes <- c("^rsconnect$", "^packrat$", "^renv$", "^\\..*$", "^_.*$", "^.*_cache$", extensions_regex, utils::glob2rx(exclude))
  files <- all_files
  for (exclude in excludes) files <- files[!grepl(exclude, files)]
  includes <- utils::glob2rx(include)
  for (include in includes) {
    include_files <- all_files[grepl(include, all_files)]
    files <- unique(c(files, include_files))
  }
  if (recursive) {
    recursive_files <- c()
    for (file in files) {
      file_path <- file.path(site_dir, file)
      if (dir_exists(file_path)) {
        dir_files <- file.path(list.files(file_path, full.names = FALSE, recursive = TRUE))
        dir_files <- file.path(file, dir_files)
        recursive_files <- c(recursive_files, dir_files)
      }
      else {
        recursive_files <- c(recursive_files, file)
      }
    }
    recursive_files
  }
  else {
    files
  }
}, function (site_dir, include = NULL, exclude = NULL, recursive = FALSE) 
{
  all_files <- list.files(site_dir, all.files = TRUE)
  extensions <- c("R", "r", "S", "s", "Rmd", "rmd", "md", "Rmarkdown", "rmarkdown", "Rproj", "rproj", "RData", "rdata", "rds")
  extensions_regex <- utils::glob2rx(paste0("*.", extensions))
  excludes <- c("^rsconnect$", "^packrat$", "^renv$", "^\\..*$", "^_.*$", "^.*_cache$", extensions_regex, utils::glob2rx(exclude))
  files <- all_files
  for (exclude in excludes) files <- files[!grepl(exclude, files)]
  includes <- utils::glob2rx(include)
  for (include in includes) {
    include_files <- all_files[grepl(include, all_files)]
    files <- unique(c(files, include_files))
  }
  if (recursive) {
    recursive_files <- c()
    for (file in files) {
      file_path <- file.path(site_dir, file)
      if (dir_exists(file_path)) {
        dir_files <- file.path(list.files(file_path, full.names = FALSE, recursive = TRUE))
        dir_files <- file.path(file, dir_files)
        recursive_files <- c(recursive_files, dir_files)
      }
      else {
        recursive_files <- c(recursive_files, file)
      }
    }
    recursive_files
  }
  else {
    files
  }
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
slidy_presentation
list(`package:rmarkdown` = function (number_sections = FALSE, incremental = FALSE, slide_level = NULL, duration = NULL, footer = NULL, font_adjustment = 0, fig_width = 8, fig_height = 6, fig_retina = 2, fig_caption = TRUE, dev = "png", df_print = "default", self_contained = TRUE, highlight = "default", math_method = "default", mathjax = "default", template = "default", css = NULL, includes = NULL, keep_md = FALSE, lib_dir = NULL, md_extensions = NULL, pandoc_args = NULL, extra_dependencies = NULL, 
                                     ...) 
{
  args <- c()
  if (identical(template, "default")) 
    template <- pkg_file("rmd/slidy/default.html")
  if (!is.null(template)) 
    args <- c(args, "--template", pandoc_path_arg(template))
  extra_dependencies <- append(extra_dependencies, list(html_dependency_slidy(), html_dependency_slidy_shiny()))
  if (incremental) 
    args <- c(args, "--incremental")
  if (!is.null(slide_level)) 
    args <- c(args, "--slide-level", slide_level)
  if (!is.null(duration)) 
    args <- c(args, pandoc_variable_arg("duration", duration))
  if (!is.null(footer)) 
    args <- c(args, pandoc_variable_arg("footer", footer))
  if (font_adjustment != 0) 
    args <- c(args, pandoc_variable_arg("font-size-adjustment", font_adjustment))
  args <- c(args, includes_to_pandoc_args(includes))
  if (identical(df_print, "paged")) {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_pagedtable()))
  }
  for (css_file in css) args <- c(args, "--css", pandoc_path_arg(css_file, backslash = FALSE))
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    if (is.null(lib_dir)) 
      lib_dir <- files_dir
    args <- c()
    if (!is.null(highlight)) 
      highlight <- resolve_highlight(highlight, highlighters())
    args <- c(args, pandoc_highlight_args(highlight, default = "pygments"))
    args
  }
  output_format(knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md, dev), pandoc = pandoc_options(to = "slidy", from = from_rmarkdown(fig_caption, md_extensions), args = args, lua_filters = if (number_sections) 
    pkg_file_lua("number-sections.lua")), keep_md = keep_md, clean_supporting = self_contained, df_print = df_print, pre_processor = pre_processor, base_format = html_document_base(lib_dir = lib_dir, self_contained = self_contained, math_method = math_method, mathjax = mathjax, bootstrap_compatible = TRUE, pandoc_args = pandoc_args, extra_dependencies = extra_dependencies, ...))
}, function (number_sections = FALSE, incremental = FALSE, slide_level = NULL, duration = NULL, footer = NULL, font_adjustment = 0, fig_width = 8, fig_height = 6, fig_retina = 2, fig_caption = TRUE, dev = "png", df_print = "default", self_contained = TRUE, highlight = "default", math_method = "default", mathjax = "default", template = "default", css = NULL, includes = NULL, keep_md = FALSE, lib_dir = NULL, md_extensions = NULL, pandoc_args = NULL, extra_dependencies = NULL, ...) 
{
  args <- c()
  if (identical(template, "default")) 
    template <- pkg_file("rmd/slidy/default.html")
  if (!is.null(template)) 
    args <- c(args, "--template", pandoc_path_arg(template))
  extra_dependencies <- append(extra_dependencies, list(html_dependency_slidy(), html_dependency_slidy_shiny()))
  if (incremental) 
    args <- c(args, "--incremental")
  if (!is.null(slide_level)) 
    args <- c(args, "--slide-level", slide_level)
  if (!is.null(duration)) 
    args <- c(args, pandoc_variable_arg("duration", duration))
  if (!is.null(footer)) 
    args <- c(args, pandoc_variable_arg("footer", footer))
  if (font_adjustment != 0) 
    args <- c(args, pandoc_variable_arg("font-size-adjustment", font_adjustment))
  args <- c(args, includes_to_pandoc_args(includes))
  if (identical(df_print, "paged")) {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_pagedtable()))
  }
  for (css_file in css) args <- c(args, "--css", pandoc_path_arg(css_file, backslash = FALSE))
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    if (is.null(lib_dir)) 
      lib_dir <- files_dir
    args <- c()
    if (!is.null(highlight)) 
      highlight <- resolve_highlight(highlight, highlighters())
    args <- c(args, pandoc_highlight_args(highlight, default = "pygments"))
    args
  }
  output_format(knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md, dev), pandoc = pandoc_options(to = "slidy", from = from_rmarkdown(fig_caption, md_extensions), args = args, lua_filters = if (number_sections) 
    pkg_file_lua("number-sections.lua")), keep_md = keep_md, clean_supporting = self_contained, df_print = df_print, pre_processor = pre_processor, base_format = html_document_base(lib_dir = lib_dir, self_contained = self_contained, math_method = math_method, mathjax = mathjax, bootstrap_compatible = TRUE, pandoc_args = pandoc_args, extra_dependencies = extra_dependencies, ...))
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
tufte_handout
list(`package:rmarkdown` = function (fig_width = 4, fig_height = 2.5, fig_crop = TRUE, dev = "pdf", highlight = "default", keep_tex = FALSE, citation_package = c("default", "natbib", "biblatex"), includes = NULL, md_extensions = NULL, pandoc_args = NULL) 
{
  warning("The function rmarkdown::tufte_handout() has been deprecated. Please use", "tufte::tufte_handout instead.")
  if (!requireNamespace("tufte", quietly = TRUE)) 
    stop("The 'tufte' package is required to render the tufte_handout format.")
  tufte::tufte_handout(fig_width = fig_width, fig_height = fig_height, fig_crop = fig_crop, dev = dev, highlight = highlight, keep_tex = keep_tex, citation_package = citation_package, includes = includes, md_extensions = md_extensions, pandoc_args = pandoc_args)
}, function (fig_width = 4, fig_height = 2.5, fig_crop = TRUE, dev = "pdf", highlight = "default", keep_tex = FALSE, citation_package = c("default", "natbib", "biblatex"), includes = NULL, md_extensions = NULL, pandoc_args = NULL) 
{
  warning("The function rmarkdown::tufte_handout() has been deprecated. Please use", "tufte::tufte_handout instead.")
  if (!requireNamespace("tufte", quietly = TRUE)) 
    stop("The 'tufte' package is required to render the tufte_handout format.")
  tufte::tufte_handout(fig_width = fig_width, fig_height = fig_height, fig_crop = fig_crop, dev = dev, highlight = highlight, keep_tex = keep_tex, citation_package = citation_package, includes = includes, md_extensions = md_extensions, pandoc_args = pandoc_args)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
word_document
list(`package:rmarkdown` = function (toc = FALSE, toc_depth = 3, number_sections = FALSE, fig_width = 5, fig_height = 4, fig_caption = TRUE, df_print = "default", highlight = "default", reference_docx = "default", keep_md = FALSE, md_extensions = NULL, pandoc_args = NULL) 
{
  knitr <- knitr_options(opts_chunk = list(dev = "png", dpi = 96, fig.width = fig_width, fig.height = fig_height))
  args <- c()
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  lua_filters <- pkg_file_lua("pagebreak.lua")
  if (number_sections) {
    if (pandoc_available("2.10.1")) {
      args <- c(args, "--number-sections")
    }
    else {
      lua_filters <- c(lua_filters, pkg_file_lua("number-sections.lua"))
    }
  }
  if (!is.null(highlight)) 
    highlight <- resolve_highlight(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))
  args <- c(args, reference_doc_args("docx", reference_docx))
  args <- c(args, pandoc_args)
  saved_files_dir <- NULL
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    saved_files_dir <<- files_dir
    NULL
  }
  intermediates_generator <- function(...) {
    reference_intermediates_generator(saved_files_dir, ..., reference_docx)
  }
  output_format(knitr = knitr, pandoc = pandoc_options(to = "docx", from = from_rmarkdown(fig_caption, md_extensions), args = args, lua_filters = lua_filters), keep_md = keep_md, df_print = df_print, pre_processor = pre_processor, intermediates_generator = intermediates_generator)
}, function (toc = FALSE, toc_depth = 3, number_sections = FALSE, fig_width = 5, fig_height = 4, fig_caption = TRUE, df_print = "default", highlight = "default", reference_docx = "default", keep_md = FALSE, md_extensions = NULL, pandoc_args = NULL) 
{
  knitr <- knitr_options(opts_chunk = list(dev = "png", dpi = 96, fig.width = fig_width, fig.height = fig_height))
  args <- c()
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  lua_filters <- pkg_file_lua("pagebreak.lua")
  if (number_sections) {
    if (pandoc_available("2.10.1")) {
      args <- c(args, "--number-sections")
    }
    else {
      lua_filters <- c(lua_filters, pkg_file_lua("number-sections.lua"))
    }
  }
  if (!is.null(highlight)) 
    highlight <- resolve_highlight(highlight, highlighters())
  args <- c(args, pandoc_highlight_args(highlight))
  args <- c(args, reference_doc_args("docx", reference_docx))
  args <- c(args, pandoc_args)
  saved_files_dir <- NULL
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    saved_files_dir <<- files_dir
    NULL
  }
  intermediates_generator <- function(...) {
    reference_intermediates_generator(saved_files_dir, ..., reference_docx)
  }
  output_format(knitr = knitr, pandoc = pandoc_options(to = "docx", from = from_rmarkdown(fig_caption, md_extensions), args = args, lua_filters = lua_filters), keep_md = keep_md, df_print = df_print, pre_processor = pre_processor, intermediates_generator = intermediates_generator)
})
c("package:rmarkdown", "namespace:rmarkdown")
c(TRUE, FALSE)
c(FALSE, TRUE)
yaml_front_matter
list(`package:rmarkdown` = function (input, encoding = "UTF-8") 
{
  parse_yaml_front_matter(read_utf8(input))
}, function (input, encoding = "UTF-8") 
{
  parse_yaml_front_matter(read_utf8(input))
}, function (lines) 
{
  has_front_matter = function(delimiters) {
    length(delimiters) >= 2 && (delimiters[2] - delimiters[1] > 1) && (delimiters[1] == 1 || is_blank(head(lines, delimiters[1] - 1))) && grepl("^---\\s*$", lines[delimiters[1]])
  }
  delimiters = grep("^(---|\\.\\.\\.)\\s*$", lines)
  if (!has_front_matter(delimiters)) 
    return()
  front_matter_lines = lines[(delimiters[1]):(delimiters[2])]
  if (length(front_matter_lines) <= 2) 
    return()
  front_matter = front_matter_lines
  front_matter = front_matter[2:(length(front_matter) - 1)]
  if (length(grep("^params:", front_matter)) == 0) 
    return()
  front_matter = paste(front_matter, collapse = "\n")
  if (!grepl(":\\s*$", front_matter)) 
    front_matter
})
c("package:rmarkdown", "namespace:rmarkdown", "namespace:knitr")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)


####################################################################################################
library(readr)
AccumulateCallback
list(`package:readr` = <environment>, <environment>)
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
as.col_spec
list(`package:readr` = function (x) 
  UseMethod("as.col_spec"), function (x) 
    UseMethod("as.col_spec"), function (x) 
      UseMethod("as.col_spec"))
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, TRUE)
ChunkCallback
list(`package:readr` = <environment>, <environment>)
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
clipboard
list(`package:readr` = function () 
{
  if (edition_first()) {
    return(clipr::read_clip())
  }
  I(paste0(clipr::read_clip(), collapse = "\n"))
}, function () 
{
  if (edition_first()) {
    return(clipr::read_clip())
  }
  I(paste0(clipr::read_clip(), collapse = "\n"))
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
col_character
list(`package:readr` = function () 
{
  collector("character")
}, function () 
{
  collector("character")
}, function (...) 
{
  collector("character", ...)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
col_date
list(`package:readr` = function (format = "") 
{
  collector("date", format = format)
}, function (format = "") 
{
  collector("date", format = format)
}, function (format = "", ...) 
{
  collector("date", format = format, ...)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
col_datetime
list(`package:readr` = function (format = "") 
{
  collector("datetime", format = format)
}, function (format = "") 
{
  collector("datetime", format = format)
}, function (format = "", ...) 
{
  collector("datetime", format = format, ...)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
col_double
list(`package:readr` = function () 
{
  collector("double")
}, function () 
{
  collector("double")
}, function (...) 
{
  collector("double", ...)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
col_factor
list(`package:readr` = function (levels = NULL, ordered = FALSE, include_na = FALSE) 
{
  if (!(is.null(levels) || is.character(levels))) {
    stop(sprintf("`levels` must be `NULL` or a character vector:\n- `levels` is a '%s'", class(levels)), call. = FALSE)
  }
  collector("factor", levels = levels, ordered = ordered, include_na = include_na)
}, function (palette, domain, levels = NULL, ordered = FALSE, na.color = "#808080", alpha = FALSE, reverse = FALSE) 
{
  if (missing(domain) && !is.null(levels)) {
    domain <- NULL
  }
  if (!is.null(levels) && anyDuplicated(levels)) {
    warning("Duplicate levels detected", call. = FALSE)
    levels <- unique(levels)
  }
  lvls <- getLevels(domain, NULL, levels, ordered)
  force(palette)
  withColorAttr("factor", list(na.color = na.color), function(x) {
    if (length(x) == 0 || all(is.na(x))) {
      return(rep.int(na.color, length(x)))
    }
    lvls <- getLevels(domain, x, lvls, ordered)
    pf <- safePaletteFunc(palette, na.color, alpha, nlevels = length(lvls) * ifelse(reverse, -1, 1))
    origNa <- is.na(x)
    x <- match(as.character(x), lvls)
    if (any(is.na(x) != origNa)) {
      warning("Some values were outside the color scale and will be treated as NA", call. = FALSE)
    }
    scaled <- rescale(as.integer(x), from = c(1, length(lvls)))
    if (any(scaled < 0 | scaled > 1, na.rm = TRUE)) {
      warning("Some values were outside the color scale and will be treated as NA", call. = FALSE)
    }
    if (reverse) {
      scaled <- 1 - scaled
    }
    pf(scaled)
  })
}, function (levels = NULL, ordered = FALSE, include_na = FALSE) 
{
  if (!(is.null(levels) || is.character(levels))) {
    stop(sprintf("`levels` must be `NULL` or a character vector:\n- `levels` is a '%s'", class(levels)), call. = FALSE)
  }
  collector("factor", levels = levels, ordered = ordered, include_na = include_na)
}, function (levels = NULL, ordered = FALSE, include_na = FALSE, ...) 
{
  collector("factor", levels = levels, ordered = ordered, include_na = include_na, ...)
})
c("package:readr", "namespace:scales", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, FALSE)
col_guess
list(`package:readr` = function () 
{
  collector("guess")
}, function () 
{
  collector("guess")
}, function (...) 
{
  collector("guess", ...)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
col_integer
list(`package:readr` = function () 
{
  collector("integer")
}, function () 
{
  collector("integer")
}, function (...) 
{
  collector("integer", ...)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
col_logical
list(`package:readr` = function () 
{
  collector("logical")
}, function () 
{
  collector("logical")
}, function (...) 
{
  collector("logical", ...)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
col_number
list(`package:readr` = function () 
{
  collector("number")
}, function () 
{
  collector("number")
}, function (...) 
{
  collector("number", ...)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
col_skip
list(`package:readr` = function () 
{
  collector("skip")
}, function () 
{
  collector("skip")
}, function (...) 
{
  collector("skip", ...)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
col_time
list(`package:readr` = function (format = "") 
{
  collector("time", format = format)
}, function (format = "") 
{
  collector("time", format = format)
}, function (format = "", ...) 
{
  collector("time", format = format, ...)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
cols
list(`package:readr` = function (..., .default = col_guess()) 
{
  if (edition_first()) {
    col_types <- list(...)
    is_character <- vapply(col_types, is.character, logical(1))
    col_types[is_character] <- lapply(col_types[is_character], col_concise)
    if (is.character(.default)) {
      .default <- col_concise(.default)
    }
    return(col_spec(col_types, .default))
  }
  vroom::cols(..., .default = .default)
}, function (..., .default = col_guess()) 
{
  if (edition_first()) {
    col_types <- list(...)
    is_character <- vapply(col_types, is.character, logical(1))
    col_types[is_character] <- lapply(col_types[is_character], col_concise)
    if (is.character(.default)) {
      .default <- col_concise(.default)
    }
    return(col_spec(col_types, .default))
  }
  vroom::cols(..., .default = .default)
}, function (..., .default = col_guess(), .delim = NULL) 
{
  col_types <- rlang::list2(...)
  is_character <- vapply(col_types, is.character, logical(1))
  col_types[is_character] <- lapply(col_types[is_character], col_concise)
  if (is.character(.default)) {
    .default <- col_concise(.default)
  }
  col_spec(col_types, .default, .delim)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
cols_condense
list(`package:readr` = function (x) 
{
  types <- vapply(x$cols, function(xx) class(xx)[[1]], character(1))
  counts <- table(types)
  most_common <- names(counts)[counts == max(counts)][[1]]
  x$default <- x$cols[types == most_common][[1]]
  x$cols <- x$cols[types != most_common]
  x
}, function (x) 
{
  types <- vapply(x$cols, function(xx) class(xx)[[1]], character(1))
  counts <- table(types)
  most_common <- names(counts)[counts == max(counts)][[1]]
  x$default <- x$cols[types == most_common][[1]]
  x$cols <- x$cols[types != most_common]
  x
}, function (x) 
{
  types <- vapply(x$cols, function(xx) class(xx)[[1]], character(1))
  counts <- table(types)
  most_common <- names(counts)[counts == max(counts)][[1]]
  x$default <- x$cols[types == most_common][[1]]
  x$cols <- x$cols[types != most_common]
  x
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, TRUE)
cols_only
list(`package:readr` = function (...) 
{
  cols(..., .default = col_skip())
}, function (...) 
{
  cols(..., .default = col_skip())
}, function (...) 
{
  cols(..., .default = col_skip())
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, TRUE)
count_fields
list(`package:readr` = function (file, tokenizer, skip = 0, n_max = -1) 
{
  ds <- datasource(file, skip = skip, skip_empty_rows = FALSE)
  count_fields_(ds, tokenizer, n_max)
}, function (file, tokenizer, skip = 0, n_max = -1) 
{
  ds <- datasource(file, skip = skip, skip_empty_rows = FALSE)
  count_fields_(ds, tokenizer, n_max)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
DataFrameCallback
list(`package:readr` = <environment>, <environment>)
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
datasource
list(`package:readr` = function (file, skip = 0, skip_empty_rows = FALSE, comment = "", skip_quote = TRUE) 
{
  if (inherits(file, "source")) {
    if (!missing(skip)) {
      file$skip <- skip
    }
    if (!missing(comment)) {
      file$comment <- comment
    }
    file
  }
  else if (is.connection(file)) {
    datasource_connection(file, skip, skip_empty_rows, comment, skip_quote)
  }
  else if (is.raw(file)) {
    datasource_raw(file, skip, skip_empty_rows, comment, skip_quote)
  }
  else if (is.character(file)) {
    if (length(file) > 1) {
      datasource_string(paste(file, collapse = "\n"), skip, skip_empty_rows, comment, skip_quote)
    }
    else if (grepl("\n", file)) {
      datasource_string(file, skip, skip_empty_rows, comment, skip_quote)
    }
    else {
      file <- standardise_path(file)
      if (is.connection(file)) {
        datasource_connection(file, skip, skip_empty_rows, comment, skip_quote)
      }
      else {
        datasource_file(file, skip, skip_empty_rows, comment, skip_quote)
      }
    }
  }
  else {
    stop("`file` must be a string, raw vector or a connection.", call. = FALSE)
  }
}, function (file, skip = 0, skip_empty_rows = FALSE, comment = "", skip_quote = TRUE) 
{
  if (inherits(file, "source")) {
    if (!missing(skip)) {
      file$skip <- skip
    }
    if (!missing(comment)) {
      file$comment <- comment
    }
    file
  }
  else if (is.connection(file)) {
    datasource_connection(file, skip, skip_empty_rows, comment, skip_quote)
  }
  else if (is.raw(file)) {
    datasource_raw(file, skip, skip_empty_rows, comment, skip_quote)
  }
  else if (is.character(file)) {
    if (length(file) > 1) {
      datasource_string(paste(file, collapse = "\n"), skip, skip_empty_rows, comment, skip_quote)
    }
    else if (grepl("\n", file)) {
      datasource_string(file, skip, skip_empty_rows, comment, skip_quote)
    }
    else {
      file <- standardise_path(file)
      if (is.connection(file)) {
        datasource_connection(file, skip, skip_empty_rows, comment, skip_quote)
      }
      else {
        datasource_file(file, skip, skip_empty_rows, comment, skip_quote)
      }
    }
  }
  else {
    stop("`file` must be a string, raw vector or a connection.", call. = FALSE)
  }
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
date_names
list(`package:readr` = function (mon, mon_ab = mon, day, day_ab = day, am_pm = c("AM", "PM")) 
{
  stopifnot(is.character(mon), length(mon) == 12)
  stopifnot(is.character(mon_ab), length(mon_ab) == 12)
  stopifnot(is.character(day), length(day) == 7)
  stopifnot(is.character(day_ab), length(day_ab) == 7)
  structure(list(mon = enc2utf8(mon), mon_ab = enc2utf8(mon_ab), day = enc2utf8(day), day_ab = enc2utf8(day_ab), am_pm = enc2utf8(am_pm)), class = "date_names")
}, function (mon, mon_ab = mon, day, day_ab = day, am_pm = c("AM", "PM")) 
{
  stopifnot(is.character(mon), length(mon) == 12)
  stopifnot(is.character(mon_ab), length(mon_ab) == 12)
  stopifnot(is.character(day), length(day) == 7)
  stopifnot(is.character(day_ab), length(day_ab) == 7)
  structure(list(mon = enc2utf8(mon), mon_ab = enc2utf8(mon_ab), day = enc2utf8(day), day_ab = enc2utf8(day_ab), am_pm = enc2utf8(am_pm)), class = "date_names")
}, function (mon, mon_ab = mon, day, day_ab = day, am_pm = c("AM", "PM")) 
{
  stopifnot(is.character(mon), length(mon) == 12)
  stopifnot(is.character(mon_ab), length(mon_ab) == 12)
  stopifnot(is.character(day), length(day) == 7)
  stopifnot(is.character(day_ab), length(day_ab) == 7)
  structure(list(mon = enc2utf8(mon), mon_ab = enc2utf8(mon_ab), day = enc2utf8(day), day_ab = enc2utf8(day_ab), am_pm = enc2utf8(am_pm)), class = "date_names")
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, TRUE)
date_names_lang
list(`package:readr` = function (language) 
{
  check_string(language)
  symbols <- date_symbols[[language]]
  if (is.null(symbols)) {
    stop("Unknown language '", language, "'", call. = FALSE)
  }
  symbols
}, function (language) 
{
  check_string(language)
  symbols <- date_symbols[[language]]
  if (is.null(symbols)) {
    stop("Unknown language '", language, "'", call. = FALSE)
  }
  symbols
}, function (language) 
{
  stopifnot(is.character(language), length(language) == 1)
  symbols <- date_symbols[[language]]
  if (is.null(symbols)) {
    stop("Unknown language '", language, "'", call. = FALSE)
  }
  symbols
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
date_names_langs
list(`package:readr` = function () 
{
  names(date_symbols)
}, function () 
{
  names(date_symbols)
}, function () 
{
  names(date_symbols)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, TRUE)
default_locale
list(`package:readr` = function () 
{
  loc <- getOption("readr.default_locale")
  if (is.null(loc)) {
    loc <- locale()
    options(readr.default_locale = loc)
  }
  loc
}, function () 
{
  loc <- getOption("readr.default_locale")
  if (is.null(loc)) {
    loc <- locale()
    options(readr.default_locale = loc)
  }
  loc
}, function () 
{
  loc <- getOption("vroom.default_locale")
  if (is.null(loc)) {
    loc <- locale()
    options(vroom.default_locale = loc)
  }
  loc
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
edition_get
list(`package:readr` = function () 
{
  getOption("readr.edition", 2)
}, function () 
{
  getOption("readr.edition", 2)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
format_csv
list(`package:readr` = function (x, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", quote_escape = deprecated()) 
{
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  format_delim(x, delim = ",", na = na, append = append, col_names = col_names, eol = eol, quote = quote, escape = escape)
}, function (x, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", quote_escape = deprecated()) 
{
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  format_delim(x, delim = ",", na = na, append = append, col_names = col_names, eol = eol, quote = quote, escape = escape)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
format_csv2
list(`package:readr` = function (x, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", quote_escape = deprecated()) 
{
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  x <- change_decimal_separator(x, decimal_mark = ",")
  format_delim(x, delim = ";", na = na, append = append, col_names = col_names, eol = eol, quote = quote, escape = escape)
}, function (x, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", quote_escape = deprecated()) 
{
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  x <- change_decimal_separator(x, decimal_mark = ",")
  format_delim(x, delim = ";", na = na, append = append, col_names = col_names, eol = eol, quote = quote, escape = escape)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
format_delim
list(`package:readr` = function (x, delim, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", quote_escape = deprecated()) 
{
  stopifnot(is.data.frame(x))
  check_column_types(x)
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  x[] <- lapply(x, output_column)
  if (edition_first()) {
    res <- stream_delim(df = x, file = NULL, delim = delim, col_names = col_names, append = append, na = na, quote_escape = escape, eol = eol)
    Encoding(res) <- "UTF-8"
    return(res)
  }
  res <- vroom::vroom_format(x, delim = delim, eol = eol, col_names = col_names, na = na, quote = quote, escape = escape)
  Encoding(res) <- "UTF-8"
  res
}, function (x, delim, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", quote_escape = deprecated()) 
{
  stopifnot(is.data.frame(x))
  check_column_types(x)
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  x[] <- lapply(x, output_column)
  if (edition_first()) {
    res <- stream_delim(df = x, file = NULL, delim = delim, col_names = col_names, append = append, na = na, quote_escape = escape, eol = eol)
    Encoding(res) <- "UTF-8"
    return(res)
  }
  res <- vroom::vroom_format(x, delim = delim, eol = eol, col_names = col_names, na = na, quote = quote, escape = escape)
  Encoding(res) <- "UTF-8"
  res
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
format_tsv
list(`package:readr` = function (x, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", quote_escape = deprecated()) 
{
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  format_delim(x, delim = "\t", na = na, append = append, col_names = col_names, eol = eol, quote = quote, escape = escape)
}, function (x, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", quote_escape = deprecated()) 
{
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  format_delim(x, delim = "\t", na = na, append = append, col_names = col_names, eol = eol, quote = quote, escape = escape)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
fwf_cols
list(`package:readr` = function (...) 
{
  if (edition_first()) {
    x <- lapply(list(...), as.integer)
    names(x) <- fwf_col_names(names(x), length(x))
    x <- tibble::as_tibble(x)
    if (nrow(x) == 2) {
      res <- fwf_positions(as.integer(x[1, ]), as.integer(x[2, ]), names(x))
    }
    else if (nrow(x) == 1) {
      res <- fwf_widths(as.integer(x[1, ]), names(x))
    }
    else {
      stop("All variables must have either one (width) two (start, end) values.", call. = FALSE)
    }
    return(res)
  }
  vroom::fwf_cols(...)
}, function (...) 
{
  if (edition_first()) {
    x <- lapply(list(...), as.integer)
    names(x) <- fwf_col_names(names(x), length(x))
    x <- tibble::as_tibble(x)
    if (nrow(x) == 2) {
      res <- fwf_positions(as.integer(x[1, ]), as.integer(x[2, ]), names(x))
    }
    else if (nrow(x) == 1) {
      res <- fwf_widths(as.integer(x[1, ]), names(x))
    }
    else {
      stop("All variables must have either one (width) two (start, end) values.", call. = FALSE)
    }
    return(res)
  }
  vroom::fwf_cols(...)
}, function (...) 
{
  x <- lapply(list(...), as.integer)
  names(x) <- fwf_col_names(names(x), length(x))
  x <- tibble::as_tibble(x)
  if (nrow(x) == 2) {
    fwf_positions(as.integer(x[1, ]), as.integer(x[2, ]), names(x))
  }
  else if (nrow(x) == 1) {
    fwf_widths(as.integer(x[1, ]), names(x))
  }
  else {
    stop("All variables must have either one (width) two (start, end) values.", call. = FALSE)
  }
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
fwf_empty
list(`package:readr` = function (file, skip = 0, skip_empty_rows = FALSE, col_names = NULL, comment = "", n = 100) 
{
  if (edition_first()) {
    ds <- datasource(file, skip = skip, skip_empty_rows = skip_empty_rows)
    out <- whitespaceColumns(ds, comment = comment, n = n)
    out$end[length(out$end)] <- NA
    col_names <- fwf_col_names(col_names, length(out$begin))
    out$col_names <- col_names
    return(out)
  }
  if (!missing(skip_empty_rows)) {
    lifecycle::deprecate_soft("2.0.0", "readr::fwf_empty(skip_empty_rows = )")
  }
  vroom::fwf_empty(file = file, skip = skip, col_names = col_names, comment = comment, n = n)
}, function (file, skip = 0, skip_empty_rows = FALSE, col_names = NULL, comment = "", n = 100) 
{
  if (edition_first()) {
    ds <- datasource(file, skip = skip, skip_empty_rows = skip_empty_rows)
    out <- whitespaceColumns(ds, comment = comment, n = n)
    out$end[length(out$end)] <- NA
    col_names <- fwf_col_names(col_names, length(out$begin))
    out$col_names <- col_names
    return(out)
  }
  if (!missing(skip_empty_rows)) {
    lifecycle::deprecate_soft("2.0.0", "readr::fwf_empty(skip_empty_rows = )")
  }
  vroom::fwf_empty(file = file, skip = skip, col_names = col_names, comment = comment, n = n)
}, function (file, skip = 0, col_names = NULL, comment = "", n = 100) 
{
  file <- standardise_one_path(standardise_path(file)[[1]])
  if (inherits(file, "connection")) {
    stop("`file` must be a regular file, not a connection", call. = FALSE)
  }
  if (n < 0 || is.infinite(n)) {
    n <- -1
  }
  out <- whitespace_columns_(file[[1]], skip, comment = comment, n = n)
  out$end[length(out$end)] <- NA
  col_names <- fwf_col_names(col_names, length(out$begin))
  out$col_names <- col_names
  out
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
fwf_positions
list(`package:readr` = function (start, end = NULL, col_names = NULL) 
{
  if (edition_first()) {
    stopifnot(length(start) == length(end))
    col_names <- fwf_col_names(col_names, length(start))
    return(tibble(begin = start - 1, end = end, col_names = as.character(col_names)))
  }
  vroom::fwf_positions(start = start, end = end, col_names = col_names)
}, function (start, end = NULL, col_names = NULL) 
{
  if (edition_first()) {
    stopifnot(length(start) == length(end))
    col_names <- fwf_col_names(col_names, length(start))
    return(tibble(begin = start - 1, end = end, col_names = as.character(col_names)))
  }
  vroom::fwf_positions(start = start, end = end, col_names = col_names)
}, function (start, end = NULL, col_names = NULL) 
{
  stopifnot(length(start) == length(end))
  col_names <- fwf_col_names(col_names, length(start))
  tibble::tibble(begin = start - 1, end = end, col_names = as.character(col_names))
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
fwf_widths
list(`package:readr` = function (widths, col_names = NULL) 
{
  if (edition_first()) {
    pos <- cumsum(c(1, abs(widths)))
    return(fwf_positions(pos[-length(pos)], pos[-1] - 1, col_names))
  }
  vroom::fwf_widths(widths = widths, col_names = col_names)
}, function (widths, col_names = NULL) 
{
  if (edition_first()) {
    pos <- cumsum(c(1, abs(widths)))
    return(fwf_positions(pos[-length(pos)], pos[-1] - 1, col_names))
  }
  vroom::fwf_widths(widths = widths, col_names = col_names)
}, function (widths, col_names = NULL) 
{
  pos <- cumsum(c(1, abs(widths)))
  fwf_positions(pos[-length(pos)], pos[-1] - 1, col_names)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
guess_encoding
list(`package:readr` = function (file, n_max = 10000, threshold = 0.2) 
{
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("stringi package required for encoding operations", call. = FALSE)
  }
  if (is.character(file)) {
    lines <- unlist(read_lines_raw(file, n_max = n_max))
  }
  else if (is.raw(file)) {
    lines <- file
  }
  else if (is.list(file)) {
    lines <- unlist(file)
  }
  else {
    stop("Unknown input to `file`", call. = FALSE)
  }
  if (stringi::stri_enc_isascii(lines)) {
    return(tibble::tibble(encoding = "ASCII", confidence = 1))
  }
  guess <- stringi::stri_enc_detect(lines)
  df <- tibble::as_tibble(guess[[1]])
  names(df) <- tolower(names(df))
  df[df$confidence > threshold, c("encoding", "confidence")]
}, function (encoding = NULL, type = NULL) 
{
  if (is.null(encoding)) {
    encoding <- tryCatch(error = function(err) NULL, parse_media(type)$params$charset)
  }
  if (is.null(encoding)) {
    message("No encoding supplied: defaulting to UTF-8.")
    return("UTF-8")
  }
  check_encoding(encoding)
}, function (file, n_max = 10000, threshold = 0.2) 
{
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("stringi package required for encoding operations", call. = FALSE)
  }
  if (is.character(file)) {
    lines <- unlist(read_lines_raw(file, n_max = n_max))
  }
  else if (is.raw(file)) {
    lines <- file
  }
  else if (is.list(file)) {
    lines <- unlist(file)
  }
  else {
    stop("Unknown input to `file`", call. = FALSE)
  }
  if (stringi::stri_enc_isascii(lines)) {
    return(tibble::tibble(encoding = "ASCII", confidence = 1))
  }
  guess <- stringi::stri_enc_detect(lines)
  df <- tibble::as_tibble(guess[[1]])
  names(df) <- tolower(names(df))
  df[df$confidence > threshold, c("encoding", "confidence")]
})
c("package:readr", "namespace:httr", "namespace:readr")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
guess_parser
list(`package:readr` = function (x, locale = default_locale(), guess_integer = FALSE, na = c("", "NA")) 
{
  x[x %in% na] <- NA
  stopifnot(is.locale(locale))
  collectorGuess(x, locale, guessInteger = guess_integer)
}, function (x, locale = default_locale(), guess_integer = FALSE, na = c("", "NA")) 
{
  x[x %in% na] <- NA
  stopifnot(is.locale(locale))
  collectorGuess(x, locale, guessInteger = guess_integer)
}, function (x, na = c("", "NA"), locale = default_locale(), guess_integer = FALSE) 
{
  guess_type_(x, na = na, locale = locale, guess_integer = guess_integer)
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
ListCallback
list(`package:readr` = <environment>, <environment>)
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
local_edition
list(`package:readr` = function (edition, env = parent.frame()) 
{
  rlang::check_installed("withr")
  old <- edition_set(edition)
  withr::defer(edition_set(old), envir = env)
}, function (edition, env = parent.frame()) 
{
  rlang::check_installed("withr")
  old <- edition_set(edition)
  withr::defer(edition_set(old), envir = env)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
locale
list(`package:readr` = function (date_names = "en", date_format = "%AD", time_format = "%AT", decimal_mark = ".", grouping_mark = ",", tz = "UTC", encoding = "UTF-8", asciify = FALSE) 
{
  if (is.character(date_names)) {
    date_names <- date_names_lang(date_names)
  }
  stopifnot(is.date_names(date_names))
  if (asciify) {
    date_names[] <- lapply(date_names, stringi::stri_trans_general, id = "latin-ascii")
  }
  if (missing(grouping_mark) && !missing(decimal_mark)) {
    grouping_mark <- if (decimal_mark == ".") 
      ","
    else "."
  }
  else if (missing(decimal_mark) && !missing(grouping_mark)) {
    decimal_mark <- if (grouping_mark == ".") 
      ","
    else "."
  }
  stopifnot(decimal_mark %in% c(".", ","))
  check_string(grouping_mark)
  if (decimal_mark == grouping_mark) {
    stop("`decimal_mark` and `grouping_mark` must be different", call. = FALSE)
  }
  tz <- check_tz(tz)
  check_encoding(encoding)
  structure(list(date_names = date_names, date_format = date_format, time_format = time_format, decimal_mark = decimal_mark, grouping_mark = grouping_mark, tz = tz, encoding = encoding), class = "locale")
}, function (date_names = "en", date_format = "%AD", time_format = "%AT", decimal_mark = ".", grouping_mark = ",", tz = "UTC", encoding = "UTF-8", asciify = FALSE) 
{
  if (is.character(date_names)) {
    date_names <- date_names_lang(date_names)
  }
  stopifnot(is.date_names(date_names))
  if (asciify) {
    date_names[] <- lapply(date_names, stringi::stri_trans_general, id = "latin-ascii")
  }
  if (missing(grouping_mark) && !missing(decimal_mark)) {
    grouping_mark <- if (decimal_mark == ".") 
      ","
    else "."
  }
  else if (missing(decimal_mark) && !missing(grouping_mark)) {
    decimal_mark <- if (grouping_mark == ".") 
      ","
    else "."
  }
  stopifnot(decimal_mark %in% c(".", ","))
  check_string(grouping_mark)
  if (decimal_mark == grouping_mark) {
    stop("`decimal_mark` and `grouping_mark` must be different", call. = FALSE)
  }
  tz <- check_tz(tz)
  check_encoding(encoding)
  structure(list(date_names = date_names, date_format = date_format, time_format = time_format, decimal_mark = decimal_mark, grouping_mark = grouping_mark, tz = tz, encoding = encoding), class = "locale")
}, function (date_names = "en", date_format = "%AD", time_format = "%AT", decimal_mark = ".", grouping_mark = ",", tz = "UTC", encoding = "UTF-8") 
{
  if (is.character(date_names)) {
    date_names <- date_names_lang(date_names)
  }
  stopifnot(is.date_names(date_names))
  if (missing(grouping_mark) && !missing(decimal_mark)) {
    grouping_mark <- if (decimal_mark == ".") 
      ","
    else "."
  }
  else if (missing(decimal_mark) && !missing(grouping_mark)) {
    decimal_mark <- if (grouping_mark == ".") 
      ","
    else "."
  }
  stopifnot(is.character(decimal_mark), length(decimal_mark) == 1)
  stopifnot(is.character(grouping_mark), length(grouping_mark) == 1)
  if (decimal_mark == grouping_mark) {
    stop("`decimal_mark` and `grouping_mark` must be different", call. = FALSE)
  }
  tz <- check_tz(tz)
  check_encoding(encoding)
  structure(list(date_names = date_names, date_format = date_format, time_format = time_format, decimal_mark = decimal_mark, grouping_mark = grouping_mark, tz = tz, encoding = encoding), class = "locale")
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
melt_csv
list(`package:readr` = function (file, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_csv()", details = "Please use `meltr::melt_csv()` instead")
  }
  tokenizer <- tokenizer_csv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited(file, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, progress = progress)
}, function (file, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_csv()", details = "Please use `meltr::melt_csv()` instead")
  }
  tokenizer <- tokenizer_csv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited(file, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, progress = progress)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
melt_csv_chunked
list(`package:readr` = function (file, callback, chunk_size = 10000, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_csv()", details = "Please use `meltr::melt_csv()` instead")
  }
  tokenizer <- tokenizer_csv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, progress = progress)
}, function (file, callback, chunk_size = 10000, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_csv()", details = "Please use `meltr::melt_csv()` instead")
  }
  tokenizer <- tokenizer_csv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, progress = progress)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
melt_csv2
list(`package:readr` = function (file, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_csv2()", details = "Please use `meltr::melt_csv2()` instead")
  }
  if (locale$decimal_mark == ".") {
    cli::cli_alert_info("Using {.val ','} as decimal and {.val '.'} as grouping mark. Use {.fn read_delim} for more control.")
    locale$decimal_mark <- ","
    locale$grouping_mark <- "."
  }
  tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited(file, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, progress = progress)
}, function (file, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_csv2()", details = "Please use `meltr::melt_csv2()` instead")
  }
  if (locale$decimal_mark == ".") {
    cli::cli_alert_info("Using {.val ','} as decimal and {.val '.'} as grouping mark. Use {.fn read_delim} for more control.")
    locale$decimal_mark <- ","
    locale$grouping_mark <- "."
  }
  tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited(file, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, progress = progress)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
melt_csv2_chunked
list(`package:readr` = function (file, callback, chunk_size = 10000, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_csv2()", details = "Please use `meltr::melt_csv2()` instead")
  }
  if (locale$decimal_mark == ".") {
    cli::cli_alert_info("Using {.val ','} as decimal and {.val '.'} as grouping mark. Use {.fn read_delim} for more control.")
    locale$decimal_mark <- ","
    locale$grouping_mark <- "."
  }
  tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, progress = progress)
}, function (file, callback, chunk_size = 10000, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_csv2()", details = "Please use `meltr::melt_csv2()` instead")
  }
  if (locale$decimal_mark == ".") {
    cli::cli_alert_info("Using {.val ','} as decimal and {.val '.'} as grouping mark. Use {.fn read_delim} for more control.")
    locale$decimal_mark <- ","
    locale$grouping_mark <- "."
  }
  tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, progress = progress)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
melt_delim
list(`package:readr` = function (file, delim, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, n_max = Inf, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_delim()", details = "Please use `meltr::melt_delim()` instead")
  }
  if (!nzchar(delim)) {
    stop("`delim` must be at least one character, ", "use `melt_table()` for whitespace delimited input.", call. = FALSE)
  }
  tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited(file, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, progress = progress)
}, function (file, delim, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, n_max = Inf, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_delim()", details = "Please use `meltr::melt_delim()` instead")
  }
  if (!nzchar(delim)) {
    stop("`delim` must be at least one character, ", "use `melt_table()` for whitespace delimited input.", call. = FALSE)
  }
  tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited(file, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, progress = progress)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
melt_delim_chunked
list(`package:readr` = function (file, callback, chunk_size = 10000, delim, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_delim()", details = "Please use `meltr::melt_delim()` instead")
  }
  if (!nzchar(delim)) {
    stop("`delim` must be at least one character, ", "use `melt_table()` for whitespace delimited input.", call. = FALSE)
  }
  tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, progress = progress)
}, function (file, callback, chunk_size = 10000, delim, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_delim()", details = "Please use `meltr::melt_delim()` instead")
  }
  if (!nzchar(delim)) {
    stop("`delim` must be at least one character, ", "use `melt_table()` for whitespace delimited input.", call. = FALSE)
  }
  tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, progress = progress)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
melt_fwf
list(`package:readr` = function (file, col_positions, locale = default_locale(), na = c("", "NA"), comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_fwf()", details = "Please use `meltr::melt_fwf()` instead")
  }
  ds <- datasource(file, skip = skip, skip_empty_rows = skip_empty_rows)
  if (inherits(ds, "source_file") && empty_file(file)) {
    return(tibble::tibble(row = double(), col = double(), data_type = character(), value = character()))
  }
  tokenizer <- tokenizer_fwf(as.integer(col_positions$begin), as.integer(col_positions$end), na = na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  out <- melt_tokens(ds, tokenizer, locale_ = locale, n_max = if (n_max == Inf) 
    -1
    else n_max, progress = progress)
  warn_problems(out)
}, function (file, col_positions, locale = default_locale(), na = c("", "NA"), comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_fwf()", details = "Please use `meltr::melt_fwf()` instead")
  }
  ds <- datasource(file, skip = skip, skip_empty_rows = skip_empty_rows)
  if (inherits(ds, "source_file") && empty_file(file)) {
    return(tibble::tibble(row = double(), col = double(), data_type = character(), value = character()))
  }
  tokenizer <- tokenizer_fwf(as.integer(col_positions$begin), as.integer(col_positions$end), na = na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  out <- melt_tokens(ds, tokenizer, locale_ = locale, n_max = if (n_max == Inf) 
    -1
    else n_max, progress = progress)
  warn_problems(out)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
melt_table
list(`package:readr` = function (file, locale = default_locale(), na = "NA", skip = 0, n_max = Inf, guess_max = min(n_max, 1000), progress = show_progress(), comment = "", skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_table()", details = "Please use `meltr::melt_table()` instead")
  }
  ds <- datasource(file, skip = skip, skip_empty_rows = skip_empty_rows)
  if (inherits(ds, "source_file") && empty_file(file)) {
    return(tibble::tibble(row = double(), col = double(), data_type = character(), value = character()))
  }
  local_edition(1)
  columns <- fwf_empty(ds, skip = skip, skip_empty_rows = skip_empty_rows, n = guess_max, comment = comment)
  tokenizer <- tokenizer_fwf(columns$begin, columns$end, na = na, comment = comment, skip_empty_rows = skip_empty_rows)
  ds <- datasource(file = ds, skip = skip, skip_empty_rows = skip_empty_rows)
  out <- melt_tokens(ds, tokenizer, locale_ = locale, n_max = n_max, progress = progress)
  warn_problems(out)
}, function (file, locale = default_locale(), na = "NA", skip = 0, n_max = Inf, guess_max = min(n_max, 1000), progress = show_progress(), comment = "", skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_table()", details = "Please use `meltr::melt_table()` instead")
  }
  ds <- datasource(file, skip = skip, skip_empty_rows = skip_empty_rows)
  if (inherits(ds, "source_file") && empty_file(file)) {
    return(tibble::tibble(row = double(), col = double(), data_type = character(), value = character()))
  }
  local_edition(1)
  columns <- fwf_empty(ds, skip = skip, skip_empty_rows = skip_empty_rows, n = guess_max, comment = comment)
  tokenizer <- tokenizer_fwf(columns$begin, columns$end, na = na, comment = comment, skip_empty_rows = skip_empty_rows)
  ds <- datasource(file = ds, skip = skip, skip_empty_rows = skip_empty_rows)
  out <- melt_tokens(ds, tokenizer, locale_ = locale, n_max = n_max, progress = progress)
  warn_problems(out)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
melt_table2
list(`package:readr` = function (file, locale = default_locale(), na = "NA", skip = 0, n_max = Inf, progress = show_progress(), comment = "", skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_table2()", details = "Please use `meltr::melt_table2()` instead")
  }
  ds <- datasource(file, skip = skip, skip_empty_rows = skip_empty_rows)
  if (inherits(ds, "source_file") && empty_file(file)) {
    return(tibble::tibble(row = double(), col = double(), data_type = character(), value = character()))
  }
  tokenizer <- tokenizer_ws(na = na, comment = comment, skip_empty_rows = skip_empty_rows)
  ds <- datasource(file = ds, skip = skip, skip_empty_rows = skip_empty_rows)
  melt_delimited(ds, tokenizer, locale = locale, skip = skip, comment = comment, n_max = n_max, progress = progress)
}, function (file, locale = default_locale(), na = "NA", skip = 0, n_max = Inf, progress = show_progress(), comment = "", skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_table2()", details = "Please use `meltr::melt_table2()` instead")
  }
  ds <- datasource(file, skip = skip, skip_empty_rows = skip_empty_rows)
  if (inherits(ds, "source_file") && empty_file(file)) {
    return(tibble::tibble(row = double(), col = double(), data_type = character(), value = character()))
  }
  tokenizer <- tokenizer_ws(na = na, comment = comment, skip_empty_rows = skip_empty_rows)
  ds <- datasource(file = ds, skip = skip, skip_empty_rows = skip_empty_rows)
  melt_delimited(ds, tokenizer, locale = locale, skip = skip, comment = comment, n_max = n_max, progress = progress)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
melt_tsv
list(`package:readr` = function (file, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_tsv()", details = "Please use `meltr::melt_tsv()` instead")
  }
  tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited(file, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, progress = progress)
}, function (file, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_tsv()", details = "Please use `meltr::melt_tsv()` instead")
  }
  tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited(file, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, progress = progress)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
melt_tsv_chunked
list(`package:readr` = function (file, callback, chunk_size = 10000, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_tsv()", details = "Please use `meltr::melt_tsv()` instead")
  }
  tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, progress = progress)
}, function (file, callback, chunk_size = 10000, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, progress = show_progress(), skip_empty_rows = FALSE) 
{
  if (!edition_first()) {
    lifecycle::deprecate_soft("2.0.0", what = "melt_tsv()", details = "Please use `meltr::melt_tsv()` instead")
  }
  tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  melt_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, progress = progress)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
output_column
list(`package:readr` = function (x, name) 
{
  UseMethod("output_column")
}, function (x, name) 
{
  UseMethod("output_column")
}, function (x) 
{
  UseMethod("output_column")
})
c("package:readr", "namespace:readr", "namespace:vroom")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
parse_character
list(`package:readr` = function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_character(), na = na, locale = locale, trim_ws = trim_ws)
}, function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_character(), na = na, locale = locale, trim_ws = trim_ws)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
parse_date
list(`package:readr` = function (x, format = "", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_date(format), na = na, locale = locale, trim_ws = trim_ws)
}, function (datestring) 
{
  out <- .Call(R_curl_getdate, datestring)
  class(out) <- c("POSIXct", "POSIXt")
  out
}, function (x, format = "", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_date(format), na = na, locale = locale, trim_ws = trim_ws)
}, function (x) 
{
  if (is.numeric(x)) {
    return(structure(x/1000, class = c("POSIXct", "POSIXt")))
  }
  else if (is.character(x)) {
    is_utc <- ifelse(all(grepl("Z$", x)), "UTC", "")
    return(as.POSIXct(strptime(x, format = "%Y-%m-%dT%H:%M:%OS", tz = is_utc)))
  }
  else {
    return(x)
  }
})
c("package:readr", "namespace:curl", "namespace:readr", "namespace:jsonlite")
c(TRUE, FALSE, FALSE, FALSE)
c(FALSE, FALSE, TRUE, FALSE)
parse_datetime
list(`package:readr` = function (x, format = "", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_datetime(format), na = na, locale = locale, trim_ws = trim_ws)
}, function (x, format = "", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_datetime(format), na = na, locale = locale, trim_ws = trim_ws)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
parse_double
list(`package:readr` = function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_double(), na = na, locale = locale, trim_ws = trim_ws)
}, function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_double(), na = na, locale = locale, trim_ws = trim_ws)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
parse_factor
list(`package:readr` = function (x, levels = NULL, ordered = FALSE, na = c("", "NA"), locale = default_locale(), include_na = TRUE, trim_ws = TRUE) 
{
  parse_vector(x, col_factor(levels, ordered, include_na), na = na, locale = locale, trim_ws = trim_ws)
}, function (x, levels = NULL, ordered = FALSE, na = c("", "NA"), locale = default_locale(), include_na = TRUE, trim_ws = TRUE) 
{
  parse_vector(x, col_factor(levels, ordered, include_na), na = na, locale = locale, trim_ws = trim_ws)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
parse_guess
list(`package:readr` = function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, guess_integer = FALSE) 
{
  parse_vector(x, guess_parser(x, locale, guess_integer = guess_integer, na = na), na = na, locale = locale, trim_ws = trim_ws)
}, function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, guess_integer = FALSE) 
{
  parse_vector(x, guess_parser(x, locale, guess_integer = guess_integer, na = na), na = na, locale = locale, trim_ws = trim_ws)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
parse_integer
list(`package:readr` = function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_integer(), na = na, locale = locale, trim_ws = trim_ws)
}, function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_integer(), na = na, locale = locale, trim_ws = trim_ws)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
parse_logical
list(`package:readr` = function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_logical(), na = na, locale = locale, trim_ws = trim_ws)
}, function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_logical(), na = na, locale = locale, trim_ws = trim_ws)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
parse_number
list(`package:readr` = function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_number(), na = na, locale = locale, trim_ws = trim_ws)
}, function (x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_number(), na = na, locale = locale, trim_ws = trim_ws)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
parse_time
list(`package:readr` = function (x, format = "", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_time(format), na = na, locale = locale, trim_ws = trim_ws)
}, function (x, format = "", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  parse_vector(x, col_time(format), na = na, locale = locale, trim_ws = trim_ws)
}, function (x, format) 
{
  difftime(strptime(as.character(x), format = format), strptime("0:0:0", format = "%X"), units = "secs", tz = "UTC")
})
c("package:readr", "namespace:readr", "namespace:hms")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
parse_vector
list(`package:readr` = function (x, collector, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  stopifnot(is.character(x))
  if (is.character(collector)) {
    collector <- collector_find(collector)
  }
  warn_problems(parse_vector_(x, collector, na = na, locale_ = locale, trim_ws = trim_ws))
}, function (x, collector, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) 
{
  stopifnot(is.character(x))
  if (is.character(collector)) {
    collector <- collector_find(collector)
  }
  warn_problems(parse_vector_(x, collector, na = na, locale_ = locale, trim_ws = trim_ws))
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
problems
list(`package:readr` = function (x = .Last.value) 
{
  problems <- probs(x)
  if (is.null(problems)) {
    return(invisible(no_problems))
  }
  if (inherits(problems, "tbl_df")) {
    return(problems)
  }
  vroom::problems(x)
}, function (x = .Last.value) 
{
  problems <- probs(x)
  if (is.null(problems)) {
    return(invisible(no_problems))
  }
  if (inherits(problems, "tbl_df")) {
    return(problems)
  }
  vroom::problems(x)
}, function (x, lazy = FALSE) 
{
  if (!isTRUE(lazy)) {
    vroom_materialize(x, replace = FALSE)
  }
  probs <- attr(x, "problems")
  if (typeof(probs) != "externalptr") {
    rlang::abort("`x` must have a problems attribute that is an external pointer.\n  Is this object from readr and not vroom?")
  }
  probs <- vroom_errors_(probs)
  probs <- probs[!duplicated(probs), ]
  probs <- probs[order(probs$file, probs$row, probs$col), ]
  tibble::as_tibble(probs)
}, function (header, ..., .problem = " problem(s)") 
{
  problems <- vec_c(..., .name_spec = "{outer}")
  problems <- set_default_name(problems, "x")
  MAX_BULLETS <- 6
  if (length(problems) >= MAX_BULLETS) {
    n_more <- length(problems) - MAX_BULLETS + 1
    problems[[MAX_BULLETS]] <- pluralise_n(paste0(pre_dots("and "), n_more, " more", .problem), n_more)
    length(problems) <- MAX_BULLETS
  }
  bullets(header, problems)
})
c("package:readr", "namespace:readr", "namespace:vroom", "namespace:tibble")
c(TRUE, FALSE, FALSE, FALSE)
c(FALSE, TRUE, FALSE, FALSE)
read_builtin
list(`package:readr` = function (x, package = NULL) 
{
  warn_to_error <- function(e) {
    stop(conditionMessage(e), call. = FALSE)
  }
  check_string(x)
  check_string(package, optional = TRUE)
  tryCatch(warning = function(e) warn_to_error(e), expr = {
    res <- utils::data(list = x, package = package, envir = environment(), verbose = FALSE)
    get(res[[1]], envir = environment())
  })
}, function (x, package = NULL) 
{
  warn_to_error <- function(e) {
    stop(conditionMessage(e), call. = FALSE)
  }
  check_string(x)
  check_string(package, optional = TRUE)
  tryCatch(warning = function(e) warn_to_error(e), expr = {
    res <- utils::data(list = x, package = package, envir = environment(), verbose = FALSE)
    get(res[[1]], envir = environment())
  })
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_csv
list(`package:readr` = function (file, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), name_repair = "unique", num_threads = readr_threads(), progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
{
  if (edition_first()) {
    tokenizer <- tokenizer_csv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
    return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
  }
  if (!missing(quoted_na)) {
    lifecycle::deprecate_soft("2.0.0", "readr::read_csv(quoted_na = )")
  }
  vroom::vroom(file, delim = ",", col_names = col_names, col_types = col_types, col_select = {
    {
      col_select
    }
  }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, locale = locale, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
}, function (file, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), name_repair = "unique", num_threads = readr_threads(), progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
{
  if (edition_first()) {
    tokenizer <- tokenizer_csv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
    return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
  }
  if (!missing(quoted_na)) {
    lifecycle::deprecate_soft("2.0.0", "readr::read_csv(quoted_na = )")
  }
  vroom::vroom(file, delim = ",", col_names = col_names, col_types = col_types, col_select = {
    {
      col_select
    }
  }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, locale = locale, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_csv_chunked
list(`package:readr` = function (file, callback, chunk_size = 10000, col_names = TRUE, col_types = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, guess_max = chunk_size, progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE) 
{
  tokenizer <- tokenizer_csv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  read_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer = tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, guess_max = guess_max, progress = progress, show_col_types = show_col_types)
}, function (file, callback, chunk_size = 10000, col_names = TRUE, col_types = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, guess_max = chunk_size, progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE) 
{
  tokenizer <- tokenizer_csv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  read_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer = tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, guess_max = guess_max, progress = progress, show_col_types = show_col_types)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_csv2
list(`package:readr` = function (file, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = show_progress(), name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
{
  if (locale$decimal_mark == ".") {
    cli::cli_alert_info("Using {.val ','} as decimal and {.val '.'} as grouping mark. Use {.fn read_delim} for more control.")
    locale$decimal_mark <- ","
    locale$grouping_mark <- "."
  }
  if (edition_first()) {
    tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
    return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
  }
  vroom::vroom(file, delim = ";", col_names = col_names, col_types = col_types, col_select = {
    {
      col_select
    }
  }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, locale = locale, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
}, function (file, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = show_progress(), name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
{
  if (locale$decimal_mark == ".") {
    cli::cli_alert_info("Using {.val ','} as decimal and {.val '.'} as grouping mark. Use {.fn read_delim} for more control.")
    locale$decimal_mark <- ","
    locale$grouping_mark <- "."
  }
  if (edition_first()) {
    tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
    return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
  }
  vroom::vroom(file, delim = ";", col_names = col_names, col_types = col_types, col_select = {
    {
      col_select
    }
  }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, locale = locale, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_csv2_chunked
list(`package:readr` = function (file, callback, chunk_size = 10000, col_names = TRUE, col_types = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, guess_max = chunk_size, progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE) 
{
  tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  read_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer = tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, guess_max = guess_max, progress = progress, show_col_types = show_col_types)
}, function (file, callback, chunk_size = 10000, col_names = TRUE, col_types = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, guess_max = chunk_size, progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE) 
{
  tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  read_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer = tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, guess_max = guess_max, progress = progress, show_col_types = show_col_types)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_delim
list(`package:readr` = function (file, delim = NULL, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), name_repair = "unique", num_threads = readr_threads(), progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
{
  if (!is.null(delim) && !nzchar(delim)) {
    stop("`delim` must be at least one character, ", "use `read_table()` for whitespace delimited input.", call. = FALSE)
  }
  if (edition_first()) {
    tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
    return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
  }
  if (!missing(quoted_na)) {
    lifecycle::deprecate_soft("2.0.0", "readr::read_delim(quoted_na = )")
  }
  vroom::vroom(file, delim = delim, col_names = col_names, col_types = col_types, col_select = {
    {
      col_select
    }
  }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = escape_double, escape_backslash = escape_backslash, locale = locale, guess_max = guess_max, progress = progress, altrep = lazy, show_col_types = show_col_types, num_threads = num_threads)
}, function (file, delim = NULL, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), name_repair = "unique", num_threads = readr_threads(), progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
{
  if (!is.null(delim) && !nzchar(delim)) {
    stop("`delim` must be at least one character, ", "use `read_table()` for whitespace delimited input.", call. = FALSE)
  }
  if (edition_first()) {
    tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
    return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
  }
  if (!missing(quoted_na)) {
    lifecycle::deprecate_soft("2.0.0", "readr::read_delim(quoted_na = )")
  }
  vroom::vroom(file, delim = delim, col_names = col_names, col_types = col_types, col_select = {
    {
      col_select
    }
  }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = escape_double, escape_backslash = escape_backslash, locale = locale, guess_max = guess_max, progress = progress, altrep = lazy, show_col_types = show_col_types, num_threads = num_threads)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_delim_chunked
list(`package:readr` = function (file, callback, delim = NULL, chunk_size = 10000, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, col_names = TRUE, col_types = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, guess_max = chunk_size, progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE) 
{
  tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  read_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer = tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, guess_max = guess_max, progress = progress, show_col_types = show_col_types)
}, function (file, callback, delim = NULL, chunk_size = 10000, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, col_names = TRUE, col_types = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, guess_max = chunk_size, progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE) 
{
  tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  read_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer = tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, guess_max = guess_max, progress = progress, show_col_types = show_col_types)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_file
list(`package:readr` = function (file, locale = default_locale()) 
{
  if (empty_file(file)) {
    return("")
  }
  ds <- datasource(file, skip_empty_rows = FALSE)
  read_file_(ds, locale)
}, function (file, locale = default_locale()) 
{
  if (empty_file(file)) {
    return("")
  }
  ds <- datasource(file, skip_empty_rows = FALSE)
  read_file_(ds, locale)
}, function (path, binary = FALSE) 
{
  n <- file.info(path)$size
  if (binary) {
    readBin(path, raw(), n)
  }
  else {
    readChar(path, n, TRUE)
  }
}, function (x) 
{
  readChar(x, file.info(x)$size)
})
c("package:readr", "namespace:readr", "namespace:rmarkdown", "namespace:webdriver")
c(TRUE, FALSE, FALSE, FALSE)
c(FALSE, TRUE, FALSE, FALSE)
read_file_raw
list(`package:readr` = function (file) 
{
  if (empty_file(file)) {
    return(raw())
  }
  ds <- datasource(file, skip_empty_rows = FALSE)
  read_file_raw_(ds)
}, function (file) 
{
  if (empty_file(file)) {
    return(raw())
  }
  ds <- datasource(file, skip_empty_rows = FALSE)
  read_file_raw_(ds)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_fwf
list(`package:readr` = function (file, col_positions = fwf_empty(file, skip, n = guess_max), col_types = NULL, col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(n_max, 1000), progress = show_progress(), name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), lazy = should_read_lazy(), skip_empty_rows = TRUE) 
{
  if (edition_first()) {
    ds <- datasource(file, skip = skip, skip_empty_rows = skip_empty_rows)
    if (inherits(ds, "source_file") && empty_file(file)) {
      return(tibble::tibble())
    }
    tokenizer <- tokenizer_fwf(col_positions$begin, col_positions$end, na = na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
    spec <- col_spec_standardise(file, skip = skip, guess_max = guess_max, tokenizer = tokenizer, locale = locale, col_names = col_positions$col_names, col_types = col_types, drop_skipped_names = TRUE)
    if (is.null(col_types) && !inherits(ds, "source_string") && !is_testing()) {
      show_cols_spec(spec)
    }
    out <- read_tokens(datasource(file, skip = spec$skip, skip_empty_rows = skip_empty_rows), tokenizer, spec$cols, names(spec$cols), locale_ = locale, n_max = if (n_max == Inf) 
      -1
      else n_max, progress = progress)
    out <- name_problems(out, names(spec$cols), source_name(file))
    attr(out, "spec") <- spec
    return(warn_problems(out))
  }
  vroom::vroom_fwf(file, col_positions = col_positions, col_types = col_types, col_select = {
    {
      col_select
    }
  }, id = id, .name_repair = name_repair, locale = locale, na = na, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
}, function (file, col_positions = fwf_empty(file, skip, n = guess_max), col_types = NULL, col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(n_max, 1000), progress = show_progress(), name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), lazy = should_read_lazy(), skip_empty_rows = TRUE) 
{
  if (edition_first()) {
    ds <- datasource(file, skip = skip, skip_empty_rows = skip_empty_rows)
    if (inherits(ds, "source_file") && empty_file(file)) {
      return(tibble::tibble())
    }
    tokenizer <- tokenizer_fwf(col_positions$begin, col_positions$end, na = na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
    spec <- col_spec_standardise(file, skip = skip, guess_max = guess_max, tokenizer = tokenizer, locale = locale, col_names = col_positions$col_names, col_types = col_types, drop_skipped_names = TRUE)
    if (is.null(col_types) && !inherits(ds, "source_string") && !is_testing()) {
      show_cols_spec(spec)
    }
    out <- read_tokens(datasource(file, skip = spec$skip, skip_empty_rows = skip_empty_rows), tokenizer, spec$cols, names(spec$cols), locale_ = locale, n_max = if (n_max == Inf) 
      -1
      else n_max, progress = progress)
    out <- name_problems(out, names(spec$cols), source_name(file))
    attr(out, "spec") <- spec
    return(warn_problems(out))
  }
  vroom::vroom_fwf(file, col_positions = col_positions, col_types = col_types, col_select = {
    {
      col_select
    }
  }, id = id, .name_repair = name_repair, locale = locale, na = na, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_lines
list(`package:readr` = function (file, skip = 0, skip_empty_rows = FALSE, n_max = Inf, locale = default_locale(), na = character(), lazy = should_read_lazy(), num_threads = readr_threads(), progress = show_progress()) 
{
  if (edition_first()) {
    if (is.infinite(n_max)) {
      n_max <- -1
    }
    if (empty_file(file)) {
      return(character())
    }
    ds <- datasource(file, skip = skip, skip_empty_rows = skip_empty_rows, skip_quote = FALSE)
    return(read_lines_(ds, skip_empty_rows = skip_empty_rows, locale_ = locale, na = na, n_max = n_max, progress = progress))
  }
  vroom::vroom_lines(file, skip = skip, locale = locale, n_max = n_max, progress = progress, altrep = lazy, skip_empty_rows = skip_empty_rows, na = na, num_threads = num_threads)
}, function (file, skip = 0, skip_empty_rows = FALSE, n_max = Inf, locale = default_locale(), na = character(), lazy = should_read_lazy(), num_threads = readr_threads(), progress = show_progress()) 
{
  if (edition_first()) {
    if (is.infinite(n_max)) {
      n_max <- -1
    }
    if (empty_file(file)) {
      return(character())
    }
    ds <- datasource(file, skip = skip, skip_empty_rows = skip_empty_rows, skip_quote = FALSE)
    return(read_lines_(ds, skip_empty_rows = skip_empty_rows, locale_ = locale, na = na, n_max = n_max, progress = progress))
  }
  vroom::vroom_lines(file, skip = skip, locale = locale, n_max = n_max, progress = progress, altrep = lazy, skip_empty_rows = skip_empty_rows, na = na, num_threads = num_threads)
}, function (path) 
{
  suppressWarnings(con <- file(path, open = "r"))
  on.exit(close(con), add = TRUE)
  suppressWarnings(readLines(con))
})
c("package:readr", "namespace:readr", "namespace:ps")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
read_lines_chunked
list(`package:readr` = function (file, callback, chunk_size = 10000, skip = 0, locale = default_locale(), na = character(), progress = show_progress()) 
{
  if (empty_file(file)) {
    return(character())
  }
  ds <- datasource(file, skip = skip, skip_empty_rows = FALSE)
  callback <- as_chunk_callback(callback)
  on.exit(callback$finally(), add = TRUE)
  read_lines_chunked_(ds, locale, na, chunk_size, callback, FALSE, progress)
  return(callback$result())
}, function (file, callback, chunk_size = 10000, skip = 0, locale = default_locale(), na = character(), progress = show_progress()) 
{
  if (empty_file(file)) {
    return(character())
  }
  ds <- datasource(file, skip = skip, skip_empty_rows = FALSE)
  callback <- as_chunk_callback(callback)
  on.exit(callback$finally(), add = TRUE)
  read_lines_chunked_(ds, locale, na, chunk_size, callback, FALSE, progress)
  return(callback$result())
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_lines_raw
list(`package:readr` = function (file, skip = 0, n_max = -1, num_threads = readr_threads(), progress = show_progress()) 
{
  if (empty_file(file)) {
    return(list())
  }
  ds <- datasource(file, skip = skip, skip_empty_rows = FALSE, skip_quote = FALSE)
  read_lines_raw_(ds, n_max = n_max, progress = progress)
}, function (file, skip = 0, n_max = -1, num_threads = readr_threads(), progress = show_progress()) 
{
  if (empty_file(file)) {
    return(list())
  }
  ds <- datasource(file, skip = skip, skip_empty_rows = FALSE, skip_quote = FALSE)
  read_lines_raw_(ds, n_max = n_max, progress = progress)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_lines_raw_chunked
list(`package:readr` = function (file, callback, chunk_size = 10000, skip = 0, progress = show_progress()) 
{
  if (empty_file(file)) {
    return(character())
  }
  ds <- datasource(file, skip = skip, skip_empty_rows = FALSE)
  callback <- as_chunk_callback(callback)
  on.exit(callback$finally(), add = TRUE)
  read_lines_raw_chunked_(ds, chunk_size, callback, progress)
  return(callback$result())
}, function (file, callback, chunk_size = 10000, skip = 0, progress = show_progress()) 
{
  if (empty_file(file)) {
    return(character())
  }
  ds <- datasource(file, skip = skip, skip_empty_rows = FALSE)
  callback <- as_chunk_callback(callback)
  on.exit(callback$finally(), add = TRUE)
  read_lines_raw_chunked_(ds, chunk_size, callback, progress)
  return(callback$result())
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_log
list(`package:readr` = function (file, col_names = FALSE, col_types = NULL, trim_ws = TRUE, skip = 0, n_max = Inf, show_col_types = should_show_types(), progress = show_progress()) 
{
  tokenizer <- tokenizer_log(trim_ws = trim_ws)
  read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, skip = skip, n_max = n_max, progress = progress, show_col_types = show_col_types)
}, function (file, col_names = FALSE, col_types = NULL, trim_ws = TRUE, skip = 0, n_max = Inf, show_col_types = should_show_types(), progress = show_progress()) 
{
  tokenizer <- tokenizer_log(trim_ws = trim_ws)
  read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, skip = skip, n_max = n_max, progress = progress, show_col_types = show_col_types)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_rds
list(`package:readr` = function (file, refhook = NULL) 
{
  con <- file(file)
  on.exit(close(con))
  readRDS(con, refhook = refhook)
}, function (file, refhook = NULL) 
{
  con <- file(file)
  on.exit(close(con))
  readRDS(con, refhook = refhook)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_table
list(`package:readr` = function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), na = "NA", skip = 0, n_max = Inf, guess_max = min(n_max, 1000), progress = show_progress(), comment = "", show_col_types = should_show_types(), skip_empty_rows = TRUE) 
{
  tokenizer <- tokenizer_ws(na = na, comment = comment, skip_empty_rows = skip_empty_rows)
  read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, skip_quote = FALSE, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types)
}, function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), na = "NA", skip = 0, n_max = Inf, guess_max = min(n_max, 1000), progress = show_progress(), comment = "", show_col_types = should_show_types(), skip_empty_rows = TRUE) 
{
  tokenizer <- tokenizer_ws(na = na, comment = comment, skip_empty_rows = skip_empty_rows)
  read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, skip_quote = FALSE, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_table2
list(`package:readr` = function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), na = "NA", skip = 0, n_max = Inf, guess_max = min(n_max, 1000), progress = show_progress(), comment = "", skip_empty_rows = TRUE) 
{
  lifecycle::deprecate_soft("2.0.0", "read_table2()", "read_table()")
  read_table(file = file, col_names = col_names, col_types = col_types, locale = locale, na = na, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress, comment = comment, skip_empty_rows = skip_empty_rows)
}, function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), na = "NA", skip = 0, n_max = Inf, guess_max = min(n_max, 1000), progress = show_progress(), comment = "", skip_empty_rows = TRUE) 
{
  lifecycle::deprecate_soft("2.0.0", "read_table2()", "read_table()")
  read_table(file = file, col_names = col_names, col_types = col_types, locale = locale, na = na, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress, comment = comment, skip_empty_rows = skip_empty_rows)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_tsv
list(`package:readr` = function (file, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = show_progress(), name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
{
  tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  if (edition_first()) {
    return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
  }
  vroom::vroom(file, delim = "\t", col_names = col_names, col_types = col_types, col_select = {
    {
      col_select
    }
  }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, locale = locale, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
}, function (file, col_names = TRUE, col_types = NULL, col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = show_progress(), name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
{
  tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  if (edition_first()) {
    return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
  }
  vroom::vroom(file, delim = "\t", col_names = col_names, col_types = col_types, col_select = {
    {
      col_select
    }
  }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, locale = locale, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_tsv_chunked
list(`package:readr` = function (file, callback, chunk_size = 10000, col_names = TRUE, col_types = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, guess_max = chunk_size, progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE) 
{
  tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  read_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer = tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, guess_max = guess_max, progress = progress, show_col_types = show_col_types)
}, function (file, callback, chunk_size = 10000, col_names = TRUE, col_types = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, guess_max = chunk_size, progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE) 
{
  tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
  read_delimited_chunked(file, callback = callback, chunk_size = chunk_size, tokenizer = tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, guess_max = guess_max, progress = progress, show_col_types = show_col_types)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
readr_example
list(`package:readr` = function (file = NULL) 
{
  if (is.null(file)) {
    dir(system.file("extdata", package = "readr"))
  }
  else {
    system.file("extdata", file, package = "readr", mustWork = TRUE)
  }
}, function (file = NULL) 
{
  if (is.null(file)) {
    dir(system.file("extdata", package = "readr"))
  }
  else {
    system.file("extdata", file, package = "readr", mustWork = TRUE)
  }
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
readr_threads
list(`package:readr` = function () 
{
  res <- getOption("readr.num_threads")
  if (is.null(res)) {
    res <- as.integer(Sys.getenv("VROOM_THREADS", parallel::detectCores()))
    options(readr.num_threads = res)
  }
  if (is.na(res) || res <= 0) {
    res <- 1
  }
  res
}, function () 
{
  res <- getOption("readr.num_threads")
  if (is.null(res)) {
    res <- as.integer(Sys.getenv("VROOM_THREADS", parallel::detectCores()))
    options(readr.num_threads = res)
  }
  if (is.na(res) || res <= 0) {
    res <- 1
  }
  res
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
should_read_lazy
list(`package:readr` = function () 
{
  identical(getOption("readr.read_lazy", FALSE), TRUE)
}, function () 
{
  identical(getOption("readr.read_lazy", FALSE), TRUE)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
should_show_types
list(`package:readr` = function () 
{
  opt <- getOption("readr.show_col_types", NA)
  if (isTRUE(opt)) {
    TRUE
  }
  else if (identical(opt, FALSE)) {
    FALSE
  }
  else if (is.na(opt) && is_testing()) {
    FALSE
  }
  else {
    NULL
  }
}, function () 
{
  opt <- getOption("readr.show_col_types", NA)
  if (isTRUE(opt)) {
    TRUE
  }
  else if (identical(opt, FALSE)) {
    FALSE
  }
  else if (is.na(opt) && is_testing()) {
    FALSE
  }
  else {
    NULL
  }
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
show_progress
list(`package:readr` = function () 
{
  isTRUE(getOption("readr.show_progress")) && rlang::is_interactive() && !isTRUE(getOption("rstudio.notebook.executing"))
}, function () 
{
  isTRUE(getOption("readr.show_progress")) && rlang::is_interactive() && !isTRUE(getOption("rstudio.notebook.executing"))
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
SideEffectChunkCallback
list(`package:readr` = <environment>, <environment>)
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
spec
list(`package:readr` = function (x) 
{
  stopifnot(inherits(x, "tbl_df"))
  attr(x, "spec")
}, function (x) 
{
  stopifnot(inherits(x, "tbl_df"))
  attr(x, "spec")
}, function (x) 
{
  stopifnot(inherits(x, "tbl_df"))
  attr(x, "spec")
}, function (spec, env = caller_env(), signaller = "signal_lifecycle") 
{
  what <- spec_what(spec, "spec", signaller)
  fn <- spec_fn(what$call)
  arg <- spec_arg(what$call, signaller)
  reason <- spec_reason(what$call, signaller)
  if (is_null(what$pkg) && !is.null(env)) {
    pkg <- spec_package(env, signaller = signaller)
  }
  else {
    pkg <- what$pkg
  }
  list(fn = fn, arg = arg, pkg = pkg, reason = reason, from = signaller)
})
c("package:readr", "namespace:readr", "namespace:vroom", "namespace:lifecycle")
c(TRUE, FALSE, FALSE, FALSE)
c(FALSE, TRUE, TRUE, FALSE)
spec_csv
list(`package:readr` = function (file, col_names = TRUE, col_types = list(), col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, name_repair = "unique", num_threads = readr_threads(), progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
  spec(with_edition(1, (function() {
    if (edition_first()) {
      tokenizer <- tokenizer_csv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
      return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
    }
    if (!missing(quoted_na)) {
      lifecycle::deprecate_soft("2.0.0", "readr::read_csv(quoted_na = )")
    }
    vroom::vroom(file, delim = ",", col_names = col_names, col_types = col_types, col_select = {
      {
        col_select
      }
    }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, locale = locale, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
  })())), function (file, col_names = TRUE, col_types = list(), col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, name_repair = "unique", num_threads = readr_threads(), progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
    spec(with_edition(1, (function() {
      if (edition_first()) {
        tokenizer <- tokenizer_csv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
        return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
      }
      if (!missing(quoted_na)) {
        lifecycle::deprecate_soft("2.0.0", "readr::read_csv(quoted_na = )")
      }
      vroom::vroom(file, delim = ",", col_names = col_names, col_types = col_types, col_select = {
        {
          col_select
        }
      }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, locale = locale, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
    })())))
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
spec_csv2
list(`package:readr` = function (file, col_names = TRUE, col_types = list(), col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(), name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
  spec(with_edition(1, (function() {
    if (locale$decimal_mark == ".") {
      cli::cli_alert_info("Using {.val ','} as decimal and {.val '.'} as grouping mark. Use {.fn read_delim} for more control.")
      locale$decimal_mark <- ","
      locale$grouping_mark <- "."
    }
    if (edition_first()) {
      tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
      return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
    }
    vroom::vroom(file, delim = ";", col_names = col_names, col_types = col_types, col_select = {
      {
        col_select
      }
    }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, locale = locale, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
  })())), function (file, col_names = TRUE, col_types = list(), col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(), name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
    spec(with_edition(1, (function() {
      if (locale$decimal_mark == ".") {
        cli::cli_alert_info("Using {.val ','} as decimal and {.val '.'} as grouping mark. Use {.fn read_delim} for more control.")
        locale$decimal_mark <- ","
        locale$grouping_mark <- "."
      }
      if (edition_first()) {
        tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
        return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
      }
      vroom::vroom(file, delim = ";", col_names = col_names, col_types = col_types, col_select = {
        {
          col_select
        }
      }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, locale = locale, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
    })())))
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
spec_delim
list(`package:readr` = function (file, delim = NULL, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, col_names = TRUE, col_types = list(), col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, n_max = 0, guess_max = 1000, name_repair = "unique", num_threads = readr_threads(), progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
  spec(with_edition(1, (function() {
    if (!is.null(delim) && !nzchar(delim)) {
      stop("`delim` must be at least one character, ", "use `read_table()` for whitespace delimited input.", call. = FALSE)
    }
    if (edition_first()) {
      tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
      return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
    }
    if (!missing(quoted_na)) {
      lifecycle::deprecate_soft("2.0.0", "readr::read_delim(quoted_na = )")
    }
    vroom::vroom(file, delim = delim, col_names = col_names, col_types = col_types, col_select = {
      {
        col_select
      }
    }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = escape_double, escape_backslash = escape_backslash, locale = locale, guess_max = guess_max, progress = progress, altrep = lazy, show_col_types = show_col_types, num_threads = num_threads)
  })())), function (file, delim = NULL, quote = "\"", escape_backslash = FALSE, escape_double = TRUE, col_names = TRUE, col_types = list(), col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, n_max = 0, guess_max = 1000, name_repair = "unique", num_threads = readr_threads(), progress = show_progress(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
    spec(with_edition(1, (function() {
      if (!is.null(delim) && !nzchar(delim)) {
        stop("`delim` must be at least one character, ", "use `read_table()` for whitespace delimited input.", call. = FALSE)
      }
      if (edition_first()) {
        tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
        return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
      }
      if (!missing(quoted_na)) {
        lifecycle::deprecate_soft("2.0.0", "readr::read_delim(quoted_na = )")
      }
      vroom::vroom(file, delim = delim, col_names = col_names, col_types = col_types, col_select = {
        {
          col_select
        }
      }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = escape_double, escape_backslash = escape_backslash, locale = locale, guess_max = guess_max, progress = progress, altrep = lazy, show_col_types = show_col_types, num_threads = num_threads)
    })())))
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
spec_table
list(`package:readr` = function (file, col_names = TRUE, col_types = list(), locale = default_locale(), na = "NA", skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(), comment = "", show_col_types = should_show_types(), skip_empty_rows = TRUE) 
  spec(with_edition(1, (function() {
    tokenizer <- tokenizer_ws(na = na, comment = comment, skip_empty_rows = skip_empty_rows)
    read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, skip_quote = FALSE, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types)
  })())), function (file, col_names = TRUE, col_types = list(), locale = default_locale(), na = "NA", skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(), comment = "", show_col_types = should_show_types(), skip_empty_rows = TRUE) 
    spec(with_edition(1, (function() {
      tokenizer <- tokenizer_ws(na = na, comment = comment, skip_empty_rows = skip_empty_rows)
      read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, skip_quote = FALSE, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types)
    })())))
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
spec_tsv
list(`package:readr` = function (file, col_names = TRUE, col_types = list(), col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(), name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
  spec(with_edition(1, (function() {
    tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
    if (edition_first()) {
      return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
    }
    vroom::vroom(file, delim = "\t", col_names = col_names, col_types = col_types, col_select = {
      {
        col_select
      }
    }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, locale = locale, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
  })())), function (file, col_names = TRUE, col_types = list(), col_select = NULL, id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(), name_repair = "unique", num_threads = readr_threads(), show_col_types = should_show_types(), skip_empty_rows = TRUE, lazy = should_read_lazy()) 
    spec(with_edition(1, (function() {
      tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows)
      if (edition_first()) {
        return(read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, locale = locale, skip = skip, skip_empty_rows = skip_empty_rows, comment = comment, n_max = n_max, guess_max = guess_max, progress = progress, show_col_types = show_col_types))
      }
      vroom::vroom(file, delim = "\t", col_names = col_names, col_types = col_types, col_select = {
        {
          col_select
        }
      }, id = id, .name_repair = name_repair, skip = skip, n_max = n_max, na = na, quote = quote, comment = comment, skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, locale = locale, guess_max = guess_max, show_col_types = show_col_types, progress = progress, altrep = lazy, num_threads = num_threads)
    })())))
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
stop_for_problems
list(`package:readr` = function (x) 
{
  n <- n_problems(x)
  if (n == 0) {
    return(invisible(x))
  }
  stop(n, " parsing failure", if (n > 1) 
    "s", call. = FALSE)
}, function (x) 
{
  n <- n_problems(x)
  if (n == 0) {
    return(invisible(x))
  }
  stop(n, " parsing failure", if (n > 1) 
    "s", call. = FALSE)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
tokenize
list(`package:readr` = function (file, tokenizer = tokenizer_csv(), skip = 0, n_max = -1) 
{
  ds <- datasource(file, skip = skip, skip_empty_rows = FALSE)
  tokenize_(ds, tokenizer, n_max)
}, function (file, tokenizer = tokenizer_csv(), skip = 0, n_max = -1) 
{
  ds <- datasource(file, skip = skip, skip_empty_rows = FALSE)
  tokenize_(ds, tokenizer, n_max)
}, function (s) 
{
  pos <- 1
  i <- 1
  len_s <- nchar(s)
  results <- list()
  while (pos <= len_s) {
    ss <- substring(s, pos, len_s)
    match <- match_whitespace(ss)
    if (!anyNA(match) && match[1] == 1) {
      results[[i]] <- Token$new("S", " ", pos)
      match_end <- match[2]
      pos <- pos + match_end
      i <- i + 1
      next
    }
    match <- match_number(ss)
    if (!anyNA(match) && match[1] == 1) {
      match_start <- match[1]
      match_end <- max(match[1], match[2])
      value <- substring(ss, match_start, match_end)
      results[[i]] <- Token$new("NUMBER", value, pos)
      pos <- pos + match_end
      i <- i + 1
      next
    }
    match <- match_ident(ss)
    if (!anyNA(match) && match[1] == 1) {
      match_start <- match[1]
      match_end <- max(match[1], match[2])
      value <- substring(ss, match_start, match_end)
      value <- sub_simple_escape(sub_unicode_escape(value))
      results[[i]] <- Token$new("IDENT", value, pos)
      pos <- pos + match_end
      i <- i + 1
      next
    }
    match <- match_hash(ss)
    if (!anyNA(match) && match[1] == 1) {
      match_start <- match[1]
      match_end <- max(match[1], match[2])
      value <- substring(ss, match_start, match_end)
      value <- sub_simple_escape(sub_unicode_escape(value))
      hash_id <- substring(value, 2)
      results[[i]] <- Token$new("HASH", hash_id, pos)
      pos <- pos + match_end
      i <- i + 1
      next
    }
    nc_inds <- seq_len(nchar(ss))
    if (length(nc_inds)%%2 == 1) 
      nc_inds <- c(nc_inds, length(nc_inds) + 1)
    split_ss_2ch <- substring(ss, nc_inds[(nc_inds%%2) == 1], nc_inds[(nc_inds%%2) == 0])
    delim_inds_2ch <- which(split_ss_2ch %in% delims_2ch)
    if (length(delim_inds_2ch) && delim_inds_2ch[1] == 1) {
      results[[i]] <- Token$new("DELIM", split_ss_2ch[1], pos)
      pos <- pos + 2
      i <- i + 1
      next
    }
    split_ss_1ch <- substring(ss, nc_inds, nc_inds)
    delim_inds_1ch <- which(split_ss_1ch %in% delims_1ch)
    if (length(delim_inds_1ch) && delim_inds_1ch[1] == 1) {
      results[[i]] <- Token$new("DELIM", split_ss_1ch[1], pos)
      pos <- pos + 1
      i <- i + 1
      next
    }
    quote <- substring(s, pos, pos)
    if (quote %in% c("'", "\"")) {
      ncs <- nchar(s)
      split_chars <- substring(s, (pos + 1):ncs, (pos + 1):ncs)
      matching_quotes <- which(split_chars == quote)
      is_escaped <- logical(length(matching_quotes))
      if (length(matching_quotes)) {
        for (j in seq_along(matching_quotes)) {
          end_quote <- matching_quotes[j]
          if (end_quote > 1) {
            is_escaped[j] <- split_chars[end_quote - 1] == "\\"
          }
        }
        if (all(is_escaped)) {
          stop("Unclosed string at ", pos)
        }
        end_quote <- matching_quotes[min(which(!is_escaped))]
        value <- substring(s, pos + 1, pos + end_quote - 1)
        value <- sub_simple_escape(sub_unicode_escape(sub_newline_escape(value)))
        results[[i]] <- Token$new("STRING", value, pos)
        pos <- pos + end_quote + 1
        i <- i + 1
      }
      else {
        stop("Unclosed string at ", pos)
      }
    }
    pos1 <- pos + 1
    if (substring(s, pos, pos1) == "/*") {
      rel_pos <- str_locate(ss, "\\*/")[1]
      pos <- if (is.na(rel_pos)) {
        len_s + 1
      }
      else {
        pos + rel_pos + 1
      }
      next
    }
    tmp <- substring(ss, 1, 1)
    if (!tmp %in% c(delims_1ch, "\"", "'")) {
      stop("Unexpected character '", tmp, "' found at position ", pos)
    }
  }
  results[[i]] <- EOFToken$new(pos)
  results
})
c("package:readr", "namespace:readr", "namespace:selectr")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
tokenizer_csv
list(`package:readr` = function (na = "NA", quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip_empty_rows = TRUE) 
{
  tokenizer_delim(delim = ",", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, skip_empty_rows = skip_empty_rows)
}, function (na = "NA", quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip_empty_rows = TRUE) 
{
  tokenizer_delim(delim = ",", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, skip_empty_rows = skip_empty_rows)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
tokenizer_delim
list(`package:readr` = function (delim, quote = "\"", na = "NA", quoted_na = TRUE, comment = "", trim_ws = TRUE, escape_double = TRUE, escape_backslash = FALSE, skip_empty_rows = TRUE) 
{
  structure(list(delim = delim, quote = quote, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, escape_double = escape_double, escape_backslash = escape_backslash, skip_empty_rows = skip_empty_rows), class = "tokenizer_delim")
}, function (delim, quote = "\"", na = "NA", quoted_na = TRUE, comment = "", trim_ws = TRUE, escape_double = TRUE, escape_backslash = FALSE, skip_empty_rows = TRUE) 
{
  structure(list(delim = delim, quote = quote, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, escape_double = escape_double, escape_backslash = escape_backslash, skip_empty_rows = skip_empty_rows), class = "tokenizer_delim")
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
tokenizer_fwf
list(`package:readr` = function (begin, end, na = "NA", comment = "", trim_ws = TRUE, skip_empty_rows = TRUE) 
{
  structure(list(begin = as.integer(begin), end = as.integer(end), na = na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows), class = "tokenizer_fwf")
}, function (begin, end, na = "NA", comment = "", trim_ws = TRUE, skip_empty_rows = TRUE) 
{
  structure(list(begin = as.integer(begin), end = as.integer(end), na = na, comment = comment, trim_ws = trim_ws, skip_empty_rows = skip_empty_rows), class = "tokenizer_fwf")
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
tokenizer_line
list(`package:readr` = function (na = character(), skip_empty_rows = TRUE) 
{
  structure(list(na = na, skip_empty_rows = skip_empty_rows), class = "tokenizer_line")
}, function (na = character(), skip_empty_rows = TRUE) 
{
  structure(list(na = na, skip_empty_rows = skip_empty_rows), class = "tokenizer_line")
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
tokenizer_log
list(`package:readr` = function (trim_ws) 
{
  structure(list(trim_ws = trim_ws), class = "tokenizer_log")
}, function (trim_ws) 
{
  structure(list(trim_ws = trim_ws), class = "tokenizer_log")
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
tokenizer_tsv
list(`package:readr` = function (na = "NA", quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip_empty_rows = TRUE) 
{
  tokenizer_delim(delim = "\t", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, skip_empty_rows = skip_empty_rows)
}, function (na = "NA", quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, skip_empty_rows = TRUE) 
{
  tokenizer_delim(delim = "\t", na = na, quoted_na = quoted_na, quote = quote, comment = comment, trim_ws = trim_ws, escape_double = TRUE, escape_backslash = FALSE, skip_empty_rows = skip_empty_rows)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
tokenizer_ws
list(`package:readr` = function (na = "NA", comment = "", skip_empty_rows = TRUE) 
{
  structure(list(na = na, comment = comment, skip_empty_rows = skip_empty_rows), class = "tokenizer_ws")
}, function (na = "NA", comment = "", skip_empty_rows = TRUE) 
{
  structure(list(na = na, comment = comment, skip_empty_rows = skip_empty_rows), class = "tokenizer_ws")
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
type_convert
list(`package:readr` = function (df, col_types = NULL, na = c("", "NA"), trim_ws = TRUE, locale = default_locale(), guess_integer = FALSE) 
{
  stopifnot(is.data.frame(df))
  is_character <- vapply(df, is.character, logical(1))
  if (!any(is_character)) {
    warning("`type_convert()` only converts columns of type 'character'.\n- `df` has no columns of type 'character'", call. = FALSE)
  }
  char_cols <- df[is_character]
  col_types <- keep_character_col_types(df, col_types)
  guesses <- lapply(char_cols, guess_parser, locale = locale, na = na, guess_integer = guess_integer)
  specs <- col_spec_standardise(col_types = col_types, col_names = names(char_cols), guessed_types = guesses)
  if (is.null(col_types) && !is_testing()) {
    show_cols_spec(specs)
  }
  df[is_character] <- lapply(seq_along(char_cols), function(i) {
    type_convert_col(char_cols[[i]], specs$cols[[i]], which(is_character)[i], locale_ = locale, na = na, trim_ws = trim_ws)
  })
  attr(df, "spec") <- NULL
  df
}, function (df, col_types = NULL, na = c("", "NA"), trim_ws = TRUE, locale = default_locale(), guess_integer = FALSE) 
{
  stopifnot(is.data.frame(df))
  is_character <- vapply(df, is.character, logical(1))
  if (!any(is_character)) {
    warning("`type_convert()` only converts columns of type 'character'.\n- `df` has no columns of type 'character'", call. = FALSE)
  }
  char_cols <- df[is_character]
  col_types <- keep_character_col_types(df, col_types)
  guesses <- lapply(char_cols, guess_parser, locale = locale, na = na, guess_integer = guess_integer)
  specs <- col_spec_standardise(col_types = col_types, col_names = names(char_cols), guessed_types = guesses)
  if (is.null(col_types) && !is_testing()) {
    show_cols_spec(specs)
  }
  df[is_character] <- lapply(seq_along(char_cols), function(i) {
    type_convert_col(char_cols[[i]], specs$cols[[i]], which(is_character)[i], locale_ = locale, na = na, trim_ws = trim_ws)
  })
  attr(df, "spec") <- NULL
  df
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
with_edition
list(`package:readr` = function (edition, code) 
{
  local_edition(edition)
  code
}, function (edition, code) 
{
  local_edition(edition)
  code
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
write_csv
list(`package:readr` = function (x, file, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), progress = show_progress(), path = deprecated(), quote_escape = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_csv(path = )", "write_csv(file = )")
    file <- path
  }
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  write_delim(x, file, delim = ",", na = na, append = append, col_names = col_names, quote = quote, escape = escape, eol = eol, num_threads = num_threads, progress = progress)
}, function (x, file, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), progress = show_progress(), path = deprecated(), quote_escape = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_csv(path = )", "write_csv(file = )")
    file <- path
  }
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  write_delim(x, file, delim = ",", na = na, append = append, col_names = col_names, quote = quote, escape = escape, eol = eol, num_threads = num_threads, progress = progress)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
write_csv2
list(`package:readr` = function (x, file, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), progress = show_progress(), path = deprecated(), quote_escape = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_csv2(path = )", "write_csv2(file = )")
    file <- path
  }
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  x_out <- x
  x <- change_decimal_separator(x, decimal_mark = ",")
  write_delim(x, file, delim = ";", na = na, append = append, col_names = col_names, quote = quote, escape = escape, eol = eol, num_threads = num_threads, progress = progress)
  invisible(x_out)
}, function (x, file, na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), progress = show_progress(), path = deprecated(), quote_escape = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_csv2(path = )", "write_csv2(file = )")
    file <- path
  }
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  x_out <- x
  x <- change_decimal_separator(x, decimal_mark = ",")
  write_delim(x, file, delim = ";", na = na, append = append, col_names = col_names, quote = quote, escape = escape, eol = eol, num_threads = num_threads, progress = progress)
  invisible(x_out)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
write_delim
list(`package:readr` = function (x, file, delim = " ", na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), progress = show_progress(), path = deprecated(), quote_escape = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_delim(path = )", "write_delim(file = )")
    file <- path
  }
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  stopifnot(is.data.frame(x))
  check_column_types(x)
  x_out <- x
  x[] <- lapply(x, output_column)
  if (edition_first()) {
    stream_delim(x, file, delim = delim, col_names = col_names, append = append, na = na, quote_escape = escape, eol = eol)
    return(invisible(x_out))
  }
  vroom::vroom_write(x, file, delim = delim, col_names = col_names, append = append, na = na, eol = eol, quote = quote, escape = escape, num_threads = num_threads, progress = progress)
  invisible(x_out)
}, function (x, file, delim = " ", na = "NA", append = FALSE, col_names = !append, quote = c("needed", "all", "none"), escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), progress = show_progress(), path = deprecated(), quote_escape = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_delim(path = )", "write_delim(file = )")
    file <- path
  }
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  stopifnot(is.data.frame(x))
  check_column_types(x)
  x_out <- x
  x[] <- lapply(x, output_column)
  if (edition_first()) {
    stream_delim(x, file, delim = delim, col_names = col_names, append = append, na = na, quote_escape = escape, eol = eol)
    return(invisible(x_out))
  }
  vroom::vroom_write(x, file, delim = delim, col_names = col_names, append = append, na = na, eol = eol, quote = quote, escape = escape, num_threads = num_threads, progress = progress)
  invisible(x_out)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
write_excel_csv
list(`package:readr` = function (x, file, na = "NA", append = FALSE, col_names = !append, delim = ",", quote = "all", escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), progress = show_progress(), path = deprecated(), quote_escape = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_excel_csv(path = )", "write_excel_csv(file = )")
    file <- path
  }
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  stopifnot(is.data.frame(x))
  check_column_types(x)
  x_out <- x
  datetime_cols <- vapply(x, inherits, logical(1), "POSIXt")
  x[datetime_cols] <- lapply(x[datetime_cols], format, "%Y/%m/%d %H:%M:%S")
  x[] <- lapply(x, output_column)
  if (edition_first()) {
    stream_delim(x, file, delim, col_names = col_names, append = append, na = na, bom = !append, quote_escape = escape, eol = eol)
    return(invisible(x_out))
  }
  vroom::vroom_write(x, file, delim, col_names = col_names, append = append, na = na, bom = !append, quote = quote, escape = escape, eol = eol, num_threads = num_threads, progress = progress)
  invisible(x_out)
}, function (x, file, na = "NA", append = FALSE, col_names = !append, delim = ",", quote = "all", escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), progress = show_progress(), path = deprecated(), quote_escape = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_excel_csv(path = )", "write_excel_csv(file = )")
    file <- path
  }
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  stopifnot(is.data.frame(x))
  check_column_types(x)
  x_out <- x
  datetime_cols <- vapply(x, inherits, logical(1), "POSIXt")
  x[datetime_cols] <- lapply(x[datetime_cols], format, "%Y/%m/%d %H:%M:%S")
  x[] <- lapply(x, output_column)
  if (edition_first()) {
    stream_delim(x, file, delim, col_names = col_names, append = append, na = na, bom = !append, quote_escape = escape, eol = eol)
    return(invisible(x_out))
  }
  vroom::vroom_write(x, file, delim, col_names = col_names, append = append, na = na, bom = !append, quote = quote, escape = escape, eol = eol, num_threads = num_threads, progress = progress)
  invisible(x_out)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
write_excel_csv2
list(`package:readr` = function (x, file, na = "NA", append = FALSE, col_names = !append, delim = ";", quote = "all", escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), progress = show_progress(), path = deprecated(), quote_escape = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_excel_csv2(path = )", "write_excel_csv2(file = )")
    file <- path
  }
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  stopifnot(is.data.frame(x))
  check_column_types(x)
  x_out <- x
  x <- change_decimal_separator(x, decimal_mark = ",")
  datetime_cols <- vapply(x, inherits, logical(1), "POSIXt")
  x[datetime_cols] <- lapply(x[datetime_cols], format, "%Y/%m/%d %H:%M:%S")
  x[] <- lapply(x, output_column)
  write_excel_csv(x, file, na, append, col_names, delim, quote = quote, escape = escape, eol = eol, num_threads = num_threads, progress = progress)
  invisible(x_out)
}, function (x, file, na = "NA", append = FALSE, col_names = !append, delim = ";", quote = "all", escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), progress = show_progress(), path = deprecated(), quote_escape = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_excel_csv2(path = )", "write_excel_csv2(file = )")
    file <- path
  }
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  stopifnot(is.data.frame(x))
  check_column_types(x)
  x_out <- x
  x <- change_decimal_separator(x, decimal_mark = ",")
  datetime_cols <- vapply(x, inherits, logical(1), "POSIXt")
  x[datetime_cols] <- lapply(x[datetime_cols], format, "%Y/%m/%d %H:%M:%S")
  x[] <- lapply(x, output_column)
  write_excel_csv(x, file, na, append, col_names, delim, quote = quote, escape = escape, eol = eol, num_threads = num_threads, progress = progress)
  invisible(x_out)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
write_file
list(`package:readr` = function (x, file, append = FALSE, path = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_file(path = )", "write_file(file = )")
    file <- path
  }
  force(x)
  file <- standardise_path(file, input = FALSE)
  if (!isOpen(file)) {
    on.exit(close(file), add = TRUE)
    if (isTRUE(append)) {
      open(file, "ab")
    }
    else {
      open(file, "wb")
    }
  }
  if (is.raw(x)) {
    write_file_raw_(x, file)
  }
  else {
    write_file_(x, file)
  }
  invisible(x)
}, function (x, file, append = FALSE, path = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_file(path = )", "write_file(file = )")
    file <- path
  }
  force(x)
  file <- standardise_path(file, input = FALSE)
  if (!isOpen(file)) {
    on.exit(close(file), add = TRUE)
    if (isTRUE(append)) {
      open(file, "ab")
    }
    else {
      open(file, "wb")
    }
  }
  if (is.raw(x)) {
    write_file_raw_(x, file)
  }
  else {
    write_file_(x, file)
  }
  invisible(x)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
write_lines
list(`package:readr` = function (x, file, sep = "\n", na = "NA", append = FALSE, num_threads = readr_threads(), path = deprecated()) 
{
  is_raw <- is.list(x) && inherits(x[[1]], "raw")
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_lines(path = )", "write_lines(file = )")
    file <- path
  }
  if (is_raw || edition_first()) {
    is_raw <- is.list(x) && inherits(x[[1]], "raw")
    if (!is_raw) {
      x <- as.character(x)
    }
    file <- standardise_path(file, input = FALSE)
    if (!isOpen(file)) {
      on.exit(close(file), add = TRUE)
      open(file, if (isTRUE(append)) 
        "ab"
        else "wb")
    }
    if (is_raw) {
      write_lines_raw_(x, file, sep)
    }
    else {
      write_lines_(x, file, na, sep)
    }
    return(invisible(x))
  }
  vroom::vroom_write_lines(as.character(x), file, eol = sep, na = na, append = append, num_threads = num_threads)
  invisible(x)
}, function (x, file, sep = "\n", na = "NA", append = FALSE, num_threads = readr_threads(), path = deprecated()) 
{
  is_raw <- is.list(x) && inherits(x[[1]], "raw")
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_lines(path = )", "write_lines(file = )")
    file <- path
  }
  if (is_raw || edition_first()) {
    is_raw <- is.list(x) && inherits(x[[1]], "raw")
    if (!is_raw) {
      x <- as.character(x)
    }
    file <- standardise_path(file, input = FALSE)
    if (!isOpen(file)) {
      on.exit(close(file), add = TRUE)
      open(file, if (isTRUE(append)) 
        "ab"
        else "wb")
    }
    if (is_raw) {
      write_lines_raw_(x, file, sep)
    }
    else {
      write_lines_(x, file, na, sep)
    }
    return(invisible(x))
  }
  vroom::vroom_write_lines(as.character(x), file, eol = sep, na = na, append = append, num_threads = num_threads)
  invisible(x)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
write_rds
list(`package:readr` = function (x, file, compress = c("none", "gz", "bz2", "xz"), version = 2, refhook = NULL, text = FALSE, path = deprecated(), ...) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_rds(path = )", "write_rds(file = )")
    file <- path
  }
  compress <- match.arg(compress)
  con <- switch(compress, none = file(file, ...), gz = gzfile(file, ...), bz2 = bzfile(file, ...), xz = xzfile(file, ...))
  on.exit(close(con), add = TRUE)
  saveRDS(x, con, version = version, refhook = refhook, ascii = text)
  invisible(x)
}, function (x, file, compress = c("none", "gz", "bz2", "xz"), version = 2, refhook = NULL, text = FALSE, path = deprecated(), ...) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_rds(path = )", "write_rds(file = )")
    file <- path
  }
  compress <- match.arg(compress)
  con <- switch(compress, none = file(file, ...), gz = gzfile(file, ...), bz2 = bzfile(file, ...), xz = xzfile(file, ...))
  on.exit(close(con), add = TRUE)
  saveRDS(x, con, version = version, refhook = refhook, ascii = text)
  invisible(x)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)
write_tsv
list(`package:readr` = function (x, file, na = "NA", append = FALSE, col_names = !append, quote = "none", escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), progress = show_progress(), path = deprecated(), quote_escape = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_tsv(path = )", "write_tsv(file = )")
    file <- path
  }
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  write_delim(x, file, delim = "\t", na = na, append = append, col_names = col_names, quote = quote, escape = escape, eol = eol, num_threads = num_threads, progress = progress)
}, function (x, file, na = "NA", append = FALSE, col_names = !append, quote = "none", escape = c("double", "backslash", "none"), eol = "\n", num_threads = readr_threads(), progress = show_progress(), path = deprecated(), quote_escape = deprecated()) 
{
  if (is_present(path)) {
    deprecate_warn("1.4.0", "write_tsv(path = )", "write_tsv(file = )")
    file <- path
  }
  if (is_present(quote_escape)) {
    deprecate_soft("2.0.0", "write_delim(quote_escape = )", "write_delim(escape = )")
    escape <- quote_escape
  }
  write_delim(x, file, delim = "\t", na = na, append = append, col_names = col_names, quote = quote, escape = escape, eol = eol, num_threads = num_threads, progress = progress)
})
c("package:readr", "namespace:readr")
c(TRUE, FALSE)
c(FALSE, TRUE)


#################################################################################################
library(readxl)
anchored
list(`package:readxl` = function (anchor = "A1", dim = c(1, 1), input = NULL, col_names = NULL, byrow = FALSE) 
{
  anchorCL <- as.cell_limits(anchor)
  stopifnot(dim(anchorCL) == c(1, 1), isTOGGLE(col_names), isTOGGLE(byrow))
  if (is.null(input)) {
    stopifnot(length(dim) == 2)
    input_extent <- as.integer(dim)
    if (is.null(col_names)) {
      col_names <- FALSE
    }
  }
  else {
    if (is.null(dim(input))) {
      col_names <- FALSE
      input_extent <- c(length(input), 1)
      if (byrow) {
        input_extent <- rev(input_extent)
      }
    }
    else {
      stopifnot(length(dim(input)) == 2)
      if (is.null(col_names)) {
        col_names <- !is.null(colnames(input))
      }
      input_extent <- dim(input)
    }
  }
  if (col_names) {
    input_extent[1] <- input_extent[1] + 1
  }
  cell_limits(ul = anchorCL$ul, lr = anchorCL$lr + input_extent - 1)
}, function (anchor = "A1", dim = c(1, 1), input = NULL, col_names = NULL, byrow = FALSE) 
{
  anchorCL <- as.cell_limits(anchor)
  stopifnot(dim(anchorCL) == c(1, 1), isTOGGLE(col_names), isTOGGLE(byrow))
  if (is.null(input)) {
    stopifnot(length(dim) == 2)
    input_extent <- as.integer(dim)
    if (is.null(col_names)) {
      col_names <- FALSE
    }
  }
  else {
    if (is.null(dim(input))) {
      col_names <- FALSE
      input_extent <- c(length(input), 1)
      if (byrow) {
        input_extent <- rev(input_extent)
      }
    }
    else {
      stopifnot(length(dim(input)) == 2)
      if (is.null(col_names)) {
        col_names <- !is.null(colnames(input))
      }
      input_extent <- dim(input)
    }
  }
  if (col_names) {
    input_extent[1] <- input_extent[1] + 1
  }
  cell_limits(ul = anchorCL$ul, lr = anchorCL$lr + input_extent - 1)
})
c("package:readxl", "namespace:cellranger")
c(TRUE, FALSE)
c(FALSE, TRUE)
cell_cols
list(`package:readxl` = function (x) 
{
  if (all(is.na(x))) {
    return(cell_limits())
  }
  stopifnot(is.numeric(x) || is.character(x))
  if (is.character(x)) {
    if (length(x) == 1) {
      x <- strsplit(x, ":")[[1]]
    }
    x <- letter_to_num(x)
  }
  if (length(x) != 2) {
    x <- range(x, na.rm = TRUE)
  }
  cell_limits(as.integer(c(NA, x[1])), as.integer(c(NA, x[2])))
}, function (x) 
{
  if (all(is.na(x))) {
    return(cell_limits())
  }
  stopifnot(is.numeric(x) || is.character(x))
  if (is.character(x)) {
    if (length(x) == 1) {
      x <- strsplit(x, ":")[[1]]
    }
    x <- letter_to_num(x)
  }
  if (length(x) != 2) {
    x <- range(x, na.rm = TRUE)
  }
  cell_limits(as.integer(c(NA, x[1])), as.integer(c(NA, x[2])))
})
c("package:readxl", "namespace:cellranger")
c(TRUE, FALSE)
c(FALSE, TRUE)
cell_limits
list(`package:readxl` = function (ul = c(NA, NA), lr = c(NA, NA), sheet = NA) 
{
  stopifnot(length(ul) == 2, length(lr) == 2, length(sheet) == 1, is.character(sheet))
  ul <- as.integer(ul)
  lr <- as.integer(lr)
  NA_or_pos <- function(x) is.na(x) | x > 0
  stopifnot(all(NA_or_pos(ul)))
  stopifnot(all(NA_or_pos(lr)))
  if (is.na(ul[1]) && !is.na(lr[1])) 
    ul[1] <- 1
  if (is.na(ul[2]) && !is.na(lr[2])) 
    ul[2] <- 1
  rows <- c(ul[1], lr[1])
  cols <- c(ul[2], lr[2])
  if (!anyNA(rows)) 
    stopifnot(rows[1] <= rows[2])
  if (!anyNA(cols)) 
    stopifnot(cols[1] <= cols[2])
  structure(list(ul = ul, lr = lr, sheet = sheet), class = c("cell_limits", "list"))
}, function (ul = c(NA, NA), lr = c(NA, NA), sheet = NA) 
{
  stopifnot(length(ul) == 2, length(lr) == 2, length(sheet) == 1, is.character(sheet))
  ul <- as.integer(ul)
  lr <- as.integer(lr)
  NA_or_pos <- function(x) is.na(x) | x > 0
  stopifnot(all(NA_or_pos(ul)))
  stopifnot(all(NA_or_pos(lr)))
  if (is.na(ul[1]) && !is.na(lr[1])) 
    ul[1] <- 1
  if (is.na(ul[2]) && !is.na(lr[2])) 
    ul[2] <- 1
  rows <- c(ul[1], lr[1])
  cols <- c(ul[2], lr[2])
  if (!anyNA(rows)) 
    stopifnot(rows[1] <= rows[2])
  if (!anyNA(cols)) 
    stopifnot(cols[1] <= cols[2])
  structure(list(ul = ul, lr = lr, sheet = sheet), class = c("cell_limits", "list"))
})
c("package:readxl", "namespace:cellranger")
c(TRUE, FALSE)
c(FALSE, TRUE)
cell_rows
list(`package:readxl` = function (x) 
{
  if (all(is.na(x))) {
    return(cell_limits())
  }
  stopifnot(is.numeric(x))
  if (length(x) != 2) {
    x <- range(x, na.rm = TRUE)
  }
  cell_limits(as.integer(c(x[1], NA)), as.integer(c(x[2], NA)))
}, function (x) 
{
  if (all(is.na(x))) {
    return(cell_limits())
  }
  stopifnot(is.numeric(x))
  if (length(x) != 2) {
    x <- range(x, na.rm = TRUE)
  }
  cell_limits(as.integer(c(x[1], NA)), as.integer(c(x[2], NA)))
})
c("package:readxl", "namespace:cellranger")
c(TRUE, FALSE)
c(FALSE, TRUE)
excel_format
list(`package:readxl` = function (path, guess = TRUE) 
{
  format <- format_from_ext(path)
  if (!isTRUE(guess)) {
    return(format)
  }
  guess_me <- is.na(format) & file.exists(path)
  format[guess_me] <- format_from_signature(path[guess_me])
  format
}, function (path, guess = TRUE) 
{
  format <- format_from_ext(path)
  if (!isTRUE(guess)) {
    return(format)
  }
  guess_me <- is.na(format) & file.exists(path)
  format[guess_me] <- format_from_signature(path[guess_me])
  format
})
c("package:readxl", "namespace:readxl")
c(TRUE, FALSE)
c(FALSE, TRUE)
excel_sheets
list(`package:readxl` = function (path) 
{
  path <- check_file(path)
  format <- check_format(path)
  path <- normalizePath(path)
  switch(format, xls = xls_sheets(path), xlsx = xlsx_sheets(path))
}, function (path) 
{
  path <- check_file(path)
  format <- check_format(path)
  path <- normalizePath(path)
  switch(format, xls = xls_sheets(path), xlsx = xlsx_sheets(path))
})
c("package:readxl", "namespace:readxl")
c(TRUE, FALSE)
c(FALSE, TRUE)
format_from_ext
list(`package:readxl` = function (path) 
{
  ext <- tolower(tools::file_ext(path))
  formats <- c(xls = "xls", xlsx = "xlsx", xlsm = "xlsx", xltx = "xlsx", xltm = "xlsx")
  unname(formats[ext])
}, function (path) 
{
  ext <- tolower(tools::file_ext(path))
  formats <- c(xls = "xls", xlsx = "xlsx", xlsm = "xlsx", xltx = "xlsx", xltm = "xlsx")
  unname(formats[ext])
})
c("package:readxl", "namespace:readxl")
c(TRUE, FALSE)
c(FALSE, TRUE)
format_from_signature
list(`package:readxl` = function (path) 
{
  signature <- lapply(path, first_8_bytes)
  vapply(signature, sig_to_fmt, "xlsx?")
}, function (path) 
{
  signature <- lapply(path, first_8_bytes)
  vapply(signature, sig_to_fmt, "xlsx?")
})
c("package:readxl", "namespace:readxl")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_excel
list(`package:readxl` = function (path, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = readxl_progress(), .name_repair = "unique") 
{
  path <- check_file(path)
  format <- check_format(path)
  read_excel_(path = path, sheet = sheet, range = range, col_names = col_names, col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress, .name_repair = .name_repair, format = format)
}, function (path, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = readxl_progress(), .name_repair = "unique") 
{
  path <- check_file(path)
  format <- check_format(path)
  read_excel_(path = path, sheet = sheet, range = range, col_names = col_names, col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress, .name_repair = .name_repair, format = format)
})
c("package:readxl", "namespace:readxl")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_xls
list(`package:readxl` = function (path, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = readxl_progress(), .name_repair = "unique") 
{
  path <- check_file(path)
  read_excel_(path = path, sheet = sheet, range = range, col_names = col_names, col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress, .name_repair = .name_repair, format = "xls")
}, function (path, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = readxl_progress(), .name_repair = "unique") 
{
  path <- check_file(path)
  read_excel_(path = path, sheet = sheet, range = range, col_names = col_names, col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress, .name_repair = .name_repair, format = "xls")
})
c("package:readxl", "namespace:readxl")
c(TRUE, FALSE)
c(FALSE, TRUE)
read_xlsx
list(`package:readxl` = function (path, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = readxl_progress(), .name_repair = "unique") 
{
  path <- check_file(path)
  read_excel_(path = path, sheet = sheet, range = range, col_names = col_names, col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress, .name_repair = .name_repair, format = "xlsx")
}, function (path, sheet = NULL, range = NULL, col_names = TRUE, col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = readxl_progress(), .name_repair = "unique") 
{
  path <- check_file(path)
  read_excel_(path = path, sheet = sheet, range = range, col_names = col_names, col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress, .name_repair = .name_repair, format = "xlsx")
})
c("package:readxl", "namespace:readxl")
c(TRUE, FALSE)
c(FALSE, TRUE)
readxl_example
list(`package:readxl` = function (path = NULL) 
{
  if (is.null(path)) {
    dir(system.file("extdata", package = "readxl"))
  }
  else {
    system.file("extdata", path, package = "readxl", mustWork = TRUE)
  }
}, function (path = NULL) 
{
  if (is.null(path)) {
    dir(system.file("extdata", package = "readxl"))
  }
  else {
    system.file("extdata", path, package = "readxl", mustWork = TRUE)
  }
})
c("package:readxl", "namespace:readxl")
c(TRUE, FALSE)
c(FALSE, TRUE)
readxl_progress
list(`package:readxl` = function () 
{
  isTRUE(getOption("readxl.show_progress", default = TRUE)) && interactive() && !isTRUE(getOption("knitr.in.progress")) && !isTRUE(getOption("rstudio.notebook.executing"))
}, function () 
{
  isTRUE(getOption("readxl.show_progress", default = TRUE)) && interactive() && !isTRUE(getOption("knitr.in.progress")) && !isTRUE(getOption("rstudio.notebook.executing"))
})
c("package:readxl", "namespace:readxl")
c(TRUE, FALSE)
c(FALSE, TRUE)


############################################################################################
library(RODBC)
getSqlTypeInfo
list(`package:RODBC` = function (driver) 
{
  if (missing(driver)) {
    res <- t(data.frame(lapply(typesR2DBMS, as.character), check.names = FALSE))
    colnames(res) <- c("double", "integer", "character", "logical")
    as.data.frame(res)
  }
  else typesR2DBMS[[driver]]
}, function (driver) 
{
  if (missing(driver)) {
    res <- t(data.frame(lapply(typesR2DBMS, as.character), check.names = FALSE))
    colnames(res) <- c("double", "integer", "character", "logical")
    as.data.frame(res)
  }
  else typesR2DBMS[[driver]]
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcClearError
list(`package:RODBC` = function (channel) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  .Call(C_RODBCClearError, attr(channel, "handle_ptr"))
  invisible()
}, function (channel) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  .Call(C_RODBCClearError, attr(channel, "handle_ptr"))
  invisible()
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcClose
list(`package:RODBC` = function (channel) 
{
  if (!odbcValidChannel(channel)) 
    stop("argument is not an open RODBC channel")
  res <- .Call(C_RODBCClose, attr(channel, "handle_ptr"))
  if (res > 0) 
    invisible(FALSE)
  else {
    warning(paste(odbcGetErrMsg(channel), sep = "\n"))
    FALSE
  }
  invisible(TRUE)
}, function (channel) 
{
  if (!odbcValidChannel(channel)) 
    stop("argument is not an open RODBC channel")
  res <- .Call(C_RODBCClose, attr(channel, "handle_ptr"))
  if (res > 0) 
    invisible(FALSE)
  else {
    warning(paste(odbcGetErrMsg(channel), sep = "\n"))
    FALSE
  }
  invisible(TRUE)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcCloseAll
list(`package:RODBC` = function () 
{
  .Call(C_RODBCCloseAll)
  invisible()
}, function () 
{
  .Call(C_RODBCCloseAll)
  invisible()
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcConnect
list(`package:RODBC` = function (dsn, uid = "", pwd = "", ...) 
{
  Call <- match.call()
  Call$uid <- Call$pwd <- NULL
  Call[[1]] <- quote(RODBC::odbcDriverConnect)
  st <- paste("DSN=", dsn, sep = "")
  if (nchar(uid)) 
    st <- paste(st, ";UID=", uid, sep = "")
  if (nchar(pwd)) 
    st <- paste(st, ";PWD=", pwd, sep = "")
  Call[[2]] <- st
  names(Call)[2] <- ""
  eval.parent(Call)
}, function (dsn, uid = "", pwd = "", ...) 
{
  Call <- match.call()
  Call$uid <- Call$pwd <- NULL
  Call[[1]] <- quote(RODBC::odbcDriverConnect)
  st <- paste("DSN=", dsn, sep = "")
  if (nchar(uid)) 
    st <- paste(st, ";UID=", uid, sep = "")
  if (nchar(pwd)) 
    st <- paste(st, ";PWD=", pwd, sep = "")
  Call[[2]] <- st
  names(Call)[2] <- ""
  eval.parent(Call)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcConnectAccess
list(`package:RODBC` = function (access.file, uid = "", pwd = "", ...) 
{
  if (.Machine$sizeof.pointer > 4) 
    stop("odbcConnectAccess is only usable with 32-bit Windows")
  con <- if (missing(access.file)) 
    "Driver={Microsoft Access Driver (*.mdb)};Dbq="
  else paste("Driver={Microsoft Access Driver (*.mdb)};Dbq=", full.path(access.file), ";Uid=", uid, ";Pwd=", pwd, ";", sep = "")
  odbcDriverConnect(con, ...)
}, function (access.file, uid = "", pwd = "", ...) 
{
  if (.Machine$sizeof.pointer > 4) 
    stop("odbcConnectAccess is only usable with 32-bit Windows")
  con <- if (missing(access.file)) 
    "Driver={Microsoft Access Driver (*.mdb)};Dbq="
  else paste("Driver={Microsoft Access Driver (*.mdb)};Dbq=", full.path(access.file), ";Uid=", uid, ";Pwd=", pwd, ";", sep = "")
  odbcDriverConnect(con, ...)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcConnectAccess2007
list(`package:RODBC` = function (access.file, uid = "", pwd = "", ...) 
{
  con <- if (missing(access.file)) 
    "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
  else paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", full.path(access.file), ";Uid=", uid, ";Pwd=", pwd, ";", sep = "")
  odbcDriverConnect(con, ...)
}, function (access.file, uid = "", pwd = "", ...) 
{
  con <- if (missing(access.file)) 
    "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
  else paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", full.path(access.file), ";Uid=", uid, ";Pwd=", pwd, ";", sep = "")
  odbcDriverConnect(con, ...)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcConnectDbase
list(`package:RODBC` = function (dbf.file, ...) 
{
  if (.Machine$sizeof.pointer > 4) 
    warning("odbcConnectDbase is probably only usable with 32-bit Windows")
  con <- if (missing(dbf.file)) 
    "Driver={Microsoft dBASE Driver (*.dbf)};DriverID=277;Dbq="
  else paste("Driver={Microsoft dBASE Driver (*.dbf)};DriverID=277;Dbq=", dirname(full.path(dbf.file)), ";", sep = "")
  odbcDriverConnect(con, tabQuote = c("[", "]"), ...)
}, function (dbf.file, ...) 
{
  if (.Machine$sizeof.pointer > 4) 
    warning("odbcConnectDbase is probably only usable with 32-bit Windows")
  con <- if (missing(dbf.file)) 
    "Driver={Microsoft dBASE Driver (*.dbf)};DriverID=277;Dbq="
  else paste("Driver={Microsoft dBASE Driver (*.dbf)};DriverID=277;Dbq=", dirname(full.path(dbf.file)), ";", sep = "")
  odbcDriverConnect(con, tabQuote = c("[", "]"), ...)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcConnectExcel
list(`package:RODBC` = function (xls.file, readOnly = TRUE, ...) 
{
  if (.Machine$sizeof.pointer > 4) 
    stop("odbcConnectExcel is only usable with 32-bit Windows")
  con <- if (missing(xls.file)) 
    "Driver={Microsoft Excel Driver (*.xls)};DriverId=790;Dbq="
  else {
    fp <- full.path(xls.file)
    paste("Driver={Microsoft Excel Driver (*.xls)};DriverId=790;Dbq=", fp, ";DefaultDir=", dirname(fp), ";", sep = "")
  }
  if (!readOnly) 
    con = paste(con, "ReadOnly=False", sep = ";")
  odbcDriverConnect(con, tabQuote = c("[", "]"), ...)
}, function (xls.file, readOnly = TRUE, ...) 
{
  if (.Machine$sizeof.pointer > 4) 
    stop("odbcConnectExcel is only usable with 32-bit Windows")
  con <- if (missing(xls.file)) 
    "Driver={Microsoft Excel Driver (*.xls)};DriverId=790;Dbq="
  else {
    fp <- full.path(xls.file)
    paste("Driver={Microsoft Excel Driver (*.xls)};DriverId=790;Dbq=", fp, ";DefaultDir=", dirname(fp), ";", sep = "")
  }
  if (!readOnly) 
    con = paste(con, "ReadOnly=False", sep = ";")
  odbcDriverConnect(con, tabQuote = c("[", "]"), ...)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcConnectExcel2007
list(`package:RODBC` = function (xls.file, readOnly = TRUE, ...) 
{
  con <- if (missing(xls.file)) 
    "Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};Dbq="
  else {
    fp <- full.path(xls.file)
    paste("Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};Dbq=", fp, ";DefaultDir=", dirname(fp), ";", sep = "")
  }
  if (!readOnly) 
    con = paste(con, "ReadOnly=False", sep = ";")
  odbcDriverConnect(con, tabQuote = c("[", "]"), ...)
}, function (xls.file, readOnly = TRUE, ...) 
{
  con <- if (missing(xls.file)) 
    "Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};Dbq="
  else {
    fp <- full.path(xls.file)
    paste("Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};Dbq=", fp, ";DefaultDir=", dirname(fp), ";", sep = "")
  }
  if (!readOnly) 
    con = paste(con, "ReadOnly=False", sep = ";")
  odbcDriverConnect(con, tabQuote = c("[", "]"), ...)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcDataSources
list(`package:RODBC` = function (type = c("all", "user", "system")) 
{
  type <- match.arg(type)
  type <- match(type, c("all", "user", "system"))
  .Call(C_RODBCListDataSources, type)
}, function (type = c("all", "user", "system")) 
{
  type <- match.arg(type)
  type <- match(type, c("all", "user", "system"))
  .Call(C_RODBCListDataSources, type)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcDriverConnect
list(`package:RODBC` = function (connection = "", case = "nochange", believeNRows = TRUE, colQuote, tabQuote = colQuote, interpretDot = TRUE, DBMSencoding = "", rows_at_time = 100, readOnlyOptimize = FALSE) 
{
  id <- as.integer(1 + runif(1, 0, 1e+05))
  stat <- .Call(C_RODBCDriverConnect, as.character(connection), id, as.integer(believeNRows), as.logical(readOnlyOptimize))
  if (stat < 0) {
    warning("ODBC connection failed")
    return(stat)
  }
  Call <- match.call()
  res <- .Call(C_RODBCGetInfo, attr(stat, "handle_ptr"))
  isMySQL <- res[1] == "MySQL"
  if (missing(colQuote)) 
    colQuote <- ifelse(isMySQL, "`", "\"")
  if (missing(case)) 
    case <- switch(res[1], MySQL = "mysql", PostgreSQL = "postgresql", "nochange")
  switch(case, toupper = case <- 1, tolower = case <- 2, postgresql = case <- 2, nochange = case <- 0, msaccess = case <- 0, mysql = case <- ifelse(.Platform$OS.type == "windows", 2, 0), stop("Invalid case parameter: nochange | toupper | tolower | common db names"))
  case <- switch(case + 1, "nochange", "toupper", "tolower")
  rows_at_time <- max(1, min(1024, rows_at_time))
  cs <- attr(stat, "connection.string")
  if (grepl("PWD=", cs)) {
    attr(stat, "connection.string") <- sub("PWD=[^;]+($|;)", "PWD=******;", cs)
    Call$connection <- sub("PWD=[^;]+($|;)", "PWD=******;", connection)
  }
  structure(stat, class = "RODBC", case = case, id = id, believeNRows = believeNRows, colQuote = colQuote, tabQuote = tabQuote, interpretDot = interpretDot, encoding = DBMSencoding, rows_at_time = rows_at_time, isMySQL = isMySQL, call = Call)
}, function (connection = "", case = "nochange", believeNRows = TRUE, colQuote, tabQuote = colQuote, interpretDot = TRUE, DBMSencoding = "", rows_at_time = 100, readOnlyOptimize = FALSE) 
{
  id <- as.integer(1 + runif(1, 0, 1e+05))
  stat <- .Call(C_RODBCDriverConnect, as.character(connection), id, as.integer(believeNRows), as.logical(readOnlyOptimize))
  if (stat < 0) {
    warning("ODBC connection failed")
    return(stat)
  }
  Call <- match.call()
  res <- .Call(C_RODBCGetInfo, attr(stat, "handle_ptr"))
  isMySQL <- res[1] == "MySQL"
  if (missing(colQuote)) 
    colQuote <- ifelse(isMySQL, "`", "\"")
  if (missing(case)) 
    case <- switch(res[1], MySQL = "mysql", PostgreSQL = "postgresql", "nochange")
  switch(case, toupper = case <- 1, tolower = case <- 2, postgresql = case <- 2, nochange = case <- 0, msaccess = case <- 0, mysql = case <- ifelse(.Platform$OS.type == "windows", 2, 0), stop("Invalid case parameter: nochange | toupper | tolower | common db names"))
  case <- switch(case + 1, "nochange", "toupper", "tolower")
  rows_at_time <- max(1, min(1024, rows_at_time))
  cs <- attr(stat, "connection.string")
  if (grepl("PWD=", cs)) {
    attr(stat, "connection.string") <- sub("PWD=[^;]+($|;)", "PWD=******;", cs)
    Call$connection <- sub("PWD=[^;]+($|;)", "PWD=******;", connection)
  }
  structure(stat, class = "RODBC", case = case, id = id, believeNRows = believeNRows, colQuote = colQuote, tabQuote = tabQuote, interpretDot = interpretDot, encoding = DBMSencoding, rows_at_time = rows_at_time, isMySQL = isMySQL, call = Call)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcEndTran
list(`package:RODBC` = function (channel, commit = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  .Call(C_RODBCEndTran, attr(channel, "handle_ptr"), commit)
}, function (channel, commit = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  .Call(C_RODBCEndTran, attr(channel, "handle_ptr"), commit)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcFetchRows
list(`package:RODBC` = function (channel, max = 0, buffsize = 1000, nullstring = NA, believeNRows = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  .Call(C_RODBCFetchRows, attr(channel, "handle_ptr"), max, buffsize, as.character(nullstring), believeNRows)
}, function (channel, max = 0, buffsize = 1000, nullstring = NA, believeNRows = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  .Call(C_RODBCFetchRows, attr(channel, "handle_ptr"), max, buffsize, as.character(nullstring), believeNRows)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcGetErrMsg
list(`package:RODBC` = function (channel) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  err <- .Call(C_RODBCGetErrMsg, attr(channel, "handle_ptr"))
  .Call(C_RODBCClearError, attr(channel, "handle_ptr"))
  return(err)
}, function (channel) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  err <- .Call(C_RODBCGetErrMsg, attr(channel, "handle_ptr"))
  .Call(C_RODBCClearError, attr(channel, "handle_ptr"))
  return(err)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcGetInfo
list(`package:RODBC` = function (channel) 
{
  if (!odbcValidChannel(channel)) 
    stop("argument is not an open RODBC channel")
  res <- .Call(C_RODBCGetInfo, attr(channel, "handle_ptr"))
  names(res) <- c("DBMS_Name", "DBMS_Ver", "Driver_ODBC_Ver", "Data_Source_Name", "Driver_Name", "Driver_Ver", "ODBC_Ver", "Server_Name")
  res
}, function (channel) 
{
  if (!odbcValidChannel(channel)) 
    stop("argument is not an open RODBC channel")
  res <- .Call(C_RODBCGetInfo, attr(channel, "handle_ptr"))
  names(res) <- c("DBMS_Name", "DBMS_Ver", "Driver_ODBC_Ver", "Data_Source_Name", "Driver_Name", "Driver_Ver", "ODBC_Ver", "Server_Name")
  res
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcQuery
list(`package:RODBC` = function (channel, query, rows_at_time = attr(channel, "rows_at_time")) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (nchar(enc <- attr(channel, "encoding"))) 
    query <- iconv(query, to = enc)
  .Call(C_RODBCQuery, attr(channel, "handle_ptr"), as.character(query), as.integer(rows_at_time))
}, function (channel, query, rows_at_time = attr(channel, "rows_at_time")) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (nchar(enc <- attr(channel, "encoding"))) 
    query <- iconv(query, to = enc)
  .Call(C_RODBCQuery, attr(channel, "handle_ptr"), as.character(query), as.integer(rows_at_time))
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcReConnect
list(`package:RODBC` = function (channel, ...) 
{
  if (!inherits(channel, "RODBC")) 
    stop("Argument 'channel' must inherit from class RODBC")
  Call <- attr(channel, "call")
  dots <- list(...)
  if ("uid" %in% names(dots)) {
    uid <- dots$uid
    dots$uid <- NULL
    Call$Connection <- sub("UID=[^;]+($|;)", paste("UID=", uid, ";", sep = ""), Call$connection)
  }
  if ("pwd" %in% names(dots)) {
    pwd <- dots$pwd
    dots$pwd <- NULL
    Call$connection <- sub("PWD=[^;]+($|;)", paste("PWD=", pwd, ";", sep = ""), Call$connection)
  }
  if (length(dots)) 
    Call[names(dots)] <- dots
  eval.parent(Call)
}, function (channel, ...) 
{
  if (!inherits(channel, "RODBC")) 
    stop("Argument 'channel' must inherit from class RODBC")
  Call <- attr(channel, "call")
  dots <- list(...)
  if ("uid" %in% names(dots)) {
    uid <- dots$uid
    dots$uid <- NULL
    Call$Connection <- sub("UID=[^;]+($|;)", paste("UID=", uid, ";", sep = ""), Call$connection)
  }
  if ("pwd" %in% names(dots)) {
    pwd <- dots$pwd
    dots$pwd <- NULL
    Call$connection <- sub("PWD=[^;]+($|;)", paste("PWD=", pwd, ";", sep = ""), Call$connection)
  }
  if (length(dots)) 
    Call[names(dots)] <- dots
  eval.parent(Call)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcSetAutoCommit
list(`package:RODBC` = function (channel, autoCommit = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  .Call(C_RODBCSetAutoCommit, attr(channel, "handle_ptr"), autoCommit)
}, function (channel, autoCommit = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  .Call(C_RODBCSetAutoCommit, attr(channel, "handle_ptr"), autoCommit)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcTables
list(`package:RODBC` = function (channel, catalog = NULL, schema = NULL, tableName = NULL, tableType = NULL, literal = FALSE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  tableType <- if (is.character(tableType) && length(tableType)) 
    paste(tableType, collapse = ",")
  else NULL
  .Call(C_RODBCTables, attr(channel, "handle_ptr"), catalog, schema, tableName, tableType, as.logical(literal))
}, function (channel, catalog = NULL, schema = NULL, tableName = NULL, tableType = NULL, literal = FALSE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  tableType <- if (is.character(tableType) && length(tableType)) 
    paste(tableType, collapse = ",")
  else NULL
  .Call(C_RODBCTables, attr(channel, "handle_ptr"), catalog, schema, tableName, tableType, as.logical(literal))
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
odbcUpdate
list(`package:RODBC` = function (channel, query, data, params, test = FALSE, verbose = FALSE, nastring = NULL) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (length(params) == 0 || nrow(params) == 0) 
    stop("no parameters, so nothing to update")
  if (nchar(enc <- attr(channel, "encoding"))) 
    query <- iconv(query, to = enc)
  vflag <- 0
  if (verbose) 
    vflag <- 1
  if (test) 
    vflag <- 2
  cnames <- mangleColNames(names(data))
  cnames <- switch(attr(channel, "case"), nochange = cnames, toupper = toupper(cnames), tolower = tolower(cnames))
  for (i in seq_along(data)) if (!is.numeric(data[[i]])) {
    data[[i]] <- as.character(data[[i]])
    if (nchar(enc)) 
      data[[i]] <- iconv(data[[i]], to = enc)
  }
  ds <- match(params[[1]], cnames)
  if (any(is.na(ds))) 
    stop("missing columns in 'data'")
  .Call(C_RODBCUpdate, attr(channel, "handle_ptr"), as.character(query), data, ds - 1, params, as.integer(vflag))
}, function (channel, query, data, params, test = FALSE, verbose = FALSE, nastring = NULL) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (length(params) == 0 || nrow(params) == 0) 
    stop("no parameters, so nothing to update")
  if (nchar(enc <- attr(channel, "encoding"))) 
    query <- iconv(query, to = enc)
  vflag <- 0
  if (verbose) 
    vflag <- 1
  if (test) 
    vflag <- 2
  cnames <- mangleColNames(names(data))
  cnames <- switch(attr(channel, "case"), nochange = cnames, toupper = toupper(cnames), tolower = tolower(cnames))
  for (i in seq_along(data)) if (!is.numeric(data[[i]])) {
    data[[i]] <- as.character(data[[i]])
    if (nchar(enc)) 
      data[[i]] <- iconv(data[[i]], to = enc)
  }
  ds <- match(params[[1]], cnames)
  if (any(is.na(ds))) 
    stop("missing columns in 'data'")
  .Call(C_RODBCUpdate, attr(channel, "handle_ptr"), as.character(query), data, ds - 1, params, as.integer(vflag))
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
setSqlTypeInfo
list(`package:RODBC` = function (driver, value) 
{
  if (!is.character(driver) || length(driver) != 1) 
    stop("argument 'driver' must be a character string")
  if (!is.list(value) || length(value) < 4 || is.null(names(value))) 
    stop("argument 'value' must be a named list of length >= 4")
  typesR2DBMS[[driver]] <<- value[c("double", "integer", "character", "logical")]
}, function (driver, value) 
{
  if (!is.character(driver) || length(driver) != 1) 
    stop("argument 'driver' must be a character string")
  if (!is.list(value) || length(value) < 4 || is.null(names(value))) 
    stop("argument 'value' must be a named list of length >= 4")
  typesR2DBMS[[driver]] <<- value[c("double", "integer", "character", "logical")]
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlClear
list(`package:RODBC` = function (channel, sqtable, errors = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (missing(sqtable)) 
    stop("missing argument 'sqtable'")
  dbname <- odbcTableExists(channel, sqtable, abort = errors)
  if (!length(dbname)) {
    if (errors) 
      stop("table ", sQuote(sqtable), " not found")
    return(invisible(-1))
  }
  res <- sqlQuery(channel, paste("TRUNCATE TABLE", dbname), errors = errors)
  if (errors && (!length(res) || identical(res, "No Data"))) 
    invisible()
  else invisible(res)
}, function (channel, sqtable, errors = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (missing(sqtable)) 
    stop("missing argument 'sqtable'")
  dbname <- odbcTableExists(channel, sqtable, abort = errors)
  if (!length(dbname)) {
    if (errors) 
      stop("table ", sQuote(sqtable), " not found")
    return(invisible(-1))
  }
  res <- sqlQuery(channel, paste("TRUNCATE TABLE", dbname), errors = errors)
  if (errors && (!length(res) || identical(res, "No Data"))) 
    invisible()
  else invisible(res)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlColumns
list(`package:RODBC` = function (channel, sqtable, errors = FALSE, as.is = TRUE, special = FALSE, catalog = NULL, schema = NULL, literal = FALSE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (length(sqtable) != 1) 
    stop(sQuote(sqtable), " should be a name")
  if (is.null(catalog) && is.null(schema)) {
    dbname <- odbcTableExists(channel, sqtable, FALSE, FALSE)
    if (!length(dbname)) {
      caseprob <- ""
      if (is.data.frame(nm <- sqlTables(channel)) && tolower(sqtable) %in% tolower(nm[, 3])) 
        caseprob <- "\nCheck case parameter in odbcConnect"
      stop(sQuote(sqtable), ": table not found on channel", caseprob)
    }
    if (grepl(".", dbname, fixed = TRUE)) {
      parts <- strsplit(dbname, ".", fixed = TRUE)[[1]]
      if (length(parts) > 2) 
        stop("dot.dot.dot names are not supported")
      if (attr(channel, "isMySQL")) {
        catalog <- parts[1]
        dbname <- parts[2]
      }
      else {
        schema <- parts[1]
        dbname <- parts[2]
      }
    }
  }
  else dbname <- switch(attr(channel, "case"), nochange = sqtable, toupper = toupper(sqtable), tolower = tolower(sqtable))
  stat <- if (special) 
    odbcSpecialColumns(channel, dbname, catalog, schema)
  else odbcColumns(channel, dbname, catalog, schema, literal)
  if (stat < 0) {
    if (errors) {
      if (stat == -2) 
        stop("invalid channel")
      else return(odbcGetErrMsg(channel))
    }
    else return(invisible(-1))
  }
  else return(sqlGetResults(channel, as.is = as.is))
}, function (channel, sqtable, errors = FALSE, as.is = TRUE, special = FALSE, catalog = NULL, schema = NULL, literal = FALSE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (length(sqtable) != 1) 
    stop(sQuote(sqtable), " should be a name")
  if (is.null(catalog) && is.null(schema)) {
    dbname <- odbcTableExists(channel, sqtable, FALSE, FALSE)
    if (!length(dbname)) {
      caseprob <- ""
      if (is.data.frame(nm <- sqlTables(channel)) && tolower(sqtable) %in% tolower(nm[, 3])) 
        caseprob <- "\nCheck case parameter in odbcConnect"
      stop(sQuote(sqtable), ": table not found on channel", caseprob)
    }
    if (grepl(".", dbname, fixed = TRUE)) {
      parts <- strsplit(dbname, ".", fixed = TRUE)[[1]]
      if (length(parts) > 2) 
        stop("dot.dot.dot names are not supported")
      if (attr(channel, "isMySQL")) {
        catalog <- parts[1]
        dbname <- parts[2]
      }
      else {
        schema <- parts[1]
        dbname <- parts[2]
      }
    }
  }
  else dbname <- switch(attr(channel, "case"), nochange = sqtable, toupper = toupper(sqtable), tolower = tolower(sqtable))
  stat <- if (special) 
    odbcSpecialColumns(channel, dbname, catalog, schema)
  else odbcColumns(channel, dbname, catalog, schema, literal)
  if (stat < 0) {
    if (errors) {
      if (stat == -2) 
        stop("invalid channel")
      else return(odbcGetErrMsg(channel))
    }
    else return(invisible(-1))
  }
  else return(sqlGetResults(channel, as.is = as.is))
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlCopy
list(`package:RODBC` = function (channel, query, destination, destchannel = channel, verbose = FALSE, errors = TRUE, ...) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (!odbcValidChannel(destchannel)) 
    stop("destination argument is not an open RODBC channel")
  if (missing(query) || missing(destination)) 
    stop("missing parameter")
  if (length(destination) != 1) 
    stop("destination should be a name")
  dataset <- sqlQuery(channel, query, errors = errors)
  sqlSave(destchannel, dataset, destination, verbose = verbose, ...)
}, function (channel, query, destination, destchannel = channel, verbose = FALSE, errors = TRUE, ...) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (!odbcValidChannel(destchannel)) 
    stop("destination argument is not an open RODBC channel")
  if (missing(query) || missing(destination)) 
    stop("missing parameter")
  if (length(destination) != 1) 
    stop("destination should be a name")
  dataset <- sqlQuery(channel, query, errors = errors)
  sqlSave(destchannel, dataset, destination, verbose = verbose, ...)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlCopyTable
list(`package:RODBC` = function (channel, srctable, desttable, destchannel = channel, verbose = FALSE, errors = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (!odbcValidChannel(destchannel)) 
    stop("destination argument is not an open RODBC channel")
  if (missing(srctable) || missing(desttable)) 
    stop("missing parameter")
  dtablename <- as.character(desttable)
  if (length(dtablename) != 1) 
    stop(sQuote(dtablename), " should be a name")
  stablename <- as.character(srctable)
  if (!length(odbcTableExists(channel, stablename, abort = errors))) 
    return(invisible(-1))
  query <- sqltablecreate(channel, dtablename, coldata = sqlColumns(channel, stablename), keys = sqlPrimaryKeys(channel, stablename))
  if (verbose) 
    cat("Query: ", query, "\n", sep = "")
  sqlQuery(destchannel, query, errors = errors)
}, function (channel, srctable, desttable, destchannel = channel, verbose = FALSE, errors = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (!odbcValidChannel(destchannel)) 
    stop("destination argument is not an open RODBC channel")
  if (missing(srctable) || missing(desttable)) 
    stop("missing parameter")
  dtablename <- as.character(desttable)
  if (length(dtablename) != 1) 
    stop(sQuote(dtablename), " should be a name")
  stablename <- as.character(srctable)
  if (!length(odbcTableExists(channel, stablename, abort = errors))) 
    return(invisible(-1))
  query <- sqltablecreate(channel, dtablename, coldata = sqlColumns(channel, stablename), keys = sqlPrimaryKeys(channel, stablename))
  if (verbose) 
    cat("Query: ", query, "\n", sep = "")
  sqlQuery(destchannel, query, errors = errors)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlDrop
list(`package:RODBC` = function (channel, sqtable, errors = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (missing(sqtable)) 
    stop("missing argument 'sqtable'")
  dbname <- odbcTableExists(channel, sqtable, abort = errors)
  if (!length(dbname)) {
    if (errors) 
      stop("table ", sQuote(sqtable), " not found")
    return(invisible(-1))
  }
  res <- sqlQuery(channel, paste("DROP TABLE", dbname), errors = errors)
  if (errors && (!length(res) || identical(res, "No Data"))) 
    invisible()
  else invisible(res)
}, function (channel, sqtable, errors = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (missing(sqtable)) 
    stop("missing argument 'sqtable'")
  dbname <- odbcTableExists(channel, sqtable, abort = errors)
  if (!length(dbname)) {
    if (errors) 
      stop("table ", sQuote(sqtable), " not found")
    return(invisible(-1))
  }
  res <- sqlQuery(channel, paste("DROP TABLE", dbname), errors = errors)
  if (errors && (!length(res) || identical(res, "No Data"))) 
    invisible()
  else invisible(res)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlFetch
list(`package:RODBC` = function (channel, sqtable, ..., colnames = FALSE, rownames = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (missing(sqtable)) 
    stop("missing argument 'sqtable'")
  dbname <- odbcTableExists(channel, sqtable)
  ans <- sqlQuery(channel, paste("SELECT * FROM", dbname), ...)
  if (is.data.frame(ans)) {
    if (is.logical(colnames) && colnames) {
      colnames(ans) <- as.character(as.matrix(ans[1, ]))
      ans <- ans[-1, ]
    }
    if (is.logical(rownames) && rownames) 
      rownames <- "rownames"
    if (is.character(rownames)) {
      cn <- names(ans)
      if (!is.na(rn <- match(rownames, cn))) {
        row.names(ans) <- as.character(ans[, rn])
        ans <- ans[, -rn, drop = FALSE]
      }
    }
  }
  ans
}, function (channel, sqtable, ..., colnames = FALSE, rownames = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (missing(sqtable)) 
    stop("missing argument 'sqtable'")
  dbname <- odbcTableExists(channel, sqtable)
  ans <- sqlQuery(channel, paste("SELECT * FROM", dbname), ...)
  if (is.data.frame(ans)) {
    if (is.logical(colnames) && colnames) {
      colnames(ans) <- as.character(as.matrix(ans[1, ]))
      ans <- ans[-1, ]
    }
    if (is.logical(rownames) && rownames) 
      rownames <- "rownames"
    if (is.character(rownames)) {
      cn <- names(ans)
      if (!is.na(rn <- match(rownames, cn))) {
        row.names(ans) <- as.character(ans[, rn])
        ans <- ans[, -rn, drop = FALSE]
      }
    }
  }
  ans
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlFetchMore
list(`package:RODBC` = function (channel, ..., colnames = FALSE, rownames = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  ans <- sqlGetResults(channel, ...)
  if (is.data.frame(ans)) {
    if (is.logical(colnames) && colnames) {
      colnames(ans) <- as.character(as.matrix(ans[1, ]))
      ans <- ans[-1, ]
    }
    if (is.logical(rownames) && rownames) 
      rownames <- "rownames"
    if (is.character(rownames)) {
      cn <- names(ans)
      if (!is.na(rn <- match(rownames, cn))) {
        row.names(ans) <- as.character(ans[, rn])
        ans <- ans[, -rn, drop = FALSE]
      }
    }
  }
  ans
}, function (channel, ..., colnames = FALSE, rownames = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  ans <- sqlGetResults(channel, ...)
  if (is.data.frame(ans)) {
    if (is.logical(colnames) && colnames) {
      colnames(ans) <- as.character(as.matrix(ans[1, ]))
      ans <- ans[-1, ]
    }
    if (is.logical(rownames) && rownames) 
      rownames <- "rownames"
    if (is.character(rownames)) {
      cn <- names(ans)
      if (!is.na(rn <- match(rownames, cn))) {
        row.names(ans) <- as.character(ans[, rn])
        ans <- ans[, -rn, drop = FALSE]
      }
    }
  }
  ans
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlGetResults
list(`package:RODBC` = function (channel, as.is = FALSE, errors = FALSE, max = 0, buffsize = 1000, nullstring = NA, na.strings = "NA", believeNRows = TRUE, dec = getOption("dec"), stringsAsFactors = FALSE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  as.df <- function(value, colnames) {
    for (i in seq_along(value)) if (is.list(value[[i]])) 
      class(value[[i]]) <- "ODBC_binary"
    class(value) <- "data.frame"
    names(value) <- make.unique(colnames)
    row.names(value) <- seq_along(value[[1]])
    value
  }
  cols <- .Call(C_RODBCNumCols, attr(channel, "handle_ptr"))
  if (cols < 0) {
    if (errors) 
      return("No data")
    else return(invisible(-1))
  }
  cData <- .Call(C_RODBCColData, attr(channel, "handle_ptr"))
  dbdata <- odbcFetchRows(channel, max = max, buffsize = buffsize, nullstring = nullstring, believeNRows = believeNRows)
  if (dbdata$stat < 0) {
    if (errors) 
      return(odbcGetErrMsg(channel))
    else return(invisible(dbdata$stat))
  }
  data <- as.df(dbdata$data, cData$names)
  if (nrow(data) > 0) {
    cols <- ncol(data)
    enc <- attr(channel, "encoding")
    if (length(na.strings)) 
      for (i in 1:cols) if (is.character(data[, i])) 
        data[data[, i] %in% na.strings, i] <- NA
    if (is.logical(as.is)) {
      as.is <- rep(as.is, length.out = cols)
    }
    else if (is.numeric(as.is)) {
      if (any(as.is < 1 | as.is > cols)) 
        stop("invalid numeric 'as.is' expression")
      i <- rep(FALSE, cols)
      i[as.is] <- TRUE
      as.is <- i
    }
    else if (length(as.is) != cols) 
      stop("'as.is' has the wrong length ", length(as.is), " != cols = ", cols)
    for (i in seq_len(cols)) {
      if (is.character(data[[i]]) && nchar(enc)) 
        data[[i]] <- iconv(data[[i]], from = enc)
      if (as.is[i] || is.list(data[[i]])) 
        next
      if (is.numeric(data[[i]])) 
        next
      if (cData$type[i] == "date") 
        data[[i]] <- as.Date(data[[i]])
      else if (cData$type[i] == "timestamp") 
        data[[i]] <- as.POSIXct(data[[i]])
      else data[[i]] <- type.convert(as.character(data[[i]]), na.strings = na.strings, as.is = !stringsAsFactors, dec = dec)
    }
  }
  data
}, function (channel, as.is = FALSE, errors = FALSE, max = 0, buffsize = 1000, nullstring = NA, na.strings = "NA", believeNRows = TRUE, dec = getOption("dec"), stringsAsFactors = FALSE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  as.df <- function(value, colnames) {
    for (i in seq_along(value)) if (is.list(value[[i]])) 
      class(value[[i]]) <- "ODBC_binary"
    class(value) <- "data.frame"
    names(value) <- make.unique(colnames)
    row.names(value) <- seq_along(value[[1]])
    value
  }
  cols <- .Call(C_RODBCNumCols, attr(channel, "handle_ptr"))
  if (cols < 0) {
    if (errors) 
      return("No data")
    else return(invisible(-1))
  }
  cData <- .Call(C_RODBCColData, attr(channel, "handle_ptr"))
  dbdata <- odbcFetchRows(channel, max = max, buffsize = buffsize, nullstring = nullstring, believeNRows = believeNRows)
  if (dbdata$stat < 0) {
    if (errors) 
      return(odbcGetErrMsg(channel))
    else return(invisible(dbdata$stat))
  }
  data <- as.df(dbdata$data, cData$names)
  if (nrow(data) > 0) {
    cols <- ncol(data)
    enc <- attr(channel, "encoding")
    if (length(na.strings)) 
      for (i in 1:cols) if (is.character(data[, i])) 
        data[data[, i] %in% na.strings, i] <- NA
    if (is.logical(as.is)) {
      as.is <- rep(as.is, length.out = cols)
    }
    else if (is.numeric(as.is)) {
      if (any(as.is < 1 | as.is > cols)) 
        stop("invalid numeric 'as.is' expression")
      i <- rep(FALSE, cols)
      i[as.is] <- TRUE
      as.is <- i
    }
    else if (length(as.is) != cols) 
      stop("'as.is' has the wrong length ", length(as.is), " != cols = ", cols)
    for (i in seq_len(cols)) {
      if (is.character(data[[i]]) && nchar(enc)) 
        data[[i]] <- iconv(data[[i]], from = enc)
      if (as.is[i] || is.list(data[[i]])) 
        next
      if (is.numeric(data[[i]])) 
        next
      if (cData$type[i] == "date") 
        data[[i]] <- as.Date(data[[i]])
      else if (cData$type[i] == "timestamp") 
        data[[i]] <- as.POSIXct(data[[i]])
      else data[[i]] <- type.convert(as.character(data[[i]]), na.strings = na.strings, as.is = !stringsAsFactors, dec = dec)
    }
  }
  data
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlPrimaryKeys
list(`package:RODBC` = function (channel, sqtable, errors = FALSE, as.is = TRUE, catalog = NULL, schema = NULL) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (length(sqtable) != 1) 
    stop(sQuote(sqtable), " should be a name")
  if (is.null(catalog) && is.null(schema)) {
    dbname <- odbcTableExists(channel, sqtable, FALSE, FALSE)
    if (!length(dbname)) {
      caseprob <- ""
      if (is.data.frame(nm <- sqlTables(channel)) && tolower(sqtable) %in% tolower(nm[, 3])) 
        caseprob <- "\nCheck case parameter in odbcConnect"
      stop(sQuote(sqtable), ": table not found on channel", caseprob)
    }
    if (grepl(".", dbname, fixed = TRUE)) {
      parts <- strsplit(dbname, ".", fixed = TRUE)[[1]]
      if (length(parts) > 2) 
        stop("dot.dot.dot names are not supported")
      if (attr(channel, "isMySQL")) {
        catalog <- parts[1]
        dbname <- parts[2]
      }
      else {
        schema <- parts[1]
        dbname <- parts[2]
      }
    }
  }
  else dbname <- switch(attr(channel, "case"), nochange = sqtable, toupper = toupper(sqtable), tolower = tolower(sqtable))
  stat <- odbcPrimaryKeys(channel, dbname, catalog, schema)
  if (stat < 0) {
    if (errors) {
      if (stat == -2) 
        stop("invalid channel")
      else return(odbcGetErrMsg(channel))
    }
    else return(invisible(-1))
  }
  else return(sqlGetResults(channel, as.is = as.is))
}, function (channel, sqtable, errors = FALSE, as.is = TRUE, catalog = NULL, schema = NULL) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (length(sqtable) != 1) 
    stop(sQuote(sqtable), " should be a name")
  if (is.null(catalog) && is.null(schema)) {
    dbname <- odbcTableExists(channel, sqtable, FALSE, FALSE)
    if (!length(dbname)) {
      caseprob <- ""
      if (is.data.frame(nm <- sqlTables(channel)) && tolower(sqtable) %in% tolower(nm[, 3])) 
        caseprob <- "\nCheck case parameter in odbcConnect"
      stop(sQuote(sqtable), ": table not found on channel", caseprob)
    }
    if (grepl(".", dbname, fixed = TRUE)) {
      parts <- strsplit(dbname, ".", fixed = TRUE)[[1]]
      if (length(parts) > 2) 
        stop("dot.dot.dot names are not supported")
      if (attr(channel, "isMySQL")) {
        catalog <- parts[1]
        dbname <- parts[2]
      }
      else {
        schema <- parts[1]
        dbname <- parts[2]
      }
    }
  }
  else dbname <- switch(attr(channel, "case"), nochange = sqtable, toupper = toupper(sqtable), tolower = tolower(sqtable))
  stat <- odbcPrimaryKeys(channel, dbname, catalog, schema)
  if (stat < 0) {
    if (errors) {
      if (stat == -2) 
        stop("invalid channel")
      else return(odbcGetErrMsg(channel))
    }
    else return(invisible(-1))
  }
  else return(sqlGetResults(channel, as.is = as.is))
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlQuery
list(`package:RODBC` = function (channel, query, errors = TRUE, ..., rows_at_time) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (missing(query)) 
    stop("missing argument 'query'")
  rows_at_time <- if (missing(rows_at_time)) 
    attr(channel, "rows_at_time")
  else max(1, min(1024, rows_at_time))
  stat <- odbcQuery(channel, query, rows_at_time)
  if (stat == -1) {
    if (errors) 
      return(odbcGetErrMsg(channel))
    else return(invisible(stat))
  }
  else return(sqlGetResults(channel, errors = errors, ...))
}, function (channel, query, errors = TRUE, ..., rows_at_time) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (missing(query)) 
    stop("missing argument 'query'")
  rows_at_time <- if (missing(rows_at_time)) 
    attr(channel, "rows_at_time")
  else max(1, min(1024, rows_at_time))
  stat <- odbcQuery(channel, query, rows_at_time)
  if (stat == -1) {
    if (errors) 
      return(odbcGetErrMsg(channel))
    else return(invisible(stat))
  }
  else return(sqlGetResults(channel, errors = errors, ...))
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlSave
list(`package:RODBC` = function (channel, dat, tablename = NULL, append = FALSE, rownames = TRUE, colnames = FALSE, verbose = FALSE, safer = TRUE, addPK = FALSE, typeInfo, varTypes, fast = TRUE, test = FALSE, nastring = NULL) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (missing(dat)) 
    stop("missing parameter")
  if (!is.data.frame(dat)) 
    stop("should be a data frame")
  if (is.null(tablename)) 
    tablename <- if (length(substitute(dat)) == 1) 
      as.character(substitute(dat))
  else as.character(substitute(dat)[[2]])
  if (length(tablename) != 1) 
    stop(sQuote(tablename), " should be a name")
  switch(attr(channel, "case"), nochange = {
  }, toupper = {
    tablename <- toupper(tablename)
    colnames(dat) <- toupper(colnames(dat))
  }, tolower = {
    tablename <- tolower(tablename)
    colnames(dat) <- tolower(colnames(dat))
  })
  keys <- -1
  if (is.logical(rownames) && rownames) 
    rownames <- "rownames"
  if (is.character(rownames)) {
    dat <- cbind(row.names(dat), dat)
    names(dat)[1] <- rownames
    if (addPK) {
      keys <- vector("list", 4)
      keys[[4]] <- rownames
    }
  }
  if (is.logical(colnames) && colnames) {
    dat <- as.data.frame(rbind(colnames(dat), as.matrix(dat)))
  }
  dbname <- odbcTableExists(channel, tablename, abort = FALSE)
  if (length(dbname)) {
    if (!append) {
      if (safer) 
        stop("table ", sQuote(tablename), " already exists")
      query <- paste("DELETE FROM", dbname)
      if (verbose) 
        cat("Query: ", query, "\n", sep = "")
      res <- sqlQuery(channel, query, errors = FALSE)
      if (is.numeric(res) && res == -1) 
        stop(paste(odbcGetErrMsg(channel), collapse = "\n"))
    }
    if (sqlwrite(channel, tablename, dat, verbose = verbose, fast = fast, test = test, nastring = nastring) == -1) {
      query <- paste("DROP TABLE", dbname)
      if (verbose) {
        cat("sqlwrite returned ", odbcGetErrMsg(channel), "\n", sep = "\n")
        cat("Query: ", query, "\n", sep = "")
      }
      if (safer) 
        stop("unable to append to table ", sQuote(tablename))
      res <- sqlQuery(channel, query, errors = FALSE)
      if (is.numeric(res) && res == -1) 
        stop(paste(odbcGetErrMsg(channel), collapse = "\n"))
    }
    else {
      return(invisible(1))
    }
  }
  types <- sapply(dat, typeof)
  facs <- sapply(dat, is.factor)
  isreal <- (types == "double")
  isint <- (types == "integer") & !facs
  islogi <- (types == "logical")
  colspecs <- rep("varchar(255)", length(dat))
  if (!missing(typeInfo) || !is.null(typeInfo <- typesR2DBMS[[odbcGetInfo(channel)[1]]])) {
    colspecs <- rep(typeInfo$character[1], length(dat))
    colspecs[isreal] <- typeInfo$double[1]
    colspecs[isint] <- typeInfo$integer[1]
    colspecs[islogi] <- typeInfo$logical[1]
  }
  else {
    typeinfo <- sqlTypeInfo(channel, "all", errors = FALSE)
    if (is.data.frame(typeinfo)) {
      if (any(isreal)) {
        realinfo <- sqlTypeInfo(channel, "double")[, 1]
        if (length(realinfo) > 0) {
          if (length(realinfo) > 1) {
            nm <- match("double", tolower(realinfo))
            if (!is.na(nm)) 
              realinfo <- realinfo[nm]
          }
          colspecs[isreal] <- realinfo[1]
        }
        else {
          realinfo <- sqlTypeInfo(channel, "float")[, 1]
          if (length(realinfo) > 0) {
            if (length(realinfo) > 1) {
              nm <- match("float", tolower(realinfo))
              if (!is.na(nm)) 
                realinfo <- realinfo[nm]
            }
            colspecs[isreal] <- realinfo[1]
          }
        }
      }
      if (any(isint)) {
        intinfo <- sqlTypeInfo(channel, "integer")[, 1]
        if (length(intinfo) > 0) {
          if (length(intinfo) > 1) {
            nm <- match("integer", tolower(intinfo))
            if (!is.na(nm)) 
              intinfo <- intinfo[nm]
          }
          colspecs[isint] <- intinfo[1]
        }
      }
    }
  }
  names(colspecs) <- names(dat)
  if (!missing(varTypes)) {
    if (!length(nm <- names(varTypes))) 
      warning("argument 'varTypes' has no names and will be ignored")
    OK <- names(colspecs) %in% nm
    colspecs[OK] <- varTypes[names(colspecs)[OK]]
    notOK <- !(nm %in% names(colspecs))
    if (any(notOK)) 
      warning("column(s) ", paste(nm[notOK], collapse = ", "), " 'dat' are not in the names of 'varTypes'")
  }
  query <- sqltablecreate(channel, tablename, colspecs = colspecs, keys = keys)
  if (verbose) 
    cat("Query: ", query, "\n", sep = "")
  res <- sqlQuery(channel, query, errors = FALSE)
  if (is.numeric(res) && res == -1) 
    stop(paste(odbcGetErrMsg(channel), collapse = "\n"))
  if (sqlwrite(channel, tablename, dat, verbose = verbose, fast = fast, test = test, nastring = nastring) < 0) {
    err <- odbcGetErrMsg(channel)
    msg <- paste(err, collapse = "\n")
    if ("missing column name" %in% err) 
      msg <- paste(msg, "Check case conversion parameter in odbcConnect", sep = "\n")
    stop(msg)
  }
  invisible(1)
}, function (channel, dat, tablename = NULL, append = FALSE, rownames = TRUE, colnames = FALSE, verbose = FALSE, safer = TRUE, addPK = FALSE, typeInfo, varTypes, fast = TRUE, test = FALSE, nastring = NULL) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (missing(dat)) 
    stop("missing parameter")
  if (!is.data.frame(dat)) 
    stop("should be a data frame")
  if (is.null(tablename)) 
    tablename <- if (length(substitute(dat)) == 1) 
      as.character(substitute(dat))
  else as.character(substitute(dat)[[2]])
  if (length(tablename) != 1) 
    stop(sQuote(tablename), " should be a name")
  switch(attr(channel, "case"), nochange = {
  }, toupper = {
    tablename <- toupper(tablename)
    colnames(dat) <- toupper(colnames(dat))
  }, tolower = {
    tablename <- tolower(tablename)
    colnames(dat) <- tolower(colnames(dat))
  })
  keys <- -1
  if (is.logical(rownames) && rownames) 
    rownames <- "rownames"
  if (is.character(rownames)) {
    dat <- cbind(row.names(dat), dat)
    names(dat)[1] <- rownames
    if (addPK) {
      keys <- vector("list", 4)
      keys[[4]] <- rownames
    }
  }
  if (is.logical(colnames) && colnames) {
    dat <- as.data.frame(rbind(colnames(dat), as.matrix(dat)))
  }
  dbname <- odbcTableExists(channel, tablename, abort = FALSE)
  if (length(dbname)) {
    if (!append) {
      if (safer) 
        stop("table ", sQuote(tablename), " already exists")
      query <- paste("DELETE FROM", dbname)
      if (verbose) 
        cat("Query: ", query, "\n", sep = "")
      res <- sqlQuery(channel, query, errors = FALSE)
      if (is.numeric(res) && res == -1) 
        stop(paste(odbcGetErrMsg(channel), collapse = "\n"))
    }
    if (sqlwrite(channel, tablename, dat, verbose = verbose, fast = fast, test = test, nastring = nastring) == -1) {
      query <- paste("DROP TABLE", dbname)
      if (verbose) {
        cat("sqlwrite returned ", odbcGetErrMsg(channel), "\n", sep = "\n")
        cat("Query: ", query, "\n", sep = "")
      }
      if (safer) 
        stop("unable to append to table ", sQuote(tablename))
      res <- sqlQuery(channel, query, errors = FALSE)
      if (is.numeric(res) && res == -1) 
        stop(paste(odbcGetErrMsg(channel), collapse = "\n"))
    }
    else {
      return(invisible(1))
    }
  }
  types <- sapply(dat, typeof)
  facs <- sapply(dat, is.factor)
  isreal <- (types == "double")
  isint <- (types == "integer") & !facs
  islogi <- (types == "logical")
  colspecs <- rep("varchar(255)", length(dat))
  if (!missing(typeInfo) || !is.null(typeInfo <- typesR2DBMS[[odbcGetInfo(channel)[1]]])) {
    colspecs <- rep(typeInfo$character[1], length(dat))
    colspecs[isreal] <- typeInfo$double[1]
    colspecs[isint] <- typeInfo$integer[1]
    colspecs[islogi] <- typeInfo$logical[1]
  }
  else {
    typeinfo <- sqlTypeInfo(channel, "all", errors = FALSE)
    if (is.data.frame(typeinfo)) {
      if (any(isreal)) {
        realinfo <- sqlTypeInfo(channel, "double")[, 1]
        if (length(realinfo) > 0) {
          if (length(realinfo) > 1) {
            nm <- match("double", tolower(realinfo))
            if (!is.na(nm)) 
              realinfo <- realinfo[nm]
          }
          colspecs[isreal] <- realinfo[1]
        }
        else {
          realinfo <- sqlTypeInfo(channel, "float")[, 1]
          if (length(realinfo) > 0) {
            if (length(realinfo) > 1) {
              nm <- match("float", tolower(realinfo))
              if (!is.na(nm)) 
                realinfo <- realinfo[nm]
            }
            colspecs[isreal] <- realinfo[1]
          }
        }
      }
      if (any(isint)) {
        intinfo <- sqlTypeInfo(channel, "integer")[, 1]
        if (length(intinfo) > 0) {
          if (length(intinfo) > 1) {
            nm <- match("integer", tolower(intinfo))
            if (!is.na(nm)) 
              intinfo <- intinfo[nm]
          }
          colspecs[isint] <- intinfo[1]
        }
      }
    }
  }
  names(colspecs) <- names(dat)
  if (!missing(varTypes)) {
    if (!length(nm <- names(varTypes))) 
      warning("argument 'varTypes' has no names and will be ignored")
    OK <- names(colspecs) %in% nm
    colspecs[OK] <- varTypes[names(colspecs)[OK]]
    notOK <- !(nm %in% names(colspecs))
    if (any(notOK)) 
      warning("column(s) ", paste(nm[notOK], collapse = ", "), " 'dat' are not in the names of 'varTypes'")
  }
  query <- sqltablecreate(channel, tablename, colspecs = colspecs, keys = keys)
  if (verbose) 
    cat("Query: ", query, "\n", sep = "")
  res <- sqlQuery(channel, query, errors = FALSE)
  if (is.numeric(res) && res == -1) 
    stop(paste(odbcGetErrMsg(channel), collapse = "\n"))
  if (sqlwrite(channel, tablename, dat, verbose = verbose, fast = fast, test = test, nastring = nastring) < 0) {
    err <- odbcGetErrMsg(channel)
    msg <- paste(err, collapse = "\n")
    if ("missing column name" %in% err) 
      msg <- paste(msg, "Check case conversion parameter in odbcConnect", sep = "\n")
    stop(msg)
  }
  invisible(1)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlTables
list(`package:RODBC` = function (channel, errors = FALSE, as.is = TRUE, catalog = NULL, schema = NULL, tableName = NULL, tableType = NULL, literal = FALSE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  stat <- odbcTables(channel, catalog = catalog, schema = schema, tableName = tableName, tableType = tableType, literal = literal)
  if (stat < 0) {
    if (errors) {
      if (stat == -2) 
        stop("invalid channel")
      else return(odbcGetErrMsg(channel))
    }
    else return(invisible(-1))
  }
  else return(sqlGetResults(channel, as.is = as.is))
}, function (channel, errors = FALSE, as.is = TRUE, catalog = NULL, schema = NULL, tableName = NULL, tableType = NULL, literal = FALSE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  stat <- odbcTables(channel, catalog = catalog, schema = schema, tableName = tableName, tableType = tableType, literal = literal)
  if (stat < 0) {
    if (errors) {
      if (stat == -2) 
        stop("invalid channel")
      else return(odbcGetErrMsg(channel))
    }
    else return(invisible(-1))
  }
  else return(sqlGetResults(channel, as.is = as.is))
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlTypeInfo
list(`package:RODBC` = function (channel, type = "all", errors = TRUE, as.is = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  type <- match(type, c("all", "char", "varchar", "real", "double", "integer", "smallint", "timestamp", "float", "bit", "wchar", "wvarchar", "date", "time", "binary", "varbinary", "longvarbinary", "blob"), nomatch = 1) - 1
  stat <- .Call(C_RODBCTypeInfo, attr(channel, "handle_ptr"), as.integer(type), PACKAGE = "RODBC")
  if (!stat) {
    if (errors) 
      return(odbcGetErrMsg(channel))
    else return(invisible(-1))
  }
  else {
    return(sqlGetResults(channel, as.is = as.is, errors = errors, believeNRows = FALSE))
  }
}, function (channel, type = "all", errors = TRUE, as.is = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  type <- match(type, c("all", "char", "varchar", "real", "double", "integer", "smallint", "timestamp", "float", "bit", "wchar", "wvarchar", "date", "time", "binary", "varbinary", "longvarbinary", "blob"), nomatch = 1) - 1
  stat <- .Call(C_RODBCTypeInfo, attr(channel, "handle_ptr"), as.integer(type), PACKAGE = "RODBC")
  if (!stat) {
    if (errors) 
      return(odbcGetErrMsg(channel))
    else return(invisible(-1))
  }
  else {
    return(sqlGetResults(channel, as.is = as.is, errors = errors, believeNRows = FALSE))
  }
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)
sqlUpdate
list(`package:RODBC` = function (channel, dat, tablename = NULL, index = NULL, verbose = FALSE, test = FALSE, nastring = NULL, fast = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (missing(dat)) 
    stop("missing parameter")
  if (!is.data.frame(dat)) 
    stop("should be a data frame or matrix")
  if (is.null(tablename)) 
    tablename <- if (length(substitute(dat)) == 1) 
      as.character(substitute(dat))
  else as.character(substitute(dat)[[2]])
  if (length(tablename) != 1) 
    stop(sQuote(tablename), " should be a name")
  dbname <- odbcTableExists(channel, tablename)
  cnames <- colnames(dat)
  cnames <- mangleColNames(cnames)
  cnames <- switch(attr(channel, "case"), nochange = cnames, toupper = toupper(cnames), tolower = tolower(cnames))
  cdata <- sqlColumns(channel, tablename)
  coldata <- cdata[c(4, 5, 7, 9)]
  if (is.character(index)) {
    intable <- index %in% coldata[, 1]
    if (any(!intable)) 
      stop("index column(s) ", paste(index[!intable], collapse = " "), " not in database table")
    intable <- index %in% cnames
    if (any(!intable)) 
      stop("index column(s) ", paste(index[!intable], collapse = " "), " not in data frame")
    indexcols <- index
  }
  else {
    haveKey <- FALSE
    indexcols <- sqlPrimaryKeys(channel, tablename)
    if (!(is.numeric(indexcols) || nrow(indexcols) == 0)) {
      index <- as.character(indexcols[, 4])
      intable <- index %in% cnames
      if (any(intable)) {
        indexcols <- index[intable][1]
        haveKey <- TRUE
      }
    }
    if (!haveKey) {
      indexcols <- sqlColumns(channel, tablename, special = TRUE)
      if (!(is.numeric(indexcols) || nrow(indexcols) == 0)) {
        indexcols <- indexcols[c(2, 3, 5, 7)]
        indexflags <- indexcols[, 1] %in% cnames
        if (all(indexflags)) {
          incoldata <- indexcols[, 1] %in% coldata[, 1]
          if (any(!incoldata)) 
            coldata <- rbind(coldata, indexcols[!incoldata])
          indexcols <- as.character(indexcols[, 1])
          haveKey <- TRUE
        }
      }
    }
    if (!haveKey) {
      m <- match("rownames", tolower(coldata[, 1]))
      if (is.na(m)) 
        stop("cannot update ", sQuote(tablename), " without unique column")
      indexcols <- coldata[m, 1]
      dat <- cbind(row.names(dat), dat)
      names(dat)[1] <- indexcols
      cnames <- c(indexcols, cnames)
    }
  }
  intable <- cnames %in% coldata[, 1]
  if (any(!intable)) 
    stop("data frame column(s) ", paste(cnames[!intable], collapse = " "), " not in database table")
  cn1 <- cnames[!cnames %in% indexcols]
  cn2 <- quoteColNames(channel, cn1)
  if (fast) {
    query <- paste("UPDATE", dbname, "SET")
    query <- paste(query, paste(paste(cn2, "=?", sep = ""), collapse = ", "))
    paramnames <- c(cn1, indexcols)
    if (length(indexcols)) {
      ind <- quoteColNames(channel, indexcols)
      query <- paste(query, "WHERE", paste(paste(ind, "=?", sep = ""), collapse = " AND "))
    }
    row.names(coldata) <- coldata[, 1]
    paramdata <- coldata[paramnames, ]
    if (test | verbose) 
      cat("Query: ", query, "\n", sep = "")
    stat <- odbcUpdate(channel, query, dat, paramdata, test = test, verbose = verbose, nastring = nastring)
  }
  else {
    data <- as.matrix(dat)
    if (nchar(enc <- attr(channel, "encoding")) && is.character(data)) 
      data[] <- iconv(data, to = enc)
    colnames(data) <- cnames
    cdata <- sub("\\([[:digit:]]*\\)", "", sqlColumns(channel, tablename)[, "TYPE_NAME"])
    tdata <- sqlTypeInfo(channel)
    tdata <- as.matrix(tdata[match(cdata, tdata[, 1]), c(4, 5)])
    for (cn in seq_along(cdata)) {
      td <- as.vector(tdata[cn, ])
      if (is.na(td[1])) 
        next
      if (identical(td, rep("'", 2))) 
        data[, cn] <- gsub("'", "''", data[, cn])
      data[, cn] <- paste(td[1], data[, cn], td[2], sep = "")
    }
    data[is.na(dat)] <- if (is.null(nastring)) 
      "NULL"
    else nastring
    for (i in 1:nrow(data)) {
      query <- paste("UPDATE", dbname, "SET")
      query <- paste(query, paste(paste(cn2, "=", data[i, cn1], sep = ""), collapse = ", "))
      if (length(indexcols)) {
        ind <- quoteColNames(channel, indexcols)
        query <- paste(query, "WHERE", paste(paste(ind, "=", data[i, indexcols], sep = ""), collapse = " AND "))
      }
      if (verbose) 
        cat("Query: ", query, "\n", sep = "")
      if ((stat <- odbcQuery(channel, query)) < 0) 
        break
    }
  }
  if (stat < 0) 
    stop(paste(odbcGetErrMsg(channel), sep = "\n"))
  invisible(stat)
}, function (channel, dat, tablename = NULL, index = NULL, verbose = FALSE, test = FALSE, nastring = NULL, fast = TRUE) 
{
  if (!odbcValidChannel(channel)) 
    stop("first argument is not an open RODBC channel")
  if (missing(dat)) 
    stop("missing parameter")
  if (!is.data.frame(dat)) 
    stop("should be a data frame or matrix")
  if (is.null(tablename)) 
    tablename <- if (length(substitute(dat)) == 1) 
      as.character(substitute(dat))
  else as.character(substitute(dat)[[2]])
  if (length(tablename) != 1) 
    stop(sQuote(tablename), " should be a name")
  dbname <- odbcTableExists(channel, tablename)
  cnames <- colnames(dat)
  cnames <- mangleColNames(cnames)
  cnames <- switch(attr(channel, "case"), nochange = cnames, toupper = toupper(cnames), tolower = tolower(cnames))
  cdata <- sqlColumns(channel, tablename)
  coldata <- cdata[c(4, 5, 7, 9)]
  if (is.character(index)) {
    intable <- index %in% coldata[, 1]
    if (any(!intable)) 
      stop("index column(s) ", paste(index[!intable], collapse = " "), " not in database table")
    intable <- index %in% cnames
    if (any(!intable)) 
      stop("index column(s) ", paste(index[!intable], collapse = " "), " not in data frame")
    indexcols <- index
  }
  else {
    haveKey <- FALSE
    indexcols <- sqlPrimaryKeys(channel, tablename)
    if (!(is.numeric(indexcols) || nrow(indexcols) == 0)) {
      index <- as.character(indexcols[, 4])
      intable <- index %in% cnames
      if (any(intable)) {
        indexcols <- index[intable][1]
        haveKey <- TRUE
      }
    }
    if (!haveKey) {
      indexcols <- sqlColumns(channel, tablename, special = TRUE)
      if (!(is.numeric(indexcols) || nrow(indexcols) == 0)) {
        indexcols <- indexcols[c(2, 3, 5, 7)]
        indexflags <- indexcols[, 1] %in% cnames
        if (all(indexflags)) {
          incoldata <- indexcols[, 1] %in% coldata[, 1]
          if (any(!incoldata)) 
            coldata <- rbind(coldata, indexcols[!incoldata])
          indexcols <- as.character(indexcols[, 1])
          haveKey <- TRUE
        }
      }
    }
    if (!haveKey) {
      m <- match("rownames", tolower(coldata[, 1]))
      if (is.na(m)) 
        stop("cannot update ", sQuote(tablename), " without unique column")
      indexcols <- coldata[m, 1]
      dat <- cbind(row.names(dat), dat)
      names(dat)[1] <- indexcols
      cnames <- c(indexcols, cnames)
    }
  }
  intable <- cnames %in% coldata[, 1]
  if (any(!intable)) 
    stop("data frame column(s) ", paste(cnames[!intable], collapse = " "), " not in database table")
  cn1 <- cnames[!cnames %in% indexcols]
  cn2 <- quoteColNames(channel, cn1)
  if (fast) {
    query <- paste("UPDATE", dbname, "SET")
    query <- paste(query, paste(paste(cn2, "=?", sep = ""), collapse = ", "))
    paramnames <- c(cn1, indexcols)
    if (length(indexcols)) {
      ind <- quoteColNames(channel, indexcols)
      query <- paste(query, "WHERE", paste(paste(ind, "=?", sep = ""), collapse = " AND "))
    }
    row.names(coldata) <- coldata[, 1]
    paramdata <- coldata[paramnames, ]
    if (test | verbose) 
      cat("Query: ", query, "\n", sep = "")
    stat <- odbcUpdate(channel, query, dat, paramdata, test = test, verbose = verbose, nastring = nastring)
  }
  else {
    data <- as.matrix(dat)
    if (nchar(enc <- attr(channel, "encoding")) && is.character(data)) 
      data[] <- iconv(data, to = enc)
    colnames(data) <- cnames
    cdata <- sub("\\([[:digit:]]*\\)", "", sqlColumns(channel, tablename)[, "TYPE_NAME"])
    tdata <- sqlTypeInfo(channel)
    tdata <- as.matrix(tdata[match(cdata, tdata[, 1]), c(4, 5)])
    for (cn in seq_along(cdata)) {
      td <- as.vector(tdata[cn, ])
      if (is.na(td[1])) 
        next
      if (identical(td, rep("'", 2))) 
        data[, cn] <- gsub("'", "''", data[, cn])
      data[, cn] <- paste(td[1], data[, cn], td[2], sep = "")
    }
    data[is.na(dat)] <- if (is.null(nastring)) 
      "NULL"
    else nastring
    for (i in 1:nrow(data)) {
      query <- paste("UPDATE", dbname, "SET")
      query <- paste(query, paste(paste(cn2, "=", data[i, cn1], sep = ""), collapse = ", "))
      if (length(indexcols)) {
        ind <- quoteColNames(channel, indexcols)
        query <- paste(query, "WHERE", paste(paste(ind, "=", data[i, indexcols], sep = ""), collapse = " AND "))
      }
      if (verbose) 
        cat("Query: ", query, "\n", sep = "")
      if ((stat <- odbcQuery(channel, query)) < 0) 
        break
    }
  }
  if (stat < 0) 
    stop(paste(odbcGetErrMsg(channel), sep = "\n"))
  invisible(stat)
})
c("package:RODBC", "namespace:RODBC")
c(TRUE, FALSE)
c(FALSE, TRUE)

####################################################################################################
library(shiny)
..stacktraceoff..
list(`package:shiny` = function (expr) 
  expr, function (expr) 
    expr)
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
..stacktraceon..
list(`package:shiny` = function (expr) 
  expr, function (expr) 
    expr)
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
.Depends
list(`package:shiny` = "methods", `package:mlr` = "ParamHelpers")
c("package:shiny", "package:mlr")
c(TRUE, TRUE)
c(FALSE, FALSE)
a
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("a", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("a", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
absolutePanel
list(`package:shiny` = function (..., top = NULL, left = NULL, right = NULL, bottom = NULL, width = NULL, height = NULL, draggable = FALSE, fixed = FALSE, cursor = c("auto", "move", "default", "inherit")) 
{
  cssProps <- list(top = top, left = left, right = right, bottom = bottom, width = width, height = height)
  cssProps <- cssProps[!sapply(cssProps, is.null)]
  cssProps <- sapply(cssProps, validateCssUnit)
  cssProps[["position"]] <- ifelse(fixed, "fixed", "absolute")
  cssProps[["cursor"]] <- match.arg(cursor)
  if (identical(cssProps[["cursor"]], "auto")) 
    cssProps[["cursor"]] <- ifelse(draggable, "move", "inherit")
  style <- paste(paste(names(cssProps), cssProps, sep = ":", collapse = ";"), ";", sep = "")
  divTag <- tags$div(style = style, ...)
  if (isTRUE(draggable)) {
    divTag <- tagAppendAttributes(divTag, class = "draggable")
    return(tagList(divTag, jqueryuiDependency(), tags$script("$(\".draggable\").draggable();")))
  }
  else {
    return(divTag)
  }
}, function (..., top = NULL, left = NULL, right = NULL, bottom = NULL, width = NULL, height = NULL, draggable = FALSE, fixed = FALSE, cursor = c("auto", "move", "default", "inherit")) 
{
  cssProps <- list(top = top, left = left, right = right, bottom = bottom, width = width, height = height)
  cssProps <- cssProps[!sapply(cssProps, is.null)]
  cssProps <- sapply(cssProps, validateCssUnit)
  cssProps[["position"]] <- ifelse(fixed, "fixed", "absolute")
  cssProps[["cursor"]] <- match.arg(cursor)
  if (identical(cssProps[["cursor"]], "auto")) 
    cssProps[["cursor"]] <- ifelse(draggable, "move", "inherit")
  style <- paste(paste(names(cssProps), cssProps, sep = ":", collapse = ";"), ";", sep = "")
  divTag <- tags$div(style = style, ...)
  if (isTRUE(draggable)) {
    divTag <- tagAppendAttributes(divTag, class = "draggable")
    return(tagList(divTag, jqueryuiDependency(), tags$script("$(\".draggable\").draggable();")))
  }
  else {
    return(divTag)
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
actionButton
list(`package:shiny` = function (inputId, label, icon = NULL, width = NULL, ...) 
{
  value <- restoreInput(id = inputId, default = NULL)
  tags$button(id = inputId, style = css(width = validateCssUnit(width)), type = "button", class = "btn btn-default action-button", `data-val` = value, list(validateIcon(icon), label), ...)
}, function (inputId, label, icon = NULL, width = NULL, ...) 
{
  value <- restoreInput(id = inputId, default = NULL)
  tags$button(id = inputId, style = css(width = validateCssUnit(width)), type = "button", class = "btn btn-default action-button", `data-val` = value, list(validateIcon(icon), label), ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
actionLink
list(`package:shiny` = function (inputId, label, icon = NULL, ...) 
{
  value <- restoreInput(id = inputId, default = NULL)
  tags$a(id = inputId, href = "#", class = "action-button", `data-val` = value, list(validateIcon(icon), label), ...)
}, function (inputId, label, icon = NULL, ...) 
{
  value <- restoreInput(id = inputId, default = NULL)
  tags$a(id = inputId, href = "#", class = "action-button", `data-val` = value, list(validateIcon(icon), label), ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
addResourcePath
list(`package:shiny` = function (prefix, directoryPath) 
{
  if (length(prefix) != 1) 
    stop("prefix must be of length 1")
  if (grepl("^\\.+$", prefix)) 
    stop("prefix can't be composed of dots only")
  if (!grepl("[a-z0-9\\-_.]+$", prefix, ignore.case = TRUE, perl = TRUE)) {
    stop("addResourcePath called with invalid prefix; please see documentation")
  }
  if (prefix %in% c("shared")) {
    stop("addResourcePath called with the reserved prefix '", prefix, "'; ", "please use a different prefix")
  }
  normalizedPath <- tryCatch(normalizePath(directoryPath, mustWork = TRUE), error = function(e) {
    stop("Couldn't normalize path in `addResourcePath`, with arguments: ", "`prefix` = '", prefix, "'; `directoryPath` = '", directoryPath, "'")
  })
  if (!is.null(getShinyOption("server", default = NULL))) {
    getShinyOption("server")$setStaticPath(.list = stats::setNames(normalizedPath, prefix))
  }
  .globals$resourcePaths[[prefix]] <- staticPath(normalizedPath)
  .globals$resources[[prefix]] <- list(directoryPath = normalizedPath, func = staticHandler(normalizedPath))
}, function (prefix, directoryPath) 
{
  if (length(prefix) != 1) 
    stop("prefix must be of length 1")
  if (grepl("^\\.+$", prefix)) 
    stop("prefix can't be composed of dots only")
  if (!grepl("[a-z0-9\\-_.]+$", prefix, ignore.case = TRUE, perl = TRUE)) {
    stop("addResourcePath called with invalid prefix; please see documentation")
  }
  if (prefix %in% c("shared")) {
    stop("addResourcePath called with the reserved prefix '", prefix, "'; ", "please use a different prefix")
  }
  normalizedPath <- tryCatch(normalizePath(directoryPath, mustWork = TRUE), error = function(e) {
    stop("Couldn't normalize path in `addResourcePath`, with arguments: ", "`prefix` = '", prefix, "'; `directoryPath` = '", directoryPath, "'")
  })
  if (!is.null(getShinyOption("server", default = NULL))) {
    getShinyOption("server")$setStaticPath(.list = stats::setNames(normalizedPath, prefix))
  }
  .globals$resourcePaths[[prefix]] <- staticPath(normalizedPath)
  .globals$resources[[prefix]] <- list(directoryPath = normalizedPath, func = staticHandler(normalizedPath))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
animationOptions
list(`package:shiny` = function (interval = 1000, loop = FALSE, playButton = NULL, pauseButton = NULL) 
{
  list(interval = interval, loop = loop, playButton = playButton, pauseButton = pauseButton)
}, function (interval = 1000, loop = FALSE, playButton = NULL, pauseButton = NULL) 
{
  list(interval = interval, loop = loop, playButton = playButton, pauseButton = pauseButton)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
appendTab
list(`package:shiny` = function (inputId, tab, select = FALSE, menuName = NULL, session = getDefaultReactiveDomain()) 
{
  bslib::nav_append(inputId, tab, menu_title = menuName, select = select, session = session)
}, function (inputId, tab, select = FALSE, menuName = NULL, session = getDefaultReactiveDomain()) 
{
  bslib::nav_append(inputId, tab, menu_title = menuName, select = select, session = session)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
as.shiny.appobj
list(`package:shiny` = function (x) 
{
  UseMethod("as.shiny.appobj", x)
}, function (x) 
{
  UseMethod("as.shiny.appobj", x)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
basicPage
list(`package:shiny` = function (...) 
{
  bootstrapPage(div(class = "container-fluid", list(...)))
}, function (...) 
{
  bootstrapPage(div(class = "container-fluid", list(...)))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
bindCache
list(`package:shiny` = function (x, ..., cache = "app") 
{
  force(cache)
  UseMethod("bindCache")
}, function (x, ..., cache = "app") 
{
  force(cache)
  UseMethod("bindCache")
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
bindEvent
list(`package:shiny` = function (x, ..., ignoreNULL = TRUE, ignoreInit = FALSE, once = FALSE, label = NULL) 
{
  check_dots_unnamed()
  force(ignoreNULL)
  force(ignoreInit)
  force(once)
  UseMethod("bindEvent")
}, function (x, ..., ignoreNULL = TRUE, ignoreInit = FALSE, once = FALSE, label = NULL) 
{
  check_dots_unnamed()
  force(ignoreNULL)
  force(ignoreInit)
  force(once)
  UseMethod("bindEvent")
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
bookmarkButton
list(`package:shiny` = function (label = "Bookmark...", icon = shiny::icon("link", lib = "glyphicon"), title = "Bookmark this application's state and get a URL for sharing.", ..., id = "._bookmark_") 
{
  actionButton(id, label, icon, title = title, ...)
}, function (label = "Bookmark...", icon = shiny::icon("link", lib = "glyphicon"), title = "Bookmark this application's state and get a URL for sharing.", ..., id = "._bookmark_") 
{
  actionButton(id, label, icon, title = title, ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
bootstrapLib
list(`package:shiny` = function (theme = NULL) 
{
  tagFunction(function() {
    if (isRunning()) {
      setCurrentTheme(theme)
    }
    if (!is_bs_theme(theme)) {
      return(bootstrapDependency(theme))
    }
    if (isRunning()) {
      registerThemeDependency(bs_theme_deps)
    }
    else {
    }
    bslib::bs_theme_dependencies(theme)
  })
}, function (theme = NULL) 
{
  tagFunction(function() {
    if (isRunning()) {
      setCurrentTheme(theme)
    }
    if (!is_bs_theme(theme)) {
      return(bootstrapDependency(theme))
    }
    if (isRunning()) {
      registerThemeDependency(bs_theme_deps)
    }
    else {
    }
    bslib::bs_theme_dependencies(theme)
  })
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
bootstrapPage
list(`package:shiny` = function (..., title = NULL, theme = NULL, lang = NULL) 
{
  args <- list(jqueryDependency(), if (!is.null(title)) tags$head(tags$title(title)), if (is.character(theme)) {
    if (length(theme) > 1) stop("`theme` must point to a single CSS file, not multiple files.")
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = theme))
  }, list2(...))
  if (is_bs_theme(theme)) {
    args <- c(bootstrapLib(theme), args)
    ui <- do.call(tagList, args)
  }
  else {
    ui <- do.call(tagList, args)
    ui <- attachDependencies(ui, bootstrapLib())
  }
  setLang(ui, lang)
}, function (..., title = NULL, theme = NULL, lang = NULL) 
{
  args <- list(jqueryDependency(), if (!is.null(title)) tags$head(tags$title(title)), if (is.character(theme)) {
    if (length(theme) > 1) stop("`theme` must point to a single CSS file, not multiple files.")
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = theme))
  }, list2(...))
  if (is_bs_theme(theme)) {
    args <- c(bootstrapLib(theme), args)
    ui <- do.call(tagList, args)
  }
  else {
    ui <- do.call(tagList, args)
    ui <- attachDependencies(ui, bootstrapLib())
  }
  setLang(ui, lang)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
br
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("br", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("br", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
browserViewer
list(`package:shiny` = function (browser = getOption("browser")) 
{
  function(url) {
    utils::browseURL(url, browser = browser)
  }
}, function (browser = getOption("browser")) 
{
  function(url) {
    utils::browseURL(url, browser = browser)
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
brushedPoints
list(`package:shiny` = function (df, brush, xvar = NULL, yvar = NULL, panelvar1 = NULL, panelvar2 = NULL, allRows = FALSE) 
{
  if (is.null(brush)) {
    if (allRows) 
      df$selected_ <- FALSE
    else df <- df[0, , drop = FALSE]
    return(df)
  }
  if (is.null(brush$xmin)) {
    stop("brushedPoints requires a brush object with xmin, xmax, ymin, and ymax.")
  }
  use_x <- grepl("x", brush$direction)
  use_y <- grepl("y", brush$direction)
  if (is_na(xvar)) {
    xvar <- NULL
    warning("xvar should be NULL, not NA.")
  }
  if (is_na(yvar)) {
    yvar <- NULL
    warning("yvar should be NULL, not NA.")
  }
  if (is_na(panelvar1)) {
    panelvar1 <- NULL
    warning("panelvar1 should be NULL, not NA.")
  }
  if (is_na(panelvar2)) {
    panelvar2 <- NULL
    warning("panelvar2 should be NULL, not NA.")
  }
  xvar <- xvar %||% brush$mapping$x
  yvar <- yvar %||% brush$mapping$y
  panelvar1 <- panelvar1 %||% brush$mapping$panelvar1
  panelvar2 <- panelvar2 %||% brush$mapping$panelvar2
  keep_rows <- rep(TRUE, nrow(df))
  if (use_x) {
    if (is.null(xvar)) 
      stop("brushedPoints: not able to automatically infer `xvar` from brush")
    if (!(xvar %in% names(df))) 
      stop("brushedPoints: `xvar` ('", xvar, "')  not in names of input")
    keep_rows <- keep_rows & within_brush(df[[xvar]], brush, "x")
  }
  if (use_y) {
    if (is.null(yvar)) 
      stop("brushedPoints: not able to automatically infer `yvar` from brush")
    if (!(yvar %in% names(df))) 
      stop("brushedPoints: `yvar` ('", yvar, "') not in names of input")
    keep_rows <- keep_rows & within_brush(df[[yvar]], brush, "y")
  }
  if (!is.null(panelvar1)) 
    keep_rows <- keep_rows & panelMatch(brush$panelvar1, df[[panelvar1]])
  if (!is.null(panelvar2)) 
    keep_rows <- keep_rows & panelMatch(brush$panelvar2, df[[panelvar2]])
  if (allRows) {
    df$selected_ <- keep_rows
    df
  }
  else {
    df[keep_rows, , drop = FALSE]
  }
}, function (df, brush, xvar = NULL, yvar = NULL, panelvar1 = NULL, panelvar2 = NULL, allRows = FALSE) 
{
  if (is.null(brush)) {
    if (allRows) 
      df$selected_ <- FALSE
    else df <- df[0, , drop = FALSE]
    return(df)
  }
  if (is.null(brush$xmin)) {
    stop("brushedPoints requires a brush object with xmin, xmax, ymin, and ymax.")
  }
  use_x <- grepl("x", brush$direction)
  use_y <- grepl("y", brush$direction)
  if (is_na(xvar)) {
    xvar <- NULL
    warning("xvar should be NULL, not NA.")
  }
  if (is_na(yvar)) {
    yvar <- NULL
    warning("yvar should be NULL, not NA.")
  }
  if (is_na(panelvar1)) {
    panelvar1 <- NULL
    warning("panelvar1 should be NULL, not NA.")
  }
  if (is_na(panelvar2)) {
    panelvar2 <- NULL
    warning("panelvar2 should be NULL, not NA.")
  }
  xvar <- xvar %||% brush$mapping$x
  yvar <- yvar %||% brush$mapping$y
  panelvar1 <- panelvar1 %||% brush$mapping$panelvar1
  panelvar2 <- panelvar2 %||% brush$mapping$panelvar2
  keep_rows <- rep(TRUE, nrow(df))
  if (use_x) {
    if (is.null(xvar)) 
      stop("brushedPoints: not able to automatically infer `xvar` from brush")
    if (!(xvar %in% names(df))) 
      stop("brushedPoints: `xvar` ('", xvar, "')  not in names of input")
    keep_rows <- keep_rows & within_brush(df[[xvar]], brush, "x")
  }
  if (use_y) {
    if (is.null(yvar)) 
      stop("brushedPoints: not able to automatically infer `yvar` from brush")
    if (!(yvar %in% names(df))) 
      stop("brushedPoints: `yvar` ('", yvar, "') not in names of input")
    keep_rows <- keep_rows & within_brush(df[[yvar]], brush, "y")
  }
  if (!is.null(panelvar1)) 
    keep_rows <- keep_rows & panelMatch(brush$panelvar1, df[[panelvar1]])
  if (!is.null(panelvar2)) 
    keep_rows <- keep_rows & panelMatch(brush$panelvar2, df[[panelvar2]])
  if (allRows) {
    df$selected_ <- keep_rows
    df
  }
  else {
    df[keep_rows, , drop = FALSE]
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
brushOpts
list(`package:shiny` = function (id, fill = "#9cf", stroke = "#036", opacity = 0.25, delay = 300, delayType = c("debounce", "throttle"), clip = TRUE, direction = c("xy", "x", "y"), resetOnNew = FALSE) 
{
  if (is.null(id)) 
    stop("id must not be NULL")
  if (identical(fill, "auto")) {
    fill <- getThematicOption("accent", "auto")
  }
  if (identical(stroke, "auto")) {
    stroke <- getThematicOption("fg", "auto")
  }
  list(id = id, fill = fill, stroke = stroke, opacity = opacity, delay = delay, delayType = match.arg(delayType), clip = clip, direction = match.arg(direction), resetOnNew = resetOnNew)
}, function (id, fill = "#9cf", stroke = "#036", opacity = 0.25, delay = 300, delayType = c("debounce", "throttle"), clip = TRUE, direction = c("xy", "x", "y"), resetOnNew = FALSE) 
{
  if (is.null(id)) 
    stop("id must not be NULL")
  if (identical(fill, "auto")) {
    fill <- getThematicOption("accent", "auto")
  }
  if (identical(stroke, "auto")) {
    stroke <- getThematicOption("fg", "auto")
  }
  list(id = id, fill = fill, stroke = stroke, opacity = opacity, delay = delay, delayType = match.arg(delayType), clip = clip, direction = match.arg(direction), resetOnNew = resetOnNew)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
callModule
list(`package:shiny` = function (module, id, ..., session = getDefaultReactiveDomain()) 
{
  if (!inherits(session, c("ShinySession", "session_proxy", "MockShinySession"))) {
    stop("session must be a ShinySession or session_proxy object.")
  }
  childScope <- session$makeScope(id)
  withReactiveDomain(childScope, {
    if (!is.function(module)) {
      stop("module argument must be a function")
    }
    module(childScope$input, childScope$output, childScope, ...)
  })
}, function (module, id, ..., session = getDefaultReactiveDomain()) 
{
  if (!inherits(session, c("ShinySession", "session_proxy", "MockShinySession"))) {
    stop("session must be a ShinySession or session_proxy object.")
  }
  childScope <- session$makeScope(id)
  withReactiveDomain(childScope, {
    if (!is.function(module)) {
      stop("module argument must be a function")
    }
    module(childScope$input, childScope$output, childScope, ...)
  })
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
captureStackTraces
list(`package:shiny` = function (expr) 
{
  promises::with_promise_domain(createStackTracePromiseDomain(), expr)
}, function (expr) 
{
  promises::with_promise_domain(createStackTracePromiseDomain(), expr)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
checkboxGroupInput
list(`package:shiny` = function (inputId, label, choices = NULL, selected = NULL, inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL) 
{
  if (is.null(choices) && is.null(choiceNames) && is.null(choiceValues)) {
    choices <- character(0)
  }
  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues)
  selected <- restoreInput(id = inputId, default = selected)
  if (!is.null(selected)) 
    selected <- as.character(selected)
  options <- generateOptions(inputId, selected, inline, "checkbox", args$choiceNames, args$choiceValues)
  divClass <- "form-group shiny-input-checkboxgroup shiny-input-container"
  if (inline) 
    divClass <- paste(divClass, "shiny-input-container-inline")
  inputLabel <- shinyInputLabel(inputId, label)
  tags$div(id = inputId, style = css(width = validateCssUnit(width)), class = divClass, role = "group", `aria-labelledby` = inputLabel$attribs$id, inputLabel, options)
}, function (inputId, label, choices = NULL, selected = NULL, inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL) 
{
  if (is.null(choices) && is.null(choiceNames) && is.null(choiceValues)) {
    choices <- character(0)
  }
  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues)
  selected <- restoreInput(id = inputId, default = selected)
  if (!is.null(selected)) 
    selected <- as.character(selected)
  options <- generateOptions(inputId, selected, inline, "checkbox", args$choiceNames, args$choiceValues)
  divClass <- "form-group shiny-input-checkboxgroup shiny-input-container"
  if (inline) 
    divClass <- paste(divClass, "shiny-input-container-inline")
  inputLabel <- shinyInputLabel(inputId, label)
  tags$div(id = inputId, style = css(width = validateCssUnit(width)), class = divClass, role = "group", `aria-labelledby` = inputLabel$attribs$id, inputLabel, options)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
checkboxInput
list(`package:shiny` = function (inputId, label, value = FALSE, width = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "checkbox")
  if (!is.null(value) && value) 
    inputTag$attribs$checked <- "checked"
  div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), div(class = "checkbox", tags$label(inputTag, tags$span(label))))
}, function (inputId, label, value = FALSE, width = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "checkbox")
  if (!is.null(value) && value) 
    inputTag$attribs$checked <- "checked"
  div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), div(class = "checkbox", tags$label(inputTag, tags$span(label))))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
clickOpts
list(`package:shiny` = function (id, clip = TRUE) 
{
  if (is.null(id)) 
    stop("id must not be NULL")
  list(id = id, clip = clip)
}, function (id, clip = TRUE) 
{
  if (is.null(id)) 
    stop("id must not be NULL")
  list(id = id, clip = clip)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
code
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("code", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("code", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
column
list(`package:shiny` = function (width, ..., offset = 0) 
{
  if (!is.numeric(width) || (width < 1) || (width > 12)) 
    stop("column width must be between 1 and 12")
  colClass <- paste0("col-sm-", width)
  if (offset > 0) {
    colClass <- paste0(colClass, " offset-md-", offset, " col-sm-offset-", offset)
  }
  div(class = colClass, ...)
}, function (width, ..., offset = 0) 
{
  if (!is.numeric(width) || (width < 1) || (width > 12)) 
    stop("column width must be between 1 and 12")
  colClass <- paste0("col-sm-", width)
  if (offset > 0) {
    colClass <- paste0(colClass, " offset-md-", offset, " col-sm-offset-", offset)
  }
  div(class = colClass, ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
conditionalPanel
list(`package:shiny` = function (condition, ..., ns = NS(NULL)) 
{
  div(`data-display-if` = condition, `data-ns-prefix` = ns(""), ...)
}, function (condition, ..., ns = NS(NULL)) 
{
  div(`data-display-if` = condition, `data-ns-prefix` = ns(""), ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
conditionStackTrace
list(`package:shiny` = function (cond) 
{
  attr(cond, "stack.trace", exact = TRUE)
}, function (cond) 
{
  attr(cond, "stack.trace", exact = TRUE)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
conditionStackTrace<-
  list(`package:shiny` = function (cond, value) 
  {
    attr(cond, "stack.trace") <- value
    invisible(cond)
  }, function (cond, value) 
  {
    attr(cond, "stack.trace") <- value
    invisible(cond)
  })
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
createRenderFunction
list(`package:shiny` = function (func, transform = function(value, session, name, ...) value, outputFunc = NULL, outputArgs = NULL, cacheHint = "auto", cacheWriteHook = NULL, cacheReadHook = NULL) 
{
  renderFunc <- function(shinysession, name, ...) {
    hybrid_chain(func(), function(value) {
      transform(value, shinysession, name, ...)
    })
  }
  if (identical(cacheHint, "auto")) {
    attr(renderFunc, "wrappedFunc") <- attr(func, "wrappedFunc", exact = TRUE)
  }
  markRenderFunction(outputFunc, renderFunc, outputArgs, cacheHint, cacheWriteHook, cacheReadHook)
}, function (func, transform = function(value, session, name, ...) value, outputFunc = NULL, outputArgs = NULL, cacheHint = "auto", cacheWriteHook = NULL, cacheReadHook = NULL) 
{
  renderFunc <- function(shinysession, name, ...) {
    hybrid_chain(func(), function(value) {
      transform(value, shinysession, name, ...)
    })
  }
  if (identical(cacheHint, "auto")) {
    attr(renderFunc, "wrappedFunc") <- attr(func, "wrappedFunc", exact = TRUE)
  }
  markRenderFunction(outputFunc, renderFunc, outputArgs, cacheHint, cacheWriteHook, cacheReadHook)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
createWebDependency
list(`package:shiny` = function (dependency, scrubFile = TRUE) 
{
  if (is.null(dependency)) 
    return(NULL)
  if (!inherits(dependency, "html_dependency")) 
    stop("Unexpected non-html_dependency type")
  if (is.null(dependency$src$href)) {
    prefix <- paste(dependency$name, "-", dependency$version, sep = "")
    addResourcePath(prefix, dependency$src$file)
    dependency$src$href <- prefix
  }
  if (scrubFile) 
    dependency$src$file <- NULL
  return(dependency)
}, function (dependency, scrubFile = TRUE) 
{
  if (is.null(dependency)) 
    return(NULL)
  if (!inherits(dependency, "html_dependency")) 
    stop("Unexpected non-html_dependency type")
  if (is.null(dependency$src$href)) {
    prefix <- paste(dependency$name, "-", dependency$version, sep = "")
    addResourcePath(prefix, dependency$src$file)
    dependency$src$href <- prefix
  }
  if (scrubFile) 
    dependency$src$file <- NULL
  return(dependency)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
dataTableOutput
list(`package:shiny` = function (outputId) 
{
  attachDependencies(div(id = outputId, class = "shiny-datatable-output"), dataTableDependency)
}, function (outputId) 
{
  attachDependencies(div(id = outputId, class = "shiny-datatable-output"), dataTableDependency)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
dateInput
list(`package:shiny` = function (inputId, label, value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", width = NULL, autoclose = TRUE, datesdisabled = NULL, daysofweekdisabled = NULL) 
{
  value <- dateYMD(value, "value")
  min <- dateYMD(min, "min")
  max <- dateYMD(max, "max")
  datesdisabled <- dateYMD(datesdisabled, "datesdisabled")
  value <- restoreInput(id = inputId, default = value)
  tags$div(id = inputId, class = "shiny-date-input form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), tags$input(type = "text", class = "form-control", `aria-labelledby` = paste0(inputId, "-label"), title = paste("Date format:", format), `data-date-language` = language, `data-date-week-start` = weekstart, `data-date-format` = format, `data-date-start-view` = startview, `data-min-date` = min, `data-max-date` = max, `data-initial-date` = value, 
                                                                                                                                                                               `data-date-autoclose` = if (autoclose) 
                                                                                                                                                                                 "true"
                                                                                                                                                                               else "false", `data-date-dates-disabled` = jsonlite::toJSON(datesdisabled, null = "null"), `data-date-days-of-week-disabled` = jsonlite::toJSON(daysofweekdisabled, null = "null")), datePickerDependency())
}, function (inputId, label, value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", width = NULL, autoclose = TRUE, datesdisabled = NULL, daysofweekdisabled = NULL) 
{
  value <- dateYMD(value, "value")
  min <- dateYMD(min, "min")
  max <- dateYMD(max, "max")
  datesdisabled <- dateYMD(datesdisabled, "datesdisabled")
  value <- restoreInput(id = inputId, default = value)
  tags$div(id = inputId, class = "shiny-date-input form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), tags$input(type = "text", class = "form-control", `aria-labelledby` = paste0(inputId, "-label"), title = paste("Date format:", format), `data-date-language` = language, `data-date-week-start` = weekstart, `data-date-format` = format, `data-date-start-view` = startview, `data-min-date` = min, `data-max-date` = max, `data-initial-date` = value, 
                                                                                                                                                                               `data-date-autoclose` = if (autoclose) 
                                                                                                                                                                                 "true"
                                                                                                                                                                               else "false", `data-date-dates-disabled` = jsonlite::toJSON(datesdisabled, null = "null"), `data-date-days-of-week-disabled` = jsonlite::toJSON(daysofweekdisabled, null = "null")), datePickerDependency())
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
dateRangeInput
list(`package:shiny` = function (inputId, label, start = NULL, end = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", separator = " to ", width = NULL, autoclose = TRUE) 
{
  start <- dateYMD(start, "start")
  end <- dateYMD(end, "end")
  min <- dateYMD(min, "min")
  max <- dateYMD(max, "max")
  restored <- restoreInput(id = inputId, default = list(start, end))
  start <- restored[[1]]
  end <- restored[[2]]
  attachDependencies(div(id = inputId, class = "shiny-date-range-input form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), div(class = "input-daterange input-group input-group-sm", tags$input(class = "form-control", type = "text", `aria-labelledby` = paste0(inputId, "-label"), title = paste("Date format:", format), `data-date-language` = language, `data-date-week-start` = weekstart, `data-date-format` = format, `data-date-start-view` = startview, 
                                                                                                                                                                                                                                                             `data-min-date` = min, `data-max-date` = max, `data-initial-date` = start, `data-date-autoclose` = if (autoclose) 
                                                                                                                                                                                                                                                               "true"
                                                                                                                                                                                                                                                             else "false"), span(class = "input-group-addon input-group-prepend input-group-append", span(class = "input-group-text", separator)), tags$input(class = "form-control", type = "text", `aria-labelledby` = paste0(inputId, "-label"), title = paste("Date format:", format), `data-date-language` = language, `data-date-week-start` = weekstart, `data-date-format` = format, `data-date-start-view` = startview, `data-min-date` = min, `data-max-date` = max, `data-initial-date` = end, `data-date-autoclose` = if (autoclose) 
                                                                                                                                                                                                                                                               "true"
                                                                                                                                                                                                                                                               else "false"))), datePickerDependency())
}, function (inputId, label, start = NULL, end = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", separator = " to ", width = NULL, autoclose = TRUE) 
{
  start <- dateYMD(start, "start")
  end <- dateYMD(end, "end")
  min <- dateYMD(min, "min")
  max <- dateYMD(max, "max")
  restored <- restoreInput(id = inputId, default = list(start, end))
  start <- restored[[1]]
  end <- restored[[2]]
  attachDependencies(div(id = inputId, class = "shiny-date-range-input form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), div(class = "input-daterange input-group input-group-sm", tags$input(class = "form-control", type = "text", `aria-labelledby` = paste0(inputId, "-label"), title = paste("Date format:", format), `data-date-language` = language, `data-date-week-start` = weekstart, `data-date-format` = format, `data-date-start-view` = startview, 
                                                                                                                                                                                                                                                             `data-min-date` = min, `data-max-date` = max, `data-initial-date` = start, `data-date-autoclose` = if (autoclose) 
                                                                                                                                                                                                                                                               "true"
                                                                                                                                                                                                                                                             else "false"), span(class = "input-group-addon input-group-prepend input-group-append", span(class = "input-group-text", separator)), tags$input(class = "form-control", type = "text", `aria-labelledby` = paste0(inputId, "-label"), title = paste("Date format:", format), `data-date-language` = language, `data-date-week-start` = weekstart, `data-date-format` = format, `data-date-start-view` = startview, `data-min-date` = min, `data-max-date` = max, `data-initial-date` = end, `data-date-autoclose` = if (autoclose) 
                                                                                                                                                                                                                                                               "true"
                                                                                                                                                                                                                                                               else "false"))), datePickerDependency())
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
dblclickOpts
list(`package:shiny` = function (id, clip = TRUE, delay = 400) 
{
  if (is.null(id)) 
    stop("id must not be NULL")
  list(id = id, clip = clip, delay = delay)
}, function (id, clip = TRUE, delay = 400) 
{
  if (is.null(id)) 
    stop("id must not be NULL")
  list(id = id, clip = clip, delay = delay)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
debounce
list(`package:shiny` = function (r, millis, priority = 100, domain = getDefaultReactiveDomain()) 
{
  force(r)
  force(millis)
  if (!is.function(millis)) {
    origMillis <- millis
    millis <- function() origMillis
  }
  v <- reactiveValues(trigger = NULL, when = NULL)
  firstRun <- TRUE
  observe({
    if (firstRun) {
      firstRun <<- FALSE
      try(r(), silent = TRUE)
      return()
    }
    try(r(), silent = TRUE)
    v$when <- getDomainTimeMs(domain) + millis()
  }, label = "debounce tracker", domain = domain, priority = priority)
  observe({
    if (is.null(v$when)) 
      return()
    now <- getDomainTimeMs(domain)
    if (now >= v$when) {
      v$trigger <- isolate(v$trigger %||% 0)%%999999999 + 1
      v$when <- NULL
    }
    else {
      invalidateLater(v$when - now)
    }
  }, label = "debounce timer", domain = domain, priority = priority)
  er <- eventReactive(v$trigger, {
    r()
  }, label = "debounce result", ignoreNULL = FALSE, domain = domain)
  primer <- observe({
    primer$destroy()
    try(er(), silent = TRUE)
  }, label = "debounce primer", domain = domain, priority = priority)
  er
}, function (r, millis, priority = 100, domain = getDefaultReactiveDomain()) 
{
  force(r)
  force(millis)
  if (!is.function(millis)) {
    origMillis <- millis
    millis <- function() origMillis
  }
  v <- reactiveValues(trigger = NULL, when = NULL)
  firstRun <- TRUE
  observe({
    if (firstRun) {
      firstRun <<- FALSE
      try(r(), silent = TRUE)
      return()
    }
    try(r(), silent = TRUE)
    v$when <- getDomainTimeMs(domain) + millis()
  }, label = "debounce tracker", domain = domain, priority = priority)
  observe({
    if (is.null(v$when)) 
      return()
    now <- getDomainTimeMs(domain)
    if (now >= v$when) {
      v$trigger <- isolate(v$trigger %||% 0)%%999999999 + 1
      v$when <- NULL
    }
    else {
      invalidateLater(v$when - now)
    }
  }, label = "debounce timer", domain = domain, priority = priority)
  er <- eventReactive(v$trigger, {
    r()
  }, label = "debounce result", ignoreNULL = FALSE, domain = domain)
  primer <- observe({
    primer$destroy()
    try(er(), silent = TRUE)
  }, label = "debounce primer", domain = domain, priority = priority)
  er
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
devmode
list(`package:shiny` = function (devmode = getOption("shiny.devmode", TRUE), verbose = getOption("shiny.devmode.verbose", TRUE)) 
{
  options(shiny.devmode = devmode, shiny.devmode.verbose = verbose)
}, function (devmode = getOption("shiny.devmode", TRUE), verbose = getOption("shiny.devmode.verbose", TRUE)) 
{
  options(shiny.devmode = devmode, shiny.devmode.verbose = verbose)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
dialogViewer
list(`package:shiny` = function (dialogName, width = 600, height = 600) 
{
  viewer <- getOption("shinygadgets.showdialog")
  if (is.null(viewer)) {
    utils::browseURL
  }
  else {
    function(url) {
      viewer(dialogName, url, width = width, height = height)
    }
  }
}, function (dialogName, width = 600, height = 600) 
{
  viewer <- getOption("shinygadgets.showdialog")
  if (is.null(viewer)) {
    utils::browseURL
  }
  else {
    function(url) {
      viewer(dialogName, url, width = width, height = height)
    }
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
diskCache
list(`package:shiny` = function (dir = NULL, max_size = 500 * 1024^2, max_age = Inf, max_n = Inf, evict = c("lru", "fifo"), destroy_on_finalize = FALSE, missing = key_missing(), exec_missing = deprecated(), logfile = NULL) 
{
  shinyDeprecated("1.6.0", "diskCache()", "cachem::cache_disk()")
  if (is_present(exec_missing)) {
    shinyDeprecated("1.6.0", "diskCache(exec_missing =)")
  }
  cachem::cache_disk(dir = dir, max_size = max_size, max_age = max_age, max_n = max_n, evict = evict, destroy_on_finalize = destroy_on_finalize, missing = missing, logfile = logfile)
}, function (dir = NULL, max_size = 500 * 1024^2, max_age = Inf, max_n = Inf, evict = c("lru", "fifo"), destroy_on_finalize = FALSE, missing = key_missing(), exec_missing = deprecated(), logfile = NULL) 
{
  shinyDeprecated("1.6.0", "diskCache()", "cachem::cache_disk()")
  if (is_present(exec_missing)) {
    shinyDeprecated("1.6.0", "diskCache(exec_missing =)")
  }
  cachem::cache_disk(dir = dir, max_size = max_size, max_age = max_age, max_n = max_n, evict = evict, destroy_on_finalize = destroy_on_finalize, missing = missing, logfile = logfile)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
div
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("div", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("div", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
downloadButton
list(`package:shiny` = function (outputId, label = "Download", class = NULL, ..., icon = shiny::icon("download")) 
{
  aTag <- tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class), href = "", target = "_blank", download = NA, validateIcon(icon), label, ...)
}, function (outputId, label = "Download", class = NULL, ..., icon = shiny::icon("download")) 
{
  aTag <- tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class), href = "", target = "_blank", download = NA, validateIcon(icon), label, ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
downloadHandler
list(`package:shiny` = function (filename, content, contentType = NULL, outputArgs = list()) 
{
  renderFunc <- function(shinysession, name, ...) {
    shinysession$registerDownload(name, filename, contentType, content)
  }
  snapshotExclude(markRenderFunction(downloadButton, renderFunc, outputArgs, cacheHint = FALSE))
}, function (filename, content, contentType = NULL, outputArgs = list()) 
{
  renderFunc <- function(shinysession, name, ...) {
    shinysession$registerDownload(name, filename, contentType, content)
  }
  snapshotExclude(markRenderFunction(downloadButton, renderFunc, outputArgs, cacheHint = FALSE))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
downloadLink
list(`package:shiny` = function (outputId, label = "Download", class = NULL, ...) 
{
  tags$a(id = outputId, class = paste(c("shiny-download-link", class), collapse = " "), href = "", target = "_blank", download = NA, label, ...)
}, function (outputId, label = "Download", class = NULL, ...) 
{
  tags$a(id = outputId, class = paste(c("shiny-download-link", class), collapse = " "), href = "", target = "_blank", download = NA, label, ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
em
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("em", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("em", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
enableBookmarking
list(`package:shiny` = function (store = c("url", "server", "disable")) 
{
  store <- match.arg(store)
  shinyOptions(bookmarkStore = store)
}, function (store = c("url", "server", "disable")) 
{
  store <- match.arg(store)
  shinyOptions(bookmarkStore = store)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
eventReactive
list(`package:shiny` = function (eventExpr, valueExpr, event.env = parent.frame(), event.quoted = FALSE, value.env = parent.frame(), value.quoted = FALSE, ..., label = NULL, domain = getDefaultReactiveDomain(), ignoreNULL = TRUE, ignoreInit = FALSE) 
{
  check_dots_empty()
  eventQ <- exprToQuo(eventExpr, event.env, event.quoted)
  valueQ <- exprToQuo(valueExpr, value.env, value.quoted)
  label <- quoToLabel(eventQ, "eventReactive", label)
  invisible(inject(bindEvent(ignoreNULL = ignoreNULL, ignoreInit = ignoreInit, label = label, !!eventQ, x = reactive(!!valueQ, domain = domain, label = label))))
}, function (eventExpr, valueExpr, event.env = parent.frame(), event.quoted = FALSE, value.env = parent.frame(), value.quoted = FALSE, ..., label = NULL, domain = getDefaultReactiveDomain(), ignoreNULL = TRUE, ignoreInit = FALSE) 
{
  check_dots_empty()
  eventQ <- exprToQuo(eventExpr, event.env, event.quoted)
  valueQ <- exprToQuo(valueExpr, value.env, value.quoted)
  label <- quoToLabel(eventQ, "eventReactive", label)
  invisible(inject(bindEvent(ignoreNULL = ignoreNULL, ignoreInit = ignoreInit, label = label, !!eventQ, x = reactive(!!valueQ, domain = domain, label = label))))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
exportTestValues
list(`package:shiny` = function (..., quoted_ = FALSE, env_ = parent.frame(), session_ = getDefaultReactiveDomain()) 
{
  session_$exportTestValues(..., quoted_ = quoted_, env_ = env_)
}, function (..., quoted_ = FALSE, env_ = parent.frame(), session_ = getDefaultReactiveDomain()) 
{
  session_$exportTestValues(..., quoted_ = quoted_, env_ = env_)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
exprToFunction
list(`package:shiny` = function (expr, env = parent.frame(), quoted = FALSE) 
{
  if (!quoted) {
    expr <- eval(substitute(substitute(expr)), parent.frame())
  }
  q <- exprToQuo(expr, env, quoted = TRUE)
  quoToSimpleFunction(q)
}, function (expr, env = parent.frame(), quoted = FALSE) 
{
  if (!quoted) {
    expr <- eval(substitute(substitute(expr)), parent.frame())
  }
  q <- exprToQuo(expr, env, quoted = TRUE)
  quoToSimpleFunction(q)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
fileInput
list(`package:shiny` = function (inputId, label, multiple = FALSE, accept = NULL, width = NULL, buttonLabel = "Browse...", placeholder = "No file selected", capture = NULL) 
{
  restoredValue <- restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- tags$input(id = inputId, name = inputId, type = "file", style = "position: absolute !important; top: -99999px !important; left: -99999px !important;", `data-restore` = restoredValue)
  if (multiple) 
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0) 
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  if (!is.null(capture)) {
    inputTag$attribs$capture <- capture
  }
  div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), div(class = "input-group", tags$label(class = "input-group-btn input-group-prepend", span(class = "btn btn-default btn-file", buttonLabel, inputTag)), tags$input(type = "text", class = "form-control", placeholder = placeholder, readonly = "readonly")), tags$div(id = paste(inputId, "_progress", sep = ""), class = "progress active shiny-file-input-progress", tags$div(class = "progress-bar")))
}, function (inputId, label, multiple = FALSE, accept = NULL, width = NULL, buttonLabel = "Browse...", placeholder = "No file selected", capture = NULL) 
{
  restoredValue <- restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- tags$input(id = inputId, name = inputId, type = "file", style = "position: absolute !important; top: -99999px !important; left: -99999px !important;", `data-restore` = restoredValue)
  if (multiple) 
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0) 
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  if (!is.null(capture)) {
    inputTag$attribs$capture <- capture
  }
  div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), div(class = "input-group", tags$label(class = "input-group-btn input-group-prepend", span(class = "btn btn-default btn-file", buttonLabel, inputTag)), tags$input(type = "text", class = "form-control", placeholder = placeholder, readonly = "readonly")), tags$div(id = paste(inputId, "_progress", sep = ""), class = "progress active shiny-file-input-progress", tags$div(class = "progress-bar")))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
fillCol
list(`package:shiny` = function (..., flex = 1, width = "100%", height = "100%") 
{
  flexfill(..., direction = "column", flex = flex, width = width, height = height)
}, function (..., flex = 1, width = "100%", height = "100%") 
{
  flexfill(..., direction = "column", flex = flex, width = width, height = height)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
fillPage
list(`package:shiny` = function (..., padding = 0, title = NULL, bootstrap = TRUE, theme = NULL, lang = NULL) 
{
  fillCSS <- tags$head(tags$style(type = "text/css", "html, body { width: 100%; height: 100%; overflow: hidden; }", sprintf("body { padding: %s; margin: 0; }", collapseSizes(padding))))
  if (isTRUE(bootstrap)) {
    ui <- bootstrapPage(title = title, theme = theme, fillCSS, lang = lang, ...)
  }
  else {
    ui <- tagList(fillCSS, if (!is.null(title)) 
      tags$head(tags$title(title)), ...)
    ui <- setLang(ui, lang)
  }
  return(ui)
}, function (..., padding = 0, title = NULL, bootstrap = TRUE, theme = NULL, lang = NULL) 
{
  fillCSS <- tags$head(tags$style(type = "text/css", "html, body { width: 100%; height: 100%; overflow: hidden; }", sprintf("body { padding: %s; margin: 0; }", collapseSizes(padding))))
  if (isTRUE(bootstrap)) {
    ui <- bootstrapPage(title = title, theme = theme, fillCSS, lang = lang, ...)
  }
  else {
    ui <- tagList(fillCSS, if (!is.null(title)) 
      tags$head(tags$title(title)), ...)
    ui <- setLang(ui, lang)
  }
  return(ui)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
fillRow
list(`package:shiny` = function (..., flex = 1, width = "100%", height = "100%") 
{
  flexfill(..., direction = "row", flex = flex, width = width, height = height)
}, function (..., flex = 1, width = "100%", height = "100%") 
{
  flexfill(..., direction = "row", flex = flex, width = width, height = height)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
fixedPage
list(`package:shiny` = function (..., title = NULL, theme = NULL, lang = NULL) 
{
  bootstrapPage(div(class = "container", ...), title = title, theme = theme, lang = lang)
}, function (..., title = NULL, theme = NULL, lang = NULL) 
{
  bootstrapPage(div(class = "container", ...), title = title, theme = theme, lang = lang)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
fixedPanel
list(`package:shiny` = function (..., top = NULL, left = NULL, right = NULL, bottom = NULL, width = NULL, height = NULL, draggable = FALSE, cursor = c("auto", "move", "default", "inherit")) 
{
  absolutePanel(..., top = top, left = left, right = right, bottom = bottom, width = width, height = height, draggable = draggable, cursor = match.arg(cursor), fixed = TRUE)
}, function (..., top = NULL, left = NULL, right = NULL, bottom = NULL, width = NULL, height = NULL, draggable = FALSE, cursor = c("auto", "move", "default", "inherit")) 
{
  absolutePanel(..., top = top, left = left, right = right, bottom = bottom, width = width, height = height, draggable = draggable, cursor = match.arg(cursor), fixed = TRUE)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
fixedRow
list(`package:shiny` = function (...) 
{
  div(class = "row", ...)
}, function (...) 
{
  div(class = "row", ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
flowLayout
list(`package:shiny` = function (..., cellArgs = list()) 
{
  children <- list2(...)
  childIdx <- !nzchar(names(children) %||% character(length(children)))
  attribs <- children[!childIdx]
  children <- children[childIdx]
  do.call(tags$div, c(list(class = "shiny-flow-layout"), attribs, lapply(children, function(x) {
    do.call(tags$div, c(cellArgs, list(x)))
  })))
}, function (..., cellArgs = list()) 
{
  children <- list2(...)
  childIdx <- !nzchar(names(children) %||% character(length(children)))
  attribs <- children[!childIdx]
  children <- children[childIdx]
  do.call(tags$div, c(list(class = "shiny-flow-layout"), attribs, lapply(children, function(x) {
    do.call(tags$div, c(cellArgs, list(x)))
  })))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
fluidPage
list(`package:shiny` = function (..., title = NULL, theme = NULL, lang = NULL) 
{
  bootstrapPage(div(class = "container-fluid", ...), title = title, theme = theme, lang = lang)
}, function (..., title = NULL, theme = NULL, lang = NULL) 
{
  bootstrapPage(div(class = "container-fluid", ...), title = title, theme = theme, lang = lang)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
fluidRow
list(`package:shiny` = function (...) 
{
  div(class = "row", ...)
}, function (...) 
{
  div(class = "row", ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
freezeReactiveVal
list(`package:shiny` = function (x) 
{
  if (getOption("shiny.deprecation.messages", TRUE) && getOption("shiny.deprecation.messages.freeze", TRUE)) {
    rlang::warn("freezeReactiveVal() is soft-deprecated, and may be removed in a future version of Shiny. (See https://github.com/rstudio/shiny/issues/3063)", .frequency = "once", .frequency_id = "freezeReactiveVal")
  }
  domain <- getDefaultReactiveDomain()
  checkReactiveDomain(domain)
  if (!inherits(x, "reactiveVal")) {
    rlang::abort("`x` must be a reactiveVal.")
  }
  attr(x, ".impl", exact = TRUE)$freeze(domain)
  invisible()
}, function (x) 
{
  if (getOption("shiny.deprecation.messages", TRUE) && getOption("shiny.deprecation.messages.freeze", TRUE)) {
    rlang::warn("freezeReactiveVal() is soft-deprecated, and may be removed in a future version of Shiny. (See https://github.com/rstudio/shiny/issues/3063)", .frequency = "once", .frequency_id = "freezeReactiveVal")
  }
  domain <- getDefaultReactiveDomain()
  checkReactiveDomain(domain)
  if (!inherits(x, "reactiveVal")) {
    rlang::abort("`x` must be a reactiveVal.")
  }
  attr(x, ".impl", exact = TRUE)$freeze(domain)
  invisible()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
freezeReactiveValue
list(`package:shiny` = function (x, name) 
{
  domain <- getDefaultReactiveDomain()
  checkReactiveDomain(domain)
  domain$freezeValue(x, name)
  invisible()
}, function (x, name) 
{
  domain <- getDefaultReactiveDomain()
  checkReactiveDomain(domain)
  domain$freezeValue(x, name)
  invisible()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
get_devmode_option
list(`package:shiny` = function (name, default = NULL, devmode_default = missing_arg(), devmode_message = missing_arg()) 
{
  getOption(name, local({
    if (!in_devmode()) {
      return(default)
    }
    info <- registered_devmode_options$get(name)
    if (is.null(info)) {
      rlang::warn(message = paste0("`get_devmode_option(name)` could not find `name = \"", name, "\"`. ", "Returning `default` value"))
      return(default)
    }
    devmode_inform(maybe_missing(devmode_message, default = info$devmode_message))
    maybe_missing(devmode_default, default = info$devmode_default)
  }))
}, function (name, default = NULL, devmode_default = missing_arg(), devmode_message = missing_arg()) 
{
  getOption(name, local({
    if (!in_devmode()) {
      return(default)
    }
    info <- registered_devmode_options$get(name)
    if (is.null(info)) {
      rlang::warn(message = paste0("`get_devmode_option(name)` could not find `name = \"", name, "\"`. ", "Returning `default` value"))
      return(default)
    }
    devmode_inform(maybe_missing(devmode_message, default = info$devmode_message))
    maybe_missing(devmode_default, default = info$devmode_default)
  }))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
getCurrentOutputInfo
list(`package:shiny` = function (session = getDefaultReactiveDomain()) 
{
  if (is.null(session)) 
    return(NULL)
  session$getCurrentOutputInfo()
}, function (session = getDefaultReactiveDomain()) 
{
  if (is.null(session)) 
    return(NULL)
  session$getCurrentOutputInfo()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
getCurrentTheme
list(`package:shiny` = function () 
{
  getShinyOption("bootstrapTheme", default = NULL)
}, function () 
{
  getShinyOption("bootstrapTheme", default = NULL)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
getDefaultReactiveDomain
list(`package:shiny` = function () 
{
  .globals$domain
}, function () 
{
  shiny::getDefaultReactiveDomain()
}, function () 
{
  .globals$domain
})
c("package:shiny", "namespace:bslib", "namespace:shiny")
c(TRUE, FALSE, FALSE)
c(FALSE, FALSE, TRUE)
getQueryString
list(`package:shiny` = function (session = getDefaultReactiveDomain()) 
{
  parseQueryString(session$clientData$url_search)
}, function (session = getDefaultReactiveDomain()) 
{
  parseQueryString(session$clientData$url_search)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
getShinyOption
list(`package:shiny` = function (name, default = NULL) 
{
  name <- as.character(name)
  session <- getDefaultReactiveDomain()
  if (!is.null(session)) {
    if (name %in% names(session$options)) {
      return(session$options[[name]])
    }
    else {
      return(default)
    }
  }
  app_state <- getCurrentAppState()
  if (!is.null(app_state)) {
    if (name %in% names(app_state$options)) {
      return(app_state$options[[name]])
    }
    else {
      return(default)
    }
  }
  if (name %in% names(.globals$options)) {
    return(.globals$options[[name]])
  }
  else {
    return(default)
  }
}, function (name, default = NULL) 
{
  name <- as.character(name)
  session <- getDefaultReactiveDomain()
  if (!is.null(session)) {
    if (name %in% names(session$options)) {
      return(session$options[[name]])
    }
    else {
      return(default)
    }
  }
  app_state <- getCurrentAppState()
  if (!is.null(app_state)) {
    if (name %in% names(app_state$options)) {
      return(app_state$options[[name]])
    }
    else {
      return(default)
    }
  }
  if (name %in% names(.globals$options)) {
    return(.globals$options[[name]])
  }
  else {
    return(default)
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
getUrlHash
list(`package:shiny` = function (session = getDefaultReactiveDomain()) 
{
  session$clientData$url_hash
}, function (session = getDefaultReactiveDomain()) 
{
  session$clientData$url_hash
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
h1
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("h1", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("h1", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
h2
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("h2", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("h2", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
h3
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("h3", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("h3", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
h4
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("h4", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("h4", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
h5
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("h5", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("h5", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
h6
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("h6", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("h6", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
headerPanel
list(`package:shiny` = function (title, windowTitle = title) 
{
  tagList(tags$head(tags$title(windowTitle)), div(class = "col-sm-12", h1(title)))
}, function (title, windowTitle = title) 
{
  tagList(tags$head(tags$title(windowTitle)), div(class = "col-sm-12", h1(title)))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
helpText
list(`package:shiny` = function (...) 
{
  span(class = "help-block", ...)
}, function (...) 
{
  span(class = "help-block", ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
hideTab
list(`package:shiny` = function (inputId, target, session = getDefaultReactiveDomain()) 
{
  force(target)
  inputId <- session$ns(inputId)
  callback <- function() {
    session$sendChangeTabVisibility(inputId = inputId, target = target, type = "hide")
  }
  session$onFlush(callback, once = TRUE)
}, function (inputId, target, session = getDefaultReactiveDomain()) 
{
  force(target)
  inputId <- session$ns(inputId)
  callback <- function() {
    session$sendChangeTabVisibility(inputId = inputId, target = target, type = "hide")
  }
  session$onFlush(callback, once = TRUE)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
hoverOpts
list(`package:shiny` = function (id, delay = 300, delayType = c("debounce", "throttle"), clip = TRUE, nullOutside = TRUE) 
{
  if (is.null(id)) 
    stop("id must not be NULL")
  list(id = id, delay = delay, delayType = match.arg(delayType), clip = clip, nullOutside = nullOutside)
}, function (id, delay = 300, delayType = c("debounce", "throttle"), clip = TRUE, nullOutside = TRUE) 
{
  if (is.null(id)) 
    stop("id must not be NULL")
  list(id = id, delay = delay, delayType = match.arg(delayType), clip = clip, nullOutside = nullOutside)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
hr
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("hr", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("hr", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
HTML
list(`package:shiny` = function (text, ..., .noWS = NULL) 
{
  htmlText <- c(text, as.character(dots_list(...)))
  htmlText <- paste8(htmlText, collapse = " ")
  attr(htmlText, "html") <- TRUE
  attr(htmlText, "noWS") <- .noWS
  class(htmlText) <- c("html", "character")
  htmlText
}, function (text, ..., .noWS = NULL) 
{
  htmlText <- c(text, as.character(dots_list(...)))
  htmlText <- paste8(htmlText, collapse = " ")
  attr(htmlText, "html") <- TRUE
  attr(htmlText, "noWS") <- .noWS
  class(htmlText) <- c("html", "character")
  htmlText
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
htmlOutput
list(`package:shiny` = function (outputId, inline = FALSE, container = if (inline) span else div, ...) 
{
  if (any_unnamed(list(...))) {
    warning("Unnamed elements in ... will be replaced with dynamic UI.")
  }
  container(id = outputId, class = "shiny-html-output", ...)
}, function (outputId, inline = FALSE, container = if (inline) span else div, ...) 
{
  if (any_unnamed(list(...))) {
    warning("Unnamed elements in ... will be replaced with dynamic UI.")
  }
  container(id = outputId, class = "shiny-html-output", ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
htmlTemplate
list(`package:shiny` = function (filename = NULL, ..., text_ = NULL, document_ = "auto") 
{
  if (!xor(is.null(filename), is.null(text_))) {
    stop("htmlTemplate requires either `filename` or `text_`.")
  }
  if (!is.null(filename)) {
    html <- readChar(filename, file.info(filename)$size, useBytes = TRUE)
    Encoding(html) <- "UTF-8"
  }
  else if (!is.null(text_)) {
    text_ <- paste8(text_, collapse = "\n")
    html <- enc2utf8(text_)
  }
  pieces <- .Call(template_dfa, html)
  Encoding(pieces) <- "UTF-8"
  vars <- dots_list(...)
  if ("headContent" %in% names(vars)) {
    stop("Can't use reserved argument name 'headContent'.")
  }
  vars$headContent <- function() HTML("<!-- HEAD_CONTENT -->")
  env <- list2env(vars, parent = globalenv())
  pieces <- mapply(pieces, rep_len(c(FALSE, TRUE), length.out = length(pieces)), FUN = function(piece, isCode) {
    if (isCode) {
      eval(parse(text = piece), env)
    }
    else {
      HTML(piece, .noWS = "outside")
    }
  }, SIMPLIFY = FALSE)
  result <- tagList(pieces)
  if (document_ == "auto") {
    document_ = grepl("<HTML(\\s[^<]*)?>", html, ignore.case = TRUE)
  }
  if (document_) {
    class(result) <- c("html_document", class(result))
  }
  result
}, function (filename = NULL, ..., text_ = NULL, document_ = "auto") 
{
  if (!xor(is.null(filename), is.null(text_))) {
    stop("htmlTemplate requires either `filename` or `text_`.")
  }
  if (!is.null(filename)) {
    html <- readChar(filename, file.info(filename)$size, useBytes = TRUE)
    Encoding(html) <- "UTF-8"
  }
  else if (!is.null(text_)) {
    text_ <- paste8(text_, collapse = "\n")
    html <- enc2utf8(text_)
  }
  pieces <- .Call(template_dfa, html)
  Encoding(pieces) <- "UTF-8"
  vars <- dots_list(...)
  if ("headContent" %in% names(vars)) {
    stop("Can't use reserved argument name 'headContent'.")
  }
  vars$headContent <- function() HTML("<!-- HEAD_CONTENT -->")
  env <- list2env(vars, parent = globalenv())
  pieces <- mapply(pieces, rep_len(c(FALSE, TRUE), length.out = length(pieces)), FUN = function(piece, isCode) {
    if (isCode) {
      eval(parse(text = piece), env)
    }
    else {
      HTML(piece, .noWS = "outside")
    }
  }, SIMPLIFY = FALSE)
  result <- tagList(pieces)
  if (document_ == "auto") {
    document_ = grepl("<HTML(\\s[^<]*)?>", html, ignore.case = TRUE)
  }
  if (document_) {
    class(result) <- c("html_document", class(result))
  }
  result
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
httpResponse
list(`package:shiny` = function (status = 200, content_type = "text/html; charset=UTF-8", content = "", headers = list()) 
{
  headers <- as.list(headers)
  if (is.null(headers$`X-UA-Compatible`)) 
    headers$`X-UA-Compatible` <- "IE=edge,chrome=1"
  resp <- list(status = status, content_type = content_type, content = content, headers = headers)
  class(resp) <- "httpResponse"
  return(resp)
}, function (status = 200, content_type = "text/html; charset=UTF-8", content = "", headers = list()) 
{
  headers <- as.list(headers)
  if (is.null(headers$`X-UA-Compatible`)) 
    headers$`X-UA-Compatible` <- "IE=edge,chrome=1"
  resp <- list(status = status, content_type = content_type, content = content, headers = headers)
  class(resp) <- "httpResponse"
  return(resp)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
icon
list(`package:shiny` = function (name, class = NULL, lib = "font-awesome", ...) 
{
  if (is.null(name)) {
    lib <- "none"
  }
  switch(lib %||% "", none = iconTag(name, class = class, ...), `font-awesome` = fontawesome::fa_i(name = name, class = class, ...), glyphicon = iconTag(name, class = "glyphicon", class = paste0("glyphicon-", name), class = class, ...), stop("Unknown icon library: ", lib, ". See `?icon` for supported libraries."))
}, function (name, class = NULL, lib = "font-awesome", ...) 
{
  if (is.null(name)) {
    lib <- "none"
  }
  switch(lib %||% "", none = iconTag(name, class = class, ...), `font-awesome` = fontawesome::fa_i(name = name, class = class, ...), glyphicon = iconTag(name, class = "glyphicon", class = paste0("glyphicon-", name), class = class, ...), stop("Unknown icon library: ", lib, ". See `?icon` for supported libraries."))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
imageOutput
list(`package:shiny` = function (outputId, width = "100%", height = "400px", click = NULL, dblclick = NULL, hover = NULL, brush = NULL, inline = FALSE) 
{
  style <- if (!inline) {
    css(width = validateCssUnit(width), height = validateCssUnit(height))
  }
  args <- list(id = outputId, class = "shiny-image-output", style = style)
  formatOptNames <- function(opts, prefix) {
    newNames <- paste("data", prefix, names(opts), sep = "-")
    newNames <- gsub("([A-Z])", "-\\L\\1", newNames, perl = TRUE)
    names(opts) <- newNames
    opts
  }
  if (!is.null(click)) {
    if (is.character(click)) {
      click <- clickOpts(id = click)
    }
    args <- c(args, formatOptNames(click, "click"))
  }
  if (!is.null(dblclick)) {
    if (is.character(dblclick)) {
      dblclick <- clickOpts(id = dblclick)
    }
    args <- c(args, formatOptNames(dblclick, "dblclick"))
  }
  if (!is.null(hover)) {
    if (is.character(hover)) {
      hover <- hoverOpts(id = hover)
    }
    args <- c(args, formatOptNames(hover, "hover"))
  }
  if (!is.null(brush)) {
    if (is.character(brush)) {
      brush <- brushOpts(id = brush)
    }
    args <- c(args, formatOptNames(brush, "brush"))
  }
  container <- if (inline) 
    span
  else div
  do.call(container, args)
}, function (outputId, width = "100%", height = "400px", click = NULL, dblclick = NULL, hover = NULL, brush = NULL, inline = FALSE) 
{
  style <- if (!inline) {
    css(width = validateCssUnit(width), height = validateCssUnit(height))
  }
  args <- list(id = outputId, class = "shiny-image-output", style = style)
  formatOptNames <- function(opts, prefix) {
    newNames <- paste("data", prefix, names(opts), sep = "-")
    newNames <- gsub("([A-Z])", "-\\L\\1", newNames, perl = TRUE)
    names(opts) <- newNames
    opts
  }
  if (!is.null(click)) {
    if (is.character(click)) {
      click <- clickOpts(id = click)
    }
    args <- c(args, formatOptNames(click, "click"))
  }
  if (!is.null(dblclick)) {
    if (is.character(dblclick)) {
      dblclick <- clickOpts(id = dblclick)
    }
    args <- c(args, formatOptNames(dblclick, "dblclick"))
  }
  if (!is.null(hover)) {
    if (is.character(hover)) {
      hover <- hoverOpts(id = hover)
    }
    args <- c(args, formatOptNames(hover, "hover"))
  }
  if (!is.null(brush)) {
    if (is.character(brush)) {
      brush <- brushOpts(id = brush)
    }
    args <- c(args, formatOptNames(brush, "brush"))
  }
  container <- if (inline) 
    span
  else div
  do.call(container, args)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
img
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("img", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("img", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
in_devmode
list(`package:shiny` = function () 
{
  isTRUE(getOption("shiny.devmode", FALSE)) && !identical(Sys.getenv("TESTTHAT"), "true")
}, function () 
{
  isTRUE(getOption("shiny.devmode", FALSE)) && !identical(Sys.getenv("TESTTHAT"), "true")
}, function () 
{
  isTRUE(getOption("shiny.devmode", FALSE)) && !identical(Sys.getenv("TESTTHAT"), "true")
})
c("package:shiny", "namespace:sass", "namespace:shiny")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, TRUE)
includeCSS
list(`package:shiny` = function (path, ...) 
{
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  args <- dots_list(...)
  if (is.null(args$type)) 
    args$type <- "text/css"
  return(do.call(tags$style, c(list(HTML(paste8(lines, collapse = "\n"))), args)))
}, function (path, ...) 
{
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  args <- dots_list(...)
  if (is.null(args$type)) 
    args$type <- "text/css"
  return(do.call(tags$style, c(list(HTML(paste8(lines, collapse = "\n"))), args)))
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
includeHTML
list(`package:shiny` = function (path) 
{
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  return(HTML(paste8(lines, collapse = "\n")))
}, function (path) 
{
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  return(HTML(paste8(lines, collapse = "\n")))
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
includeMarkdown
list(`package:shiny` = function (path) 
{
  html <- markdown::markdownToHTML(path, fragment.only = TRUE)
  Encoding(html) <- "UTF-8"
  return(HTML(html))
}, function (path) 
{
  html <- markdown::markdownToHTML(path, fragment.only = TRUE)
  Encoding(html) <- "UTF-8"
  return(HTML(html))
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
includeScript
list(`package:shiny` = function (path, ...) 
{
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  return(tags$script(HTML(paste8(lines, collapse = "\n")), ...))
}, function (path, ...) 
{
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  return(tags$script(HTML(paste8(lines, collapse = "\n")), ...))
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
includeText
list(`package:shiny` = function (path) 
{
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  return(paste8(lines, collapse = "\n"))
}, function (path) 
{
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  return(paste8(lines, collapse = "\n"))
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
incProgress
list(`package:shiny` = function (amount = 0.1, message = NULL, detail = NULL, session = getDefaultReactiveDomain()) 
{
  if (is.null(session$progressStack)) 
    stop("'session' is not a ShinySession object.")
  if (session$progressStack$size() == 0) {
    warning("incProgress was called outside of withProgress; ignoring")
    return()
  }
  p <- session$progressStack$peek()
  p$inc(amount, message, detail)
  invisible()
}, function (amount = 0.1, message = NULL, detail = NULL, session = getDefaultReactiveDomain()) 
{
  if (is.null(session$progressStack)) 
    stop("'session' is not a ShinySession object.")
  if (session$progressStack$size() == 0) {
    warning("incProgress was called outside of withProgress; ignoring")
    return()
  }
  p <- session$progressStack$peek()
  p$inc(amount, message, detail)
  invisible()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
inputPanel
list(`package:shiny` = function (...) 
{
  div(class = "shiny-input-panel", flowLayout(...))
}, function (...) 
{
  div(class = "shiny-input-panel", flowLayout(...))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
insertTab
list(`package:shiny` = function (inputId, tab, target = NULL, position = c("after", "before"), select = FALSE, session = getDefaultReactiveDomain()) 
{
  bslib::nav_insert(inputId, tab, target, match.arg(position), select, session)
}, function (inputId, tab, target = NULL, position = c("after", "before"), select = FALSE, session = getDefaultReactiveDomain()) 
{
  bslib::nav_insert(inputId, tab, target, match.arg(position), select, session)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
insertUI
list(`package:shiny` = function (selector, where = c("beforeBegin", "afterBegin", "beforeEnd", "afterEnd"), ui, multiple = FALSE, immediate = FALSE, session = getDefaultReactiveDomain()) 
{
  force(selector)
  force(ui)
  force(session)
  force(multiple)
  if (missing(where)) 
    where <- "beforeEnd"
  where <- match.arg(where)
  callback <- function() {
    session$sendInsertUI(selector = selector, multiple = multiple, where = where, content = processDeps(ui, session))
  }
  if (!immediate) 
    session$onFlushed(callback, once = TRUE)
  else callback()
}, function (selector, where = c("beforeBegin", "afterBegin", "beforeEnd", "afterEnd"), ui, multiple = FALSE, immediate = FALSE, session = getDefaultReactiveDomain()) 
{
  force(selector)
  force(ui)
  force(session)
  force(multiple)
  if (missing(where)) 
    where <- "beforeEnd"
  where <- match.arg(where)
  callback <- function() {
    session$sendInsertUI(selector = selector, multiple = multiple, where = where, content = processDeps(ui, session))
  }
  if (!immediate) 
    session$onFlushed(callback, once = TRUE)
  else callback()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
installExprFunction
list(`package:shiny` = function (expr, name, eval.env = parent.frame(2), quoted = FALSE, assign.env = parent.frame(1), label = sys.call(-1)[[1]], wrappedWithLabel = TRUE, ..stacktraceon = FALSE) 
{
  if (!quoted) {
    quoted <- TRUE
    expr <- eval(substitute(substitute(expr)), parent.frame())
  }
  func <- exprToFunction(expr, eval.env, quoted)
  if (length(label) > 1) {
    label <- paste0(label, collapse = "\n")
  }
  wrappedWithLabel <- isTRUE(wrappedWithLabel)
  if (wrappedWithLabel) {
    func <- wrapFunctionLabel(func, updateFunctionLabel(label), ..stacktraceon = ..stacktraceon, dots = FALSE)
  }
  assign(name, func, envir = assign.env)
  if (!wrappedWithLabel) {
    registerDebugHook(name, assign.env, label)
  }
  invisible(func)
}, function (expr, name, eval.env = parent.frame(2), quoted = FALSE, assign.env = parent.frame(1), label = sys.call(-1)[[1]], wrappedWithLabel = TRUE, ..stacktraceon = FALSE) 
{
  if (!quoted) {
    quoted <- TRUE
    expr <- eval(substitute(substitute(expr)), parent.frame())
  }
  func <- exprToFunction(expr, eval.env, quoted)
  if (length(label) > 1) {
    label <- paste0(label, collapse = "\n")
  }
  wrappedWithLabel <- isTRUE(wrappedWithLabel)
  if (wrappedWithLabel) {
    func <- wrapFunctionLabel(func, updateFunctionLabel(label), ..stacktraceon = ..stacktraceon, dots = FALSE)
  }
  assign(name, func, envir = assign.env)
  if (!wrappedWithLabel) {
    registerDebugHook(name, assign.env, label)
  }
  invisible(func)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
invalidateLater
list(`package:shiny` = function (millis, session = getDefaultReactiveDomain()) 
{
  force(session)
  ctx <- getCurrentContext()
  rLog$invalidateLater(ctx$.reactId, ctx$id, millis, session)
  clear_on_ended_callback <- function() {
  }
  scheduler <- defineScheduler(session)
  timerHandle <- scheduler(millis, function() {
    if (is.null(session)) {
      ctx$invalidate()
      return(invisible())
    }
    clear_on_ended_callback()
    if (!session$isClosed()) {
      session$cycleStartAction(function() {
        ctx$invalidate()
      })
    }
    invisible()
  })
  if (!is.null(session)) {
    clear_on_ended_callback <- session$onEnded(timerHandle)
  }
  invisible()
}, function (millis, session = getDefaultReactiveDomain()) 
{
  force(session)
  ctx <- getCurrentContext()
  rLog$invalidateLater(ctx$.reactId, ctx$id, millis, session)
  clear_on_ended_callback <- function() {
  }
  scheduler <- defineScheduler(session)
  timerHandle <- scheduler(millis, function() {
    if (is.null(session)) {
      ctx$invalidate()
      return(invisible())
    }
    clear_on_ended_callback()
    if (!session$isClosed()) {
      session$cycleStartAction(function() {
        ctx$invalidate()
      })
    }
    invisible()
  })
  if (!is.null(session)) {
    clear_on_ended_callback <- session$onEnded(timerHandle)
  }
  invisible()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
is.key_missing
list(`package:shiny` = function (x) 
{
  inherits(x, "key_missing")
}, function (x) 
{
  inherits(x, "key_missing")
})
c("package:shiny", "namespace:fastmap")
c(TRUE, FALSE)
c(FALSE, TRUE)
is.reactive
list(`package:shiny` = function (x) 
{
  inherits(x, "reactive")
}, function (x) 
{
  inherits(x, "reactive")
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
is.reactivevalues
list(`package:shiny` = function (x) 
  inherits(x, "reactivevalues"), function (x) 
    inherits(x, "reactivevalues"))
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
is.shiny.appobj
list(`package:shiny` = function (x) 
{
  inherits(x, "shiny.appobj")
}, function (x) 
{
  inherits(x, "shiny.appobj")
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
is.singleton
list(`package:shiny` = function (x) 
{
  isTRUE(attr(x, "htmltools.singleton"))
}, function (x) 
{
  isTRUE(attr(x, "htmltools.singleton"))
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
isolate
list(`package:shiny` = function (expr) 
{
  if (hasCurrentContext()) {
    reactId <- getCurrentContext()$.reactId
  }
  else {
    reactId <- rLog$noReactId
  }
  ctx <- Context$new(getDefaultReactiveDomain(), "[isolate]", type = "isolate", reactId = reactId)
  on.exit(ctx$invalidate())
  ..stacktraceoff..(ctx$run(function() {
    ..stacktraceon..(expr)
  }))
}, function (expr) 
{
  if (hasCurrentContext()) {
    reactId <- getCurrentContext()$.reactId
  }
  else {
    reactId <- rLog$noReactId
  }
  ctx <- Context$new(getDefaultReactiveDomain(), "[isolate]", type = "isolate", reactId = reactId)
  on.exit(ctx$invalidate())
  ..stacktraceoff..(ctx$run(function() {
    ..stacktraceon..(expr)
  }))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
isRunning
list(`package:shiny` = function () 
{
  !is.null(getCurrentAppState())
}, function () 
{
  !is.null(getCurrentAppState())
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
isTruthy
list(`package:shiny` = function (x) 
{
  if (inherits(x, "try-error")) 
    return(FALSE)
  if (!is.atomic(x)) 
    return(TRUE)
  if (is.null(x)) 
    return(FALSE)
  if (length(x) == 0) 
    return(FALSE)
  if (all(is.na(x))) 
    return(FALSE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x)))) 
    return(FALSE)
  if (inherits(x, "shinyActionButtonValue") && x == 0) 
    return(FALSE)
  if (is.logical(x) && !any(stats::na.omit(x))) 
    return(FALSE)
  return(TRUE)
}, function (x) 
{
  if (inherits(x, "try-error")) 
    return(FALSE)
  if (!is.atomic(x)) 
    return(TRUE)
  if (is.null(x)) 
    return(FALSE)
  if (length(x) == 0) 
    return(FALSE)
  if (all(is.na(x))) 
    return(FALSE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x)))) 
    return(FALSE)
  if (inherits(x, "shinyActionButtonValue") && x == 0) 
    return(FALSE)
  if (is.logical(x) && !any(stats::na.omit(x))) 
    return(FALSE)
  return(TRUE)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
key_missing
list(`package:shiny` = function () 
{
  x <- list()
  class(x) <- "key_missing"
  x
}, function () 
{
  x <- list()
  class(x) <- "key_missing"
  x
})
c("package:shiny", "namespace:fastmap")
c(TRUE, FALSE)
c(FALSE, TRUE)
loadSupport
list(`package:shiny` = function (appDir = NULL, renv = new.env(parent = globalenv()), globalrenv = globalenv()) 
{
  require(shiny)
  if (is.null(appDir)) {
    appDir <- findEnclosingApp(".")
  }
  descFile <- file.path.ci(appDir, "DESCRIPTION")
  if (file.exists(file.path.ci(appDir, "NAMESPACE")) || (file.exists(descFile) && identical(as.character(read.dcf(descFile, fields = "Type")), "Package"))) {
    warning("Loading R/ subdirectory for Shiny application, but this directory appears ", "to contain an R package. Sourcing files in R/ may cause unexpected behavior.")
  }
  if (!is.null(globalrenv)) {
    globalPath <- file.path.ci(appDir, "global.R")
    if (file.exists(globalPath)) {
      withr::with_dir(appDir, {
        sourceUTF8(basename(globalPath), envir = globalrenv)
      })
    }
  }
  helpersDir <- file.path(appDir, "R")
  disabled <- list.files(helpersDir, pattern = "^_disable_autoload\\.r$", recursive = FALSE, ignore.case = TRUE)
  if (length(disabled) > 0) {
    return(invisible(renv))
  }
  helpers <- list.files(helpersDir, pattern = "\\.[rR]$", recursive = FALSE, full.names = TRUE)
  helpers <- sort_c(helpers)
  helpers <- normalizePath(helpers)
  withr::with_dir(appDir, {
    lapply(helpers, sourceUTF8, envir = renv)
  })
  invisible(renv)
}, function (appDir = NULL, renv = new.env(parent = globalenv()), globalrenv = globalenv()) 
{
  require(shiny)
  if (is.null(appDir)) {
    appDir <- findEnclosingApp(".")
  }
  descFile <- file.path.ci(appDir, "DESCRIPTION")
  if (file.exists(file.path.ci(appDir, "NAMESPACE")) || (file.exists(descFile) && identical(as.character(read.dcf(descFile, fields = "Type")), "Package"))) {
    warning("Loading R/ subdirectory for Shiny application, but this directory appears ", "to contain an R package. Sourcing files in R/ may cause unexpected behavior.")
  }
  if (!is.null(globalrenv)) {
    globalPath <- file.path.ci(appDir, "global.R")
    if (file.exists(globalPath)) {
      withr::with_dir(appDir, {
        sourceUTF8(basename(globalPath), envir = globalrenv)
      })
    }
  }
  helpersDir <- file.path(appDir, "R")
  disabled <- list.files(helpersDir, pattern = "^_disable_autoload\\.r$", recursive = FALSE, ignore.case = TRUE)
  if (length(disabled) > 0) {
    return(invisible(renv))
  }
  helpers <- list.files(helpersDir, pattern = "\\.[rR]$", recursive = FALSE, full.names = TRUE)
  helpers <- sort_c(helpers)
  helpers <- normalizePath(helpers)
  withr::with_dir(appDir, {
    lapply(helpers, sourceUTF8, envir = renv)
  })
  invisible(renv)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
mainPanel
list(`package:shiny` = function (..., width = 8) 
{
  div(class = paste0("col-sm-", width), role = "main", ...)
}, function (..., width = 8) 
{
  div(class = paste0("col-sm-", width), role = "main", ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
makeReactiveBinding
list(`package:shiny` = function (symbol, env = parent.frame()) 
{
  if (exists(symbol, envir = env, inherits = FALSE)) {
    initialValue <- env[[symbol]]
    rm(list = symbol, envir = env, inherits = FALSE)
  }
  else {
    initialValue <- NULL
  }
  val <- reactiveVal(initialValue, label = symbol)
  makeActiveBinding(symbol, val, env = env)
  invisible()
}, function (symbol, env = parent.frame()) 
{
  if (exists(symbol, envir = env, inherits = FALSE)) {
    initialValue <- env[[symbol]]
    rm(list = symbol, envir = env, inherits = FALSE)
  }
  else {
    initialValue <- NULL
  }
  val <- reactiveVal(initialValue, label = symbol)
  makeActiveBinding(symbol, val, env = env)
  invisible()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
markdown
list(`package:shiny` = function (mds, extensions = TRUE, .noWS = NULL, ...) 
{
  html <- rlang::exec(commonmark::markdown_html, glue::trim(mds), extensions = extensions, ...)
  htmltools::HTML(html, .noWS = .noWS)
}, function (mds, extensions = TRUE, .noWS = NULL, ...) 
{
  html <- rlang::exec(commonmark::markdown_html, glue::trim(mds), extensions = extensions, ...)
  htmltools::HTML(html, .noWS = .noWS)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
markRenderFunction
list(`package:shiny` = function (uiFunc, renderFunc, outputArgs = list(), cacheHint = "auto", cacheWriteHook = NULL, cacheReadHook = NULL) 
{
  force(renderFunc)
  hasExecuted <- Mutable$new()
  hasExecuted$set(FALSE)
  if (is.null(uiFunc)) {
    uiFunc <- function(id) {
      pre("No UI/output function provided for render function. ", "Please see ?shiny::markRenderFunction and ?shiny::createRenderFunction.")
    }
  }
  if (identical(cacheHint, "auto")) {
    origUserFunc <- attr(renderFunc, "wrappedFunc", exact = TRUE)
    if (is.null(origUserFunc)) {
      cacheHint <- NULL
    }
    else {
      cacheHint <- list(origUserFunc = origUserFunc, renderFunc = renderFunc, outputFunc = uiFunc)
    }
  }
  if (!is.null(cacheHint) && !is_false(cacheHint)) {
    if (!is.list(cacheHint)) {
      cacheHint <- list(cacheHint)
    }
    cacheHint <- lapply(cacheHint, function(x) {
      if (is.function(x)) 
        formalsAndBody(x)
      else if (is_quosure(x)) 
        zap_srcref(quo_get_expr(x))
      else if (is.language(x)) 
        zap_srcref(x)
      else x
    })
  }
  wrappedRenderFunc <- function(...) {
    if (length(outputArgs) != 0 && !hasExecuted$get()) {
      warning("Unused argument: outputArgs. The argument outputArgs is only ", "meant to be used when embedding snippets of Shiny code in an ", "R Markdown code chunk (using runtime: shiny). When running a ", "full Shiny app, please set the output arguments directly in ", "the corresponding output function of your UI code.")
      hasExecuted$set(TRUE)
    }
    if (is.null(formals(renderFunc))) 
      renderFunc()
    else renderFunc(...)
  }
  structure(wrappedRenderFunc, class = c("shiny.render.function", "function"), outputFunc = uiFunc, outputArgs = outputArgs, hasExecuted = hasExecuted, cacheHint = cacheHint, cacheWriteHook = cacheWriteHook, cacheReadHook = cacheReadHook)
}, function (uiFunc, renderFunc, outputArgs = list(), cacheHint = "auto", cacheWriteHook = NULL, cacheReadHook = NULL) 
{
  force(renderFunc)
  hasExecuted <- Mutable$new()
  hasExecuted$set(FALSE)
  if (is.null(uiFunc)) {
    uiFunc <- function(id) {
      pre("No UI/output function provided for render function. ", "Please see ?shiny::markRenderFunction and ?shiny::createRenderFunction.")
    }
  }
  if (identical(cacheHint, "auto")) {
    origUserFunc <- attr(renderFunc, "wrappedFunc", exact = TRUE)
    if (is.null(origUserFunc)) {
      cacheHint <- NULL
    }
    else {
      cacheHint <- list(origUserFunc = origUserFunc, renderFunc = renderFunc, outputFunc = uiFunc)
    }
  }
  if (!is.null(cacheHint) && !is_false(cacheHint)) {
    if (!is.list(cacheHint)) {
      cacheHint <- list(cacheHint)
    }
    cacheHint <- lapply(cacheHint, function(x) {
      if (is.function(x)) 
        formalsAndBody(x)
      else if (is_quosure(x)) 
        zap_srcref(quo_get_expr(x))
      else if (is.language(x)) 
        zap_srcref(x)
      else x
    })
  }
  wrappedRenderFunc <- function(...) {
    if (length(outputArgs) != 0 && !hasExecuted$get()) {
      warning("Unused argument: outputArgs. The argument outputArgs is only ", "meant to be used when embedding snippets of Shiny code in an ", "R Markdown code chunk (using runtime: shiny). When running a ", "full Shiny app, please set the output arguments directly in ", "the corresponding output function of your UI code.")
      hasExecuted$set(TRUE)
    }
    if (is.null(formals(renderFunc))) 
      renderFunc()
    else renderFunc(...)
  }
  structure(wrappedRenderFunc, class = c("shiny.render.function", "function"), outputFunc = uiFunc, outputArgs = outputArgs, hasExecuted = hasExecuted, cacheHint = cacheHint, cacheWriteHook = cacheWriteHook, cacheReadHook = cacheReadHook)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
maskReactiveContext
list(`package:shiny` = function (expr) 
{
  .getReactiveEnvironment()$runWith(NULL, function() {
    expr
  })
}, function (expr) 
{
  .getReactiveEnvironment()$runWith(NULL, function() {
    expr
  })
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
memoryCache
list(`package:shiny` = function (max_size = 200 * 1024^2, max_age = Inf, max_n = Inf, evict = c("lru", "fifo"), missing = key_missing(), exec_missing = deprecated(), logfile = NULL) 
{
  shinyDeprecated("1.6.0", "diskCache()", "cachem::cache_mem()")
  if (is_present(exec_missing)) {
    shinyDeprecated("1.6.0", "diskCache(exec_missing =)")
  }
  cachem::cache_mem(max_size = max_size, max_age = max_age, max_n = max_n, evict = evict, missing = missing, logfile = logfile)
}, function (max_size = 200 * 1024^2, max_age = Inf, max_n = Inf, evict = c("lru", "fifo"), missing = key_missing(), exec_missing = deprecated(), logfile = NULL) 
{
  shinyDeprecated("1.6.0", "diskCache()", "cachem::cache_mem()")
  if (is_present(exec_missing)) {
    shinyDeprecated("1.6.0", "diskCache(exec_missing =)")
  }
  cachem::cache_mem(max_size = max_size, max_age = max_age, max_n = max_n, evict = evict, missing = missing, logfile = logfile)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
MockShinySession
list(`package:shiny` = <environment>, <environment>)
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
modalButton
list(`package:shiny` = function (label, icon = NULL) 
{
  tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", `data-bs-dismiss` = "modal", validateIcon(icon), label)
}, function (label, icon = NULL) 
{
  tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", `data-bs-dismiss` = "modal", validateIcon(icon), label)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
modalDialog
list(`package:shiny` = function (..., title = NULL, footer = modalButton("Dismiss"), size = c("m", "s", "l", "xl"), easyClose = FALSE, fade = TRUE) 
{
  size <- match.arg(size)
  backdrop <- if (!easyClose) 
    "static"
  keyboard <- if (!easyClose) 
    "false"
  div(id = "shiny-modal", class = "modal", class = if (fade) 
    "fade", tabindex = "-1", `data-backdrop` = backdrop, `data-bs-backdrop` = backdrop, `data-keyboard` = keyboard, `data-bs-keyboard` = keyboard, div(class = "modal-dialog", class = switch(size, s = "modal-sm", m = NULL, l = "modal-lg", xl = "modal-xl"), div(class = "modal-content", if (!is.null(title)) 
      div(class = "modal-header", tags$h4(class = "modal-title", title)), div(class = "modal-body", ...), if (!is.null(footer)) 
        div(class = "modal-footer", footer))), tags$script(HTML("if (window.bootstrap && !window.bootstrap.Modal.VERSION.match(/^4\\./)) {\n         var modal = new bootstrap.Modal(document.getElementById('shiny-modal'));\n         modal.show();\n      } else {\n         $('#shiny-modal').modal().focus();\n      }")))
}, function (..., title = NULL, footer = modalButton("Dismiss"), size = c("m", "s", "l", "xl"), easyClose = FALSE, fade = TRUE) 
{
  size <- match.arg(size)
  backdrop <- if (!easyClose) 
    "static"
  keyboard <- if (!easyClose) 
    "false"
  div(id = "shiny-modal", class = "modal", class = if (fade) 
    "fade", tabindex = "-1", `data-backdrop` = backdrop, `data-bs-backdrop` = backdrop, `data-keyboard` = keyboard, `data-bs-keyboard` = keyboard, div(class = "modal-dialog", class = switch(size, s = "modal-sm", m = NULL, l = "modal-lg", xl = "modal-xl"), div(class = "modal-content", if (!is.null(title)) 
      div(class = "modal-header", tags$h4(class = "modal-title", title)), div(class = "modal-body", ...), if (!is.null(footer)) 
        div(class = "modal-footer", footer))), tags$script(HTML("if (window.bootstrap && !window.bootstrap.Modal.VERSION.match(/^4\\./)) {\n         var modal = new bootstrap.Modal(document.getElementById('shiny-modal'));\n         modal.show();\n      } else {\n         $('#shiny-modal').modal().focus();\n      }")))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
moduleServer
list(`package:shiny` = function (id, module, session = getDefaultReactiveDomain()) 
{
  if (inherits(session, "MockShinySession")) {
    body(module) <- rlang::expr({
      session$setEnv(base::environment())
      !!body(module)
    })
    session$setReturned(callModule(module, id, session = session))
  }
  else {
    callModule(module, id, session = session)
  }
}, function (id, module, session = getDefaultReactiveDomain()) 
{
  if (inherits(session, "MockShinySession")) {
    body(module) <- rlang::expr({
      session$setEnv(base::environment())
      !!body(module)
    })
    session$setReturned(callModule(module, id, session = session))
  }
  else {
    callModule(module, id, session = session)
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
navbarMenu
list(`package:shiny` = function (title, ..., menuName = title, icon = NULL) 
{
  bslib::nav_menu(title, ..., value = menuName, icon = icon)
}, function (title, ..., menuName = title, icon = NULL) 
{
  bslib::nav_menu(title, ..., value = menuName, icon = icon)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
navbarPage
list(`package:shiny` = function (title, ..., id = NULL, selected = NULL, position = c("static-top", "fixed-top", "fixed-bottom"), header = NULL, footer = NULL, inverse = FALSE, collapsible = FALSE, fluid = TRUE, theme = NULL, windowTitle = NA, lang = NULL) 
{
  remove_first_class(bslib::page_navbar(..., title = title, id = id, selected = selected, position = match.arg(position), header = header, footer = footer, inverse = inverse, collapsible = collapsible, fluid = fluid, theme = theme, window_title = windowTitle, lang = lang))
}, function (title, ..., id = NULL, selected = NULL, position = c("static-top", "fixed-top", "fixed-bottom"), header = NULL, footer = NULL, inverse = FALSE, collapsible = FALSE, fluid = TRUE, theme = NULL, windowTitle = NA, lang = NULL) 
{
  remove_first_class(bslib::page_navbar(..., title = title, id = id, selected = selected, position = match.arg(position), header = header, footer = footer, inverse = inverse, collapsible = collapsible, fluid = fluid, theme = theme, window_title = windowTitle, lang = lang))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
navlistPanel
list(`package:shiny` = function (..., id = NULL, selected = NULL, header = NULL, footer = NULL, well = TRUE, fluid = TRUE, widths = c(4, 8)) 
{
  remove_first_class(bslib::navs_pill_list(..., id = id, selected = selected, header = header, footer = footer, well = well, fluid = fluid, widths = widths))
}, function (..., id = NULL, selected = NULL, header = NULL, footer = NULL, well = TRUE, fluid = TRUE, widths = c(4, 8)) 
{
  remove_first_class(bslib::navs_pill_list(..., id = id, selected = selected, header = header, footer = footer, well = well, fluid = fluid, widths = widths))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
nearPoints
list(`package:shiny` = function (df, coordinfo, xvar = NULL, yvar = NULL, panelvar1 = NULL, panelvar2 = NULL, threshold = 5, maxpoints = NULL, addDist = FALSE, allRows = FALSE) 
{
  if (is.null(coordinfo)) {
    if (addDist) 
      df$dist_ <- NA
    if (allRows) 
      df$selected_ <- FALSE
    else df <- df[0, , drop = FALSE]
    return(df)
  }
  if (is.null(coordinfo$x)) {
    stop("nearPoints requires a click/hover/double-click object with x and y values.")
  }
  if (is_na(xvar)) {
    xvar <- NULL
    warning("xvar should be NULL, not NA.")
  }
  if (is_na(yvar)) {
    yvar <- NULL
    warning("yvar should be NULL, not NA.")
  }
  if (is_na(panelvar1)) {
    panelvar1 <- NULL
    warning("panelvar1 should be NULL, not NA.")
  }
  if (is_na(panelvar2)) {
    panelvar2 <- NULL
    warning("panelvar2 should be NULL, not NA.")
  }
  xvar <- xvar %||% coordinfo$mapping$x
  yvar <- yvar %||% coordinfo$mapping$y
  panelvar1 <- panelvar1 %||% coordinfo$mapping$panelvar1
  panelvar2 <- panelvar2 %||% coordinfo$mapping$panelvar2
  if (is.null(xvar)) 
    stop("nearPoints: not able to automatically infer `xvar` from coordinfo")
  if (is.null(yvar)) 
    stop("nearPoints: not able to automatically infer `yvar` from coordinfo")
  if (!(xvar %in% names(df))) 
    stop("nearPoints: `xvar` ('", xvar, "')  not in names of input")
  if (!(yvar %in% names(df))) 
    stop("nearPoints: `yvar` ('", yvar, "')  not in names of input")
  coordinfo <- fortifyDiscreteLimits(coordinfo)
  x <- asNumber(df[[xvar]], coordinfo$domain$discrete_limits$x)
  y <- asNumber(df[[yvar]], coordinfo$domain$discrete_limits$y)
  point_img <- coordinfo$coords_img
  data_img <- scaleCoords(x, y, coordinfo)
  dist_css <- list(x = (data_img$x - point_img$x)/coordinfo$img_css_ratio$x, y = (data_img$y - point_img$y)/coordinfo$img_css_ratio$y)
  dists <- sqrt(dist_css$x^2 + dist_css$y^2)
  if (addDist) 
    df$dist_ <- dists
  keep_rows <- (dists <= threshold)
  if (!is.null(panelvar1)) 
    keep_rows <- keep_rows & panelMatch(coordinfo$panelvar1, df[[panelvar1]])
  if (!is.null(panelvar2)) 
    keep_rows <- keep_rows & panelMatch(coordinfo$panelvar2, df[[panelvar2]])
  keep_idx <- which(keep_rows)
  dists <- dists[keep_idx]
  keep_idx <- keep_idx[order(dists)]
  if (!is.null(maxpoints) && length(keep_idx) > maxpoints) {
    keep_idx <- keep_idx[seq_len(maxpoints)]
  }
  if (allRows) {
    df$selected_ <- FALSE
    df$selected_[keep_idx] <- TRUE
  }
  else {
    df <- df[keep_idx, , drop = FALSE]
  }
  df
}, function (df, coordinfo, xvar = NULL, yvar = NULL, panelvar1 = NULL, panelvar2 = NULL, threshold = 5, maxpoints = NULL, addDist = FALSE, allRows = FALSE) 
{
  if (is.null(coordinfo)) {
    if (addDist) 
      df$dist_ <- NA
    if (allRows) 
      df$selected_ <- FALSE
    else df <- df[0, , drop = FALSE]
    return(df)
  }
  if (is.null(coordinfo$x)) {
    stop("nearPoints requires a click/hover/double-click object with x and y values.")
  }
  if (is_na(xvar)) {
    xvar <- NULL
    warning("xvar should be NULL, not NA.")
  }
  if (is_na(yvar)) {
    yvar <- NULL
    warning("yvar should be NULL, not NA.")
  }
  if (is_na(panelvar1)) {
    panelvar1 <- NULL
    warning("panelvar1 should be NULL, not NA.")
  }
  if (is_na(panelvar2)) {
    panelvar2 <- NULL
    warning("panelvar2 should be NULL, not NA.")
  }
  xvar <- xvar %||% coordinfo$mapping$x
  yvar <- yvar %||% coordinfo$mapping$y
  panelvar1 <- panelvar1 %||% coordinfo$mapping$panelvar1
  panelvar2 <- panelvar2 %||% coordinfo$mapping$panelvar2
  if (is.null(xvar)) 
    stop("nearPoints: not able to automatically infer `xvar` from coordinfo")
  if (is.null(yvar)) 
    stop("nearPoints: not able to automatically infer `yvar` from coordinfo")
  if (!(xvar %in% names(df))) 
    stop("nearPoints: `xvar` ('", xvar, "')  not in names of input")
  if (!(yvar %in% names(df))) 
    stop("nearPoints: `yvar` ('", yvar, "')  not in names of input")
  coordinfo <- fortifyDiscreteLimits(coordinfo)
  x <- asNumber(df[[xvar]], coordinfo$domain$discrete_limits$x)
  y <- asNumber(df[[yvar]], coordinfo$domain$discrete_limits$y)
  point_img <- coordinfo$coords_img
  data_img <- scaleCoords(x, y, coordinfo)
  dist_css <- list(x = (data_img$x - point_img$x)/coordinfo$img_css_ratio$x, y = (data_img$y - point_img$y)/coordinfo$img_css_ratio$y)
  dists <- sqrt(dist_css$x^2 + dist_css$y^2)
  if (addDist) 
    df$dist_ <- dists
  keep_rows <- (dists <= threshold)
  if (!is.null(panelvar1)) 
    keep_rows <- keep_rows & panelMatch(coordinfo$panelvar1, df[[panelvar1]])
  if (!is.null(panelvar2)) 
    keep_rows <- keep_rows & panelMatch(coordinfo$panelvar2, df[[panelvar2]])
  keep_idx <- which(keep_rows)
  dists <- dists[keep_idx]
  keep_idx <- keep_idx[order(dists)]
  if (!is.null(maxpoints) && length(keep_idx) > maxpoints) {
    keep_idx <- keep_idx[seq_len(maxpoints)]
  }
  if (allRows) {
    df$selected_ <- FALSE
    df$selected_[keep_idx] <- TRUE
  }
  else {
    df <- df[keep_idx, , drop = FALSE]
  }
  df
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
need
list(`package:shiny` = function (expr, message = paste(label, "must be provided"), label) 
{
  force(message)
  if (!isTruthy(expr)) 
    return(message)
  else return(invisible(NULL))
}, function (expr, message = paste(label, "must be provided"), label) 
{
  force(message)
  if (!isTruthy(expr)) 
    return(message)
  else return(invisible(NULL))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
NS
list(`package:shiny` = function (namespace, id = NULL) 
{
  if (length(namespace) == 0) 
    ns_prefix <- character(0)
  else ns_prefix <- paste(namespace, collapse = ns.sep)
  f <- function(id) {
    if (length(id) == 0) 
      return(ns_prefix)
    if (length(ns_prefix) == 0) 
      return(id)
    paste(ns_prefix, id, sep = ns.sep)
  }
  if (missing(id)) {
    f
  }
  else {
    f(id)
  }
}, function (namespace, id = NULL) 
{
  if (length(namespace) == 0) 
    ns_prefix <- character(0)
  else ns_prefix <- paste(namespace, collapse = ns.sep)
  f <- function(id) {
    if (length(id) == 0) 
      return(ns_prefix)
    if (length(ns_prefix) == 0) 
      return(id)
    paste(ns_prefix, id, sep = ns.sep)
  }
  if (missing(id)) {
    f
  }
  else {
    f(id)
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
ns.sep
list(`package:shiny` = "-", "-")
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
numericInput
list(`package:shiny` = function (inputId, label, value, min = NA, max = NA, step = NA, width = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "number", class = "form-control", value = formatNoSci(value))
  if (!is.na(min)) 
    inputTag$attribs$min = min
  if (!is.na(max)) 
    inputTag$attribs$max = max
  if (!is.na(step)) 
    inputTag$attribs$step = step
  div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), inputTag)
}, function (inputId, label, value, min = NA, max = NA, step = NA, width = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "number", class = "form-control", value = formatNoSci(value))
  if (!is.na(min)) 
    inputTag$attribs$min = min
  if (!is.na(max)) 
    inputTag$attribs$max = max
  if (!is.na(step)) 
    inputTag$attribs$step = step
  div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), inputTag)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
observe
list(`package:shiny` = function (x, env = parent.frame(), quoted = FALSE, ..., label = NULL, suspended = FALSE, priority = 0, domain = getDefaultReactiveDomain(), autoDestroy = TRUE, ..stacktraceon = TRUE) 
{
  check_dots_empty()
  func <- installExprFunction(x, "func", env, quoted)
  label <- funcToLabel(func, "observe", label)
  o <- Observer$new(func, label = label, suspended = suspended, priority = priority, domain = domain, autoDestroy = autoDestroy, ..stacktraceon = ..stacktraceon)
  invisible(o)
}, function (x, env = parent.frame(), quoted = FALSE, ..., label = NULL, suspended = FALSE, priority = 0, domain = getDefaultReactiveDomain(), autoDestroy = TRUE, ..stacktraceon = TRUE) 
{
  check_dots_empty()
  func <- installExprFunction(x, "func", env, quoted)
  label <- funcToLabel(func, "observe", label)
  o <- Observer$new(func, label = label, suspended = suspended, priority = priority, domain = domain, autoDestroy = autoDestroy, ..stacktraceon = ..stacktraceon)
  invisible(o)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
observeEvent
list(`package:shiny` = function (eventExpr, handlerExpr, event.env = parent.frame(), event.quoted = FALSE, handler.env = parent.frame(), handler.quoted = FALSE, ..., label = NULL, suspended = FALSE, priority = 0, domain = getDefaultReactiveDomain(), autoDestroy = TRUE, ignoreNULL = TRUE, ignoreInit = FALSE, once = FALSE) 
{
  check_dots_empty()
  eventQ <- exprToQuo(eventExpr, event.env, event.quoted)
  handlerQ <- exprToQuo(handlerExpr, handler.env, handler.quoted)
  label <- quoToLabel(eventQ, "observeEvent", label)
  handler <- inject(observe(!!handlerQ, label = label, suspended = suspended, priority = priority, domain = domain, autoDestroy = TRUE, ..stacktraceon = FALSE))
  o <- inject(bindEvent(ignoreNULL = ignoreNULL, ignoreInit = ignoreInit, once = once, label = label, !!eventQ, x = handler))
  invisible(o)
}, function (eventExpr, handlerExpr, event.env = parent.frame(), event.quoted = FALSE, handler.env = parent.frame(), handler.quoted = FALSE, ..., label = NULL, suspended = FALSE, priority = 0, domain = getDefaultReactiveDomain(), autoDestroy = TRUE, ignoreNULL = TRUE, ignoreInit = FALSE, once = FALSE) 
{
  check_dots_empty()
  eventQ <- exprToQuo(eventExpr, event.env, event.quoted)
  handlerQ <- exprToQuo(handlerExpr, handler.env, handler.quoted)
  label <- quoToLabel(eventQ, "observeEvent", label)
  handler <- inject(observe(!!handlerQ, label = label, suspended = suspended, priority = priority, domain = domain, autoDestroy = TRUE, ..stacktraceon = FALSE))
  o <- inject(bindEvent(ignoreNULL = ignoreNULL, ignoreInit = ignoreInit, once = once, label = label, !!eventQ, x = handler))
  invisible(o)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
onBookmark
list(`package:shiny` = function (fun, session = getDefaultReactiveDomain()) 
{
  session$onBookmark(fun)
}, function (fun, session = getDefaultReactiveDomain()) 
{
  session$onBookmark(fun)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
onBookmarked
list(`package:shiny` = function (fun, session = getDefaultReactiveDomain()) 
{
  session$onBookmarked(fun)
}, function (fun, session = getDefaultReactiveDomain()) 
{
  session$onBookmarked(fun)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
onFlush
list(`package:shiny` = function (fun, once = TRUE, session = getDefaultReactiveDomain()) 
{
  session$onFlush(fun, once = once)
}, function (fun, once = TRUE, session = getDefaultReactiveDomain()) 
{
  session$onFlush(fun, once = once)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
onFlushed
list(`package:shiny` = function (fun, once = TRUE, session = getDefaultReactiveDomain()) 
{
  session$onFlushed(fun, once = once)
}, function (fun, once = TRUE, session = getDefaultReactiveDomain()) 
{
  session$onFlushed(fun, once = once)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
onReactiveDomainEnded
list(`package:shiny` = function (domain, callback, failIfNull = FALSE) 
{
  if (is.null(domain)) {
    if (isTRUE(failIfNull)) 
      stop("onReactiveDomainEnded called with null domain and failIfNull=TRUE")
    else return()
  }
  domain$onEnded(callback)
}, function (domain, callback, failIfNull = FALSE) 
{
  if (is.null(domain)) {
    if (isTRUE(failIfNull)) 
      stop("onReactiveDomainEnded called with null domain and failIfNull=TRUE")
    else return()
  }
  domain$onEnded(callback)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
onRestore
list(`package:shiny` = function (fun, session = getDefaultReactiveDomain()) 
{
  session$onRestore(fun)
}, function (fun, session = getDefaultReactiveDomain()) 
{
  session$onRestore(fun)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
onRestored
list(`package:shiny` = function (fun, session = getDefaultReactiveDomain()) 
{
  session$onRestored(fun)
}, function (fun, session = getDefaultReactiveDomain()) 
{
  session$onRestored(fun)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
onSessionEnded
list(`package:shiny` = function (fun, session = getDefaultReactiveDomain()) 
{
  session$onSessionEnded(fun)
}, function (fun, session = getDefaultReactiveDomain()) 
{
  session$onSessionEnded(fun)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
onStop
list(`package:shiny` = function (fun, session = getDefaultReactiveDomain()) 
{
  if (is.null(session)) {
    return(.globals$onStopCallbacks$register(fun))
  }
  else {
    return(session$onSessionEnded(fun))
  }
}, function (fun, session = getDefaultReactiveDomain()) 
{
  if (is.null(session)) {
    return(.globals$onStopCallbacks$register(fun))
  }
  else {
    return(session$onSessionEnded(fun))
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
outputOptions
list(`package:shiny` = function (x, name, ...) 
{
  if (!inherits(x, "shinyoutput")) {
    stop("x must be a shinyoutput object.")
  }
  if (!missing(name)) {
    name <- .subset2(x, "ns")(name)
  }
  else {
    name <- NULL
  }
  .subset2(x, "impl")$outputOptions(name, ...)
}, function (x, name, ...) 
{
  if (!inherits(x, "shinyoutput")) {
    stop("x must be a shinyoutput object.")
  }
  if (!missing(name)) {
    name <- .subset2(x, "ns")(name)
  }
  else {
    name <- NULL
  }
  .subset2(x, "impl")$outputOptions(name, ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
p
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("p", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("p", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
pageWithSidebar
list(`package:shiny` = function (headerPanel, sidebarPanel, mainPanel) 
{
  bootstrapPage(div(class = "container-fluid", div(class = "row", headerPanel), div(class = "row", sidebarPanel, mainPanel)))
}, function (headerPanel, sidebarPanel, mainPanel) 
{
  bootstrapPage(div(class = "container-fluid", div(class = "row", headerPanel), div(class = "row", sidebarPanel, mainPanel)))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
paneViewer
list(`package:shiny` = function (minHeight = NULL) 
{
  viewer <- getOption("viewer")
  if (is.null(viewer)) {
    utils::browseURL
  }
  else {
    function(url) {
      viewer(url, minHeight)
    }
  }
}, function (minHeight = NULL) 
{
  viewer <- getOption("viewer")
  if (is.null(viewer)) {
    utils::browseURL
  }
  else {
    function(url) {
      viewer(url, minHeight)
    }
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
parseQueryString
list(`package:shiny` = function (str, nested = FALSE) 
{
  if (is.null(str) || nchar(str) == 0) 
    return(list())
  if (substr(str, 1, 1) == "?") 
    str <- substr(str, 2, nchar(str))
  pairs <- strsplit(str, "&", fixed = TRUE)[[1]]
  pairs <- pairs[pairs != ""]
  pairs <- strsplit(pairs, "=", fixed = TRUE)
  keys <- vapply(pairs, function(x) x[1], FUN.VALUE = character(1))
  values <- vapply(pairs, function(x) x[2], FUN.VALUE = character(1))
  values[is.na(values)] <- ""
  keys <- gsub("+", " ", keys, fixed = TRUE)
  values <- gsub("+", " ", values, fixed = TRUE)
  keys <- URLdecode(keys)
  values <- URLdecode(values)
  res <- stats::setNames(as.list(values), keys)
  if (!nested) 
    return(res)
  for (i in grep("\\[.+\\]", keys)) {
    k <- strsplit(keys[i], "[][]")[[1]]
    res <- assignNestedList(res, k[k != ""], values[i])
    res[[keys[i]]] <- NULL
  }
  res
}, function (str, nested = FALSE) 
{
  if (is.null(str) || nchar(str) == 0) 
    return(list())
  if (substr(str, 1, 1) == "?") 
    str <- substr(str, 2, nchar(str))
  pairs <- strsplit(str, "&", fixed = TRUE)[[1]]
  pairs <- pairs[pairs != ""]
  pairs <- strsplit(pairs, "=", fixed = TRUE)
  keys <- vapply(pairs, function(x) x[1], FUN.VALUE = character(1))
  values <- vapply(pairs, function(x) x[2], FUN.VALUE = character(1))
  values[is.na(values)] <- ""
  keys <- gsub("+", " ", keys, fixed = TRUE)
  values <- gsub("+", " ", values, fixed = TRUE)
  keys <- URLdecode(keys)
  values <- URLdecode(values)
  res <- stats::setNames(as.list(values), keys)
  if (!nested) 
    return(res)
  for (i in grep("\\[.+\\]", keys)) {
    k <- strsplit(keys[i], "[][]")[[1]]
    res <- assignNestedList(res, k[k != ""], values[i])
    res[[keys[i]]] <- NULL
  }
  res
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
passwordInput
list(`package:shiny` = function (inputId, label, value = "", width = NULL, placeholder = NULL) 
{
  div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), tags$input(id = inputId, type = "password", class = "form-control", value = value, placeholder = placeholder))
}, function (inputId, label, value = "", width = NULL, placeholder = NULL) 
{
  div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), tags$input(id = inputId, type = "password", class = "form-control", value = value, placeholder = placeholder))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
plotOutput
list(`package:shiny` = function (outputId, width = "100%", height = "400px", click = NULL, dblclick = NULL, hover = NULL, brush = NULL, inline = FALSE) 
{
  res <- imageOutput(outputId, width, height, click, dblclick, hover, brush, inline)
  res$attribs$class <- "shiny-plot-output"
  res
}, function (outputId, width = "100%", height = "400px", click = NULL, dblclick = NULL, hover = NULL, brush = NULL, inline = FALSE) 
{
  res <- imageOutput(outputId, width, height, click, dblclick, hover, brush, inline)
  res$attribs$class <- "shiny-plot-output"
  res
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
plotPNG
list(`package:shiny` = function (func, filename = tempfile(fileext = ".png"), width = 400, height = 400, res = 72, ...) 
{
  dv <- startPNG(filename, width, height, res, ...)
  on.exit(grDevices::dev.off(dv), add = TRUE)
  func()
  filename
}, function (func, filename = tempfile(fileext = ".png"), width = 400, height = 400, res = 72, ...) 
{
  dv <- startPNG(filename, width, height, res, ...)
  on.exit(grDevices::dev.off(dv), add = TRUE)
  func()
  filename
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
pre
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("pre", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("pre", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
prependTab
list(`package:shiny` = function (inputId, tab, select = FALSE, menuName = NULL, session = getDefaultReactiveDomain()) 
{
  bslib::nav_prepend(inputId, tab, menu_title = menuName, select = select, session = session)
}, function (inputId, tab, select = FALSE, menuName = NULL, session = getDefaultReactiveDomain()) 
{
  bslib::nav_prepend(inputId, tab, menu_title = menuName, select = select, session = session)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
printError
list(`package:shiny` = function (cond, full = get_devmode_option("shiny.fullstacktrace", FALSE), offset = getOption("shiny.stacktraceoffset", TRUE)) 
{
  warning(call. = FALSE, immediate. = TRUE, sprintf("Error in %s: %s", getCallNames(list(conditionCall(cond))), conditionMessage(cond)))
  printStackTrace(cond, full = full, offset = offset)
}, function (cond, full = get_devmode_option("shiny.fullstacktrace", FALSE), offset = getOption("shiny.stacktraceoffset", TRUE)) 
{
  warning(call. = FALSE, immediate. = TRUE, sprintf("Error in %s: %s", getCallNames(list(conditionCall(cond))), conditionMessage(cond)))
  printStackTrace(cond, full = full, offset = offset)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
printStackTrace
list(`package:shiny` = function (cond, full = get_devmode_option("shiny.fullstacktrace", FALSE), offset = getOption("shiny.stacktraceoffset", TRUE)) 
{
  should_drop <- !full
  should_strip <- !full
  should_prune <- !full
  stackTraceCalls <- c(attr(cond, "deep.stack.trace", exact = TRUE), list(attr(cond, "stack.trace", exact = TRUE)))
  stackTraceParents <- lapply(stackTraceCalls, attr, which = "parents", exact = TRUE)
  stackTraceCallNames <- lapply(stackTraceCalls, getCallNames)
  stackTraceCalls <- lapply(stackTraceCalls, offsetSrcrefs, offset = offset)
  if (should_drop) {
    toKeep <- lapply(stackTraceCallNames, dropTrivialFrames)
    stackTraceCalls <- mapply(stackTraceCalls, FUN = `[`, toKeep, SIMPLIFY = FALSE)
    stackTraceCallNames <- mapply(stackTraceCallNames, FUN = `[`, toKeep, SIMPLIFY = FALSE)
    stackTraceParents <- mapply(stackTraceParents, FUN = `[`, toKeep, SIMPLIFY = FALSE)
  }
  delayedAssign("all_true", {
    lapply(stackTraceCallNames, function(st) {
      rep_len(TRUE, length(st))
    })
  })
  toShow <- mapply(if (should_strip) 
    stripStackTraces(stackTraceCallNames)
    else all_true, if (should_prune) 
      lapply(stackTraceParents, pruneStackTrace)
    else all_true, FUN = `&`, SIMPLIFY = FALSE)
  dfs <- mapply(seq_along(stackTraceCalls), rev(stackTraceCalls), rev(stackTraceCallNames), rev(toShow), FUN = function(i, calls, nms, index) {
    st <- data.frame(num = rev(which(index)), call = rev(nms[index]), loc = rev(getLocs(calls[index])), category = rev(getCallCategories(calls[index])), stringsAsFactors = FALSE)
    if (i != 1) {
      message("From earlier call:")
    }
    if (nrow(st) == 0) {
      message("  [No stack trace available]")
    }
    else {
      width <- floor(log10(max(st$num))) + 1
      formatted <- paste0("  ", formatC(st$num, width = width), ": ", mapply(paste0(st$call, st$loc), st$category, FUN = function(name, category) {
        if (category == "pkg") 
          crayon::silver(name)
        else if (category == "user") 
          crayon::blue$bold(name)
        else crayon::white(name)
      }), "\n")
      cat(file = stderr(), formatted, sep = "")
    }
    st
  }, SIMPLIFY = FALSE)
  invisible()
}, function (cond, full = get_devmode_option("shiny.fullstacktrace", FALSE), offset = getOption("shiny.stacktraceoffset", TRUE)) 
{
  should_drop <- !full
  should_strip <- !full
  should_prune <- !full
  stackTraceCalls <- c(attr(cond, "deep.stack.trace", exact = TRUE), list(attr(cond, "stack.trace", exact = TRUE)))
  stackTraceParents <- lapply(stackTraceCalls, attr, which = "parents", exact = TRUE)
  stackTraceCallNames <- lapply(stackTraceCalls, getCallNames)
  stackTraceCalls <- lapply(stackTraceCalls, offsetSrcrefs, offset = offset)
  if (should_drop) {
    toKeep <- lapply(stackTraceCallNames, dropTrivialFrames)
    stackTraceCalls <- mapply(stackTraceCalls, FUN = `[`, toKeep, SIMPLIFY = FALSE)
    stackTraceCallNames <- mapply(stackTraceCallNames, FUN = `[`, toKeep, SIMPLIFY = FALSE)
    stackTraceParents <- mapply(stackTraceParents, FUN = `[`, toKeep, SIMPLIFY = FALSE)
  }
  delayedAssign("all_true", {
    lapply(stackTraceCallNames, function(st) {
      rep_len(TRUE, length(st))
    })
  })
  toShow <- mapply(if (should_strip) 
    stripStackTraces(stackTraceCallNames)
    else all_true, if (should_prune) 
      lapply(stackTraceParents, pruneStackTrace)
    else all_true, FUN = `&`, SIMPLIFY = FALSE)
  dfs <- mapply(seq_along(stackTraceCalls), rev(stackTraceCalls), rev(stackTraceCallNames), rev(toShow), FUN = function(i, calls, nms, index) {
    st <- data.frame(num = rev(which(index)), call = rev(nms[index]), loc = rev(getLocs(calls[index])), category = rev(getCallCategories(calls[index])), stringsAsFactors = FALSE)
    if (i != 1) {
      message("From earlier call:")
    }
    if (nrow(st) == 0) {
      message("  [No stack trace available]")
    }
    else {
      width <- floor(log10(max(st$num))) + 1
      formatted <- paste0("  ", formatC(st$num, width = width), ": ", mapply(paste0(st$call, st$loc), st$category, FUN = function(name, category) {
        if (category == "pkg") 
          crayon::silver(name)
        else if (category == "user") 
          crayon::blue$bold(name)
        else crayon::white(name)
      }), "\n")
      cat(file = stderr(), formatted, sep = "")
    }
    st
  }, SIMPLIFY = FALSE)
  invisible()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
Progress
list(`package:shiny` = <environment>, <environment>, <environment>)
c("package:shiny", "namespace:shiny", "namespace:dplyr")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
quoToFunction
list(`package:shiny` = function (q, label = sys.call(-1)[[1]], ..stacktraceon = FALSE) 
{
  func <- quoToSimpleFunction(as_quosure(q))
  wrapFunctionLabel(func, updateFunctionLabel(label), ..stacktraceon = ..stacktraceon, dots = FALSE)
}, function (q, label = sys.call(-1)[[1]], ..stacktraceon = FALSE) 
{
  func <- quoToSimpleFunction(as_quosure(q))
  wrapFunctionLabel(func, updateFunctionLabel(label), ..stacktraceon = ..stacktraceon, dots = FALSE)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
radioButtons
list(`package:shiny` = function (inputId, label, choices = NULL, selected = NULL, inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL) 
{
  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues)
  selected <- restoreInput(id = inputId, default = selected)
  selected <- if (is.null(selected)) 
    args$choiceValues[[1]]
  else as.character(selected)
  if (length(selected) > 1) 
    stop("The 'selected' argument must be of length 1")
  options <- generateOptions(inputId, selected, inline, "radio", args$choiceNames, args$choiceValues)
  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline) 
    divClass <- paste(divClass, "shiny-input-container-inline")
  inputLabel <- shinyInputLabel(inputId, label)
  tags$div(id = inputId, style = css(width = validateCssUnit(width)), class = divClass, role = "radiogroup", `aria-labelledby` = inputLabel$attribs$id, inputLabel, options)
}, function (inputId, label, choices = NULL, selected = NULL, inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL) 
{
  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues)
  selected <- restoreInput(id = inputId, default = selected)
  selected <- if (is.null(selected)) 
    args$choiceValues[[1]]
  else as.character(selected)
  if (length(selected) > 1) 
    stop("The 'selected' argument must be of length 1")
  options <- generateOptions(inputId, selected, inline, "radio", args$choiceNames, args$choiceValues)
  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline) 
    divClass <- paste(divClass, "shiny-input-container-inline")
  inputLabel <- shinyInputLabel(inputId, label)
  tags$div(id = inputId, style = css(width = validateCssUnit(width)), class = divClass, role = "radiogroup", `aria-labelledby` = inputLabel$attribs$id, inputLabel, options)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
reactive
list(`package:shiny` = function (x, env = parent.frame(), quoted = FALSE, ..., label = NULL, domain = getDefaultReactiveDomain(), ..stacktraceon = TRUE) 
{
  check_dots_empty()
  func <- installExprFunction(x, "func", env, quoted, wrappedWithLabel = FALSE)
  userExpr <- fn_body(func)
  label <- exprToLabel(userExpr, "reactive", label)
  o <- Observable$new(func, label, domain, ..stacktraceon = ..stacktraceon)
  structure(o$getValue, observable = o, cacheHint = list(userExpr = zap_srcref(userExpr)), class = c("reactiveExpr", "reactive", "function"))
}, function (x, env = parent.frame(), quoted = FALSE, ..., label = NULL, domain = getDefaultReactiveDomain(), ..stacktraceon = TRUE) 
{
  check_dots_empty()
  func <- installExprFunction(x, "func", env, quoted, wrappedWithLabel = FALSE)
  userExpr <- fn_body(func)
  label <- exprToLabel(userExpr, "reactive", label)
  o <- Observable$new(func, label, domain, ..stacktraceon = ..stacktraceon)
  structure(o$getValue, observable = o, cacheHint = list(userExpr = zap_srcref(userExpr)), class = c("reactiveExpr", "reactive", "function"))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
reactiveConsole
list(`package:shiny` = function (enabled) 
{
  options(shiny.suppressMissingContextError = enabled)
  setAutoflush(enabled)
}, function (enabled) 
{
  options(shiny.suppressMissingContextError = enabled)
  setAutoflush(enabled)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
reactiveFileReader
list(`package:shiny` = function (intervalMillis, session, filePath, readFunc, ...) 
{
  filePath <- coerceToFunc(filePath)
  extraArgs <- list2(...)
  reactivePoll(intervalMillis, session, function() {
    path <- filePath()
    info <- file.info(path)
    return(paste(path, info$mtime, info$size))
  }, function() {
    do.call(readFunc, c(filePath(), extraArgs))
  })
}, function (intervalMillis, session, filePath, readFunc, ...) 
{
  filePath <- coerceToFunc(filePath)
  extraArgs <- list2(...)
  reactivePoll(intervalMillis, session, function() {
    path <- filePath()
    info <- file.info(path)
    return(paste(path, info$mtime, info$size))
  }, function() {
    do.call(readFunc, c(filePath(), extraArgs))
  })
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
reactivePoll
list(`package:shiny` = function (intervalMillis, session, checkFunc, valueFunc) 
{
  intervalMillis <- coerceToFunc(intervalMillis)
  rv <- reactiveValues(cookie = isolate(checkFunc()))
  re_finalized <- FALSE
  env <- environment()
  o <- observe({
    if (re_finalized) {
      o$destroy()
      rm(o, envir = env)
      return()
    }
    rv$cookie <- checkFunc()
    invalidateLater(intervalMillis(), session)
  })
  re <- reactive({
    rv$cookie
    valueFunc()
  }, label = NULL)
  reg.finalizer(attr(re, "observable"), function(e) {
    re_finalized <<- TRUE
  })
  on.exit(rm(re))
  return(re)
}, function (intervalMillis, session, checkFunc, valueFunc) 
{
  intervalMillis <- coerceToFunc(intervalMillis)
  rv <- reactiveValues(cookie = isolate(checkFunc()))
  re_finalized <- FALSE
  env <- environment()
  o <- observe({
    if (re_finalized) {
      o$destroy()
      rm(o, envir = env)
      return()
    }
    rv$cookie <- checkFunc()
    invalidateLater(intervalMillis(), session)
  })
  re <- reactive({
    rv$cookie
    valueFunc()
  }, label = NULL)
  reg.finalizer(attr(re, "observable"), function(e) {
    re_finalized <<- TRUE
  })
  on.exit(rm(re))
  return(re)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
reactiveTimer
list(`package:shiny` = function (intervalMs = 1000, session = getDefaultReactiveDomain()) 
{
  force(session)
  scheduler <- defineScheduler(session)
  dependents <- Map$new()
  timerHandle <- scheduler(intervalMs, function() {
    if (!is.null(session) && session$isClosed()) {
      return(invisible())
    }
    timerHandle <<- scheduler(intervalMs, sys.function())
    doInvalidate <- function() {
      lapply(dependents$values(), function(dep.ctx) {
        dep.ctx$invalidate()
        NULL
      })
    }
    if (!is.null(session)) {
      session$cycleStartAction(doInvalidate)
    }
    else {
      doInvalidate()
    }
  })
  if (!is.null(session)) {
    session$onEnded(timerHandle)
  }
  return(function() {
    newValue <- Sys.time()
    ctx <- getCurrentContext()
    if (!dependents$containsKey(ctx$id)) {
      dependents$set(ctx$id, ctx)
      ctx$onInvalidate(function() {
        dependents$remove(ctx$id)
      })
    }
    return(newValue)
  })
}, function (intervalMs = 1000, session = getDefaultReactiveDomain()) 
{
  force(session)
  scheduler <- defineScheduler(session)
  dependents <- Map$new()
  timerHandle <- scheduler(intervalMs, function() {
    if (!is.null(session) && session$isClosed()) {
      return(invisible())
    }
    timerHandle <<- scheduler(intervalMs, sys.function())
    doInvalidate <- function() {
      lapply(dependents$values(), function(dep.ctx) {
        dep.ctx$invalidate()
        NULL
      })
    }
    if (!is.null(session)) {
      session$cycleStartAction(doInvalidate)
    }
    else {
      doInvalidate()
    }
  })
  if (!is.null(session)) {
    session$onEnded(timerHandle)
  }
  return(function() {
    newValue <- Sys.time()
    ctx <- getCurrentContext()
    if (!dependents$containsKey(ctx$id)) {
      dependents$set(ctx$id, ctx)
      ctx$onInvalidate(function() {
        dependents$remove(ctx$id)
      })
    }
    return(newValue)
  })
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
reactiveVal
list(`package:shiny` = function (value = NULL, label = NULL) 
{
  if (missing(label)) {
    call <- sys.call()
    label <- rvalSrcrefToLabel(attr(call, "srcref", exact = TRUE))
  }
  rv <- ReactiveVal$new(value, label)
  structure(function(x) {
    if (missing(x)) {
      rv$get()
    }
    else {
      force(x)
      rv$set(x)
    }
  }, class = c("reactiveVal", "reactive", "function"), label = label, .impl = rv)
}, function (value = NULL, label = NULL) 
{
  if (missing(label)) {
    call <- sys.call()
    label <- rvalSrcrefToLabel(attr(call, "srcref", exact = TRUE))
  }
  rv <- ReactiveVal$new(value, label)
  structure(function(x) {
    if (missing(x)) {
      rv$get()
    }
    else {
      force(x)
      rv$set(x)
    }
  }, class = c("reactiveVal", "reactive", "function"), label = label, .impl = rv)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
reactiveValues
list(`package:shiny` = function (...) 
{
  args <- list2(...)
  if ((length(args) > 0) && (is.null(names(args)) || any(names(args) == ""))) 
    rlang::abort("All arguments passed to reactiveValues() must be named.")
  values <- .createReactiveValues(ReactiveValues$new())
  .subset2(values, "impl")$mset(args)
  values
}, function (...) 
{
  args <- list2(...)
  if ((length(args) > 0) && (is.null(names(args)) || any(names(args) == ""))) 
    rlang::abort("All arguments passed to reactiveValues() must be named.")
  values <- .createReactiveValues(ReactiveValues$new())
  .subset2(values, "impl")$mset(args)
  values
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
reactiveValuesToList
list(`package:shiny` = function (x, all.names = FALSE) 
{
  res <- .subset2(x, "impl")$toList(all.names)
  prefix <- .subset2(x, "ns")("")
  if (nzchar(prefix)) {
    fullNames <- names(res)
    fullNames <- fullNames[substring(fullNames, 1, nchar(prefix)) == prefix]
    res <- res[fullNames]
    names(res) <- substring(fullNames, nchar(prefix) + 1)
  }
  res
}, function (x, all.names = FALSE) 
{
  res <- .subset2(x, "impl")$toList(all.names)
  prefix <- .subset2(x, "ns")("")
  if (nzchar(prefix)) {
    fullNames <- names(res)
    fullNames <- fullNames[substring(fullNames, 1, nchar(prefix)) == prefix]
    res <- res[fullNames]
    names(res) <- substring(fullNames, nchar(prefix) + 1)
  }
  res
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
reactlog
list(`package:shiny` = function () 
{
  rLog$asList()
}, function () 
{
  rLog$asList()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
reactlogReset
list(`package:shiny` = function () 
{
  rLog$reset()
}, function () 
{
  rLog$reset()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
reactlogShow
list(`package:shiny` = function (time = TRUE) 
{
  check_reactlog()
  reactlog::reactlog_show(reactlog(), time = time)
}, function (time = TRUE) 
{
  check_reactlog()
  reactlog::reactlog_show(reactlog(), time = time)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
register_devmode_option
list(`package:shiny` = function (name, devmode_message = NULL, devmode_default = NULL) 
{
  if (!is.null(devmode_message)) {
    stopifnot(length(devmode_message) == 1 && is.character(devmode_message))
  }
  registered_devmode_options$set(name, list(devmode_default = devmode_default, devmode_message = devmode_message))
}, function (name, devmode_message = NULL, devmode_default = NULL) 
{
  if (!is.null(devmode_message)) {
    stopifnot(length(devmode_message) == 1 && is.character(devmode_message))
  }
  registered_devmode_options$set(name, list(devmode_default = devmode_default, devmode_message = devmode_message))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
registerInputHandler
list(`package:shiny` = function (type, fun, force = FALSE) 
{
  if (inputHandlers$containsKey(type) && !force) {
    stop("There is already an input handler for type: ", type)
  }
  inputHandlers$set(type, fun)
}, function (type, fun, force = FALSE) 
{
  if (inputHandlers$containsKey(type) && !force) {
    stop("There is already an input handler for type: ", type)
  }
  inputHandlers$set(type, fun)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
registerThemeDependency
list(`package:shiny` = function (func) 
{
  func_expr <- substitute(func)
  if (is.call(func_expr) && identical(func_expr[[1]], as.symbol("function"))) {
    warning("`func` should not be an anonymous function. ", "It should be declared outside of the function that calls registerThemeDependency(); ", "otherwise it will not be deduplicated by Shiny and multiple copies of the ", "resulting htmlDependency may be computed and sent to the client.")
  }
  if (!is.function(func) || length(formals(func)) != 1) {
    stop("`func` must be a function with one argument (the current theme)")
  }
  funcs <- getShinyOption("themeDependencyFuncs", default = list())
  have_func <- any(vapply(funcs, identical, logical(1), func))
  if (!have_func) {
    funcs[[length(funcs) + 1]] <- func
  }
  shinyOptions(themeDependencyFuncs = funcs)
}, function (func) 
{
  func_expr <- substitute(func)
  if (is.call(func_expr) && identical(func_expr[[1]], as.symbol("function"))) {
    warning("`func` should not be an anonymous function. ", "It should be declared outside of the function that calls registerThemeDependency(); ", "otherwise it will not be deduplicated by Shiny and multiple copies of the ", "resulting htmlDependency may be computed and sent to the client.")
  }
  if (!is.function(func) || length(formals(func)) != 1) {
    stop("`func` must be a function with one argument (the current theme)")
  }
  funcs <- getShinyOption("themeDependencyFuncs", default = list())
  have_func <- any(vapply(funcs, identical, logical(1), func))
  if (!have_func) {
    funcs[[length(funcs) + 1]] <- func
  }
  shinyOptions(themeDependencyFuncs = funcs)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
removeInputHandler
list(`package:shiny` = function (type) 
{
  inputHandlers$remove(type)
}, function (type) 
{
  inputHandlers$remove(type)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
removeModal
list(`package:shiny` = function (session = getDefaultReactiveDomain()) 
{
  session$sendModal("remove", NULL)
}, function (session = getDefaultReactiveDomain()) 
{
  session$sendModal("remove", NULL)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
removeNotification
list(`package:shiny` = function (id, session = getDefaultReactiveDomain()) 
{
  force(id)
  session$sendNotification("remove", id)
  id
}, function (id, session = getDefaultReactiveDomain()) 
{
  force(id)
  session$sendNotification("remove", id)
  id
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
removeResourcePath
list(`package:shiny` = function (prefix) 
{
  if (length(prefix) > 1) 
    stop("`prefix` must be of length 1.")
  if (!hasResourcePath(prefix)) {
    warning("Resource ", prefix, " not found.")
    return(invisible(FALSE))
  }
  .globals$resourcePaths[[prefix]] <- NULL
  .globals$resources[[prefix]] <- NULL
  invisible(TRUE)
}, function (prefix) 
{
  if (length(prefix) > 1) 
    stop("`prefix` must be of length 1.")
  if (!hasResourcePath(prefix)) {
    warning("Resource ", prefix, " not found.")
    return(invisible(FALSE))
  }
  .globals$resourcePaths[[prefix]] <- NULL
  .globals$resources[[prefix]] <- NULL
  invisible(TRUE)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
removeTab
list(`package:shiny` = function (inputId, target, session = getDefaultReactiveDomain()) 
{
  bslib::nav_remove(inputId, target, session)
}, function (inputId, target, session = getDefaultReactiveDomain()) 
{
  bslib::nav_remove(inputId, target, session)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
removeUI
list(`package:shiny` = function (selector, multiple = FALSE, immediate = FALSE, session = getDefaultReactiveDomain()) 
{
  force(selector)
  force(multiple)
  force(session)
  callback <- function() {
    session$sendRemoveUI(selector = selector, multiple = multiple)
  }
  if (!immediate) 
    session$onFlushed(callback, once = TRUE)
  else callback()
}, function (selector, multiple = FALSE, immediate = FALSE, session = getDefaultReactiveDomain()) 
{
  force(selector)
  force(multiple)
  force(session)
  callback <- function() {
    session$sendRemoveUI(selector = selector, multiple = multiple)
  }
  if (!immediate) 
    session$onFlushed(callback, once = TRUE)
  else callback()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
renderCachedPlot
list(`package:shiny` = function (expr, cacheKeyExpr, sizePolicy = sizeGrowthRatio(width = 400, height = 400, growthRate = 1.2), res = 72, cache = "app", ..., alt = "Plot object", outputArgs = list(), width = NULL, height = NULL) 
{
  expr <- substitute(expr)
  if (!is_quosure(expr)) {
    expr <- new_quosure(expr, env = parent.frame())
  }
  cacheKeyExpr <- substitute(cacheKeyExpr)
  if (!is_quosure(cacheKeyExpr)) {
    cacheKeyExpr <- new_quosure(cacheKeyExpr, env = parent.frame())
  }
  if (!is.null(width) || !is.null(height)) {
    warning("Unused argument(s) 'width' and/or 'height'. ", "'sizePolicy' is used instead.")
  }
  inject(bindCache(renderPlot(!!expr, res = res, alt = alt, outputArgs = outputArgs, ...), !!cacheKeyExpr, sizePolicy = sizePolicy, cache = cache))
}, function (expr, cacheKeyExpr, sizePolicy = sizeGrowthRatio(width = 400, height = 400, growthRate = 1.2), res = 72, cache = "app", ..., alt = "Plot object", outputArgs = list(), width = NULL, height = NULL) 
{
  expr <- substitute(expr)
  if (!is_quosure(expr)) {
    expr <- new_quosure(expr, env = parent.frame())
  }
  cacheKeyExpr <- substitute(cacheKeyExpr)
  if (!is_quosure(cacheKeyExpr)) {
    cacheKeyExpr <- new_quosure(cacheKeyExpr, env = parent.frame())
  }
  if (!is.null(width) || !is.null(height)) {
    warning("Unused argument(s) 'width' and/or 'height'. ", "'sizePolicy' is used instead.")
  }
  inject(bindCache(renderPlot(!!expr, res = res, alt = alt, outputArgs = outputArgs, ...), !!cacheKeyExpr, sizePolicy = sizePolicy, cache = cache))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
renderDataTable
list(`package:shiny` = function (expr, options = NULL, searchDelay = 500, callback = "function(oTable) {}", escape = TRUE, env = parent.frame(), quoted = FALSE, outputArgs = list()) 
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
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
renderImage
list(`package:shiny` = function (expr, env = parent.frame(), quoted = FALSE, deleteFile, outputArgs = list()) 
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderImage")
  if (missing(deleteFile)) {
    deleteFile <- NULL
  }
  warned <- FALSE
  createRenderFunction(func, transform = function(imageinfo, session, name, ...) {
    shouldDelete <- deleteFile
    if (is.null(shouldDelete)) {
      shouldDelete <- isTRUE(try(silent = TRUE, file.exists(imageinfo$src) && isTemp(imageinfo$src, mustExist = TRUE)))
      if (!warned) {
        warned <<- TRUE
        warning("The renderImage output named '", getCurrentOutputInfo()$name, "' is missing the deleteFile argument; as of Shiny 1.5.0, you must ", "use deleteFile=TRUE or deleteFile=FALSE. (This warning will ", "become an error in a future version of Shiny.)", call. = FALSE)
      }
    }
    if (shouldDelete) {
      on.exit(unlink(imageinfo$src), add = TRUE)
    }
    contentType <- imageinfo$contentType %||% getContentType(imageinfo$src)
    extra_attr <- imageinfo[!names(imageinfo) %in% c("src", "contentType")]
    c(src = session$fileUrl(name, file = imageinfo$src, contentType = contentType), extra_attr)
  }, imageOutput, outputArgs, cacheHint = FALSE)
}, function (expr, env = parent.frame(), quoted = FALSE, deleteFile, outputArgs = list()) 
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderImage")
  if (missing(deleteFile)) {
    deleteFile <- NULL
  }
  warned <- FALSE
  createRenderFunction(func, transform = function(imageinfo, session, name, ...) {
    shouldDelete <- deleteFile
    if (is.null(shouldDelete)) {
      shouldDelete <- isTRUE(try(silent = TRUE, file.exists(imageinfo$src) && isTemp(imageinfo$src, mustExist = TRUE)))
      if (!warned) {
        warned <<- TRUE
        warning("The renderImage output named '", getCurrentOutputInfo()$name, "' is missing the deleteFile argument; as of Shiny 1.5.0, you must ", "use deleteFile=TRUE or deleteFile=FALSE. (This warning will ", "become an error in a future version of Shiny.)", call. = FALSE)
      }
    }
    if (shouldDelete) {
      on.exit(unlink(imageinfo$src), add = TRUE)
    }
    contentType <- imageinfo$contentType %||% getContentType(imageinfo$src)
    extra_attr <- imageinfo[!names(imageinfo) %in% c("src", "contentType")]
    c(src = session$fileUrl(name, file = imageinfo$src, contentType = contentType), extra_attr)
  }, imageOutput, outputArgs, cacheHint = FALSE)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
renderPlot
list(`package:shiny` = function (expr, width = "auto", height = "auto", res = 72, ..., alt = NA, env = parent.frame(), quoted = FALSE, execOnResize = FALSE, outputArgs = list()) 
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderPlot", ..stacktraceon = TRUE)
  args <- list(...)
  if (is.reactive(width)) 
    widthWrapper <- width
  else if (is.function(width)) 
    widthWrapper <- reactive({
      width()
    })
  else widthWrapper <- function() {
    width
  }
  if (is.reactive(height)) 
    heightWrapper <- height
  else if (is.function(height)) 
    heightWrapper <- reactive({
      height()
    })
  else heightWrapper <- function() {
    height
  }
  if (is.reactive(alt)) 
    altWrapper <- alt
  else if (is.function(alt)) 
    altWrapper <- reactive({
      alt()
    })
  else altWrapper <- function() {
    alt
  }
  getDimsDefault <- function() {
    width <- widthWrapper()
    height <- heightWrapper()
    if (width == "auto") 
      width <- session$clientData[[paste0("output_", outputName, "_width")]]
    if (height == "auto") 
      height <- session$clientData[[paste0("output_", outputName, "_height")]]
    list(width = width, height = height)
  }
  session <- NULL
  outputName <- NULL
  getDims <- NULL
  drawReactive <- reactive(label = "plotObj", {
    hybrid_chain({
      dims <- if (execOnResize) 
        getDims()
      else isolate(getDims())
      pixelratio <- session$clientData$pixelratio %||% 1
      do.call("drawPlot", c(list(name = outputName, session = session, func = func, width = dims$width, height = dims$height, alt = altWrapper(), pixelratio = pixelratio, res = res), args))
    }, catch = function(reason) {
      getDims()
      stop(reason)
    })
  })
  renderFunc <- function(shinysession, name, ..., get_dims = getDimsDefault) {
    outputName <<- name
    session <<- shinysession
    if (is.null(getDims)) 
      getDims <<- get_dims
    hybrid_chain(drawReactive(), function(result) {
      dims <- getDims()
      pixelratio <- session$clientData$pixelratio %||% 1
      result <- do.call("resizeSavedPlot", c(list(name, shinysession, result, dims$width, dims$height, altWrapper(), pixelratio, res), args))
      result$img
    })
  }
  outputFunc <- plotOutput
  if (!identical(height, "auto")) 
    formals(outputFunc)["height"] <- list(NULL)
  markedFunc <- markRenderFunction(outputFunc, renderFunc, outputArgs, cacheHint = list(userExpr = installedFuncExpr(func), res = res))
  class(markedFunc) <- c("shiny.renderPlot", class(markedFunc))
  markedFunc
}, function (expr, width = "auto", height = "auto", res = 72, ..., alt = NA, env = parent.frame(), quoted = FALSE, execOnResize = FALSE, outputArgs = list()) 
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderPlot", ..stacktraceon = TRUE)
  args <- list(...)
  if (is.reactive(width)) 
    widthWrapper <- width
  else if (is.function(width)) 
    widthWrapper <- reactive({
      width()
    })
  else widthWrapper <- function() {
    width
  }
  if (is.reactive(height)) 
    heightWrapper <- height
  else if (is.function(height)) 
    heightWrapper <- reactive({
      height()
    })
  else heightWrapper <- function() {
    height
  }
  if (is.reactive(alt)) 
    altWrapper <- alt
  else if (is.function(alt)) 
    altWrapper <- reactive({
      alt()
    })
  else altWrapper <- function() {
    alt
  }
  getDimsDefault <- function() {
    width <- widthWrapper()
    height <- heightWrapper()
    if (width == "auto") 
      width <- session$clientData[[paste0("output_", outputName, "_width")]]
    if (height == "auto") 
      height <- session$clientData[[paste0("output_", outputName, "_height")]]
    list(width = width, height = height)
  }
  session <- NULL
  outputName <- NULL
  getDims <- NULL
  drawReactive <- reactive(label = "plotObj", {
    hybrid_chain({
      dims <- if (execOnResize) 
        getDims()
      else isolate(getDims())
      pixelratio <- session$clientData$pixelratio %||% 1
      do.call("drawPlot", c(list(name = outputName, session = session, func = func, width = dims$width, height = dims$height, alt = altWrapper(), pixelratio = pixelratio, res = res), args))
    }, catch = function(reason) {
      getDims()
      stop(reason)
    })
  })
  renderFunc <- function(shinysession, name, ..., get_dims = getDimsDefault) {
    outputName <<- name
    session <<- shinysession
    if (is.null(getDims)) 
      getDims <<- get_dims
    hybrid_chain(drawReactive(), function(result) {
      dims <- getDims()
      pixelratio <- session$clientData$pixelratio %||% 1
      result <- do.call("resizeSavedPlot", c(list(name, shinysession, result, dims$width, dims$height, altWrapper(), pixelratio, res), args))
      result$img
    })
  }
  outputFunc <- plotOutput
  if (!identical(height, "auto")) 
    formals(outputFunc)["height"] <- list(NULL)
  markedFunc <- markRenderFunction(outputFunc, renderFunc, outputArgs, cacheHint = list(userExpr = installedFuncExpr(func), res = res))
  class(markedFunc) <- c("shiny.renderPlot", class(markedFunc))
  markedFunc
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
renderPrint
list(`package:shiny` = function (expr, env = parent.frame(), quoted = FALSE, width = getOption("width"), outputArgs = list()) 
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderPrint")
  renderFunc <- function(shinysession, name, ...) {
    domain <- createRenderPrintPromiseDomain(width)
    hybrid_chain({
      promises::with_promise_domain(domain, func())
    }, function(value) {
      res <- withVisible(value)
      if (res$visible) {
        cat(file = domain$conn, paste(utils::capture.output(res$value, append = TRUE), collapse = "\n"))
      }
      paste(readLines(domain$conn, warn = FALSE), collapse = "\n")
    }, finally = function() {
      close(domain$conn)
    })
  }
  markRenderFunction(verbatimTextOutput, renderFunc, outputArgs, cacheHint = list(label = "renderPrint", origUserExpr = installedFuncExpr(func)))
}, function (expr, env = parent.frame(), quoted = FALSE, width = getOption("width"), outputArgs = list()) 
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderPrint")
  renderFunc <- function(shinysession, name, ...) {
    domain <- createRenderPrintPromiseDomain(width)
    hybrid_chain({
      promises::with_promise_domain(domain, func())
    }, function(value) {
      res <- withVisible(value)
      if (res$visible) {
        cat(file = domain$conn, paste(utils::capture.output(res$value, append = TRUE), collapse = "\n"))
      }
      paste(readLines(domain$conn, warn = FALSE), collapse = "\n")
    }, finally = function() {
      close(domain$conn)
    })
  }
  markRenderFunction(verbatimTextOutput, renderFunc, outputArgs, cacheHint = list(label = "renderPrint", origUserExpr = installedFuncExpr(func)))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
renderTable
list(`package:shiny` = function (expr, striped = FALSE, hover = FALSE, bordered = FALSE, spacing = c("s", "xs", "m", "l"), width = "auto", align = NULL, rownames = FALSE, colnames = TRUE, digits = NULL, na = "NA", ..., env = parent.frame(), quoted = FALSE, outputArgs = list()) 
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderTable")
  if (!is.function(spacing)) 
    spacing <- match.arg(spacing)
  createWrapper <- function(arg) {
    if (is.function(arg)) 
      wrapper <- arg
    else wrapper <- function() arg
    return(wrapper)
  }
  stripedWrapper <- createWrapper(striped)
  hoverWrapper <- createWrapper(hover)
  borderedWrapper <- createWrapper(bordered)
  spacingWrapper <- createWrapper(spacing)
  widthWrapper <- createWrapper(width)
  alignWrapper <- createWrapper(align)
  rownamesWrapper <- createWrapper(rownames)
  colnamesWrapper <- createWrapper(colnames)
  digitsWrapper <- createWrapper(digits)
  naWrapper <- createWrapper(na)
  dots <- list(...)
  createRenderFunction(func, function(data, session, name, ...) {
    striped <- stripedWrapper()
    hover <- hoverWrapper()
    bordered <- borderedWrapper()
    format <- c(striped = striped, hover = hover, bordered = bordered)
    spacing <- spacingWrapper()
    width <- widthWrapper()
    align <- alignWrapper()
    rownames <- rownamesWrapper()
    colnames <- colnamesWrapper()
    digits <- digitsWrapper()
    na <- naWrapper()
    spacing_choices <- c("s", "xs", "m", "l")
    if (!(spacing %in% spacing_choices)) {
      stop(paste("`spacing` must be one of", paste0("'", spacing_choices, "'", collapse = ", ")))
    }
    classNames <- paste0("table shiny-table", paste0(" table-", names(format)[format], collapse = ""), paste0(" spacing-", spacing))
    data <- as.data.frame(data)
    if (is.null(data) || (is.data.frame(data) && nrow(data) == 0 && ncol(data) == 0)) 
      return(NULL)
    xtable_argnames <- setdiff(names(formals(xtable)), c("x", "..."))
    xtable_args <- dots[intersect(names(dots), xtable_argnames)]
    non_xtable_args <- dots[setdiff(names(dots), xtable_argnames)]
    defaultAlignment <- function(col) {
      if (is.numeric(col)) 
        "r"
      else "l"
    }
    if (is.null(align) || align == "?") {
      names <- defaultAlignment(attr(data, "row.names"))
      cols <- paste(vapply(data, defaultAlignment, character(1)), collapse = "")
      cols <- paste0(names, cols)
    }
    else {
      num_cols <- if (rownames) 
        nchar(align)
      else nchar(align) + 1
      valid <- !grepl("[^lcr\\?]", align)
      if (num_cols == ncol(data) + 1 && valid) {
        cols <- if (rownames) 
          align
        else paste0("r", align)
        defaults <- grep("\\?", strsplit(cols, "")[[1]])
        if (length(defaults) != 0) {
          vals <- vapply(data[, defaults - 1], defaultAlignment, character(1))
          for (i in seq_len(length(defaults))) {
            substr(cols, defaults[i], defaults[i]) <- vals[i]
          }
        }
      }
      else if (nchar(align) == 1 && valid) {
        cols <- paste0(rep(align, ncol(data) + 1), collapse = "")
      }
      else {
        stop("`align` must contain only the characters `l`, `c`, `r` and/or `?` and", "have length either equal to 1 or to the total number of columns")
      }
    }
    xtable_args <- c(xtable_args, align = cols, digits = digits)
    xtable_res <- do.call(xtable, c(list(data), xtable_args))
    print_args <- list(x = xtable_res, type = "html", include.rownames = {
      if ("include.rownames" %in% names(dots)) dots$include.rownames else rownames
    }, include.colnames = {
      if ("include.colnames" %in% names(dots)) dots$include.colnames else colnames
    }, NA.string = {
      if ("NA.string" %in% names(dots)) dots$NA.string else na
    }, html.table.attributes = paste0({
      if ("html.table.attributes" %in% names(dots)) dots$html.table.attributes else ""
    }, " ", "class = '", htmlEscape(classNames, TRUE), "' ", "style = 'width:", validateCssUnit(width), ";'"), comment = {
      if ("comment" %in% names(dots)) dots$comment else FALSE
    })
    print_args <- c(print_args, non_xtable_args)
    print_args <- print_args[unique(names(print_args))]
    tab <- paste(utils::capture.output(do.call(print, print_args)), collapse = "\n")
    tab <- gsub(paste(">", na, "<"), paste(" class='NA'>", na, "<"), tab)
    if (colnames) {
      tab <- sub("<tr>", "<thead> <tr>", tab)
      tab <- sub("</tr>", "</tr> </thead> <tbody>", tab)
      tab <- sub("</table>$", "</tbody> </table>", tab)
      cols <- if (rownames) 
        cols
      else substr(cols, 2, nchar(cols))
      cols <- strsplit(cols, "")[[1]]
      cols[cols == "l"] <- "left"
      cols[cols == "r"] <- "right"
      cols[cols == "c"] <- "center"
      for (i in seq_len(length(cols))) {
        tab <- sub("<th>", paste0("<th style='text-align: ", cols[i], ";'>"), tab)
      }
    }
    return(tab)
  }, tableOutput, outputArgs)
}, function (expr, striped = FALSE, hover = FALSE, bordered = FALSE, spacing = c("s", "xs", "m", "l"), width = "auto", align = NULL, rownames = FALSE, colnames = TRUE, digits = NULL, na = "NA", ..., env = parent.frame(), quoted = FALSE, outputArgs = list()) 
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderTable")
  if (!is.function(spacing)) 
    spacing <- match.arg(spacing)
  createWrapper <- function(arg) {
    if (is.function(arg)) 
      wrapper <- arg
    else wrapper <- function() arg
    return(wrapper)
  }
  stripedWrapper <- createWrapper(striped)
  hoverWrapper <- createWrapper(hover)
  borderedWrapper <- createWrapper(bordered)
  spacingWrapper <- createWrapper(spacing)
  widthWrapper <- createWrapper(width)
  alignWrapper <- createWrapper(align)
  rownamesWrapper <- createWrapper(rownames)
  colnamesWrapper <- createWrapper(colnames)
  digitsWrapper <- createWrapper(digits)
  naWrapper <- createWrapper(na)
  dots <- list(...)
  createRenderFunction(func, function(data, session, name, ...) {
    striped <- stripedWrapper()
    hover <- hoverWrapper()
    bordered <- borderedWrapper()
    format <- c(striped = striped, hover = hover, bordered = bordered)
    spacing <- spacingWrapper()
    width <- widthWrapper()
    align <- alignWrapper()
    rownames <- rownamesWrapper()
    colnames <- colnamesWrapper()
    digits <- digitsWrapper()
    na <- naWrapper()
    spacing_choices <- c("s", "xs", "m", "l")
    if (!(spacing %in% spacing_choices)) {
      stop(paste("`spacing` must be one of", paste0("'", spacing_choices, "'", collapse = ", ")))
    }
    classNames <- paste0("table shiny-table", paste0(" table-", names(format)[format], collapse = ""), paste0(" spacing-", spacing))
    data <- as.data.frame(data)
    if (is.null(data) || (is.data.frame(data) && nrow(data) == 0 && ncol(data) == 0)) 
      return(NULL)
    xtable_argnames <- setdiff(names(formals(xtable)), c("x", "..."))
    xtable_args <- dots[intersect(names(dots), xtable_argnames)]
    non_xtable_args <- dots[setdiff(names(dots), xtable_argnames)]
    defaultAlignment <- function(col) {
      if (is.numeric(col)) 
        "r"
      else "l"
    }
    if (is.null(align) || align == "?") {
      names <- defaultAlignment(attr(data, "row.names"))
      cols <- paste(vapply(data, defaultAlignment, character(1)), collapse = "")
      cols <- paste0(names, cols)
    }
    else {
      num_cols <- if (rownames) 
        nchar(align)
      else nchar(align) + 1
      valid <- !grepl("[^lcr\\?]", align)
      if (num_cols == ncol(data) + 1 && valid) {
        cols <- if (rownames) 
          align
        else paste0("r", align)
        defaults <- grep("\\?", strsplit(cols, "")[[1]])
        if (length(defaults) != 0) {
          vals <- vapply(data[, defaults - 1], defaultAlignment, character(1))
          for (i in seq_len(length(defaults))) {
            substr(cols, defaults[i], defaults[i]) <- vals[i]
          }
        }
      }
      else if (nchar(align) == 1 && valid) {
        cols <- paste0(rep(align, ncol(data) + 1), collapse = "")
      }
      else {
        stop("`align` must contain only the characters `l`, `c`, `r` and/or `?` and", "have length either equal to 1 or to the total number of columns")
      }
    }
    xtable_args <- c(xtable_args, align = cols, digits = digits)
    xtable_res <- do.call(xtable, c(list(data), xtable_args))
    print_args <- list(x = xtable_res, type = "html", include.rownames = {
      if ("include.rownames" %in% names(dots)) dots$include.rownames else rownames
    }, include.colnames = {
      if ("include.colnames" %in% names(dots)) dots$include.colnames else colnames
    }, NA.string = {
      if ("NA.string" %in% names(dots)) dots$NA.string else na
    }, html.table.attributes = paste0({
      if ("html.table.attributes" %in% names(dots)) dots$html.table.attributes else ""
    }, " ", "class = '", htmlEscape(classNames, TRUE), "' ", "style = 'width:", validateCssUnit(width), ";'"), comment = {
      if ("comment" %in% names(dots)) dots$comment else FALSE
    })
    print_args <- c(print_args, non_xtable_args)
    print_args <- print_args[unique(names(print_args))]
    tab <- paste(utils::capture.output(do.call(print, print_args)), collapse = "\n")
    tab <- gsub(paste(">", na, "<"), paste(" class='NA'>", na, "<"), tab)
    if (colnames) {
      tab <- sub("<tr>", "<thead> <tr>", tab)
      tab <- sub("</tr>", "</tr> </thead> <tbody>", tab)
      tab <- sub("</table>$", "</tbody> </table>", tab)
      cols <- if (rownames) 
        cols
      else substr(cols, 2, nchar(cols))
      cols <- strsplit(cols, "")[[1]]
      cols[cols == "l"] <- "left"
      cols[cols == "r"] <- "right"
      cols[cols == "c"] <- "center"
      for (i in seq_len(length(cols))) {
        tab <- sub("<th>", paste0("<th style='text-align: ", cols[i], ";'>"), tab)
      }
    }
    return(tab)
  }, tableOutput, outputArgs)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
renderText
list(`package:shiny` = function (expr, env = parent.frame(), quoted = FALSE, outputArgs = list(), sep = " ") 
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderText")
  createRenderFunction(func, function(value, session, name, ...) {
    paste(utils::capture.output(cat(value, sep = sep)), collapse = "\n")
  }, textOutput, outputArgs)
}, function (expr, env = parent.frame(), quoted = FALSE, outputArgs = list(), sep = " ") 
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderText")
  createRenderFunction(func, function(value, session, name, ...) {
    paste(utils::capture.output(cat(value, sep = sep)), collapse = "\n")
  }, textOutput, outputArgs)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
renderUI
list(`package:shiny` = function (expr, env = parent.frame(), quoted = FALSE, outputArgs = list()) 
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderUI")
  createRenderFunction(func, function(result, shinysession, name, ...) {
    if (is.null(result) || length(result) == 0) 
      return(NULL)
    processDeps(result, shinysession)
  }, uiOutput, outputArgs)
}, function (expr, env = parent.frame(), quoted = FALSE, outputArgs = list()) 
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderUI")
  createRenderFunction(func, function(result, shinysession, name, ...) {
    if (is.null(result) || length(result) == 0) 
      return(NULL)
    processDeps(result, shinysession)
  }, uiOutput, outputArgs)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
repeatable
list(`package:shiny` = function (rngfunc, seed = stats::runif(1, 0, .Machine$integer.max)) 
{
  force(seed)
  function(...) {
    if (exists(".Random.seed", where = globalenv())) {
      currentSeed <- get(".Random.seed", pos = globalenv())
      on.exit(assign(".Random.seed", currentSeed, pos = globalenv()))
    }
    else {
      on.exit(rm(".Random.seed", pos = globalenv()))
    }
    set.seed(seed)
    rngfunc(...)
  }
}, function (rngfunc, seed = stats::runif(1, 0, .Machine$integer.max)) 
{
  force(seed)
  function(...) {
    if (exists(".Random.seed", where = globalenv())) {
      currentSeed <- get(".Random.seed", pos = globalenv())
      on.exit(assign(".Random.seed", currentSeed, pos = globalenv()))
    }
    else {
      on.exit(rm(".Random.seed", pos = globalenv()))
    }
    set.seed(seed)
    rngfunc(...)
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
req
list(`package:shiny` = function (..., cancelOutput = FALSE) 
{
  dotloop(function(item) {
    if (!isTruthy(item)) {
      if (isTRUE(cancelOutput)) {
        cancelOutput()
      }
      else {
        reactiveStop(class = "validation")
      }
    }
  }, ...)
  if (!missing(..1)) 
    ..1
  else invisible()
}, function (..., cancelOutput = FALSE) 
{
  dotloop(function(item) {
    if (!isTruthy(item)) {
      if (isTRUE(cancelOutput)) {
        cancelOutput()
      }
      else {
        reactiveStop(class = "validation")
      }
    }
  }, ...)
  if (!missing(..1)) 
    ..1
  else invisible()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
resourcePaths
list(`package:shiny` = function () 
{
  urls <- names(.globals$resourcePaths)
  paths <- vapply(.globals$resourcePaths, function(x) x$path, character(1))
  stats::setNames(paths, urls)
}, function () 
{
  urls <- names(.globals$resourcePaths)
  paths <- vapply(.globals$resourcePaths, function(x) x$path, character(1))
  stats::setNames(paths, urls)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
restoreInput
list(`package:shiny` = function (id, default) 
{
  force(default)
  if (!hasCurrentRestoreContext()) {
    return(default)
  }
  oldInputs <- getCurrentRestoreContext()$input
  if (oldInputs$available(id)) {
    oldInputs$get(id)
  }
  else {
    default
  }
}, function (id, default) 
{
  force(default)
  if (!hasCurrentRestoreContext()) {
    return(default)
  }
  oldInputs <- getCurrentRestoreContext()$input
  if (oldInputs$available(id)) {
    oldInputs$get(id)
  }
  else {
    default
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
runApp
list(`package:shiny` = function (appDir = getwd(), port = getOption("shiny.port"), launch.browser = getOption("shiny.launch.browser", interactive()), host = getOption("shiny.host", "127.0.0.1"), workerId = "", quiet = FALSE, display.mode = c("auto", "normal", "showcase"), test.mode = getOption("shiny.testmode", FALSE)) 
{
  on.exit({
    handlerManager$clear()
  }, add = TRUE)
  if (isRunning()) {
    stop("Can't call `runApp()` from within `runApp()`. If your ", "application code contains `runApp()`, please remove it.")
  }
  ops <- options(warn = max(1, getOption("warn", default = 1)), pool.scheduler = scheduleTask)
  on.exit(options(ops), add = TRUE)
  on.exit({
    .globals$onStopCallbacks$invoke()
    .globals$onStopCallbacks <- Callbacks$new()
  }, add = TRUE)
  require(shiny)
  appParts <- as.shiny.appobj(appDir)
  initCurrentAppState(appParts)
  on.exit(clearCurrentAppState(), add = TRUE)
  shinyOptions(appToken = createUniqueId(8))
  if (is.null(getShinyOption("cache", default = NULL))) {
    shinyOptions(cache = cachem::cache_mem(max_size = 200 * 1024^2))
  }
  applyCapturedAppOptions(appParts$appOptions)
  appOps <- appParts$options
  findVal <- function(arg, default) {
    if (arg %in% names(appOps)) 
      appOps[[arg]]
    else default
  }
  if (missing(port)) 
    port <- findVal("port", port)
  if (missing(launch.browser)) 
    launch.browser <- findVal("launch.browser", launch.browser)
  if (missing(host)) 
    host <- findVal("host", host)
  if (missing(quiet)) 
    quiet <- findVal("quiet", quiet)
  if (missing(display.mode)) 
    display.mode <- findVal("display.mode", display.mode)
  if (missing(test.mode)) 
    test.mode <- findVal("test.mode", test.mode)
  if (is.null(host) || is.na(host)) 
    host <- "0.0.0.0"
  workerId(workerId)
  if (inShinyServer()) {
    ver <- Sys.getenv("SHINY_SERVER_VERSION")
    if (utils::compareVersion(ver, .shinyServerMinVersion) < 0) {
      warning("Shiny Server v", .shinyServerMinVersion, " or later is required; please upgrade!")
    }
  }
  shinyOptions(testmode = test.mode)
  if (test.mode) {
    message("Running application in test mode.")
  }
  setShowcaseDefault(0)
  if (is.character(appDir)) {
    desc <- file.path.ci(if (tolower(tools::file_ext(appDir)) == "r") 
      dirname(appDir)
      else appDir, "DESCRIPTION")
    if (file.exists(desc)) {
      con <- file(desc, encoding = checkEncoding(desc))
      on.exit(close(con), add = TRUE)
      settings <- read.dcf(con)
      if ("DisplayMode" %in% colnames(settings)) {
        mode <- settings[1, "DisplayMode"]
        if (mode == "Showcase") {
          setShowcaseDefault(1)
          if ("IncludeWWW" %in% colnames(settings)) {
            .globals$IncludeWWW <- as.logical(settings[1, "IncludeWWW"])
            if (is.na(.globals$IncludeWWW)) {
              stop("In your Description file, `IncludeWWW` ", "must be set to `True` (default) or `False`")
            }
          }
          else {
            .globals$IncludeWWW <- TRUE
          }
        }
      }
    }
  }
  if (is.null(.globals$IncludeWWW) || is.na(.globals$IncludeWWW)) {
    .globals$IncludeWWW <- TRUE
  }
  display.mode <- match.arg(display.mode)
  if (display.mode == "normal") {
    setShowcaseDefault(0)
  }
  else if (display.mode == "showcase") {
    setShowcaseDefault(1)
  }
  if (is.null(port)) {
    for (i in 1:20) {
      if (!is.null(.globals$lastPort)) {
        port <- .globals$lastPort
        .globals$lastPort <- NULL
      }
      else {
        while (TRUE) {
          port <- p_randomInt(3000, 8000)
          if (!port %in% c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)) {
            break
          }
        }
      }
      tmp <- try(startServer(host, port, list()), silent = TRUE)
      if (!inherits(tmp, "try-error")) {
        stopServer(tmp)
        .globals$lastPort <- port
        break
      }
    }
  }
  if (!is.null(appParts$onStop)) 
    on.exit(appParts$onStop(), add = TRUE)
  if (!is.null(appParts$onStart)) 
    appParts$onStart()
  server <- startApp(appParts, port, host, quiet)
  shinyOptions(server = server)
  on.exit({
    stopServer(server)
  }, add = TRUE)
  if (!is.character(port)) {
    browseHost <- host
    if (identical(host, "0.0.0.0")) {
      browseHost <- "127.0.0.1"
    }
    else if (identical(host, "::")) {
      browseHost <- "::1"
    }
    if (httpuv::ipFamily(browseHost) == 6) {
      browseHost <- paste0("[", browseHost, "]")
    }
    appUrl <- paste("http://", browseHost, ":", port, sep = "")
    if (is.function(launch.browser)) 
      launch.browser(appUrl)
    else if (launch.browser) 
      utils::browseURL(appUrl)
  }
  else {
    appUrl <- NULL
  }
  callAppHook("onAppStart", appUrl)
  on.exit({
    callAppHook("onAppStop", appUrl)
  }, add = TRUE)
  .globals$reterror <- NULL
  .globals$retval <- NULL
  .globals$stopped <- FALSE
  ..stacktraceoff..(captureStackTraces({
    while (!.globals$stopped) {
      ..stacktracefloor..(serviceApp())
    }
  }))
  if (isTRUE(.globals$reterror)) {
    stop(.globals$retval)
  }
  else if (.globals$retval$visible) 
    .globals$retval$value
  else invisible(.globals$retval$value)
}, function (appDir = getwd(), port = getOption("shiny.port"), launch.browser = getOption("shiny.launch.browser", interactive()), host = getOption("shiny.host", "127.0.0.1"), workerId = "", quiet = FALSE, display.mode = c("auto", "normal", "showcase"), test.mode = getOption("shiny.testmode", FALSE)) 
{
  on.exit({
    handlerManager$clear()
  }, add = TRUE)
  if (isRunning()) {
    stop("Can't call `runApp()` from within `runApp()`. If your ", "application code contains `runApp()`, please remove it.")
  }
  ops <- options(warn = max(1, getOption("warn", default = 1)), pool.scheduler = scheduleTask)
  on.exit(options(ops), add = TRUE)
  on.exit({
    .globals$onStopCallbacks$invoke()
    .globals$onStopCallbacks <- Callbacks$new()
  }, add = TRUE)
  require(shiny)
  appParts <- as.shiny.appobj(appDir)
  initCurrentAppState(appParts)
  on.exit(clearCurrentAppState(), add = TRUE)
  shinyOptions(appToken = createUniqueId(8))
  if (is.null(getShinyOption("cache", default = NULL))) {
    shinyOptions(cache = cachem::cache_mem(max_size = 200 * 1024^2))
  }
  applyCapturedAppOptions(appParts$appOptions)
  appOps <- appParts$options
  findVal <- function(arg, default) {
    if (arg %in% names(appOps)) 
      appOps[[arg]]
    else default
  }
  if (missing(port)) 
    port <- findVal("port", port)
  if (missing(launch.browser)) 
    launch.browser <- findVal("launch.browser", launch.browser)
  if (missing(host)) 
    host <- findVal("host", host)
  if (missing(quiet)) 
    quiet <- findVal("quiet", quiet)
  if (missing(display.mode)) 
    display.mode <- findVal("display.mode", display.mode)
  if (missing(test.mode)) 
    test.mode <- findVal("test.mode", test.mode)
  if (is.null(host) || is.na(host)) 
    host <- "0.0.0.0"
  workerId(workerId)
  if (inShinyServer()) {
    ver <- Sys.getenv("SHINY_SERVER_VERSION")
    if (utils::compareVersion(ver, .shinyServerMinVersion) < 0) {
      warning("Shiny Server v", .shinyServerMinVersion, " or later is required; please upgrade!")
    }
  }
  shinyOptions(testmode = test.mode)
  if (test.mode) {
    message("Running application in test mode.")
  }
  setShowcaseDefault(0)
  if (is.character(appDir)) {
    desc <- file.path.ci(if (tolower(tools::file_ext(appDir)) == "r") 
      dirname(appDir)
      else appDir, "DESCRIPTION")
    if (file.exists(desc)) {
      con <- file(desc, encoding = checkEncoding(desc))
      on.exit(close(con), add = TRUE)
      settings <- read.dcf(con)
      if ("DisplayMode" %in% colnames(settings)) {
        mode <- settings[1, "DisplayMode"]
        if (mode == "Showcase") {
          setShowcaseDefault(1)
          if ("IncludeWWW" %in% colnames(settings)) {
            .globals$IncludeWWW <- as.logical(settings[1, "IncludeWWW"])
            if (is.na(.globals$IncludeWWW)) {
              stop("In your Description file, `IncludeWWW` ", "must be set to `True` (default) or `False`")
            }
          }
          else {
            .globals$IncludeWWW <- TRUE
          }
        }
      }
    }
  }
  if (is.null(.globals$IncludeWWW) || is.na(.globals$IncludeWWW)) {
    .globals$IncludeWWW <- TRUE
  }
  display.mode <- match.arg(display.mode)
  if (display.mode == "normal") {
    setShowcaseDefault(0)
  }
  else if (display.mode == "showcase") {
    setShowcaseDefault(1)
  }
  if (is.null(port)) {
    for (i in 1:20) {
      if (!is.null(.globals$lastPort)) {
        port <- .globals$lastPort
        .globals$lastPort <- NULL
      }
      else {
        while (TRUE) {
          port <- p_randomInt(3000, 8000)
          if (!port %in% c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)) {
            break
          }
        }
      }
      tmp <- try(startServer(host, port, list()), silent = TRUE)
      if (!inherits(tmp, "try-error")) {
        stopServer(tmp)
        .globals$lastPort <- port
        break
      }
    }
  }
  if (!is.null(appParts$onStop)) 
    on.exit(appParts$onStop(), add = TRUE)
  if (!is.null(appParts$onStart)) 
    appParts$onStart()
  server <- startApp(appParts, port, host, quiet)
  shinyOptions(server = server)
  on.exit({
    stopServer(server)
  }, add = TRUE)
  if (!is.character(port)) {
    browseHost <- host
    if (identical(host, "0.0.0.0")) {
      browseHost <- "127.0.0.1"
    }
    else if (identical(host, "::")) {
      browseHost <- "::1"
    }
    if (httpuv::ipFamily(browseHost) == 6) {
      browseHost <- paste0("[", browseHost, "]")
    }
    appUrl <- paste("http://", browseHost, ":", port, sep = "")
    if (is.function(launch.browser)) 
      launch.browser(appUrl)
    else if (launch.browser) 
      utils::browseURL(appUrl)
  }
  else {
    appUrl <- NULL
  }
  callAppHook("onAppStart", appUrl)
  on.exit({
    callAppHook("onAppStop", appUrl)
  }, add = TRUE)
  .globals$reterror <- NULL
  .globals$retval <- NULL
  .globals$stopped <- FALSE
  ..stacktraceoff..(captureStackTraces({
    while (!.globals$stopped) {
      ..stacktracefloor..(serviceApp())
    }
  }))
  if (isTRUE(.globals$reterror)) {
    stop(.globals$retval)
  }
  else if (.globals$retval$visible) 
    .globals$retval$value
  else invisible(.globals$retval$value)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
runExample
list(`package:shiny` = function (example = NA, port = getOption("shiny.port"), launch.browser = getOption("shiny.launch.browser", interactive()), host = getOption("shiny.host", "127.0.0.1"), display.mode = c("auto", "normal", "showcase")) 
{
  examplesDir <- system_file("examples", package = "shiny")
  dir <- resolve(examplesDir, example)
  if (is.null(dir)) {
    if (is.na(example)) {
      errFun <- message
      errMsg <- ""
    }
    else {
      errFun <- stop
      errMsg <- paste("Example", example, "does not exist. ")
    }
    errFun(errMsg, "Valid examples are \"", paste(list.files(examplesDir), collapse = "\", \""), "\"")
  }
  else {
    runApp(dir, port = port, host = host, launch.browser = launch.browser, display.mode = display.mode)
  }
}, function (example = NA, port = getOption("shiny.port"), launch.browser = getOption("shiny.launch.browser", interactive()), host = getOption("shiny.host", "127.0.0.1"), display.mode = c("auto", "normal", "showcase")) 
{
  examplesDir <- system_file("examples", package = "shiny")
  dir <- resolve(examplesDir, example)
  if (is.null(dir)) {
    if (is.na(example)) {
      errFun <- message
      errMsg <- ""
    }
    else {
      errFun <- stop
      errMsg <- paste("Example", example, "does not exist. ")
    }
    errFun(errMsg, "Valid examples are \"", paste(list.files(examplesDir), collapse = "\", \""), "\"")
  }
  else {
    runApp(dir, port = port, host = host, launch.browser = launch.browser, display.mode = display.mode)
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
runGadget
list(`package:shiny` = function (app, server = NULL, port = getOption("shiny.port"), viewer = paneViewer(), stopOnCancel = TRUE) 
{
  if (!is.shiny.appobj(app)) {
    app <- shinyApp(app, server)
  }
  if (isTRUE(stopOnCancel)) {
    app <- decorateServerFunc(app, function(input, output, session) {
      observeEvent(input$cancel, {
        stopApp(stop("User cancel", call. = FALSE))
      })
    })
  }
  if (is.null(viewer)) {
    viewer <- utils::browseURL
  }
  shiny::runApp(app, port = port, launch.browser = viewer)
}, function (app, server = NULL, port = getOption("shiny.port"), viewer = paneViewer(), stopOnCancel = TRUE) 
{
  if (!is.shiny.appobj(app)) {
    app <- shinyApp(app, server)
  }
  if (isTRUE(stopOnCancel)) {
    app <- decorateServerFunc(app, function(input, output, session) {
      observeEvent(input$cancel, {
        stopApp(stop("User cancel", call. = FALSE))
      })
    })
  }
  if (is.null(viewer)) {
    viewer <- utils::browseURL
  }
  shiny::runApp(app, port = port, launch.browser = viewer)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
runGist
list(`package:shiny` = function (gist, destdir = NULL, ...) 
{
  gistUrl <- if (is.numeric(gist) || grepl("^[0-9a-f]+$", gist)) {
    sprintf("https://gist.github.com/%s/download", gist)
  }
  else if (grepl("^https://gist.github.com/([^/]+/)?([0-9a-f]+)$", gist)) {
    paste(gist, "/download", sep = "")
  }
  else {
    stop("Unrecognized gist identifier format")
  }
  runUrl(gistUrl, filetype = ".zip", destdir = destdir, ...)
}, function (gist, destdir = NULL, ...) 
{
  gistUrl <- if (is.numeric(gist) || grepl("^[0-9a-f]+$", gist)) {
    sprintf("https://gist.github.com/%s/download", gist)
  }
  else if (grepl("^https://gist.github.com/([^/]+/)?([0-9a-f]+)$", gist)) {
    paste(gist, "/download", sep = "")
  }
  else {
    stop("Unrecognized gist identifier format")
  }
  runUrl(gistUrl, filetype = ".zip", destdir = destdir, ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
runGitHub
list(`package:shiny` = function (repo, username = getOption("github.user"), ref = "HEAD", subdir = NULL, destdir = NULL, ...) 
{
  if (grepl("/", repo)) {
    res <- strsplit(repo, "/")[[1]]
    if (length(res) != 2) 
      stop("'repo' must be of the form 'username/repo'")
    username <- res[1]
    repo <- res[2]
  }
  url <- paste("https://github.com/", username, "/", repo, "/archive/", ref, ".tar.gz", sep = "")
  runUrl(url, subdir = subdir, destdir = destdir, ...)
}, function (repo, username = getOption("github.user"), ref = "HEAD", subdir = NULL, destdir = NULL, ...) 
{
  if (grepl("/", repo)) {
    res <- strsplit(repo, "/")[[1]]
    if (length(res) != 2) 
      stop("'repo' must be of the form 'username/repo'")
    username <- res[1]
    repo <- res[2]
  }
  url <- paste("https://github.com/", username, "/", repo, "/archive/", ref, ".tar.gz", sep = "")
  runUrl(url, subdir = subdir, destdir = destdir, ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
runTests
list(`package:shiny` = function (appDir = ".", filter = NULL, assert = TRUE, envir = globalenv()) 
{
  require(shiny)
  testsDir <- file.path(appDir, "tests")
  if (!dirExists(testsDir)) {
    stop("No tests directory found: ", testsDir)
  }
  runners <- list.files(testsDir, pattern = "\\.r$", ignore.case = TRUE)
  if (length(runners) == 0) {
    message("No test runners found in ", testsDir)
    return(result_row(character(0), logical(0), list()))
  }
  if (!is.null(filter)) {
    runners <- runners[grepl(filter, runners)]
  }
  if (length(runners) == 0) {
    stop("No test runners matched the given filter: '", filter, "'")
  }
  if (is_legacy_shinytest_dir(testsDir)) {
    stop("It appears that the .R files in ", testsDir, " are all shinytests.", " This is not supported by `shiny::runTests()`.", "\nPlease see `?shinytest::migrateShinytestDir` to migrate your shinytest file structure to the new format (requires shinytest 1.4.0 or above).", "\nSee `?shiny::shinyAppTemplate` for an example of the new testing file structure.")
  }
  renv <- new.env(parent = envir)
  ret <- do.call(rbind, lapply(runners, function(r) {
    pass <- FALSE
    result <- tryCatch({
      env <- new.env(parent = renv)
      withr::with_dir(testsDir, {
        ret <- sourceUTF8(r, envir = env)
      })
      pass <- TRUE
      ret
    }, error = function(err) {
      message("Error in ", r, "\n", err)
      err
    })
    result_row(file.path(testsDir, r), pass, list(result))
  }))
  if (isTRUE(assert)) {
    if (!all(ret$pass)) {
      stop("Shiny App Test Failures detected in\n", paste0("* ", runtest_pretty_file(ret$file[!ret$pass]), collapse = "\n"), call. = FALSE)
    }
  }
  ret
}, function (appDir = ".", filter = NULL, assert = TRUE, envir = globalenv()) 
{
  require(shiny)
  testsDir <- file.path(appDir, "tests")
  if (!dirExists(testsDir)) {
    stop("No tests directory found: ", testsDir)
  }
  runners <- list.files(testsDir, pattern = "\\.r$", ignore.case = TRUE)
  if (length(runners) == 0) {
    message("No test runners found in ", testsDir)
    return(result_row(character(0), logical(0), list()))
  }
  if (!is.null(filter)) {
    runners <- runners[grepl(filter, runners)]
  }
  if (length(runners) == 0) {
    stop("No test runners matched the given filter: '", filter, "'")
  }
  if (is_legacy_shinytest_dir(testsDir)) {
    stop("It appears that the .R files in ", testsDir, " are all shinytests.", " This is not supported by `shiny::runTests()`.", "\nPlease see `?shinytest::migrateShinytestDir` to migrate your shinytest file structure to the new format (requires shinytest 1.4.0 or above).", "\nSee `?shiny::shinyAppTemplate` for an example of the new testing file structure.")
  }
  renv <- new.env(parent = envir)
  ret <- do.call(rbind, lapply(runners, function(r) {
    pass <- FALSE
    result <- tryCatch({
      env <- new.env(parent = renv)
      withr::with_dir(testsDir, {
        ret <- sourceUTF8(r, envir = env)
      })
      pass <- TRUE
      ret
    }, error = function(err) {
      message("Error in ", r, "\n", err)
      err
    })
    result_row(file.path(testsDir, r), pass, list(result))
  }))
  if (isTRUE(assert)) {
    if (!all(ret$pass)) {
      stop("Shiny App Test Failures detected in\n", paste0("* ", runtest_pretty_file(ret$file[!ret$pass]), collapse = "\n"), call. = FALSE)
    }
  }
  ret
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
runUrl
list(`package:shiny` = function (url, filetype = NULL, subdir = NULL, destdir = NULL, ...) 
{
  if (!is.null(subdir) && ".." %in% strsplit(subdir, "/")[[1]]) 
    stop("'..' not allowed in subdir")
  if (is.null(filetype)) 
    filetype <- basename(url)
  if (grepl("\\.tar\\.gz$", filetype)) 
    fileext <- ".tar.gz"
  else if (grepl("\\.tar$", filetype)) 
    fileext <- ".tar"
  else if (grepl("\\.zip$", filetype)) 
    fileext <- ".zip"
  else stop("Unknown file extension.")
  message("Downloading ", url)
  if (is.null(destdir)) {
    filePath <- tempfile("shinyapp", fileext = fileext)
    fileDir <- tempfile("shinyapp")
  }
  else {
    fileDir <- destdir
    filePath <- paste(destdir, fileext)
  }
  dir.create(fileDir, showWarnings = FALSE)
  if (download(url, filePath, mode = "wb", quiet = TRUE) != 0) 
    stop("Failed to download URL ", url)
  on.exit(unlink(filePath))
  if (fileext %in% c(".tar", ".tar.gz")) {
    first <- untar2(filePath, list = TRUE)[1]
    untar2(filePath, exdir = fileDir)
  }
  else if (fileext == ".zip") {
    first <- as.character(utils::unzip(filePath, list = TRUE)$Name)[1]
    utils::unzip(filePath, exdir = fileDir)
  }
  if (is.null(destdir)) {
    on.exit(unlink(fileDir, recursive = TRUE), add = TRUE)
  }
  appdir <- file.path(fileDir, first)
  if (!utils::file_test("-d", appdir)) 
    appdir <- dirname(appdir)
  if (!is.null(subdir)) 
    appdir <- file.path(appdir, subdir)
  runApp(appdir, ...)
}, function (url, filetype = NULL, subdir = NULL, destdir = NULL, ...) 
{
  if (!is.null(subdir) && ".." %in% strsplit(subdir, "/")[[1]]) 
    stop("'..' not allowed in subdir")
  if (is.null(filetype)) 
    filetype <- basename(url)
  if (grepl("\\.tar\\.gz$", filetype)) 
    fileext <- ".tar.gz"
  else if (grepl("\\.tar$", filetype)) 
    fileext <- ".tar"
  else if (grepl("\\.zip$", filetype)) 
    fileext <- ".zip"
  else stop("Unknown file extension.")
  message("Downloading ", url)
  if (is.null(destdir)) {
    filePath <- tempfile("shinyapp", fileext = fileext)
    fileDir <- tempfile("shinyapp")
  }
  else {
    fileDir <- destdir
    filePath <- paste(destdir, fileext)
  }
  dir.create(fileDir, showWarnings = FALSE)
  if (download(url, filePath, mode = "wb", quiet = TRUE) != 0) 
    stop("Failed to download URL ", url)
  on.exit(unlink(filePath))
  if (fileext %in% c(".tar", ".tar.gz")) {
    first <- untar2(filePath, list = TRUE)[1]
    untar2(filePath, exdir = fileDir)
  }
  else if (fileext == ".zip") {
    first <- as.character(utils::unzip(filePath, list = TRUE)$Name)[1]
    utils::unzip(filePath, exdir = fileDir)
  }
  if (is.null(destdir)) {
    on.exit(unlink(fileDir, recursive = TRUE), add = TRUE)
  }
  appdir <- file.path(fileDir, first)
  if (!utils::file_test("-d", appdir)) 
    appdir <- dirname(appdir)
  if (!is.null(subdir)) 
    appdir <- file.path(appdir, subdir)
  runApp(appdir, ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
safeError
list(`package:shiny` = function (error) 
{
  if (inherits(error, "character")) {
    error <- simpleError(error)
  }
  if (!inherits(error, "error")) {
    stop("The class of the `error` parameter must be either 'error' or 'character'")
  }
  class(error) <- c("shiny.custom.error", class(error))
  error
}, function (error) 
{
  if (inherits(error, "character")) {
    error <- simpleError(error)
  }
  if (!inherits(error, "error")) {
    stop("The class of the `error` parameter must be either 'error' or 'character'")
  }
  class(error) <- c("shiny.custom.error", class(error))
  error
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
selectInput
list(`package:shiny` = function (inputId, label, choices, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL) 
{
  selected <- restoreInput(id = inputId, default = selected)
  choices <- choicesWithNames(choices)
  if (is.null(selected)) {
    if (!multiple) 
      selected <- firstChoice(choices)
  }
  else selected <- as.character(selected)
  if (!is.null(size) && selectize) {
    stop("'size' argument is incompatible with 'selectize=TRUE'.")
  }
  selectTag <- tags$select(id = inputId, class = if (!selectize) 
    "form-control", size = size, selectOptions(choices, selected, inputId, selectize))
  if (multiple) 
    selectTag$attribs$multiple <- "multiple"
  res <- div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), div(selectTag))
  if (!selectize) 
    return(res)
  selectizeIt(inputId, res, NULL, nonempty = !multiple && !("" %in% choices))
}, function (inputId, label, choices, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL) 
{
  selected <- restoreInput(id = inputId, default = selected)
  choices <- choicesWithNames(choices)
  if (is.null(selected)) {
    if (!multiple) 
      selected <- firstChoice(choices)
  }
  else selected <- as.character(selected)
  if (!is.null(size) && selectize) {
    stop("'size' argument is incompatible with 'selectize=TRUE'.")
  }
  selectTag <- tags$select(id = inputId, class = if (!selectize) 
    "form-control", size = size, selectOptions(choices, selected, inputId, selectize))
  if (multiple) 
    selectTag$attribs$multiple <- "multiple"
  res <- div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), div(selectTag))
  if (!selectize) 
    return(res)
  selectizeIt(inputId, res, NULL, nonempty = !multiple && !("" %in% choices))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
selectizeInput
list(`package:shiny` = function (inputId, ..., options = NULL, width = NULL) 
{
  selectizeIt(inputId, selectInput(inputId, ..., selectize = FALSE, width = width), options)
}, function (inputId, ..., options = NULL, width = NULL) 
{
  selectizeIt(inputId, selectInput(inputId, ..., selectize = FALSE, width = width), options)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
serverInfo
list(`package:shiny` = function () 
{
  .globals$serverInfo
}, function () 
{
  .globals$serverInfo
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
setBookmarkExclude
list(`package:shiny` = function (names = character(0), session = getDefaultReactiveDomain()) 
{
  session$setBookmarkExclude(names)
}, function (names = character(0), session = getDefaultReactiveDomain()) 
{
  session$setBookmarkExclude(names)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
setProgress
list(`package:shiny` = function (value = NULL, message = NULL, detail = NULL, session = getDefaultReactiveDomain()) 
{
  if (is.null(session$progressStack)) 
    stop("'session' is not a ShinySession object.")
  if (session$progressStack$size() == 0) {
    warning("setProgress was called outside of withProgress; ignoring")
    return()
  }
  session$progressStack$peek()$set(value, message, detail)
  invisible()
}, function (value = NULL, message = NULL, detail = NULL, session = getDefaultReactiveDomain()) 
{
  if (is.null(session$progressStack)) 
    stop("'session' is not a ShinySession object.")
  if (session$progressStack$size() == 0) {
    warning("setProgress was called outside of withProgress; ignoring")
    return()
  }
  session$progressStack$peek()$set(value, message, detail)
  invisible()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
setSerializer
list(`package:shiny` = function (inputId, fun, session = getDefaultReactiveDomain()) 
{
  if (is.null(session)) {
    stop("setSerializer() needs a session object.")
  }
  input_impl <- .subset2(session$input, "impl")
  input_impl$setMeta(inputId, "shiny.serializer", fun)
}, function (inputId, fun, session = getDefaultReactiveDomain()) 
{
  if (is.null(session)) {
    stop("setSerializer() needs a session object.")
  }
  input_impl <- .subset2(session$input, "impl")
  input_impl$setMeta(inputId, "shiny.serializer", fun)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
shinyApp
list(`package:shiny` = function (ui, server, onStart = NULL, options = list(), uiPattern = "/", enableBookmarking = NULL) 
{
  if (!is.function(server)) {
    stop("`server` must be a function", call. = FALSE)
  }
  uiPattern <- sprintf("^%s$", uiPattern)
  httpHandler <- uiHttpHandler(ui, uiPattern)
  serverFuncSource <- function() {
    server
  }
  if (!is.null(enableBookmarking)) {
    bookmarkStore <- match.arg(enableBookmarking, c("url", "server", "disable"))
    enableBookmarking(bookmarkStore)
  }
  appOptions <- captureAppOptions()
  structure(list(httpHandler = httpHandler, serverFuncSource = serverFuncSource, onStart = onStart, options = options, appOptions = appOptions), class = "shiny.appobj")
}, function (ui, server, onStart = NULL, options = list(), uiPattern = "/", enableBookmarking = NULL) 
{
  if (!is.function(server)) {
    stop("`server` must be a function", call. = FALSE)
  }
  uiPattern <- sprintf("^%s$", uiPattern)
  httpHandler <- uiHttpHandler(ui, uiPattern)
  serverFuncSource <- function() {
    server
  }
  if (!is.null(enableBookmarking)) {
    bookmarkStore <- match.arg(enableBookmarking, c("url", "server", "disable"))
    enableBookmarking(bookmarkStore)
  }
  appOptions <- captureAppOptions()
  structure(list(httpHandler = httpHandler, serverFuncSource = serverFuncSource, onStart = onStart, options = options, appOptions = appOptions), class = "shiny.appobj")
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
shinyAppDir
list(`package:shiny` = function (appDir, options = list()) 
{
  if (!utils::file_test("-d", appDir)) {
    rlang::abort(paste0("No Shiny application exists at the path \"", appDir, "\""), class = "invalidShinyAppDir")
  }
  appDir <- normalizePath(appDir, mustWork = TRUE)
  if (file.exists.ci(appDir, "server.R")) {
    shinyAppDir_serverR(appDir, options = options)
  }
  else if (file.exists.ci(appDir, "app.R")) {
    shinyAppDir_appR("app.R", appDir, options = options)
  }
  else {
    rlang::abort("App dir must contain either app.R or server.R.", class = "invalidShinyAppDir")
  }
}, function (appDir, options = list()) 
{
  if (!utils::file_test("-d", appDir)) {
    rlang::abort(paste0("No Shiny application exists at the path \"", appDir, "\""), class = "invalidShinyAppDir")
  }
  appDir <- normalizePath(appDir, mustWork = TRUE)
  if (file.exists.ci(appDir, "server.R")) {
    shinyAppDir_serverR(appDir, options = options)
  }
  else if (file.exists.ci(appDir, "app.R")) {
    shinyAppDir_appR("app.R", appDir, options = options)
  }
  else {
    rlang::abort("App dir must contain either app.R or server.R.", class = "invalidShinyAppDir")
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
shinyAppFile
list(`package:shiny` = function (appFile, options = list()) 
{
  appFile <- normalizePath(appFile, mustWork = TRUE)
  appDir <- dirname(appFile)
  shinyAppDir_appR(basename(appFile), appDir, options = options)
}, function (appFile, options = list()) 
{
  appFile <- normalizePath(appFile, mustWork = TRUE)
  appDir <- dirname(appFile)
  shinyAppDir_appR(basename(appFile), appDir, options = options)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
shinyAppTemplate
list(`package:shiny` = function (path = NULL, examples = "default", dryrun = FALSE) 
{
  if (is.null(path)) {
    stop("Please provide a `path`.")
  }
  choices <- c(app = "app.R              : Main application file", rdir = "R/example.R        : Helper file with R code", module = "R/example-module.R : Example module", tests = "tests/testthat/    : Tests using {testthat} and {shinytest2}")
  examples[examples == "shinytest"] <- "tests"
  examples[examples == "testthat"] <- "tests"
  examples <- unique(examples)
  if (identical(examples, "default")) {
    if (rlang::is_interactive()) {
      examples <- "ask"
    }
    else {
      examples <- "all"
    }
  }
  if (!identical(examples, "ask") && !identical(examples, "all") && any(!examples %in% names(choices))) {
    stop("`examples` must be one of \"default\", \"ask\", \"all\", or any combination of \"", paste(names(choices), collapse = "\", \""), "\".")
  }
  if (identical(examples, "ask")) {
    response <- select_menu(c(all = "All", choices), title = paste0("Select which of the following to add at ", path, "/ :"), msg = "Enter one or more numbers (with spaces), or an empty line to exit: \n")
    examples <- names(response)
  }
  examples <- unique(examples)
  if ("all" %in% examples) {
    examples <- names(choices)
  }
  if (length(examples) == 0) {
    return(invisible())
  }
  if ("tests" %in% examples) {
    rlang::check_installed("shinytest2", "for {testthat} tests to work as expected")
  }
  dir_is_empty <- function(path) {
    files <- list.files(path, all.files = TRUE, no.. = TRUE)
    files <- setdiff(files, ".DS_Store")
    return(length(files) != 0)
  }
  template_path <- function(...) {
    system_file("app_template", ..., package = "shiny")
  }
  dest_path <- function(...) {
    file.path(path, ...)
  }
  mkdir <- function(path) {
    if (!dirExists(path)) {
      message("Creating ", ensure_trailing_slash(path))
      if (!dryrun) {
        dir.create(path, recursive = TRUE)
      }
    }
  }
  copy_file_one <- function(name) {
    from <- template_path(name)
    to <- dest_path(name)
    message("Creating ", to)
    if (file.exists(to)) {
      stop(to, " already exists. Please remove it and try again.", call. = FALSE)
    }
    if (!dryrun) {
      is_template <- any(grepl("{{", readLines(from), fixed = TRUE))
      if (is_template) {
        writeChar(as.character(htmlTemplate(from, rdir = "rdir" %in% examples, module = "module" %in% examples)), con = to, eos = NULL)
      }
      else {
        file.copy(from, to)
      }
    }
  }
  copy_file <- function(names) {
    for (name in names) {
      copy_file_one(name)
    }
  }
  copy_test_dir <- function() {
    files <- dir(template_path("tests"), recursive = TRUE)
    if (!"rdir" %in% examples) {
      is_r_folder_file <- !grepl("module|server|shinytest2|testthat", basename(files))
      files <- files[!is_r_folder_file]
    }
    if (!"module" %in% examples) {
      files <- files[!grepl("module", files)]
    }
    mkdir(dest_path("tests"))
    dirs <- setdiff(unique(dirname(files)), ".")
    for (dir in dirs) {
      mkdir(dest_path("tests", dir))
    }
    copy_file(file.path("tests", files))
  }
  if (is.null(path)) {
    stop("`path` is missing.")
  }
  if (file.exists(path) && !dirExists(path)) {
    stop(path, " exists but is not a directory.")
  }
  if (dirExists(path) && dir_is_empty(path)) {
    if (interactive()) {
      response <- readline(paste0(ensure_trailing_slash(path), " is not empty. Do you want to use this directory anyway? [y/n] "))
      if (tolower(response) != "y") {
        return(invisible())
      }
    }
  }
  else {
    mkdir(path)
  }
  if ("app" %in% examples) {
    copy_file("app.R")
  }
  if ("rdir" %in% examples) {
    files <- dir(template_path("R"))
    non_module_files <- files[!grepl("module.R$", files)]
    mkdir(dest_path("R"))
    copy_file(file.path("R", non_module_files))
  }
  if ("module" %in% examples) {
    files <- dir(template_path("R"))
    module_files <- files[grepl("module.R$", files)]
    mkdir(dest_path("R"))
    copy_file(file.path("R", module_files))
  }
  if ("tests" %in% examples) {
    copy_test_dir()
  }
  invisible()
}, function (path = NULL, examples = "default", dryrun = FALSE) 
{
  if (is.null(path)) {
    stop("Please provide a `path`.")
  }
  choices <- c(app = "app.R              : Main application file", rdir = "R/example.R        : Helper file with R code", module = "R/example-module.R : Example module", tests = "tests/testthat/    : Tests using {testthat} and {shinytest2}")
  examples[examples == "shinytest"] <- "tests"
  examples[examples == "testthat"] <- "tests"
  examples <- unique(examples)
  if (identical(examples, "default")) {
    if (rlang::is_interactive()) {
      examples <- "ask"
    }
    else {
      examples <- "all"
    }
  }
  if (!identical(examples, "ask") && !identical(examples, "all") && any(!examples %in% names(choices))) {
    stop("`examples` must be one of \"default\", \"ask\", \"all\", or any combination of \"", paste(names(choices), collapse = "\", \""), "\".")
  }
  if (identical(examples, "ask")) {
    response <- select_menu(c(all = "All", choices), title = paste0("Select which of the following to add at ", path, "/ :"), msg = "Enter one or more numbers (with spaces), or an empty line to exit: \n")
    examples <- names(response)
  }
  examples <- unique(examples)
  if ("all" %in% examples) {
    examples <- names(choices)
  }
  if (length(examples) == 0) {
    return(invisible())
  }
  if ("tests" %in% examples) {
    rlang::check_installed("shinytest2", "for {testthat} tests to work as expected")
  }
  dir_is_empty <- function(path) {
    files <- list.files(path, all.files = TRUE, no.. = TRUE)
    files <- setdiff(files, ".DS_Store")
    return(length(files) != 0)
  }
  template_path <- function(...) {
    system_file("app_template", ..., package = "shiny")
  }
  dest_path <- function(...) {
    file.path(path, ...)
  }
  mkdir <- function(path) {
    if (!dirExists(path)) {
      message("Creating ", ensure_trailing_slash(path))
      if (!dryrun) {
        dir.create(path, recursive = TRUE)
      }
    }
  }
  copy_file_one <- function(name) {
    from <- template_path(name)
    to <- dest_path(name)
    message("Creating ", to)
    if (file.exists(to)) {
      stop(to, " already exists. Please remove it and try again.", call. = FALSE)
    }
    if (!dryrun) {
      is_template <- any(grepl("{{", readLines(from), fixed = TRUE))
      if (is_template) {
        writeChar(as.character(htmlTemplate(from, rdir = "rdir" %in% examples, module = "module" %in% examples)), con = to, eos = NULL)
      }
      else {
        file.copy(from, to)
      }
    }
  }
  copy_file <- function(names) {
    for (name in names) {
      copy_file_one(name)
    }
  }
  copy_test_dir <- function() {
    files <- dir(template_path("tests"), recursive = TRUE)
    if (!"rdir" %in% examples) {
      is_r_folder_file <- !grepl("module|server|shinytest2|testthat", basename(files))
      files <- files[!is_r_folder_file]
    }
    if (!"module" %in% examples) {
      files <- files[!grepl("module", files)]
    }
    mkdir(dest_path("tests"))
    dirs <- setdiff(unique(dirname(files)), ".")
    for (dir in dirs) {
      mkdir(dest_path("tests", dir))
    }
    copy_file(file.path("tests", files))
  }
  if (is.null(path)) {
    stop("`path` is missing.")
  }
  if (file.exists(path) && !dirExists(path)) {
    stop(path, " exists but is not a directory.")
  }
  if (dirExists(path) && dir_is_empty(path)) {
    if (interactive()) {
      response <- readline(paste0(ensure_trailing_slash(path), " is not empty. Do you want to use this directory anyway? [y/n] "))
      if (tolower(response) != "y") {
        return(invisible())
      }
    }
  }
  else {
    mkdir(path)
  }
  if ("app" %in% examples) {
    copy_file("app.R")
  }
  if ("rdir" %in% examples) {
    files <- dir(template_path("R"))
    non_module_files <- files[!grepl("module.R$", files)]
    mkdir(dest_path("R"))
    copy_file(file.path("R", non_module_files))
  }
  if ("module" %in% examples) {
    files <- dir(template_path("R"))
    module_files <- files[grepl("module.R$", files)]
    mkdir(dest_path("R"))
    copy_file(file.path("R", module_files))
  }
  if ("tests" %in% examples) {
    copy_test_dir()
  }
  invisible()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
shinyOptions
list(`package:shiny` = function (...) 
{
  newOpts <- list2(...)
  if (length(newOpts) > 0) {
    session <- getDefaultReactiveDomain()
    if (!is.null(session)) {
      session$options <- dropNulls(mergeVectors(session$options, newOpts))
      return(invisible(session$options))
    }
    app_state <- getCurrentAppState()
    if (!is.null(app_state)) {
      app_state$options <- dropNulls(mergeVectors(app_state$options, newOpts))
      return(invisible(app_state$options))
    }
    .globals$options <- dropNulls(mergeVectors(.globals$options, newOpts))
    return(invisible(.globals$options))
  }
  session <- getDefaultReactiveDomain()
  if (!is.null(session)) {
    return(session$options)
  }
  app_state <- getCurrentAppState()
  if (!is.null(app_state)) {
    return(app_state$options)
  }
  return(.globals$options)
}, function (...) 
{
  newOpts <- list2(...)
  if (length(newOpts) > 0) {
    session <- getDefaultReactiveDomain()
    if (!is.null(session)) {
      session$options <- dropNulls(mergeVectors(session$options, newOpts))
      return(invisible(session$options))
    }
    app_state <- getCurrentAppState()
    if (!is.null(app_state)) {
      app_state$options <- dropNulls(mergeVectors(app_state$options, newOpts))
      return(invisible(app_state$options))
    }
    .globals$options <- dropNulls(mergeVectors(.globals$options, newOpts))
    return(invisible(.globals$options))
  }
  session <- getDefaultReactiveDomain()
  if (!is.null(session)) {
    return(session$options)
  }
  app_state <- getCurrentAppState()
  if (!is.null(app_state)) {
    return(app_state$options)
  }
  return(.globals$options)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
shinyServer
list(`package:shiny` = function (func) 
{
  if (in_devmode()) {
    shinyDeprecated("0.10.0", "shinyServer()", details = paste0("When removing `shinyServer()`, ", "ensure that the last expression returned from server.R ", "is the function normally supplied to `shinyServer(func)`."))
  }
  .globals$server <- list(func)
  invisible(func)
}, function (func) 
{
  if (in_devmode()) {
    shinyDeprecated("0.10.0", "shinyServer()", details = paste0("When removing `shinyServer()`, ", "ensure that the last expression returned from server.R ", "is the function normally supplied to `shinyServer(func)`."))
  }
  .globals$server <- list(func)
  invisible(func)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
shinyUI
list(`package:shiny` = function (ui) 
{
  if (in_devmode()) {
    shinyDeprecated("0.10.0", "shinyUI()", details = paste0("When removing `shinyUI()`, ", "ensure that the last expression returned from ui.R is a user interface ", "normally supplied to `shinyUI(ui)`."))
  }
  .globals$ui <- list(ui)
  ui
}, function (ui) 
{
  if (in_devmode()) {
    shinyDeprecated("0.10.0", "shinyUI()", details = paste0("When removing `shinyUI()`, ", "ensure that the last expression returned from ui.R is a user interface ", "normally supplied to `shinyUI(ui)`."))
  }
  .globals$ui <- list(ui)
  ui
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
showBookmarkUrlModal
list(`package:shiny` = function (url) 
{
  store <- getShinyOption("bookmarkStore", default = "")
  if (store == "url") {
    subtitle <- "This link stores the current state of this application."
  }
  else if (store == "server") {
    subtitle <- "The current state of this application has been stored on the server."
  }
  else {
    subtitle <- NULL
  }
  showModal(urlModal(url, subtitle = subtitle))
}, function (url) 
{
  store <- getShinyOption("bookmarkStore", default = "")
  if (store == "url") {
    subtitle <- "This link stores the current state of this application."
  }
  else if (store == "server") {
    subtitle <- "The current state of this application has been stored on the server."
  }
  else {
    subtitle <- NULL
  }
  showModal(urlModal(url, subtitle = subtitle))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
showModal
list(`package:shiny` = function (ui, session = getDefaultReactiveDomain()) 
{
  res <- processDeps(ui, session)
  session$sendModal("show", list(html = res$html, deps = res$deps))
}, function (ui, session = getDefaultReactiveDomain()) 
{
  res <- processDeps(ui, session)
  session$sendModal("show", list(html = res$html, deps = res$deps))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
showNotification
list(`package:shiny` = function (ui, action = NULL, duration = 5, closeButton = TRUE, id = NULL, type = c("default", "message", "warning", "error"), session = getDefaultReactiveDomain()) 
{
  if (is.null(id)) 
    id <- createUniqueId(8)
  res <- processDeps(ui, session)
  actionRes <- processDeps(action, session)
  session$sendNotification("show", list(html = res$html, action = actionRes$html, deps = c(res$deps, actionRes$deps), duration = if (!is.null(duration)) duration * 1000, closeButton = closeButton, id = id, type = match.arg(type)))
  id
}, function (ui, action = NULL, duration = 5, closeButton = TRUE, id = NULL, type = c("default", "message", "warning", "error"), session = getDefaultReactiveDomain()) 
{
  if (is.null(id)) 
    id <- createUniqueId(8)
  res <- processDeps(ui, session)
  actionRes <- processDeps(action, session)
  session$sendNotification("show", list(html = res$html, action = actionRes$html, deps = c(res$deps, actionRes$deps), duration = if (!is.null(duration)) duration * 1000, closeButton = closeButton, id = id, type = match.arg(type)))
  id
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
showTab
list(`package:shiny` = function (inputId, target, select = FALSE, session = getDefaultReactiveDomain()) 
{
  force(target)
  if (select) 
    updateTabsetPanel(session, inputId, selected = target)
  inputId <- session$ns(inputId)
  callback <- function() {
    session$sendChangeTabVisibility(inputId = inputId, target = target, type = "show")
  }
  session$onFlush(callback, once = TRUE)
}, function (inputId, target, select = FALSE, session = getDefaultReactiveDomain()) 
{
  force(target)
  if (select) 
    updateTabsetPanel(session, inputId, selected = target)
  inputId <- session$ns(inputId)
  callback <- function() {
    session$sendChangeTabVisibility(inputId = inputId, target = target, type = "show")
  }
  session$onFlush(callback, once = TRUE)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
sidebarLayout
list(`package:shiny` = function (sidebarPanel, mainPanel, position = c("left", "right"), fluid = TRUE) 
{
  position <- match.arg(position)
  if (position == "left") {
    firstPanel <- sidebarPanel
    secondPanel <- mainPanel
  }
  else if (position == "right") {
    firstPanel <- mainPanel
    secondPanel <- sidebarPanel
  }
  if (fluid) 
    fluidRow(firstPanel, secondPanel)
  else fixedRow(firstPanel, secondPanel)
}, function (sidebarPanel, mainPanel, position = c("left", "right"), fluid = TRUE) 
{
  position <- match.arg(position)
  if (position == "left") {
    firstPanel <- sidebarPanel
    secondPanel <- mainPanel
  }
  else if (position == "right") {
    firstPanel <- mainPanel
    secondPanel <- sidebarPanel
  }
  if (fluid) 
    fluidRow(firstPanel, secondPanel)
  else fixedRow(firstPanel, secondPanel)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
sidebarPanel
list(`package:shiny` = function (..., width = 4) 
{
  div(class = paste0("col-sm-", width), tags$form(class = "well", role = "complementary", ...))
}, function (..., width = 4) 
{
  div(class = paste0("col-sm-", width), tags$form(class = "well", role = "complementary", ...))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
singleton
list(`package:shiny` = function (x, value = TRUE) 
{
  attr(x, "htmltools.singleton") <- if (isTRUE(value)) 
    TRUE
  else NULL
  return(x)
}, function (x, value = TRUE) 
{
  attr(x, "htmltools.singleton") <- if (isTRUE(value)) 
    TRUE
  else NULL
  return(x)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
sizeGrowthRatio
list(`package:shiny` = function (width = 400, height = 400, growthRate = 1.2) 
{
  round_dim_up <- function(x, base, rate) {
    power <- ceiling(log(x/base, rate))
    ceiling(base * rate^power)
  }
  function(dims) {
    if (length(dims) != 2) {
      stop("dims must be a vector with two numbers, for width and height.")
    }
    c(round_dim_up(dims[1], width, growthRate), round_dim_up(dims[2], height, growthRate))
  }
}, function (width = 400, height = 400, growthRate = 1.2) 
{
  round_dim_up <- function(x, base, rate) {
    power <- ceiling(log(x/base, rate))
    ceiling(base * rate^power)
  }
  function(dims) {
    if (length(dims) != 2) {
      stop("dims must be a vector with two numbers, for width and height.")
    }
    c(round_dim_up(dims[1], width, growthRate), round_dim_up(dims[2], height, growthRate))
  }
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
sliderInput
list(`package:shiny` = function (inputId, label, min, max, value, step = NULL, round = FALSE, ticks = TRUE, animate = FALSE, width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL, timezone = NULL, dragRange = TRUE) 
{
  validate_slider_value(min, max, value, "sliderInput")
  dataType <- getSliderType(min, max, value)
  if (is.null(timeFormat)) {
    timeFormat <- switch(dataType, date = "%F", datetime = "%F %T", number = NULL)
  }
  value <- restoreInput(id = inputId, default = value)
  if (is.character(value)) {
    if (dataType == "date") {
      value <- as.Date(value, format = "%Y-%m-%d")
    }
    else if (dataType == "datetime") {
      value <- as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
  }
  step <- findStepSize(min, max, step)
  if (dataType %in% c("date", "datetime")) {
    to_ms <- function(x) 1000 * as.numeric(as.POSIXct(x))
    step <- to_ms(max) - to_ms(max - step)
    min <- to_ms(min)
    max <- to_ms(max)
    value <- to_ms(value)
  }
  range <- max - min
  if (ticks) {
    n_steps <- range/step
    scale_factor <- ceiling(n_steps/10)
    n_ticks <- n_steps/scale_factor
  }
  else {
    n_ticks <- NULL
  }
  sliderProps <- dropNulls(list(class = "js-range-slider", id = inputId, `data-skin` = "shiny", `data-type` = if (length(value) > 1) "double", `data-min` = formatNoSci(min), `data-max` = formatNoSci(max), `data-from` = formatNoSci(value[1]), `data-to` = if (length(value) > 1) formatNoSci(value[2]), `data-step` = formatNoSci(step), `data-grid` = ticks, `data-grid-num` = n_ticks, `data-grid-snap` = FALSE, `data-prettify-separator` = sep, `data-prettify-enabled` = (sep != ""), `data-prefix` = pre, 
                                `data-postfix` = post, `data-keyboard` = TRUE, `data-drag-interval` = if (length(value) > 1) dragRange, `data-data-type` = dataType, `data-time-format` = timeFormat, `data-timezone` = timezone))
  sliderProps <- lapply(sliderProps, function(x) {
    if (identical(x, TRUE)) 
      "true"
    else if (identical(x, FALSE)) 
      "false"
    else x
  })
  sliderTag <- div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), do.call(tags$input, sliderProps))
  if (identical(animate, TRUE)) 
    animate <- animationOptions()
  if (!is.null(animate) && !identical(animate, FALSE)) {
    if (is.null(animate$playButton)) 
      animate$playButton <- icon("play", lib = "glyphicon")
    if (is.null(animate$pauseButton)) 
      animate$pauseButton <- icon("pause", lib = "glyphicon")
    sliderTag <- tagAppendChild(sliderTag, tags$div(class = "slider-animate-container", tags$a(href = "#", class = "slider-animate-button", `data-target-id` = inputId, `data-interval` = animate$interval, `data-loop` = animate$loop, span(class = "play", animate$playButton), span(class = "pause", animate$pauseButton))))
  }
  attachDependencies(sliderTag, ionRangeSliderDependency())
}, function (inputId, label, min, max, value, step = NULL, round = FALSE, ticks = TRUE, animate = FALSE, width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL, timezone = NULL, dragRange = TRUE) 
{
  validate_slider_value(min, max, value, "sliderInput")
  dataType <- getSliderType(min, max, value)
  if (is.null(timeFormat)) {
    timeFormat <- switch(dataType, date = "%F", datetime = "%F %T", number = NULL)
  }
  value <- restoreInput(id = inputId, default = value)
  if (is.character(value)) {
    if (dataType == "date") {
      value <- as.Date(value, format = "%Y-%m-%d")
    }
    else if (dataType == "datetime") {
      value <- as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
  }
  step <- findStepSize(min, max, step)
  if (dataType %in% c("date", "datetime")) {
    to_ms <- function(x) 1000 * as.numeric(as.POSIXct(x))
    step <- to_ms(max) - to_ms(max - step)
    min <- to_ms(min)
    max <- to_ms(max)
    value <- to_ms(value)
  }
  range <- max - min
  if (ticks) {
    n_steps <- range/step
    scale_factor <- ceiling(n_steps/10)
    n_ticks <- n_steps/scale_factor
  }
  else {
    n_ticks <- NULL
  }
  sliderProps <- dropNulls(list(class = "js-range-slider", id = inputId, `data-skin` = "shiny", `data-type` = if (length(value) > 1) "double", `data-min` = formatNoSci(min), `data-max` = formatNoSci(max), `data-from` = formatNoSci(value[1]), `data-to` = if (length(value) > 1) formatNoSci(value[2]), `data-step` = formatNoSci(step), `data-grid` = ticks, `data-grid-num` = n_ticks, `data-grid-snap` = FALSE, `data-prettify-separator` = sep, `data-prettify-enabled` = (sep != ""), `data-prefix` = pre, 
                                `data-postfix` = post, `data-keyboard` = TRUE, `data-drag-interval` = if (length(value) > 1) dragRange, `data-data-type` = dataType, `data-time-format` = timeFormat, `data-timezone` = timezone))
  sliderProps <- lapply(sliderProps, function(x) {
    if (identical(x, TRUE)) 
      "true"
    else if (identical(x, FALSE)) 
      "false"
    else x
  })
  sliderTag <- div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), do.call(tags$input, sliderProps))
  if (identical(animate, TRUE)) 
    animate <- animationOptions()
  if (!is.null(animate) && !identical(animate, FALSE)) {
    if (is.null(animate$playButton)) 
      animate$playButton <- icon("play", lib = "glyphicon")
    if (is.null(animate$pauseButton)) 
      animate$pauseButton <- icon("pause", lib = "glyphicon")
    sliderTag <- tagAppendChild(sliderTag, tags$div(class = "slider-animate-container", tags$a(href = "#", class = "slider-animate-button", `data-target-id` = inputId, `data-interval` = animate$interval, `data-loop` = animate$loop, span(class = "play", animate$playButton), span(class = "pause", animate$pauseButton))))
  }
  attachDependencies(sliderTag, ionRangeSliderDependency())
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
snapshotExclude
list(`package:shiny` = function (x) 
{
  markOutputAttrs(x, snapshotExclude = TRUE)
}, function (x) 
{
  markOutputAttrs(x, snapshotExclude = TRUE)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
snapshotPreprocessInput
list(`package:shiny` = function (inputId, fun, session = getDefaultReactiveDomain()) 
{
  if (is.null(session)) {
    stop("snapshotPreprocessInput() needs a session object.")
  }
  input_impl <- .subset2(session$input, "impl")
  input_impl$setMeta(inputId, "shiny.snapshot.preprocess", fun)
}, function (inputId, fun, session = getDefaultReactiveDomain()) 
{
  if (is.null(session)) {
    stop("snapshotPreprocessInput() needs a session object.")
  }
  input_impl <- .subset2(session$input, "impl")
  input_impl$setMeta(inputId, "shiny.snapshot.preprocess", fun)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
snapshotPreprocessOutput
list(`package:shiny` = function (x, fun) 
{
  markOutputAttrs(x, snapshotPreprocess = fun)
}, function (x, fun) 
{
  markOutputAttrs(x, snapshotPreprocess = fun)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
span
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("span", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("span", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
splitLayout
list(`package:shiny` = function (..., cellWidths = NULL, cellArgs = list()) 
{
  children <- list2(...)
  childIdx <- !nzchar(names(children) %||% character(length(children)))
  attribs <- children[!childIdx]
  children <- children[childIdx]
  count <- length(children)
  if (length(cellWidths) == 0 || isTRUE(is.na(cellWidths))) {
    cellWidths <- sprintf("%.3f%%", 100/count)
  }
  cellWidths <- rep(cellWidths, length.out = count)
  cellWidths <- sapply(cellWidths, validateCssUnit)
  do.call(tags$div, c(list(class = "shiny-split-layout"), attribs, mapply(children, cellWidths, FUN = function(x, w) {
    do.call(tags$div, c(list(style = sprintf("width: %s;", w)), cellArgs, list(x)))
  }, SIMPLIFY = FALSE)))
}, function (..., cellWidths = NULL, cellArgs = list()) 
{
  children <- list2(...)
  childIdx <- !nzchar(names(children) %||% character(length(children)))
  attribs <- children[!childIdx]
  children <- children[childIdx]
  count <- length(children)
  if (length(cellWidths) == 0 || isTRUE(is.na(cellWidths))) {
    cellWidths <- sprintf("%.3f%%", 100/count)
  }
  cellWidths <- rep(cellWidths, length.out = count)
  cellWidths <- sapply(cellWidths, validateCssUnit)
  do.call(tags$div, c(list(class = "shiny-split-layout"), attribs, mapply(children, cellWidths, FUN = function(x, w) {
    do.call(tags$div, c(list(style = sprintf("width: %s;", w)), cellArgs, list(x)))
  }, SIMPLIFY = FALSE)))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
stopApp
list(`package:shiny` = function (returnValue = invisible()) 
{
  .globals$reterror <- FALSE
  ..stacktraceoff..(tryCatch({
    captureStackTraces(.globals$retval <- withVisible(..stacktraceon..(force(returnValue))))
  }, error = function(e) {
    .globals$retval <- e
    .globals$reterror <- TRUE
  }))
  .globals$stopped <- TRUE
  httpuv::interrupt()
}, function (returnValue = invisible()) 
{
  .globals$reterror <- FALSE
  ..stacktraceoff..(tryCatch({
    captureStackTraces(.globals$retval <- withVisible(..stacktraceon..(force(returnValue))))
  }, error = function(e) {
    .globals$retval <- e
    .globals$reterror <- TRUE
  }))
  .globals$stopped <- TRUE
  httpuv::interrupt()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
strong
list(`package:shiny` = function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("strong", contents, .noWS = .noWS, .renderHook = .renderHook)
}, function (..., .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  contents <- dots_list(...)
  tag("strong", contents, .noWS = .noWS, .renderHook = .renderHook)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
submitButton
list(`package:shiny` = function (text = "Apply Changes", icon = NULL, width = NULL) 
{
  div(tags$button(type = "submit", class = "btn btn-primary", style = css(width = validateCssUnit(width)), list(icon, text)))
}, function (text = "Apply Changes", icon = NULL, width = NULL) 
{
  div(tags$button(type = "submit", class = "btn btn-primary", style = css(width = validateCssUnit(width)), list(icon, text)))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
suppressDependencies
list(`package:shiny` = function (...) 
{
  lapply(dots_list(...), function(name) {
    attachDependencies(character(0), htmlDependency(name, "9999", c(href = "")))
  })
}, function (...) 
{
  lapply(dots_list(...), function(name) {
    attachDependencies(character(0), htmlDependency(name, "9999", c(href = "")))
  })
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
tableOutput
list(`package:shiny` = function (outputId) 
{
  div(id = outputId, class = "shiny-html-output")
}, function (outputId) 
{
  div(id = outputId, class = "shiny-html-output")
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
tabPanel
list(`package:shiny` = function (title, ..., value = title, icon = NULL) 
{
  bslib::nav(title, ..., value = value, icon = icon)
}, function (title, ..., value = title, icon = NULL) 
{
  bslib::nav(title, ..., value = value, icon = icon)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
tabPanelBody
list(`package:shiny` = function (value, ..., icon = NULL) 
{
  bslib::nav_content(value, ..., icon = icon)
}, function (value, ..., icon = NULL) 
{
  bslib::nav_content(value, ..., icon = icon)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
tabsetPanel
list(`package:shiny` = function (..., id = NULL, selected = NULL, type = c("tabs", "pills", "hidden"), header = NULL, footer = NULL) 
{
  func <- switch(match.arg(type), tabs = bslib::navs_tab, pills = bslib::navs_pill, hidden = bslib::navs_hidden)
  remove_first_class(func(..., id = id, selected = selected, header = header, footer = footer))
}, function (..., id = NULL, selected = NULL, type = c("tabs", "pills", "hidden"), header = NULL, footer = NULL) 
{
  func <- switch(match.arg(type), tabs = bslib::navs_tab, pills = bslib::navs_pill, hidden = bslib::navs_hidden)
  remove_first_class(func(..., id = id, selected = selected, header = header, footer = footer))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
tag
list(`package:shiny` = function (`_tag_name`, varArgs, .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  varArgsNames <- names2(varArgs)
  named_idx <- nzchar(varArgsNames)
  attribs <- dropNullsOrEmpty(varArgs[named_idx])
  children <- unname(varArgs[!named_idx])
  st <- list(name = `_tag_name`, attribs = attribs, children = children)
  if (!is.null(.noWS)) {
    st$.noWS <- .noWS
  }
  if (!is.null(.renderHook)) {
    if (!is.list(.renderHook)) {
      .renderHook <- list(.renderHook)
    }
    st$.renderHooks <- .renderHook
  }
  structure(st, class = "shiny.tag")
}, function (`_tag_name`, varArgs, .noWS = NULL, .renderHook = NULL) 
{
  validateNoWS(.noWS)
  varArgsNames <- names2(varArgs)
  named_idx <- nzchar(varArgsNames)
  attribs <- dropNullsOrEmpty(varArgs[named_idx])
  children <- unname(varArgs[!named_idx])
  st <- list(name = `_tag_name`, attribs = attribs, children = children)
  if (!is.null(.noWS)) {
    st$.noWS <- .noWS
  }
  if (!is.null(.renderHook)) {
    if (!is.list(.renderHook)) {
      .renderHook <- list(.renderHook)
    }
    st$.renderHooks <- .renderHook
  }
  structure(st, class = "shiny.tag")
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
tagAppendAttributes
list(`package:shiny` = function (tag, ..., .cssSelector = NULL) 
{
  throw_if_tag_function(tag)
  if (!is.null(.cssSelector)) {
    return(tagQuery(tag)$find(.cssSelector)$addAttrs(...)$allTags())
  }
  newAttribs <- dropNullsOrEmpty(dots_list(...))
  if (any(!nzchar(names2(newAttribs)))) {
    stop("At least one of the new attribute values did not have a name.\n", "Did you forget to include an attribute name?")
  }
  tag$attribs <- c(tag$attribs, newAttribs)
  tag
}, function (tag, ..., .cssSelector = NULL) 
{
  throw_if_tag_function(tag)
  if (!is.null(.cssSelector)) {
    return(tagQuery(tag)$find(.cssSelector)$addAttrs(...)$allTags())
  }
  newAttribs <- dropNullsOrEmpty(dots_list(...))
  if (any(!nzchar(names2(newAttribs)))) {
    stop("At least one of the new attribute values did not have a name.\n", "Did you forget to include an attribute name?")
  }
  tag$attribs <- c(tag$attribs, newAttribs)
  tag
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
tagAppendChild
list(`package:shiny` = function (tag, child, .cssSelector = NULL) 
{
  throw_if_tag_function(tag)
  if (!is.null(.cssSelector)) {
    return(tagAppendChildren(tag, child, .cssSelector = .cssSelector))
  }
  tag$children[[length(tag$children) + 1]] <- child
  tag
}, function (tag, child, .cssSelector = NULL) 
{
  throw_if_tag_function(tag)
  if (!is.null(.cssSelector)) {
    return(tagAppendChildren(tag, child, .cssSelector = .cssSelector))
  }
  tag$children[[length(tag$children) + 1]] <- child
  tag
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
tagAppendChildren
list(`package:shiny` = function (tag, ..., .cssSelector = NULL, list = NULL) 
{
  throw_if_tag_function(tag)
  children <- unname(c(dots_list(...), list))
  if (!is.null(.cssSelector)) {
    return(tagQuery(tag)$find(.cssSelector)$append(!!!children)$allTags())
  }
  tag$children <- unname(c(tag$children, children))
  tag
}, function (tag, ..., .cssSelector = NULL, list = NULL) 
{
  throw_if_tag_function(tag)
  children <- unname(c(dots_list(...), list))
  if (!is.null(.cssSelector)) {
    return(tagQuery(tag)$find(.cssSelector)$append(!!!children)$allTags())
  }
  tag$children <- unname(c(tag$children, children))
  tag
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
tagGetAttribute
list(`package:shiny` = function (tag, attr) 
{
  throw_if_tag_function(tag)
  attribs <- tag$attribs
  attrIdx <- which(attr == names(attribs))
  if (length(attrIdx) == 0) {
    return(NULL)
  }
  result <- attribs[attrIdx]
  if (anyNA(result)) {
    na_idx <- is.na(result)
    if (all(na_idx)) {
      return(NA)
    }
    result <- result[!na_idx]
  }
  if (all(vapply(result, is.atomic, logical(1)))) {
    vals <- vapply(result, function(val) {
      val <- as.character(val)
      if (length(val) > 1) {
        val <- paste0(val, collapse = " ")
      }
      val
    }, character(1))
    result <- paste0(vals, collapse = " ")
  }
  else {
    names(result) <- NULL
  }
  result
}, function (tag, attr) 
{
  throw_if_tag_function(tag)
  attribs <- tag$attribs
  attrIdx <- which(attr == names(attribs))
  if (length(attrIdx) == 0) {
    return(NULL)
  }
  result <- attribs[attrIdx]
  if (anyNA(result)) {
    na_idx <- is.na(result)
    if (all(na_idx)) {
      return(NA)
    }
    result <- result[!na_idx]
  }
  if (all(vapply(result, is.atomic, logical(1)))) {
    vals <- vapply(result, function(val) {
      val <- as.character(val)
      if (length(val) > 1) {
        val <- paste0(val, collapse = " ")
      }
      val
    }, character(1))
    result <- paste0(vals, collapse = " ")
  }
  else {
    names(result) <- NULL
  }
  result
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
tagHasAttribute
list(`package:shiny` = function (tag, attr) 
{
  throw_if_tag_function(tag)
  result <- attr %in% names(tag$attribs)
  result
}, function (tag, attr) 
{
  throw_if_tag_function(tag)
  result <- attr %in% names(tag$attribs)
  result
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
tagList
list(`package:shiny` = function (...) 
{
  lst <- dots_list(...)
  class(lst) <- c("shiny.tag.list", "list")
  return(lst)
}, function (...) 
{
  lst <- dots_list(...)
  class(lst) <- c("shiny.tag.list", "list")
  return(lst)
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
236)
tagSetChildren
list(`package:shiny` = function (tag, ..., .cssSelector = NULL, list = NULL) 
{
  throw_if_tag_function(tag)
  children <- unname(c(dots_list(...), list))
  if (!is.null(.cssSelector)) {
    return(tagQuery(tag)$find(.cssSelector)$empty()$append(!!!children)$allTags())
  }
  tag$children <- children
  tag
}, function (tag, ..., .cssSelector = NULL, list = NULL) 
{
  throw_if_tag_function(tag)
  children <- unname(c(dots_list(...), list))
  if (!is.null(.cssSelector)) {
    return(tagQuery(tag)$find(.cssSelector)$empty()$append(!!!children)$allTags())
  }
  tag$children <- children
  tag
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
testServer
list(`package:shiny` = function (app = NULL, expr, args = list(), session = MockShinySession$new()) 
{
  require(shiny)
  if (!is.null(getDefaultReactiveDomain())) 
    stop("testServer() is for use only within tests and may not indirectly call itself.")
  on.exit(if (!session$isClosed()) session$close(), add = TRUE)
  quosure <- rlang::enquo(expr)
  if (isModuleServer(app)) {
    if (!("id" %in% names(args))) 
      args[["id"]] <- session$genId()
    withMockContext(session, rlang::exec(app, !!!args))
    parent_clone <- rlang::env_clone(parent.env(session$env))
    clone <- rlang::env_clone(session$env, parent_clone)
    mask <- rlang::new_data_mask(clone, parent_clone)
    withMockContext(session, rlang::eval_tidy(quosure, mask, rlang::caller_env()))
    return(invisible())
  }
  if (is.null(app)) {
    path <- findEnclosingApp(".")
    app <- shinyAppDir(path)
  }
  else if (isServer(app)) {
    app <- shinyApp(fluidPage(), app)
  }
  else {
    app <- as.shiny.appobj(app)
  }
  if (!is.null(app$onStart)) 
    app$onStart()
  if (!is.null(app$onStop)) 
    on.exit(app$onStop(), add = TRUE)
  server <- app$serverFuncSource()
  if (!"session" %in% names(formals(server))) 
    stop("Tested application server functions must declare input, output, and session arguments.")
  if (length(args)) 
    stop("Arguments were provided to a server function.")
  body(server) <- rlang::expr({
    session$setEnv(base::environment())
    !!body(server)
  })
  withMockContext(session, server(input = session$input, output = session$output, session = session))
  mask <- rlang::new_data_mask(rlang::env_clone(session$env))
  withMockContext(session, {
    rlang::eval_tidy(quosure, mask, rlang::caller_env())
  })
  invisible()
}, function (app = NULL, expr, args = list(), session = MockShinySession$new()) 
{
  require(shiny)
  if (!is.null(getDefaultReactiveDomain())) 
    stop("testServer() is for use only within tests and may not indirectly call itself.")
  on.exit(if (!session$isClosed()) session$close(), add = TRUE)
  quosure <- rlang::enquo(expr)
  if (isModuleServer(app)) {
    if (!("id" %in% names(args))) 
      args[["id"]] <- session$genId()
    withMockContext(session, rlang::exec(app, !!!args))
    parent_clone <- rlang::env_clone(parent.env(session$env))
    clone <- rlang::env_clone(session$env, parent_clone)
    mask <- rlang::new_data_mask(clone, parent_clone)
    withMockContext(session, rlang::eval_tidy(quosure, mask, rlang::caller_env()))
    return(invisible())
  }
  if (is.null(app)) {
    path <- findEnclosingApp(".")
    app <- shinyAppDir(path)
  }
  else if (isServer(app)) {
    app <- shinyApp(fluidPage(), app)
  }
  else {
    app <- as.shiny.appobj(app)
  }
  if (!is.null(app$onStart)) 
    app$onStart()
  if (!is.null(app$onStop)) 
    on.exit(app$onStop(), add = TRUE)
  server <- app$serverFuncSource()
  if (!"session" %in% names(formals(server))) 
    stop("Tested application server functions must declare input, output, and session arguments.")
  if (length(args)) 
    stop("Arguments were provided to a server function.")
  body(server) <- rlang::expr({
    session$setEnv(base::environment())
    !!body(server)
  })
  withMockContext(session, server(input = session$input, output = session$output, session = session))
  mask <- rlang::new_data_mask(rlang::env_clone(session$env))
  withMockContext(session, {
    rlang::eval_tidy(quosure, mask, rlang::caller_env())
  })
  invisible()
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
textAreaInput
list(`package:shiny` = function (inputId, label, value = "", width = NULL, height = NULL, cols = NULL, rows = NULL, placeholder = NULL, resize = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  if (!is.null(resize)) {
    resize <- match.arg(resize, c("both", "none", "vertical", "horizontal"))
  }
  style <- css(width = if (!is.null(width)) 
    "width: 100%;", height = validateCssUnit(height), resize = resize)
  div(class = "form-group shiny-input-container", shinyInputLabel(inputId, label), style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), tags$textarea(id = inputId, class = "form-control", placeholder = placeholder, style = style, rows = rows, cols = cols, value))
}, function (inputId, label, value = "", width = NULL, height = NULL, cols = NULL, rows = NULL, placeholder = NULL, resize = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  if (!is.null(resize)) {
    resize <- match.arg(resize, c("both", "none", "vertical", "horizontal"))
  }
  style <- css(width = if (!is.null(width)) 
    "width: 100%;", height = validateCssUnit(height), resize = resize)
  div(class = "form-group shiny-input-container", shinyInputLabel(inputId, label), style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), tags$textarea(id = inputId, class = "form-control", placeholder = placeholder, style = style, rows = rows, cols = cols, value))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
textInput
list(`package:shiny` = function (inputId, label, value = "", width = NULL, placeholder = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), tags$input(id = inputId, type = "text", class = "form-control", value = value, placeholder = placeholder))
}, function (inputId, label, value = "", width = NULL, placeholder = NULL) 
{
  value <- restoreInput(id = inputId, default = value)
  div(class = "form-group shiny-input-container", style = css(width = validateCssUnit(width)), shinyInputLabel(inputId, label), tags$input(id = inputId, type = "text", class = "form-control", value = value, placeholder = placeholder))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
textOutput
list(`package:shiny` = function (outputId, container = if (inline) span else div, inline = FALSE) 
{
  container(id = outputId, class = "shiny-text-output")
}, function (outputId, container = if (inline) span else div, inline = FALSE) 
{
  container(id = outputId, class = "shiny-text-output")
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
throttle
list(`package:shiny` = function (r, millis, priority = 100, domain = getDefaultReactiveDomain()) 
{
  force(r)
  force(millis)
  if (!is.function(millis)) {
    origMillis <- millis
    millis <- function() origMillis
  }
  v <- reactiveValues(trigger = 0, lastTriggeredAt = NULL, pending = FALSE)
  blackoutMillisLeft <- function() {
    if (is.null(v$lastTriggeredAt)) {
      0
    }
    else {
      max(0, v$lastTriggeredAt + millis() - getDomainTimeMs(domain))
    }
  }
  trigger <- function() {
    v$lastTriggeredAt <- getDomainTimeMs(domain)
    v$trigger <- isolate(v$trigger)%%999999999 + 1
    v$pending <- FALSE
  }
  observeEvent(try(r(), silent = TRUE), {
    if (v$pending) {
    }
    else if (blackoutMillisLeft() > 0) {
      v$pending <- TRUE
    }
    else {
      trigger()
    }
  }, label = "throttle tracker", ignoreNULL = FALSE, priority = priority, domain = domain)
  observe({
    if (!v$pending) {
      return()
    }
    timeout <- blackoutMillisLeft()
    if (timeout > 0) {
      invalidateLater(timeout)
    }
    else {
      trigger()
    }
  }, priority = priority, domain = domain)
  eventReactive(v$trigger, {
    r()
  }, label = "throttle result", ignoreNULL = FALSE, domain = domain)
}, function (r, millis, priority = 100, domain = getDefaultReactiveDomain()) 
{
  force(r)
  force(millis)
  if (!is.function(millis)) {
    origMillis <- millis
    millis <- function() origMillis
  }
  v <- reactiveValues(trigger = 0, lastTriggeredAt = NULL, pending = FALSE)
  blackoutMillisLeft <- function() {
    if (is.null(v$lastTriggeredAt)) {
      0
    }
    else {
      max(0, v$lastTriggeredAt + millis() - getDomainTimeMs(domain))
    }
  }
  trigger <- function() {
    v$lastTriggeredAt <- getDomainTimeMs(domain)
    v$trigger <- isolate(v$trigger)%%999999999 + 1
    v$pending <- FALSE
  }
  observeEvent(try(r(), silent = TRUE), {
    if (v$pending) {
    }
    else if (blackoutMillisLeft() > 0) {
      v$pending <- TRUE
    }
    else {
      trigger()
    }
  }, label = "throttle tracker", ignoreNULL = FALSE, priority = priority, domain = domain)
  observe({
    if (!v$pending) {
      return()
    }
    timeout <- blackoutMillisLeft()
    if (timeout > 0) {
      invalidateLater(timeout)
    }
    else {
      trigger()
    }
  }, priority = priority, domain = domain)
  eventReactive(v$trigger, {
    r()
  }, label = "throttle result", ignoreNULL = FALSE, domain = domain)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
titlePanel
list(`package:shiny` = function (title, windowTitle = title) 
{
  tagList(tags$head(tags$title(windowTitle)), h2(title))
}, function (title, windowTitle = title) 
{
  tagList(tags$head(tags$title(windowTitle)), h2(title))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
uiOutput
list(`package:shiny` = function (outputId, inline = FALSE, container = if (inline) span else div, ...) 
{
  if (any_unnamed(list(...))) {
    warning("Unnamed elements in ... will be replaced with dynamic UI.")
  }
  container(id = outputId, class = "shiny-html-output", ...)
}, function (outputId, inline = FALSE, container = if (inline) span else div, ...) 
{
  if (any_unnamed(list(...))) {
    warning("Unnamed elements in ... will be replaced with dynamic UI.")
  }
  container(id = outputId, class = "shiny-html-output", ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateActionButton
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, icon = NULL) 
{
  validate_session_object(session)
  if (!is.null(icon)) 
    icon <- as.character(validateIcon(icon))
  message <- dropNulls(list(label = label, icon = icon))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, icon = NULL) 
{
  validate_session_object(session)
  if (!is.null(icon)) 
    icon <- as.character(validateIcon(icon))
  message <- dropNulls(list(label = label, icon = icon))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateActionLink
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, icon = NULL) 
{
  validate_session_object(session)
  if (!is.null(icon)) 
    icon <- as.character(validateIcon(icon))
  message <- dropNulls(list(label = label, icon = icon))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, icon = NULL) 
{
  validate_session_object(session)
  if (!is.null(icon)) 
    icon <- as.character(validateIcon(icon))
  message <- dropNulls(list(label = label, icon = icon))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateCheckboxGroupInput
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL, selected = NULL, inline = FALSE, choiceNames = NULL, choiceValues = NULL) 
{
  validate_session_object(session)
  updateInputOptions(session, inputId, label, choices, selected, inline, "checkbox", choiceNames, choiceValues)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL, selected = NULL, inline = FALSE, choiceNames = NULL, choiceValues = NULL) 
{
  validate_session_object(session)
  updateInputOptions(session, inputId, label, choices, selected, inline, "checkbox", choiceNames, choiceValues)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateCheckboxInput
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(label = label, value = value))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(label = label, value = value))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateDateInput
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, min = NULL, max = NULL) 
{
  validate_session_object(session)
  value <- dateYMD(value, "value")
  min <- dateYMD(min, "min")
  max <- dateYMD(max, "max")
  message <- dropNulls(list(label = label, value = value, min = min, max = max))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, min = NULL, max = NULL) 
{
  validate_session_object(session)
  value <- dateYMD(value, "value")
  min <- dateYMD(min, "min")
  max <- dateYMD(max, "max")
  message <- dropNulls(list(label = label, value = value, min = min, max = max))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateDateRangeInput
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, start = NULL, end = NULL, min = NULL, max = NULL) 
{
  validate_session_object(session)
  start <- dateYMD(start, "start")
  end <- dateYMD(end, "end")
  min <- dateYMD(min, "min")
  max <- dateYMD(max, "max")
  message <- dropNulls(list(label = label, value = dropNulls(list(start = start, end = end)), min = min, max = max))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, start = NULL, end = NULL, min = NULL, max = NULL) 
{
  validate_session_object(session)
  start <- dateYMD(start, "start")
  end <- dateYMD(end, "end")
  min <- dateYMD(min, "min")
  max <- dateYMD(max, "max")
  message <- dropNulls(list(label = label, value = dropNulls(list(start = start, end = end)), min = min, max = max))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateNavbarPage
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, selected = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(value = selected))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, selected = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(value = selected))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateNavlistPanel
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, selected = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(value = selected))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, selected = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(value = selected))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateNumericInput
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, min = NULL, max = NULL, step = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(label = label, value = formatNoSci(value), min = formatNoSci(min), max = formatNoSci(max), step = formatNoSci(step)))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, min = NULL, max = NULL, step = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(label = label, value = formatNoSci(value), min = formatNoSci(min), max = formatNoSci(max), step = formatNoSci(step)))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateQueryString
list(`package:shiny` = function (queryString, mode = c("replace", "push"), session = getDefaultReactiveDomain()) 
{
  mode <- match.arg(mode)
  session$updateQueryString(queryString, mode)
}, function (queryString, mode = c("replace", "push"), session = getDefaultReactiveDomain()) 
{
  mode <- match.arg(mode)
  session$updateQueryString(queryString, mode)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateRadioButtons
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL, selected = NULL, inline = FALSE, choiceNames = NULL, choiceValues = NULL) 
{
  validate_session_object(session)
  if (is.null(selected)) {
    if (!is.null(choices)) 
      selected <- choices[[1]]
    else if (!is.null(choiceValues)) 
      selected <- choiceValues[[1]]
  }
  updateInputOptions(session, inputId, label, choices, selected, inline, "radio", choiceNames, choiceValues)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL, selected = NULL, inline = FALSE, choiceNames = NULL, choiceValues = NULL) 
{
  validate_session_object(session)
  if (is.null(selected)) {
    if (!is.null(choices)) 
      selected <- choices[[1]]
    else if (!is.null(choiceValues)) 
      selected <- choiceValues[[1]]
  }
  updateInputOptions(session, inputId, label, choices, selected, inline, "radio", choiceNames, choiceValues)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateSelectInput
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL, selected = NULL) 
{
  validate_session_object(session)
  choices <- if (!is.null(choices)) 
    choicesWithNames(choices)
  if (!is.null(selected)) 
    selected <- as.character(selected)
  options <- if (!is.null(choices)) 
    selectOptions(choices, selected, inputId, FALSE)
  message <- dropNulls(list(label = label, options = options, value = selected))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL, selected = NULL) 
{
  validate_session_object(session)
  choices <- if (!is.null(choices)) 
    choicesWithNames(choices)
  if (!is.null(selected)) 
    selected <- as.character(selected)
  options <- if (!is.null(choices)) 
    selectOptions(choices, selected, inputId, FALSE)
  message <- dropNulls(list(label = label, options = options, value = selected))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateSelectizeInput
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL, selected = NULL, options = list(), server = FALSE) 
{
  validate_session_object(session)
  if (length(options)) {
    res <- checkAsIs(options)
    cfg <- tags$script(type = "application/json", `data-for` = session$ns(inputId), `data-eval` = if (length(res$eval)) 
      HTML(toJSON(res$eval)), HTML(toJSON(res$options)))
    session$sendInputMessage(inputId, list(config = as.character(cfg)))
  }
  if (!server) {
    return(updateSelectInput(session, inputId, label, choices, selected))
  }
  noOptGroup <- TRUE
  if (is.list(choices)) {
    for (i in seq_along(choices)) {
      if (is.list(choices[[i]]) || length(choices[[i]]) > 1) {
        noOptGroup <- FALSE
        (break)()
      }
    }
  }
  choices <- if (is.data.frame(choices)) {
    as.data.frame(choices, stringsAsFactors = FALSE)
  }
  else if (is.atomic(choices) || noOptGroup) {
    if (is.list(choices)) {
      choices <- unlist(choices)
    }
    if (is.null(names(choices))) {
      lab <- as.character(choices)
    }
    else {
      lab <- names(choices)
      empty_names_indices <- lab == ""
      lab[empty_names_indices] <- as.character(choices[empty_names_indices])
    }
    data.frame(label = lab, value = choices, stringsAsFactors = FALSE)
  }
  else {
    list_names <- names(choices)
    if (is.null(list_names)) {
      list_names <- rep("", length(choices))
    }
    choice_list <- mapply(choices, list_names, FUN = function(choice, name) {
      group <- ""
      lab <- name
      if (lab == "") 
        lab <- as.character(choice)
      if (is.list(choice) || length(choice) > 1) {
        group <- rep(name, length(choice))
        choice <- unlist(choice)
        if (is.null(names(choice))) {
          lab <- as.character(choice)
        }
        else {
          lab <- names(choice)
          empty_names_indices <- lab == ""
          lab[empty_names_indices] <- as.character(choice[empty_names_indices])
        }
      }
      list(label = lab, value = as.character(choice), optgroup = group)
    }, SIMPLIFY = FALSE)
    extract_vector <- function(x, name) {
      vecs <- lapply(x, `[[`, name)
      do.call(c, vecs)
    }
    data.frame(label = extract_vector(choice_list, "label"), value = extract_vector(choice_list, "value"), optgroup = extract_vector(choice_list, "optgroup"), stringsAsFactors = FALSE, row.names = NULL)
  }
  value <- unname(selected)
  attr(choices, "selected_value") <- value
  message <- dropNulls(list(label = label, value = value, url = session$registerDataObj(inputId, choices, selectizeJSON)))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, choices = NULL, selected = NULL, options = list(), server = FALSE) 
{
  validate_session_object(session)
  if (length(options)) {
    res <- checkAsIs(options)
    cfg <- tags$script(type = "application/json", `data-for` = session$ns(inputId), `data-eval` = if (length(res$eval)) 
      HTML(toJSON(res$eval)), HTML(toJSON(res$options)))
    session$sendInputMessage(inputId, list(config = as.character(cfg)))
  }
  if (!server) {
    return(updateSelectInput(session, inputId, label, choices, selected))
  }
  noOptGroup <- TRUE
  if (is.list(choices)) {
    for (i in seq_along(choices)) {
      if (is.list(choices[[i]]) || length(choices[[i]]) > 1) {
        noOptGroup <- FALSE
        (break)()
      }
    }
  }
  choices <- if (is.data.frame(choices)) {
    as.data.frame(choices, stringsAsFactors = FALSE)
  }
  else if (is.atomic(choices) || noOptGroup) {
    if (is.list(choices)) {
      choices <- unlist(choices)
    }
    if (is.null(names(choices))) {
      lab <- as.character(choices)
    }
    else {
      lab <- names(choices)
      empty_names_indices <- lab == ""
      lab[empty_names_indices] <- as.character(choices[empty_names_indices])
    }
    data.frame(label = lab, value = choices, stringsAsFactors = FALSE)
  }
  else {
    list_names <- names(choices)
    if (is.null(list_names)) {
      list_names <- rep("", length(choices))
    }
    choice_list <- mapply(choices, list_names, FUN = function(choice, name) {
      group <- ""
      lab <- name
      if (lab == "") 
        lab <- as.character(choice)
      if (is.list(choice) || length(choice) > 1) {
        group <- rep(name, length(choice))
        choice <- unlist(choice)
        if (is.null(names(choice))) {
          lab <- as.character(choice)
        }
        else {
          lab <- names(choice)
          empty_names_indices <- lab == ""
          lab[empty_names_indices] <- as.character(choice[empty_names_indices])
        }
      }
      list(label = lab, value = as.character(choice), optgroup = group)
    }, SIMPLIFY = FALSE)
    extract_vector <- function(x, name) {
      vecs <- lapply(x, `[[`, name)
      do.call(c, vecs)
    }
    data.frame(label = extract_vector(choice_list, "label"), value = extract_vector(choice_list, "value"), optgroup = extract_vector(choice_list, "optgroup"), stringsAsFactors = FALSE, row.names = NULL)
  }
  value <- unname(selected)
  attr(choices, "selected_value") <- value
  message <- dropNulls(list(label = label, value = value, url = session$registerDataObj(inputId, choices, selectizeJSON)))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateSliderInput
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, min = NULL, max = NULL, step = NULL, timeFormat = NULL, timezone = NULL) 
{
  validate_session_object(session)
  dataType <- getSliderType(min, max, value)
  if (is.null(timeFormat)) {
    timeFormat <- switch(dataType, date = "%F", datetime = "%F %T", number = NULL)
  }
  if (isTRUE(dataType %in% c("date", "datetime"))) {
    to_ms <- function(x) 1000 * as.numeric(as.POSIXct(x))
    if (!is.null(min)) 
      min <- to_ms(min)
    if (!is.null(max)) 
      max <- to_ms(max)
    if (!is.null(value)) 
      value <- to_ms(value)
  }
  message <- dropNulls(list(label = label, value = formatNoSci(value), min = formatNoSci(min), max = formatNoSci(max), step = formatNoSci(step), `data-type` = dataType, `time-format` = timeFormat, timezone = timezone))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, min = NULL, max = NULL, step = NULL, timeFormat = NULL, timezone = NULL) 
{
  validate_session_object(session)
  dataType <- getSliderType(min, max, value)
  if (is.null(timeFormat)) {
    timeFormat <- switch(dataType, date = "%F", datetime = "%F %T", number = NULL)
  }
  if (isTRUE(dataType %in% c("date", "datetime"))) {
    to_ms <- function(x) 1000 * as.numeric(as.POSIXct(x))
    if (!is.null(min)) 
      min <- to_ms(min)
    if (!is.null(max)) 
      max <- to_ms(max)
    if (!is.null(value)) 
      value <- to_ms(value)
  }
  message <- dropNulls(list(label = label, value = formatNoSci(value), min = formatNoSci(min), max = formatNoSci(max), step = formatNoSci(step), `data-type` = dataType, `time-format` = timeFormat, timezone = timezone))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateTabsetPanel
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, selected = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(value = selected))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, selected = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(value = selected))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateTextAreaInput
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, placeholder = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(label = label, value = value, placeholder = placeholder))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, placeholder = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(label = label, value = value, placeholder = placeholder))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateTextInput
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, placeholder = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(label = label, value = value, placeholder = placeholder))
  session$sendInputMessage(inputId, message)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, value = NULL, placeholder = NULL) 
{
  validate_session_object(session)
  message <- dropNulls(list(label = label, value = value, placeholder = placeholder))
  session$sendInputMessage(inputId, message)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateVarSelectInput
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, data = NULL, selected = NULL) 
{
  validate_session_object(session)
  if (is.null(data)) {
    choices <- NULL
  }
  else {
    choices <- colnames(data)
  }
  updateSelectInput(session = session, inputId = inputId, label = label, choices = choices, selected = selected)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, data = NULL, selected = NULL) 
{
  validate_session_object(session)
  if (is.null(data)) {
    choices <- NULL
  }
  else {
    choices <- colnames(data)
  }
  updateSelectInput(session = session, inputId = inputId, label = label, choices = choices, selected = selected)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
updateVarSelectizeInput
list(`package:shiny` = function (session = getDefaultReactiveDomain(), inputId, label = NULL, data = NULL, selected = NULL, options = list(), server = FALSE) 
{
  validate_session_object(session)
  if (is.null(data)) {
    choices <- NULL
  }
  else {
    choices <- colnames(data)
  }
  updateSelectizeInput(session = session, inputId = inputId, label = label, choices = choices, selected = selected, options = options, server = server)
}, function (session = getDefaultReactiveDomain(), inputId, label = NULL, data = NULL, selected = NULL, options = list(), server = FALSE) 
{
  validate_session_object(session)
  if (is.null(data)) {
    choices <- NULL
  }
  else {
    choices <- colnames(data)
  }
  updateSelectizeInput(session = session, inputId = inputId, label = label, choices = choices, selected = selected, options = options, server = server)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
urlModal
list(`package:shiny` = function (url, title = "Bookmarked application link", subtitle = NULL) 
{
  subtitleTag <- tagList(br(), span(class = "text-muted", subtitle), span(id = "shiny-bookmark-copy-text", class = "text-muted"))
  modalDialog(title = title, easyClose = TRUE, tags$textarea(class = "form-control", rows = "1", style = "resize: none;", readonly = "readonly", url), subtitleTag, tags$script("$('#shiny-modal').\n        one('show.bs.modal', function() {\n          setTimeout(function() {\n            var $textarea = $('#shiny-modal textarea');\n            $textarea.innerHeight($textarea[0].scrollHeight);\n          }, 200);\n        });\n      $('#shiny-modal')\n        .one('shown.bs.modal', function() {\n          $('#shiny-modal textarea').select().focus();\n        });\n      $('#shiny-bookmark-copy-text')\n        .text(function() {\n          if (/Mac/i.test(navigator.userAgent)) {\n            return 'Press ⌘-C to copy.';\n          } else {\n            return 'Press Ctrl-C to copy.';\n          }\n        });\n      "))
}, function (url, title = "Bookmarked application link", subtitle = NULL) 
{
  subtitleTag <- tagList(br(), span(class = "text-muted", subtitle), span(id = "shiny-bookmark-copy-text", class = "text-muted"))
  modalDialog(title = title, easyClose = TRUE, tags$textarea(class = "form-control", rows = "1", style = "resize: none;", readonly = "readonly", url), subtitleTag, tags$script("$('#shiny-modal').\n        one('show.bs.modal', function() {\n          setTimeout(function() {\n            var $textarea = $('#shiny-modal textarea');\n            $textarea.innerHeight($textarea[0].scrollHeight);\n          }, 200);\n        });\n      $('#shiny-modal')\n        .one('shown.bs.modal', function() {\n          $('#shiny-modal textarea').select().focus();\n        });\n      $('#shiny-bookmark-copy-text')\n        .text(function() {\n          if (/Mac/i.test(navigator.userAgent)) {\n            return 'Press ⌘-C to copy.';\n          } else {\n            return 'Press Ctrl-C to copy.';\n          }\n        });\n      "))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
validate
list(`package:shiny` = function (..., errorClass = character(0)) 
{
  results <- sapply(list2(...), function(x) {
    if (is.null(x)) 
      return(NA)
    else if (identical(x, FALSE)) 
      return("")
    else if (is.character(x)) 
      return(paste(as.character(x), collapse = "\n"))
    else stop("Unexpected validation result: ", as.character(x))
  })
  results <- stats::na.omit(results)
  if (length(results) == 0) 
    return(invisible())
  results <- results[nzchar(results)]
  reactiveStop(paste(results, collapse = "\n"), c(errorClass, "validation"))
}, function (..., errorClass = character(0)) 
{
  results <- sapply(list2(...), function(x) {
    if (is.null(x)) 
      return(NA)
    else if (identical(x, FALSE)) 
      return("")
    else if (is.character(x)) 
      return(paste(as.character(x), collapse = "\n"))
    else stop("Unexpected validation result: ", as.character(x))
  })
  results <- stats::na.omit(results)
  if (length(results) == 0) 
    return(invisible())
  results <- results[nzchar(results)]
  reactiveStop(paste(results, collapse = "\n"), c(errorClass, "validation"))
}, function (txt) 
{
  stopifnot(is.character(txt))
  txt <- paste(txt, collapse = "\n")
  .Call(R_validate, as.character(txt))
})
c("package:shiny", "namespace:shiny", "namespace:jsonlite")
c(TRUE, FALSE, FALSE)
c(FALSE, TRUE, FALSE)
validateCssUnit
list(`package:shiny` = function (x) 
{
  if (is.null(x) || is.na(x)) 
    return(x)
  if (length(x) > 1 || (!is.character(x) && !is.numeric(x))) 
    stop("CSS units must be a single-element numeric or character vector")
  if (is.character(x) && nchar(x) > 0 && gsub("\\d*", "", x) == "") 
    x <- as.numeric(x)
  pattern <- "^(auto|inherit|fit-content|calc\\(.*\\)|((\\.\\d+)|(\\d+(\\.\\d+)?))(%|in|cm|mm|ch|em|ex|rem|pt|pc|px|vh|vw|vmin|vmax))$"
  if (is.character(x) && !grepl(pattern, x)) {
    stop("\"", x, "\" is not a valid CSS unit (e.g., \"100%\", \"400px\", \"auto\")")
  }
  else if (is.numeric(x)) {
    x <- paste(x, "px", sep = "")
  }
  x
}, function (x) 
{
  if (is.null(x) || is.na(x)) 
    return(x)
  if (length(x) > 1 || (!is.character(x) && !is.numeric(x))) 
    stop("CSS units must be a single-element numeric or character vector")
  if (is.character(x) && nchar(x) > 0 && gsub("\\d*", "", x) == "") 
    x <- as.numeric(x)
  pattern <- "^(auto|inherit|fit-content|calc\\(.*\\)|((\\.\\d+)|(\\d+(\\.\\d+)?))(%|in|cm|mm|ch|em|ex|rem|pt|pc|px|vh|vw|vmin|vmax))$"
  if (is.character(x) && !grepl(pattern, x)) {
    stop("\"", x, "\" is not a valid CSS unit (e.g., \"100%\", \"400px\", \"auto\")")
  }
  else if (is.numeric(x)) {
    x <- paste(x, "px", sep = "")
  }
  x
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
varSelectInput
list(`package:shiny` = function (inputId, label, data, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL) 
{
  choices <- colnames(data)
  selectInputVal <- selectInput(inputId = inputId, label = label, choices = choices, selected = selected, multiple = multiple, selectize = selectize, width = width, size = size)
  selectClass <- selectInputVal$children[[2]]$children[[1]]$attribs$class
  if (is.null(selectClass)) {
    newClass <- "symbol"
  }
  else {
    newClass <- paste(selectClass, "symbol", sep = " ")
  }
  selectInputVal$children[[2]]$children[[1]]$attribs$class <- newClass
  selectInputVal
}, function (inputId, label, data, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL) 
{
  choices <- colnames(data)
  selectInputVal <- selectInput(inputId = inputId, label = label, choices = choices, selected = selected, multiple = multiple, selectize = selectize, width = width, size = size)
  selectClass <- selectInputVal$children[[2]]$children[[1]]$attribs$class
  if (is.null(selectClass)) {
    newClass <- "symbol"
  }
  else {
    newClass <- paste(selectClass, "symbol", sep = " ")
  }
  selectInputVal$children[[2]]$children[[1]]$attribs$class <- newClass
  selectInputVal
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
varSelectizeInput
list(`package:shiny` = function (inputId, ..., options = NULL, width = NULL) 
{
  selectizeIt(inputId, varSelectInput(inputId, ..., selectize = FALSE, width = width), options)
}, function (inputId, ..., options = NULL, width = NULL) 
{
  selectizeIt(inputId, varSelectInput(inputId, ..., selectize = FALSE, width = width), options)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
verbatimTextOutput
list(`package:shiny` = function (outputId, placeholder = FALSE) 
{
  pre(id = outputId, class = "shiny-text-output", class = if (!placeholder) 
    "noplaceholder")
}, function (outputId, placeholder = FALSE) 
{
  pre(id = outputId, class = "shiny-text-output", class = if (!placeholder) 
    "noplaceholder")
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
verticalLayout
list(`package:shiny` = function (..., fluid = TRUE) 
{
  lapply(list2(...), function(row) {
    col <- column(12, row)
    if (fluid) 
      fluidRow(col)
    else fixedRow(col)
  })
}, function (..., fluid = TRUE) 
{
  lapply(list2(...), function(row) {
    col <- column(12, row)
    if (fluid) 
      fluidRow(col)
    else fixedRow(col)
  })
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
wellPanel
list(`package:shiny` = function (...) 
{
  div(class = "well", ...)
}, function (...) 
{
  div(class = "well", ...)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
with_devmode
list(`package:shiny` = function (devmode, code, verbose = getOption("shiny.devmode.verbose", TRUE)) 
{
  withr::with_options(list(shiny.devmode = devmode, shiny.devmode.verbose = verbose), code)
}, function (devmode, code, verbose = getOption("shiny.devmode.verbose", TRUE)) 
{
  withr::with_options(list(shiny.devmode = devmode, shiny.devmode.verbose = verbose), code)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
withLogErrors
list(`package:shiny` = function (expr, full = get_devmode_option("shiny.fullstacktrace", FALSE), offset = getOption("shiny.stacktraceoffset", TRUE)) 
{
  withCallingHandlers({
    result <- captureStackTraces(expr)
    if (promises::is.promise(result)) {
      result <- promises::catch(result, function(cond) {
        if (cnd_inherits(cond, "shiny.silent.error")) {
          return()
        }
        if (isTRUE(getOption("show.error.messages"))) {
          printError(cond, full = full, offset = offset)
        }
      })
    }
    result
  }, error = function(cond) {
    if (cnd_inherits(cond, "shiny.silent.error")) 
      return()
    if (isTRUE(getOption("show.error.messages"))) {
      printError(cond, full = full, offset = offset)
    }
  })
}, function (expr, full = get_devmode_option("shiny.fullstacktrace", FALSE), offset = getOption("shiny.stacktraceoffset", TRUE)) 
{
  withCallingHandlers({
    result <- captureStackTraces(expr)
    if (promises::is.promise(result)) {
      result <- promises::catch(result, function(cond) {
        if (cnd_inherits(cond, "shiny.silent.error")) {
          return()
        }
        if (isTRUE(getOption("show.error.messages"))) {
          printError(cond, full = full, offset = offset)
        }
      })
    }
    result
  }, error = function(cond) {
    if (cnd_inherits(cond, "shiny.silent.error")) 
      return()
    if (isTRUE(getOption("show.error.messages"))) {
      printError(cond, full = full, offset = offset)
    }
  })
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
withMathJax
list(`package:shiny` = function (...) 
{
  path <- paste0(getOption("shiny.mathjax.url", "https://mathjax.rstudio.com/latest/MathJax.js"), "?", getOption("shiny.mathjax.config", "config=TeX-AMS-MML_HTMLorMML"))
  tagList(tags$head(singleton(tags$script(src = path, type = "text/javascript"))), ..., tags$script(HTML("if (window.MathJax) MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")))
}, function (...) 
{
  path <- paste0(getOption("shiny.mathjax.url", "https://mathjax.rstudio.com/latest/MathJax.js"), "?", getOption("shiny.mathjax.config", "config=TeX-AMS-MML_HTMLorMML"))
  tagList(tags$head(singleton(tags$script(src = path, type = "text/javascript"))), ..., tags$script(HTML("if (window.MathJax) MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")))
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
withProgress
list(`package:shiny` = function (expr, min = 0, max = 1, value = min + (max - min) * 0.1, message = NULL, detail = NULL, style = getShinyOption("progress.style", default = "notification"), session = getDefaultReactiveDomain(), env = parent.frame(), quoted = FALSE) 
{
  if (!quoted) 
    expr <- substitute(expr)
  if (is.null(session$progressStack)) 
    stop("'session' is not a ShinySession object.")
  style <- match.arg(style, c("notification", "old"))
  p <- Progress$new(session, min = min, max = max, style = style)
  session$progressStack$push(p)
  on.exit({
    session$progressStack$pop()
    p$close()
  })
  p$set(value, message, detail)
  eval(expr, env)
}, function (expr, min = 0, max = 1, value = min + (max - min) * 0.1, message = NULL, detail = NULL, style = getShinyOption("progress.style", default = "notification"), session = getDefaultReactiveDomain(), env = parent.frame(), quoted = FALSE) 
{
  if (!quoted) 
    expr <- substitute(expr)
  if (is.null(session$progressStack)) 
    stop("'session' is not a ShinySession object.")
  style <- match.arg(style, c("notification", "old"))
  p <- Progress$new(session, min = min, max = max, style = style)
  session$progressStack$push(p)
  on.exit({
    session$progressStack$pop()
    p$close()
  })
  p$set(value, message, detail)
  eval(expr, env)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
withReactiveDomain
list(`package:shiny` = function (domain, expr) 
{
  promises::with_promise_domain(createVarPromiseDomain(.globals, "domain", domain), expr)
}, function (domain, expr) 
{
  promises::with_promise_domain(createVarPromiseDomain(.globals, "domain", domain), expr)
})
c("package:shiny", "namespace:shiny")
c(TRUE, FALSE)
c(FALSE, TRUE)
withTags
list(`package:shiny` = function (code, .noWS = NULL) 
{
  if (!is.null(.noWS)) {
    .noWSWithTags <- .noWS
    tags <- lapply(tags, function(tag) {
      function(..., .noWS = .noWSWithTags) {
        tag(..., .noWS = .noWS)
      }
    })
  }
  eval(substitute(code), envir = as.list(tags), enclos = parent.frame())
}, function (code, .noWS = NULL) 
{
  if (!is.null(.noWS)) {
    .noWSWithTags <- .noWS
    tags <- lapply(tags, function(tag) {
      function(..., .noWS = .noWSWithTags) {
        tag(..., .noWS = .noWS)
      }
    })
  }
  eval(substitute(code), envir = as.list(tags), enclos = parent.frame())
})
c("package:shiny", "namespace:htmltools")
c(TRUE, FALSE)
c(FALSE, TRUE)
