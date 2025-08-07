# Deployment Instructions for Income Explorer

## Steps to Deploy

1. **Copy these files to a fresh `app` folder:**
   ```
   inst/
   R/
   www/
   server.R
   ui.R
   ```

2. **Run this command:**
   ```R
   shinylive::export(
     "app", 
     "docs", 
     wasm_packages = FALSE, 
     package_cache = FALSE, 
     template_params = list(
       title = "Income Explorer", 
       include_in_head = shiny::includeHTML("www/shinylive-export-header.html")
     )
   )
   ```

3. **Commit all changes in the `docs/` folder.**  
   RStudio can crash when "staging" the changes since there are so many big ones, so it is recommended to use Git Bash to "stage" the changes:
   ```bash
   git add docs/*
   ```

   Create a commit with these staged changes using RStudio or Git Bash.  
   You may want to note the `shinylive` versions used, which you can see using:
   ```R
   shinylive::assets_info()
   ```

4. **Push the changes:**
   ```bash
   git push
   ```

## TODO
Automate deployment using GitHub Actions based on the official example:  
[https://github.com/posit-dev/r-shinylive/tree/main/examples](https://github.com/posit-dev/r-shinylive/tree/main/examples)