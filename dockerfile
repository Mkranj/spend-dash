FROM rocker/r-ver:4.3.2

# Install Renv
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Make a directory in the container
RUN mkdir /sp-app/

WORKDIR /sp-app/

RUN mkdir -p renv

# Copy app files
COPY app.R app.R
COPY data_files data_files
COPY env_variables.R env_variables.R
COPY functions functions
COPY modules modules
COPY scripts scripts
COPY ui_definition.R ui_definition.R
COPY www www

COPY renv.lock renv.lock

# Restore used packages
RUN R -e "renv::restore()"

# Expose the application port
EXPOSE 8180

# Run the app
CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = 8180)"]