# Use the official R image as the base image
FROM ksobkowich/amrdata-visualizer:v1.6

# Install system dependencies
RUN apt-get update && apt-get install -y \
  git \
  curl \
  wget \
  aria2 \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libv8-dev \
  libxt-dev \
  pandoc \
  pandoc-citeproc \
  libcairo2-dev \
  libxt6 \
  libglu1-mesa-dev \
  libzip-dev \
  libbz2-dev \
  liblzma-dev \
  libpcre2-dev \
  libpng-dev \
  libudunits2-dev \
  libgdal-dev \
  libproj-dev \
  libgeos-dev \
  python3 \
  python3-pip \
  && rm -rf /var/lib/apt/lists/*
  
# Install renv package
RUN R -e "install.packages('renv', repos='https://cran.rstudio.com/')"

# Set environment variables to avoid treating warnings as errors
ENV CXXFLAGS="-Wno-format-security -Wno-error=format-security"

# Install Python packages
RUN pip3 install -U spacy==3.0.0

# Install spacy English model
RUN python3 -m spacy download en_core_web_sm

# Set up RENV paths
ENV RENV_PATHS_ROOT=/srv/shiny-server/renv
ENV RENV_PATHS_LIBRARY=/srv/shiny-server/renv/library

# Copy the project files into the Docker image
COPY . /srv/shiny-server/

# Set the working directory
WORKDIR /srv/shiny-server/

# Restore renv environment
RUN R -e "renv::restore()"

# Install spacyr
RUN R -e "spacyr::spacy_install()"

# Expose the port the app runs on
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 3838)"]

#Run in terminal
# cd /Users/kurtissobkowich/Git/companion_animal_amr/AMRVisualizerV2/AMRDataVisualizer
# Build the image: docker build -t ksobkowich/amrdata-visualizer:v1.6 . --platform=linux/amd64
# docker run --platform=linux/amd64 -d -p 3838:3838 ksobkowich/amrdata-visualizer:v1.6
# docker push ksobkowich/amrdata-visualizer:v1.6

#Duplicate and tag as latest
#docker tag ksobkowich/amrdata-visualizer:v1.6 ksobkowich/amrdata-visualizer:latest
#docker push ksobkowich/amrdata-visualizer:latest






