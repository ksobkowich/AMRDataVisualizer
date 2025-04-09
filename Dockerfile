# Use previous version as the base image
FROM ksobkowich/amrdata-visualizer:v1.6

# Install system and Chrome dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
  git \
  curl \
  wget \
  aria2 \
  gnupg \
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
  libasound2 \
  libatk-bridge2.0-0 \
  libcups2 \
  libdbus-1-3 \
  libgdk-pixbuf2.0-0 \
  libnspr4 \
  libnss3 \
  libxcomposite1 \
  libxdamage1 \
  libxrandr2 \
  libxss1 \
  xdg-utils \
  fonts-liberation \
  python3 \
  python3-pip && \
  rm -rf /var/lib/apt/lists/*

# Install R package: renv
RUN R -e "install.packages('renv', repos='https://cran.rstudio.com/')"

# Set environment variables to avoid treating warnings as errors
ENV CXXFLAGS="-Wno-format-security -Wno-error=format-security"

# Install Python packages and spaCy model
RUN pip3 install -U spacy==3.0.0 && \
    python3 -m spacy download en_core_web_sm

# Set up renv paths
ENV RENV_PATHS_ROOT=/srv/shiny-server/renv
ENV RENV_PATHS_LIBRARY=/srv/shiny-server/renv/library

# Copy the project files into the Docker image
COPY . /srv/shiny-server/
WORKDIR /srv/shiny-server/

# Restore renv environment
RUN R -e "renv::restore()"

# Install spacyr (which wraps and checks spaCy)
RUN R -e "spacyr::spacy_install()"

# Install Google Chrome from the official repo
RUN wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | apt-key add - && \
    echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list && \
    apt-get update && \
    apt-get install -y google-chrome-stable && \
    rm -rf /var/lib/apt/lists/*

# Let R and chromote/webshot2 know where Chrome is
ENV CHROMOTE_CHROME=/usr/bin/google-chrome

# Install Quarto CLI
RUN wget https://quarto.org/download/latest/quarto-linux-amd64.deb && \
    apt-get install -y ./quarto-linux-amd64.deb && \
    rm quarto-linux-amd64.deb

# Expose the port the app runs on
EXPOSE 3838

# Run the Shiny app with CHROMOTE_CHROME set at runtime
CMD ["R", "-e", "Sys.setenv(CHROMOTE_CHROME = '/usr/bin/google-chrome'); shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 3838)"]

#Run in terminal
# cd /Users/kurtissobkowich/Git/companion_animal_amr/AMRVisualizerV2/AMRDataVisualizer
# Build the image: docker build -t ksobkowich/amrdata-visualizer:v1.6 . --platform=linux/amd64
# docker run --platform=linux/amd64 -d -p 3838:3838 ksobkowich/amrdata-visualizer:v1.6
# docker push ksobkowich/amrdata-visualizer:v1.6

#Duplicate and tag as latest
#docker tag ksobkowich/amrdata-visualizer:v1.6 ksobkowich/amrdata-visualizer:latest
#docker push ksobkowich/amrdata-visualizer:latest






