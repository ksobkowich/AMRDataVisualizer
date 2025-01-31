# Use the previous version as the base image (much faster)
FROM ksobkowich/amrdata-visualizer:v1.1

# Copy updated project files into the Docker image
COPY . /srv/shiny-server/

# Set the working directory
WORKDIR /srv/shiny-server/

# If renv.lock has changed, restore the R environment
COPY renv.lock /srv/shiny-server/renv.lock
RUN R -e "renv::restore()"

# Expose the port the app runs on
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host = '0.0.0.0', port = 3838)"]


#Run in terminal
# cd /Users/kurtissobkowich/Git/companion_animal_amr/AMRVisualizerV2/AMRDataVisualizer
# Build the image: docker build -t ksobkowich/amrdata-visualizer:v1.1 . --platform=linux/amd64
# docker run --platform=linux/amd64 -d -p 3838:3838 ksobkowich/amrdata-visualizer:v1.1
# docker push ksobkowich/amrdata-visualizer:v1.1

#Duplicate and tag as latest
#docker tag ksobkowich/amrdata-visualizer:v1.1 ksobkowich/amrdata-visualizer:latest
#docker push ksobkowich/amrdata-visualizer:latest