FROM r-base:3.2.3 

RUN mkdir -p /app

COPY r-scripts/ad-moderation.R /app/

COPY r-scripts/ad-moderation-configs.R /app/

CMD [ "R", "-f", "/app/ad-moderation-configs.R" ]

CMD [ "R", "-f", "/app/ad-moderation.R" ]
