FROM debian:jessie

RUN apt-get update && apt-get install -y --no-install-recommends \
    python

RUN echo "#!/usr/bin/python\nprint '123'" > /entrypoint.py
RUN chmod +x /entrypoint.py
CMD ["python", "entrypoint.py"]

