FROM debian

RUN apt-get update && apt-get install -y python

RUN echo "#!/usr/bin/python\nprint '123'" > /entrypoint.py
RUN chmod +x /entrypoint.py
CMD ["python", "entrypoint.py"]
