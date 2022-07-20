from pymongo import MongoClient
from dotenv import dotenv_values

config = dotenv_values(".env")


class Mongo:

    def __init__(self):
        connection = "mongodb://{username}:{password}@{host}:{port}/{name}?authSource=admin".format(
            username=config["DATABASE_USERNAME"],
            password=config["DATABASE_PASSWORD"],
            host=config["DATABASE_HOST"],
            port=config["DATABASE_PORT"],
            name=config["DATABASE_NAME"]
        )
        self.client = MongoClient(connection)
        self.database = self.client["tcc"]
        self.topic = self.database["topic"]
        self.comments = self.database["comments"]

    def insert_topic(self, topic):
        topic_id = self.topic.insert_one(topic).inserted_id
        return topic_id

    def insert_comment(self, comments):
        comment_id = self.comments.insert_one(comments).inserted_id
        return comment_id
