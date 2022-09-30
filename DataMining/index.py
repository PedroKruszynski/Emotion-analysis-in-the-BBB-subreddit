import praw
import pandas as pd
import database
from dotenv import dotenv_values

config = dotenv_values(".env")


class RedditApi:

    def __init__(self):
        self.reddit = praw.Reddit(
            client_id=config['CLIENT_ID'],
            client_secret=config['CLIENTE_SECRET'],
            user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.110 Safari/537.36"
        )
        self.subreddit = self.reddit.subreddit("BigBrotherBrasil")
        self.database = database.Mongo()

    def begin_the_mess(self):
        topics = []

        for submission in self.subreddit.top(time_filter="year"):
            topic_id = reddit_api.database.insert_topic({
                "external_id": submission.id,
                "external_title": submission.title,
                "external_created_at": submission.created_utc,
                "permalink": submission.permalink
            })
            submission.comments.replace_more(limit=0)
            for top_level_comment in submission.comments.list():
                reddit_api.database.insert_comment({
                    "topic_id": topic_id,
                    "author": (
                        top_level_comment.author.name
                        if top_level_comment.author is not None
                        else None
                    ),
                    "likes": top_level_comment.likes,
                    "comment": top_level_comment.body
                })

                topics.append([
                    submission.id,
                    submission.title,
                    submission.created_utc,
                    top_level_comment.body
                ])

        for submission in self.subreddit.search(query="DISCUSSÃO DIÁRIA - BBB22", time_filter="year"):
            topic_id = reddit_api.database.insert_topic({
                    "external_id": submission.id,
                    "external_title": submission.title,
                    "external_created_at": submission.created_utc,
                    "permalink": submission.permalink
                })
            submission.comments.replace_more(limit=0)
            for top_level_comment in submission.comments.list():
                reddit_api.database.insert_comment({
                    "topic_id": topic_id,
                    "author": (
                        top_level_comment.author.name
                        if top_level_comment.author is not None
                        else None
                    ),
                    "likes": top_level_comment.likes,
                    "comment": top_level_comment.body
                })

                topics.append([
                    submission.id,
                    submission.title,
                    submission.created_utc,
                    top_level_comment.body
                ])

        topics_frame = pd.DataFrame(topics, columns=["id", "title", "created", "comment"])
        topics_frame.to_csv("{name_csv}.csv".format(
            name_csv=config['NAME_CSV']
        ))


if __name__ == "__main__":
    reddit_api = RedditApi()
    reddit_api.begin_the_mess()
