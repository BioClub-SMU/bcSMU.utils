import pandas as pd
from pypinyin import lazy_pinyin, Style

def py_convert(data, column='herb_cn', mode='full', new_col='Herb_pinyin_name'):
    """
    Convert Chinese characters in a specified column of a DataFrame to Pinyin.
    
    Parameters:
    - data: pd.DataFrame. Input table with at least one Chinese column.
    - column: str. Name of the column to convert.
    - mode: str. 'full' for full Pinyin, 'abbr' for initials only.
    - new_col: str. Name of the output column to store pinyin.
    
    Returns:
    - pd.DataFrame with a new column of Pinyin strings.
    """

    if not isinstance(data, pd.DataFrame):
        raise ValueError("`data` must be a pandas DataFrame.")
    
    if column not in data.columns:
        raise ValueError(f"Column '{column}' not found in the DataFrame.")

    data[column] = data[column].astype(str).str.strip()
    
    ## select specific mode
    if mode == 'full':
        data[new_col] = data[column].apply(lambda x: ''.join(lazy_pinyin(x)))
    elif mode == 'abbr':
        data[new_col] = data[column].apply(lambda x: ''.join(lazy_pinyin(x, style=Style.FIRST_LETTER)))
    else:
        raise ValueError("`mode` must be either 'full' or 'abbr'.")

    return data
