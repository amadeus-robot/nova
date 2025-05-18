defmodule WorkFlow.Db do
  @args [
    # 2GB
    {:target_file_size_base, 2 * 1024 * 1024 * 1024},
    {:target_file_size_multiplier, 2}
  ]

  def init() do
    path = Path.join(["./db/"])
    File.mkdir_p!(path)

    {:ok, db_ref, cf_ref_list} =
      :rocksdb.open_optimistic_transaction_db(
        '#{path}',
        [
          {:create_if_missing, true},
          {:create_missing_column_families, true},
          # 2GB
          {:target_file_size_base, 2 * 1024 * 1024 * 1024},
          {:target_file_size_multiplier, 2}
        ],
        [
          {'default', @args}
        ]
      )
  end

  def write() do
  end
end
